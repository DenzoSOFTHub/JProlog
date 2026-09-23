package it.denzosoft.jprolog.builtin.threading;

// START_CHANGE: ISS-2025-0119 - Threading built-in predicates (thread-safe)
import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.InferenceLimitException;
import it.denzosoft.jprolog.core.engine.QueryCancelledException;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.engine.ThreadExitException;
import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Threads, message queues and mutexes (SWI-Prolog's library(threads) subset).
 *
 * <p>START_CHANGE: ISS-2025-0479 — engine v4 wave W8, design B.13: a thread's goal runs on a
 * <b>fresh {@code Machine} over the same {@code Engine}</b> ({@code core.engine.v4.Workers}), with a
 * {@code copy_term}'d goal, and every message is copied on the way in and on the way out, so no
 * {@code Variable} cell is ever shared between two machines. END_CHANGE: ISS-2025-0479
 *
 * <p>START_CHANGE: ISS-2025-0620..0634 — 4.5 wave P6.5, rewritten on SWI's semantics:
 * <ul>
 *   <li><b>Errors are ISO terms</b> built with {@code core.engine.v4.Errors}:
 *       {@code existence_error(thread, Id)} for an unknown (or already reclaimed) thread,
 *       {@code existence_error(message_queue, Q)}, {@code existence_error(mutex, M)},
 *       {@code permission_error(create, thread, Alias)} for a duplicate alias,
 *       {@code permission_error(join, thread, Id)} for a detached thread or oneself,
 *       {@code permission_error(unlock, mutex, M)}, and the instantiation / type errors. They were
 *       message atoms that {@code catch(G, error(E, _), R)} could not match.</li>
 *   <li><b>{@code thread_detach/1}</b> on a live thread (detached or not) succeeds; on a thread that
 *       finished but was never joined it reclaims it; on an unknown or reclaimed one it raises
 *       {@code existence_error(thread, Id)}. The decision is taken under the thread record's lock,
 *       so it cannot race with the thread finishing (the flaky
 *       {@code testISS0479_ThreadCreate3Options}).</li>
 *   <li><b>No fixed timeouts</b>: {@code thread_join/2} and {@code thread_get_message/1,2} block
 *       until they can answer (SWI); they are interruptible, so a Stop still cancels them
 *       ({@code QueryCancelledException}). {@code thread_get_message/3} takes {@code timeout(T)} /
 *       {@code deadline(D)} and FAILS when it expires.</li>
 *   <li><b>Selective receive</b>: {@code thread_get_message(Q, b(X))} takes the first message that
 *       UNIFIES with the pattern, leaving the others queued in order, and blocks until one
 *       arrives; {@code thread_peek_message/1,2} look the same way without removing.</li>
 *   <li>New: {@code thread_join/1}, {@code thread_exit/1}, {@code thread_property/2},
 *       {@code message_queue_create/2} ({@code alias/1}), {@code message_queue_destroy/1},
 *       {@code mutex_create/1,2}, {@code mutex_destroy/1}, {@code mutex_lock/1},
 *       {@code mutex_trylock/1}, {@code mutex_unlock/1}, {@code mutex_unlock_all/0},
 *       {@code with_mutex/2}, and the {@code at_exit(Goal)} option of {@code thread_create/3}.</li>
 *   <li><b>Identity</b>: every thread that is not a {@code thread_create/2,3} worker — the JVM
 *       thread that runs the CLI, an IDE background solve, an embedder thread, a JUnit
 *       {@code @Test(timeout)} body — is the Prolog thread {@code main} (id 1) and shares its one
 *       message queue. The alias used to be claimed by whichever such thread touched the queues
 *       first, which made two tests order-dependent (a live JUnit thread from an earlier class held
 *       it). Ids of threads, queues and mutexes come from ONE counter, so a thread id can never
 *       be mistaken for a queue id.</li>
 * </ul>
 * END_CHANGE: ISS-2025-0620..0634
 */
public class ThreadPredicates implements BuiltInWithContext {

    public enum Mode {
        THREAD_CREATE, THREAD_JOIN, THREAD_DETACH, THREAD_SELF,
        THREAD_SLEEP, THREAD_IS_ALIVE,
        MQ_CREATE, MQ_SEND, MQ_GET, MQ_PEEK,
        // ISS-2025-0630..0632
        MQ_DESTROY, THREAD_PROPERTY, THREAD_EXIT,
        MUTEX_CREATE, MUTEX_DESTROY, MUTEX_LOCK, MUTEX_TRYLOCK, MUTEX_UNLOCK, MUTEX_UNLOCK_ALL,
        WITH_MUTEX
    }

    private final Mode mode;

    // ------------------------------------------------------------------ the process-wide tables

    /** One counter for thread, queue and mutex ids (1 is `main`). */
    private static final AtomicInteger IDS = new AtomicInteger(1);

    private static final ConcurrentHashMap<Integer, PThread> THREADS = new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<String, PThread> THREAD_ALIASES = new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Integer, MQueue> QUEUES = new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<String, MQueue> QUEUE_ALIASES = new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Integer, PMutex> MUTEXES = new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<String, PMutex> MUTEX_ALIASES = new ConcurrentHashMap<>();
    private static final ThreadLocal<PThread> SELF = new ThreadLocal<>();

    private static final String MAIN_ALIAS = "main";
    /** Every non-worker thread is `main` (see the class comment). */
    private static final PThread MAIN = new PThread(1, MAIN_ALIAS, false);
    static {
        THREADS.put(1, MAIN);
        THREAD_ALIASES.put(MAIN_ALIAS, MAIN);
    }

    private static final Atom MUTEX_FUNCTOR = new Atom("$mutex");

    /** A Prolog thread. Its fields that change are only written under the record's own lock. */
    static final class PThread {
        final int id;
        final String alias;
        final boolean worker;
        final MQueue queue;
        volatile Thread thread;
        /** null while running; true / false / exception(E) / exited(T) afterwards. */
        volatile Term status;
        boolean detached;
        /** Removed from the tables (joined, or a detached thread that finished). */
        boolean reclaimed;

        PThread(int id, String alias, boolean worker) {
            this.id = id;
            this.alias = alias;
            this.worker = worker;
            this.queue = new MQueue(id, null);
        }

        Term handle() { return alias != null ? (Term) new Atom(alias) : (Term) Number.valueOf(id); }
    }

    /** A message queue with selective receive. */
    static final class MQueue {
        final int id;
        final String alias;
        private final LinkedList<Term> items = new LinkedList<>();
        private boolean destroyed;

        MQueue(int id, String alias) { this.id = id; this.alias = alias; }

        Term handle() { return alias != null ? (Term) new Atom(alias) : (Term) Number.valueOf(id); }

        synchronized void put(Term msg) {
            if (destroyed) throw Errors.existence("message_queue", handle(), "thread_send_message/2");
            items.addLast(msg);
            notifyAll();
        }

        /**
         * The first queued message that unifies with {@code pattern} — removed when
         * {@code remove} — waiting until {@code deadlineNanos} (NO_WAIT: do not wait, FOREVER: no
         * limit). Returns null when the deadline passes.
         */
        synchronized Term take(Term pattern, boolean remove, long deadlineNanos, String ctx)
                throws InterruptedException {
            for (;;) {
                if (destroyed) throw Errors.existence("message_queue", handle(), ctx);
                for (Iterator<Term> it = items.iterator(); it.hasNext();) {
                    Term msg = it.next();
                    if (pattern.unify(msg, new HashMap<String, Term>())) {
                        if (remove) it.remove();
                        return msg;
                    }
                }
                if (deadlineNanos == NO_WAIT) return null;
                if (deadlineNanos == FOREVER) {
                    wait();
                } else {
                    long left = deadlineNanos - System.nanoTime();
                    if (left <= 0) return null;
                    TimeUnit.NANOSECONDS.timedWait(this, left);
                }
            }
        }

        synchronized void destroy() {
            destroyed = true;
            items.clear();
            notifyAll();
        }

        synchronized int size() { return items.size(); }
    }

    private static final long NO_WAIT = Long.MIN_VALUE;
    private static final long FOREVER = Long.MAX_VALUE;

    /** A recursive mutex. */
    static final class PMutex {
        final int id;
        final String alias;
        final ReentrantLock lock = new ReentrantLock();

        PMutex(int id, String alias) { this.id = id; this.alias = alias; }

        Term handle() {
            return alias != null ? (Term) new Atom(alias)
                : new CompoundTerm(MUTEX_FUNCTOR, Collections.singletonList((Term) Number.valueOf(id)));
        }
    }

    public ThreadPredicates(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        return run(null, query, bindings, solutions);
    }

    @Override
    public boolean executeWithContext(SolverContext solver, Term query,
                                      Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        return run(solver, query, bindings, solutions);
    }

    private boolean run(SolverContext solver, Term query, Map<String, Term> bindings,
                        List<Map<String, Term>> solutions) {
        List<Term> a = query.getArguments();
        int n = (a == null) ? 0 : a.size();
        Term[] args = new Term[n];
        for (int i = 0; i < n; i++) args[i] = a.get(i).resolveBindings(bindings);
        try {
            switch (mode) {
                case THREAD_CREATE:    return threadCreate(solver, args, bindings, solutions);
                case THREAD_JOIN:      return threadJoin(args, bindings, solutions);
                case THREAD_DETACH:    return threadDetach(args, bindings, solutions);
                case THREAD_SELF:      return unify(args[0], self().handle(), bindings, solutions);
                case THREAD_SLEEP:     return threadSleep(args, bindings, solutions);
                case THREAD_IS_ALIVE:  return threadIsAlive(args, bindings, solutions);
                case THREAD_PROPERTY:  return threadProperty(args, bindings, solutions);
                case THREAD_EXIT:      return threadExit(args);
                case MQ_CREATE:        return mqCreate(args, bindings, solutions);
                case MQ_DESTROY:       return mqDestroy(args, bindings, solutions);
                case MQ_SEND:          return mqSend(args, bindings, solutions);
                case MQ_GET:           return mqGet(args, bindings, solutions);
                case MQ_PEEK:          return mqPeek(args, bindings, solutions);
                case MUTEX_CREATE:     return mutexCreate(args, bindings, solutions);
                case MUTEX_DESTROY:    return mutexDestroy(args, bindings, solutions);
                case MUTEX_LOCK:       return mutexLock(args, bindings, solutions);
                case MUTEX_TRYLOCK:    return mutexTrylock(args, bindings, solutions);
                case MUTEX_UNLOCK:     return mutexUnlock(args, bindings, solutions);
                case MUTEX_UNLOCK_ALL: return mutexUnlockAll(bindings, solutions);
                case WITH_MUTEX:       return withMutex(solver, args, bindings, solutions);
                default: return false;
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new QueryCancelledException();                           // ISS-2025-0479
        }
    }

    // ================================================================ thread_create/2,3

    private boolean threadCreate(final SolverContext solver, Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        final String ctx = "thread_create/" + args.length;
        final Term goal = callable(args[0], ctx);
        String alias = null;
        boolean detached = false;
        Term atExit = null;
        if (args.length == 3) {
            for (Term o : options(args[2], ctx)) {
                if (!(o instanceof CompoundTerm) || ((CompoundTerm) o).getArguments().size() != 1) continue;
                CompoundTerm ct = (CompoundTerm) o;
                Term v = ct.getArguments().get(0);
                if ("alias".equals(ct.getName())) {
                    if (v instanceof Variable) throw Errors.instantiation(ctx);
                    if (!(v instanceof Atom)) throw Errors.type("atom", v, ctx);
                    alias = ((Atom) v).getName();
                } else if ("detached".equals(ct.getName())) {
                    detached = bool(v, ctx);
                } else if ("at_exit".equals(ct.getName())) {
                    atExit = callable(v, ctx);
                }
                // any other option (stack sizes, priority, ...) is accepted and ignored, as in SWI
            }
        }

        final PThread pt = new PThread(IDS.incrementAndGet(), alias, true);
        pt.detached = detached;
        if (alias != null && THREAD_ALIASES.putIfAbsent(alias, pt) != null) {
            throw Errors.permission("create", "thread", new Atom(alias), ctx);
        }
        THREADS.put(pt.id, pt);

        final Term fAtExit = atExit;
        Thread t = new Thread(new Runnable() {
            @Override public void run() {
                SELF.set(pt);
                Term status;
                try {
                    status = runGoal(solver, goal);
                    if (fAtExit != null) {
                        try { runGoal(solver, fAtExit); } catch (RuntimeException ignored) { /* SWI ignores it */ }
                    }
                } finally {
                    releaseMutexesOfThisThread();
                    SELF.remove();
                }
                finish(pt, status);
            }
        }, "prolog-thread-" + pt.id);
        t.setDaemon(true);
        pt.thread = t;
        t.start();
        return unify(args[1], pt.handle(), bindings, solutions);
    }

    /** Run a goal on a worker machine on THIS thread and turn the outcome into a join status. */
    private static Term runGoal(SolverContext solver, Term goal) {
        try {
            List<Map<String, Term>> sols = new ArrayList<>();
            boolean ok = (solver != null) && solver.solveInWorker(goal, new HashMap<String, Term>(), sols, 1);
            return new Atom((ok && !sols.isEmpty()) ? "true" : "false");
        } catch (ThreadExitException te) {                                  // ISS-2025-0632
            return struct("exited", te.getTerm());
        } catch (PrologException pe) {
            Term ball = pe.getErrorTerm();
            return struct("exception", ball != null ? detach(ball) : new Atom(String.valueOf(pe.getMessage())));
        // ISS-2025-0479: the control exceptions are REPORTED — the worker's top level is the analogue
        // of the embedder catching them, and the join status is how the parent learns about it.
        } catch (InferenceLimitException ile) {
            return struct("exception", new Atom("inference_limit_exceeded"));
        } catch (QueryCancelledException qce) {
            return new Atom("cancelled");
        } catch (Throwable e) {
            return struct("exception", new Atom(e.getClass().getSimpleName()
                + (e.getMessage() == null ? "" : ": " + e.getMessage())));
        }
    }

    /** The thread's goal is done: record the status; a detached thread is reclaimed at once. */
    private static void finish(PThread pt, Term status) {
        synchronized (pt) {
            pt.status = status;
            if (pt.detached) reclaim(pt);
        }
    }

    /** Remove a thread from the tables. Caller holds the record's lock. */
    private static void reclaim(PThread pt) {
        if (pt.reclaimed) return;
        pt.reclaimed = true;
        THREADS.remove(pt.id, pt);
        if (pt.alias != null) THREAD_ALIASES.remove(pt.alias, pt);
        pt.queue.destroy();
    }

    // ================================================================ join / detach / exit

    private boolean threadJoin(Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws InterruptedException {
        final String ctx = "thread_join/" + args.length;
        PThread pt = thread(args[0], ctx);
        synchronized (pt) {
            // SWI: "Cannot join detached thread" / "Cannot join self" — permission errors
            if (pt.detached || pt == self() || !pt.worker) {
                throw Errors.permission("join", "thread", args[0], ctx);
            }
        }
        Thread t = pt.thread;
        if (t != null) {
            // ISS-2025-0639: the load lock lets the joined thread load in our place while we wait
            it.denzosoft.jprolog.core.engine.ThreadWaits.enterJoin(t);
            try {
                t.join();                         // no timeout (SWI); interruptible
            } finally {
                it.denzosoft.jprolog.core.engine.ThreadWaits.exitJoin();
            }
        }
        Term status;
        synchronized (pt) {
            if (pt.reclaimed) throw Errors.existence("thread", args[0], ctx);   // joined by another
            status = pt.status;
            reclaim(pt);
        }
        if (status == null) status = new Atom("false");
        if (args.length == 1) {
            // ISS-2025-0631: thread_join/1 succeeds on `true`, raises thread_error(Id, Status) else
            if (status instanceof Atom && "true".equals(((Atom) status).getName())) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            throw new PrologException(new CompoundTerm(new Atom("error"), Arrays.asList(
                new CompoundTerm(new Atom("thread_error"), Arrays.asList(args[0], status)),
                (Term) new Variable())));
        }
        return unify(args[1], status, bindings, solutions);
    }

    // START_CHANGE: ISS-2025-0620 - thread_detach/1 semantics (SWI) and no race with the thread's end
    private boolean threadDetach(Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        PThread pt = thread(args[0], "thread_detach/1");
        synchronized (pt) {
            if (pt.reclaimed) throw Errors.existence("thread", args[0], "thread_detach/1");
            if (!pt.worker) throw Errors.permission("detach", "thread", args[0], "thread_detach/1");
            if (pt.status != null) reclaim(pt);   // finished, never joined: reclaim it now
            else pt.detached = true;              // live (detached or not): reclaimed when it ends
        }
        solutions.add(new HashMap<>(bindings));
        return true;
    }
    // END_CHANGE: ISS-2025-0620

    private boolean threadExit(Term[] args) {
        PThread me = self();
        if (!me.worker) throw Errors.permission("exit", "thread", me.handle(), "thread_exit/1");
        throw new ThreadExitException(detach(args[0]));                     // ISS-2025-0632
    }

    private boolean threadSleep(Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws InterruptedException {
        double seconds = number(args[0], "thread_sleep/1");
        if (seconds > 0) Thread.sleep((long) (seconds * 1000), (int) ((seconds * 1e9) % 1000000));
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    private boolean threadIsAlive(Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        PThread pt = lookupThread(args[0], "thread_is_alive/1");
        if (pt == null || pt.status != null || pt.reclaimed) return false;
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    // ================================================================ thread_property/2

    // START_CHANGE: ISS-2025-0630 - thread_property(?Id, ?Property): id/1, alias/1, status/1, detached/1
    private boolean threadProperty(Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        List<PThread> which = new ArrayList<>();
        if (args[0] instanceof Variable) {
            which.addAll(new TreeMap<Integer, PThread>(THREADS).values());
        } else {
            which.add(thread(args[0], "thread_property/2"));
        }
        Term prop = args[1];
        if (!(prop instanceof Variable) && !(prop instanceof CompoundTerm)) {
            throw Errors.domain("thread_property", prop, "thread_property/2");
        }
        boolean any = false;
        for (PThread pt : which) {
            List<Term> props = new ArrayList<>();
            props.add(struct("id", Number.valueOf(pt.id)));
            if (pt.alias != null) props.add(struct("alias", new Atom(pt.alias)));
            Term st = pt.status;
            props.add(struct("status", st == null ? new Atom("running") : st));
            boolean det;
            synchronized (pt) { det = pt.detached; }
            props.add(struct("detached", new Atom(det ? "true" : "false")));
            for (Term p : props) {
                Map<String, Term> nb = new HashMap<>(bindings);
                if (args[0].unify(pt.handle(), nb) && prop.resolveBindings(nb).unify(p, nb)) {
                    solutions.add(nb);
                    any = true;
                }
            }
        }
        return any;
    }
    // END_CHANGE: ISS-2025-0630

    // ================================================================ message queues

    private boolean mqCreate(Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        String ctx = "message_queue_create/" + args.length;
        String alias = null;
        if (args.length == 2) {
            for (Term o : options(args[1], ctx)) {
                if (o instanceof CompoundTerm && "alias".equals(((CompoundTerm) o).getName())
                        && ((CompoundTerm) o).getArguments().size() == 1) {
                    Term v = ((CompoundTerm) o).getArguments().get(0);
                    if (v instanceof Variable) throw Errors.instantiation(ctx);
                    if (!(v instanceof Atom)) throw Errors.type("atom", v, ctx);
                    alias = ((Atom) v).getName();
                }
            }
        }
        if (!(args[0] instanceof Variable)) throw Errors.uninstantiation(args[0], ctx);
        MQueue q = new MQueue(IDS.incrementAndGet(), alias);
        if (alias != null && (THREAD_ALIASES.containsKey(alias) || QUEUE_ALIASES.putIfAbsent(alias, q) != null)) {
            throw Errors.permission("create", "message_queue", new Atom(alias), ctx);
        }
        QUEUES.put(q.id, q);
        return unify(args[0], q.handle(), bindings, solutions);
    }

    // START_CHANGE: ISS-2025-0630 - message_queue_destroy/1
    private boolean mqDestroy(Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        String ctx = "message_queue_destroy/1";
        if (args[0] instanceof Variable) throw Errors.instantiation(ctx);
        MQueue q = null;
        if (args[0] instanceof Number && ((Number) args[0]).isInteger()) q = QUEUES.get((int) ((Number) args[0]).longValue());
        else if (args[0] instanceof Atom) q = QUEUE_ALIASES.get(((Atom) args[0]).getName());
        if (q == null) throw Errors.existence("message_queue", args[0], ctx);
        QUEUES.remove(q.id, q);
        if (q.alias != null) QUEUE_ALIASES.remove(q.alias, q);
        q.destroy();
        solutions.add(new HashMap<>(bindings));
        return true;
    }
    // END_CHANGE: ISS-2025-0630

    private boolean mqSend(Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        MQueue q = queue(args[0], "thread_send_message/2");
        // ISS-2025-0479: a message is a TERM and it is COPIED (design B.13)
        q.put(detach(args[1]));
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    // START_CHANGE: ISS-2025-0629 - selective receive; no fixed timeout; thread_get_message/3
    private boolean mqGet(Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws InterruptedException {
        String ctx = "thread_get_message/" + args.length;
        MQueue q = (args.length == 1) ? self().queue : queue(args[0], ctx);
        Term pattern = (args.length == 1) ? args[0] : args[1];
        long deadline = FOREVER;
        if (args.length == 3) {
            for (Term o : options(args[2], ctx)) {
                if (!(o instanceof CompoundTerm) || ((CompoundTerm) o).getArguments().size() != 1) continue;
                CompoundTerm ct = (CompoundTerm) o;
                Term v = ct.getArguments().get(0);
                if ("timeout".equals(ct.getName())) {
                    double s = number(v, ctx);
                    long d = (s <= 0) ? NO_WAIT : System.nanoTime() + (long) (s * 1e9);
                    deadline = (deadline == FOREVER) ? d : Math.min(deadline, d);
                } else if ("deadline".equals(ct.getName())) {
                    double abs = number(v, ctx);
                    double left = abs - System.currentTimeMillis() / 1000.0;
                    long d = (left <= 0) ? NO_WAIT : System.nanoTime() + (long) (left * 1e9);
                    deadline = (deadline == FOREVER) ? d : Math.min(deadline, d);
                }
            }
        }
        Term msg = q.take(pattern, true, deadline, ctx);
        if (msg == null) return false;                              // timeout: fail (SWI)
        return unify(pattern, msg, bindings, solutions);
    }

    private boolean mqPeek(Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws InterruptedException {
        String ctx = "thread_peek_message/" + args.length;
        MQueue q = (args.length == 1) ? self().queue : queue(args[0], ctx);
        Term pattern = (args.length == 1) ? args[0] : args[1];
        Term msg = q.take(pattern, false, NO_WAIT, ctx);
        if (msg == null) return false;
        return unify(pattern, detach(msg), bindings, solutions);
    }
    // END_CHANGE: ISS-2025-0629

    // ================================================================ mutexes

    // START_CHANGE: ISS-2025-0631 - mutex_create/1,2, mutex_destroy/1, mutex_lock/1,
    // mutex_trylock/1, mutex_unlock/1, mutex_unlock_all/0, with_mutex/2 (SWI: recursive mutexes;
    // an atom names a mutex that is created on first use by mutex_lock/1 and with_mutex/2).
    private boolean mutexCreate(Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        String ctx = "mutex_create/" + args.length;
        String alias = null;
        if (args[0] instanceof Atom) alias = ((Atom) args[0]).getName();
        else if (!(args[0] instanceof Variable)) throw Errors.uninstantiation(args[0], ctx);
        if (args.length == 2) {
            for (Term o : options(args[1], ctx)) {
                if (o instanceof CompoundTerm && "alias".equals(((CompoundTerm) o).getName())
                        && ((CompoundTerm) o).getArguments().size() == 1) {
                    Term v = ((CompoundTerm) o).getArguments().get(0);
                    if (v instanceof Variable) throw Errors.instantiation(ctx);
                    if (!(v instanceof Atom)) throw Errors.type("atom", v, ctx);
                    alias = ((Atom) v).getName();
                }
            }
        }
        PMutex mx = new PMutex(IDS.incrementAndGet(), alias);
        if (alias != null && MUTEX_ALIASES.putIfAbsent(alias, mx) != null) {
            throw Errors.permission("create", "mutex", new Atom(alias), ctx);
        }
        MUTEXES.put(mx.id, mx);
        return unify(args[0], mx.handle(), bindings, solutions);
    }

    private boolean mutexDestroy(Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        PMutex mx = mutex(args[0], false, "mutex_destroy/1");
        MUTEXES.remove(mx.id, mx);
        if (mx.alias != null) MUTEX_ALIASES.remove(mx.alias, mx);
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    private boolean mutexLock(Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws InterruptedException {
        mutex(args[0], true, "mutex_lock/1").lock.lockInterruptibly();
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    private boolean mutexTrylock(Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        if (!mutex(args[0], true, "mutex_trylock/1").lock.tryLock()) return false;
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    private boolean mutexUnlock(Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        PMutex mx = mutex(args[0], false, "mutex_unlock/1");
        if (!mx.lock.isHeldByCurrentThread()) {
            throw Errors.permission("unlock", "mutex", args[0], "mutex_unlock/1");
        }
        mx.lock.unlock();
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    private boolean mutexUnlockAll(Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        releaseMutexesOfThisThread();
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    private static void releaseMutexesOfThisThread() {
        for (PMutex mx : MUTEXES.values()) {
            while (mx.lock.isHeldByCurrentThread()) mx.lock.unlock();
        }
    }

    /** with_mutex(+Mutex, :Goal): once(Goal) holding Mutex; released however Goal ends. */
    private boolean withMutex(SolverContext solver, Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws InterruptedException {
        String ctx = "with_mutex/2";
        Term goal = callable(args[1], ctx);
        PMutex mx = mutex(args[0], true, ctx);
        if (solver == null) throw Errors.existence("procedure", Errors.pi("with_mutex", 2), ctx);
        mx.lock.lockInterruptibly();
        try {
            List<Map<String, Term>> sols = new ArrayList<>();
            Term once = new CompoundTerm(new Atom("once"), Collections.singletonList(goal));
            solver.solveMeta(once, new HashMap<String, Term>(bindings), sols);
            if (sols.isEmpty()) return false;
            solutions.add(sols.get(0));
            return true;
        } finally {
            mx.lock.unlock();
        }
    }
    // END_CHANGE: ISS-2025-0631

    // ================================================================ lookups and helpers

    /** This thread's Prolog thread: a worker's own record, or `main` for every other thread. */
    private static PThread self() {
        PThread pt = SELF.get();
        return (pt != null) ? pt : MAIN;
    }

    private static PThread lookupThread(Term t, String ctx) {
        if (t instanceof Variable) throw Errors.instantiation(ctx);
        if (t instanceof Number && ((Number) t).isInteger()) return THREADS.get((int) ((Number) t).longValue());
        if (t instanceof Atom) return THREAD_ALIASES.get(((Atom) t).getName());
        throw Errors.type("thread", t, ctx);
    }

    /** The thread named by an id or an alias; existence_error(thread, T) when there is none. */
    private static PThread thread(Term t, String ctx) {
        PThread pt = lookupThread(t, ctx);
        if (pt == null) throw Errors.existence("thread", t, ctx);
        return pt;
    }

    /** The queue named by a queue id / alias or by a thread id / alias. */
    private static MQueue queue(Term t, String ctx) {
        if (t instanceof Variable) throw Errors.instantiation(ctx);
        if (t instanceof Number && ((Number) t).isInteger()) {
            int n = (int) ((Number) t).longValue();
            MQueue q = QUEUES.get(n);
            if (q != null) return q;
            PThread pt = THREADS.get(n);
            if (pt != null) return pt.queue;
        } else if (t instanceof Atom) {
            String name = ((Atom) t).getName();
            MQueue q = QUEUE_ALIASES.get(name);
            if (q != null) return q;
            PThread pt = THREAD_ALIASES.get(name);
            if (pt != null) return pt.queue;
        } else {
            throw Errors.type("message_queue", t, ctx);
        }
        throw Errors.existence("message_queue", t, ctx);
    }

    private static PMutex mutex(Term t, boolean createAtom, String ctx) {
        if (t instanceof Variable) throw Errors.instantiation(ctx);
        PMutex mx = null;
        if (t instanceof Atom) {
            String name = ((Atom) t).getName();
            mx = MUTEX_ALIASES.get(name);
            if (mx == null && createAtom) {
                PMutex fresh = new PMutex(IDS.incrementAndGet(), name);
                mx = MUTEX_ALIASES.putIfAbsent(name, fresh);
                if (mx == null) { mx = fresh; MUTEXES.put(fresh.id, fresh); }
            }
        } else if (t instanceof CompoundTerm && MUTEX_FUNCTOR.getName().equals(((CompoundTerm) t).getName())
                && ((CompoundTerm) t).getArguments().size() == 1
                && ((CompoundTerm) t).getArguments().get(0) instanceof Number) {
            mx = MUTEXES.get((int) ((Number) ((CompoundTerm) t).getArguments().get(0)).longValue());
        } else {
            throw Errors.type("mutex", t, ctx);
        }
        if (mx == null) throw Errors.existence("mutex", t, ctx);
        return mx;
    }

    private static Term callable(Term g, String ctx) {
        if (g instanceof Variable) throw Errors.instantiation(ctx);
        if (!(g instanceof Atom) && !(g instanceof CompoundTerm)) throw Errors.type("callable", g, ctx);
        return g;
    }

    private static boolean bool(Term v, String ctx) {
        if (v instanceof Variable) throw Errors.instantiation(ctx);
        if (v instanceof Atom) {
            String s = ((Atom) v).getName();
            if ("true".equals(s)) return true;
            if ("false".equals(s)) return false;
        }
        throw Errors.type("bool", v, ctx);
    }

    private static double number(Term v, String ctx) {
        if (v instanceof Variable) throw Errors.instantiation(ctx);
        if (!(v instanceof Number)) throw Errors.type("number", v, ctx);
        return ((Number) v).doubleValue();
    }

    /** A proper option list; instantiation / type errors otherwise. */
    private static List<Term> options(Term t, String ctx) {
        List<Term> out = new ArrayList<>();
        Term cur = t;
        while (cur instanceof CompoundTerm && ".".equals(((CompoundTerm) cur).getName())
                && ((CompoundTerm) cur).getArguments().size() == 2) {
            Term o = ((CompoundTerm) cur).getArguments().get(0);
            if (o instanceof Variable) throw Errors.instantiation(ctx);
            out.add(o);
            cur = ((CompoundTerm) cur).getArguments().get(1);
        }
        if (cur instanceof Variable) throw Errors.instantiation(ctx);
        if (!(cur instanceof Atom) || !"[]".equals(((Atom) cur).getName())) throw Errors.type("list", t, ctx);
        return out;
    }

    private static Term struct(String name, Term arg) {
        return new CompoundTerm(new Atom(name), Collections.singletonList(arg));
    }

    /**
     * A copy that shares no {@code Variable} cell with the sender (ISS-2025-0479). On the v4 engine
     * a bound cell IS the binding, so a message put on a queue would otherwise be un-bound by the
     * sender's backtracking while the receiver reads it.
     */
    static Term detach(Term t) {
        return it.denzosoft.jprolog.core.engine.v4.Unify.copy(
            it.denzosoft.jprolog.core.engine.v4.Unify.resolve(t, null),
            new IdentityHashMap<Variable, Variable>(), null);
    }

    private static boolean unify(Term target, Term value, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        Map<String, Term> nb = new HashMap<>(bindings);
        if (target.resolveBindings(bindings).unify(value, nb)) { solutions.add(nb); return true; }
        return false;
    }

    /** Test hook: the number of threads the tables still hold (main included). */
    static int threadTableSize() { return THREADS.size(); }
}
// END_CHANGE: ISS-2025-0119
