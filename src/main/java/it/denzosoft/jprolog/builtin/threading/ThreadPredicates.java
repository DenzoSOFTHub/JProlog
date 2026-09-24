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
        WITH_MUTEX,
        // ISS-2025-0749/0750 - 4.6 wave Q4.1
        THREAD_SIGNAL, THREAD_STATISTICS, MQ_PROPERTY, MUTEX_PROPERTY,
        POOL_CREATE, POOL_DESTROY, POOL_CREATE_THREAD, POOL_PROPERTY, POOL_CURRENT
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
    /** ISS-2025-0750: JVM thread -> its worker record (mutex_property/2's owner). */
    private static final ConcurrentHashMap<Thread, PThread> BY_JVM = new ConcurrentHashMap<>();
    /** ISS-2025-0750: thread pools by name. */
    private static final ConcurrentHashMap<String, Pool> POOLS = new ConcurrentHashMap<>();

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
        /** ISS-2025-0749: the signal queue thread_signal/2 writes to. */
        final it.denzosoft.jprolog.core.engine.ThreadSignals.Box signals;
        /** ISS-2025-0750: the pool this thread occupies a slot of, or null. */
        volatile Pool pool;

        PThread(int id, String alias, boolean worker) {
            this.id = id;
            this.alias = alias;
            this.worker = worker;
            this.queue = new MQueue(id, null);
            this.signals = worker ? new it.denzosoft.jprolog.core.engine.ThreadSignals.Box()
                                  : it.denzosoft.jprolog.core.engine.ThreadSignals.MAIN;
        }

        Term handle() { return alias != null ? (Term) new Atom(alias) : (Term) Number.valueOf(id); }
    }

    /** A message queue with selective receive. */
    static final class MQueue {
        final int id;
        final String alias;
        private final LinkedList<Term> items = new LinkedList<>();
        private boolean destroyed;
        /** ISS-2025-0750: max_size(N) of message_queue_create/2 (0 = unbounded). */
        int maxSize;
        /** ISS-2025-0750: threads blocked receiving from this queue (message_queue_property/2). */
        int waiting;

        MQueue(int id, String alias) { this.id = id; this.alias = alias; }

        Term handle() { return alias != null ? (Term) new Atom(alias) : (Term) Number.valueOf(id); }

        synchronized void put(Term msg) {
            if (destroyed) throw Errors.existence("message_queue", handle(), "thread_send_message/2");
            items.addLast(msg);
            notifyAll();
        }

        // START_CHANGE: ISS-2025-0750 - a bounded queue: the sender waits for room (SWI), until
        // deadlineNanos; SIGNAL when the calling thread has a signal to run first, null on timeout.
        synchronized Term offer(Term msg, long deadlineNanos, String ctx) throws InterruptedException {
            for (;;) {
                if (destroyed) throw Errors.existence("message_queue", handle(), ctx);
                if (maxSize <= 0 || items.size() < maxSize) {
                    items.addLast(msg);
                    notifyAll();
                    return msg;
                }
                if (it.denzosoft.jprolog.core.engine.ThreadSignals.hasPending()) return SIGNAL;
                if (!waitSlice(this, deadlineNanos)) return null;
            }
        }
        // END_CHANGE: ISS-2025-0750

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
                        if (remove) { it.remove(); notifyAll(); }            // room for a sender
                        return msg;
                    }
                }
                if (deadlineNanos == NO_WAIT) return null;
                // ISS-2025-0749: a signal is run by the caller (outside this lock) before waiting on
                if (it.denzosoft.jprolog.core.engine.ThreadSignals.hasPending()) return SIGNAL;
                waiting++;
                try {
                    if (!waitSlice(this, deadlineNanos)) return null;
                } finally {
                    waiting--;
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

    // START_CHANGE: ISS-2025-0749 - blocking waits are signal-aware (thread_signal/2)
    /** Returned by a wait that stopped because the calling thread has a signal to run. */
    private static final Term SIGNAL = new Atom("$signal");
    /** The longest a blocked thread sleeps before it re-checks its signals and the deadline. */
    private static final long SLICE_NANOS = 100_000_000L;

    /**
     * Wait on {@code mon} (whose lock the caller holds) for at most one slice or until
     * {@code deadlineNanos}; the monitor is published on the thread's signal box so a signal wakes
     * it at once. False when the deadline has passed.
     */
    private static boolean waitSlice(Object mon, long deadlineNanos) throws InterruptedException {
        long left = (deadlineNanos == FOREVER) ? SLICE_NANOS : deadlineNanos - System.nanoTime();
        if (left <= 0) return false;
        it.denzosoft.jprolog.core.engine.ThreadSignals.Box box = it.denzosoft.jprolog.core.engine.ThreadSignals.current();
        box.monitor = mon;
        try {
            TimeUnit.NANOSECONDS.timedWait(mon, Math.min(left, SLICE_NANOS));
        } finally {
            box.monitor = null;
        }
        return true;
    }

    /** Run every signal queued for the calling thread, here, on its own machine. */
    private static void runSignals(SolverContext solver) {
        // every queued signal is taken first and run as ONE conjunction, so a signal that arrives
        // while they run cannot overtake one that was sent before it
        List<Term> sigs = new ArrayList<>();
        for (Term g; (g = it.denzosoft.jprolog.core.engine.ThreadSignals.poll()) != null;) sigs.add(g);
        if (sigs.isEmpty() || solver == null) return;
        Term conj = it.denzosoft.jprolog.core.engine.v4.Machine.signalGoal(sigs.get(sigs.size() - 1));
        for (int i = sigs.size() - 2; i >= 0; i--) {
            conj = new CompoundTerm(new Atom(","), Arrays.asList(
                it.denzosoft.jprolog.core.engine.v4.Machine.signalGoal(sigs.get(i)), conj));
        }
        it.denzosoft.jprolog.core.engine.ThreadSignals.enterHandler();   // no overtaking inside
        try {
            solver.solveMeta(conj, new HashMap<String, Term>(), new ArrayList<Map<String, Term>>());
        } finally {
            it.denzosoft.jprolog.core.engine.ThreadSignals.exitHandler();
        }
    }

    private static long deadlineOf(Term options, String ctx, long dflt) {
        long deadline = dflt;
        for (Term o : options(options, ctx)) {
            if (!(o instanceof CompoundTerm) || ((CompoundTerm) o).getArguments().size() != 1) continue;
            CompoundTerm ct = (CompoundTerm) o;
            Term v = ct.getArguments().get(0);
            if ("timeout".equals(ct.getName())) {
                double sec = number(v, ctx);
                long d = (sec <= 0) ? NO_WAIT : System.nanoTime() + (long) (sec * 1e9);
                deadline = (deadline == FOREVER) ? d : Math.min(deadline, d);
            } else if ("deadline".equals(ct.getName())) {
                double abs = number(v, ctx);
                double left = abs - System.currentTimeMillis() / 1000.0;
                long d = (left <= 0) ? NO_WAIT : System.nanoTime() + (long) (left * 1e9);
                deadline = (deadline == FOREVER) ? d : Math.min(deadline, d);
            }
        }
        return deadline;
    }
    // END_CHANGE: ISS-2025-0749

    /** A recursive mutex. */
    static final class PMutex {
        final int id;
        final String alias;
        final OwnedLock lock = new OwnedLock();
        /** ISS-2025-0750: the holder's recursion count (written only by the holder). */
        volatile int count;

        PMutex(int id, String alias) { this.id = id; this.alias = alias; }

        Term handle() {
            return alias != null ? (Term) new Atom(alias)
                : new CompoundTerm(MUTEX_FUNCTOR, Collections.singletonList((Term) Number.valueOf(id)));
        }
    }

    // START_CHANGE: ISS-2025-0750
    /** A ReentrantLock that tells who holds it (mutex_property/2, the load-cycle check). */
    static final class OwnedLock extends ReentrantLock {
        private static final long serialVersionUID = 1L;
        Thread holder() { return getOwner(); }
    }

    /** A thread pool of library(thread_pool): at most {@code size} running members. */
    static final class Pool {
        final String name;
        final int size;
        final int backlog;          // -1 = unbounded
        final Term options;         // thread_create options every member inherits
        int running;
        int waiting;
        /** The live members' JVM threads (the load-cycle check's view of a pool wait). */
        final Set<Thread> members = ConcurrentHashMap.newKeySet();

        Pool(String name, int size, int backlog, Term options) {
            this.name = name; this.size = size; this.backlog = backlog; this.options = options;
        }
    }
    // END_CHANGE: ISS-2025-0750

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
                case THREAD_JOIN:      return threadJoin(solver, args, bindings, solutions);
                case THREAD_DETACH:    return threadDetach(args, bindings, solutions);
                case THREAD_SELF:      return unify(args[0], self().handle(), bindings, solutions);
                case THREAD_SLEEP:     return threadSleep(solver, args, bindings, solutions);
                case THREAD_IS_ALIVE:  return threadIsAlive(args, bindings, solutions);
                case THREAD_PROPERTY:  return threadProperty(args, bindings, solutions);
                case THREAD_EXIT:      return threadExit(args);
                case MQ_CREATE:        return mqCreate(args, bindings, solutions);
                case MQ_DESTROY:       return mqDestroy(args, bindings, solutions);
                case MQ_SEND:          return mqSend(solver, args, bindings, solutions);
                case MQ_GET:           return mqGet(solver, args, bindings, solutions);
                case MQ_PEEK:          return mqPeek(args, bindings, solutions);
                case MUTEX_CREATE:     return mutexCreate(args, bindings, solutions);
                case MUTEX_DESTROY:    return mutexDestroy(args, bindings, solutions);
                case MUTEX_LOCK:       return mutexLock(solver, args, bindings, solutions);
                case MUTEX_TRYLOCK:    return mutexTrylock(args, bindings, solutions);
                case MUTEX_UNLOCK:     return mutexUnlock(args, bindings, solutions);
                case MUTEX_UNLOCK_ALL: return mutexUnlockAll(bindings, solutions);
                case WITH_MUTEX:       return withMutex(solver, args, bindings, solutions);
                case THREAD_SIGNAL:    return threadSignal(solver, args, bindings, solutions);
                case THREAD_STATISTICS: return threadStatistics(solver, args, bindings, solutions);
                case MQ_PROPERTY:      return mqProperty(args, bindings, solutions);
                case MUTEX_PROPERTY:   return mutexProperty(args, bindings, solutions);
                case POOL_CREATE:      return poolCreate(args, bindings, solutions);
                case POOL_DESTROY:     return poolDestroy(args, bindings, solutions);
                case POOL_CREATE_THREAD: return poolCreateThread(solver, args, bindings, solutions);
                case POOL_PROPERTY:    return poolProperty(args, bindings, solutions);
                case POOL_CURRENT:     return poolCurrent(args, bindings, solutions);
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
        CreateOptions co = new CreateOptions();
        if (args.length == 3) co.parse(args[2], ctx);
        PThread pt = startThread(solver, goal, co, null, ctx);
        return unify(args[1], pt.handle(), bindings, solutions);
    }

    // START_CHANGE: ISS-2025-0750 - thread_create/3's options, shared with thread_create_in_pool/4
    private static final class CreateOptions {
        String alias;
        boolean detached;
        Term atExit;

        void parse(Term list, String ctx) {
            for (Term o : options(list, ctx)) {
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
    }

    /** Create and start a worker thread running {@code goal}; {@code pool} is its pool or null. */
    private static PThread startThread(final SolverContext solver, final Term goal, CreateOptions co,
                                       final Pool pool, String ctx) {
        final PThread pt = new PThread(IDS.incrementAndGet(), co.alias, true);
        pt.detached = co.detached;
        pt.pool = pool;
        if (co.alias != null && THREAD_ALIASES.putIfAbsent(co.alias, pt) != null) {
            throw Errors.permission("create", "thread", new Atom(co.alias), ctx);
        }
        THREADS.put(pt.id, pt);

        final Term fAtExit = co.atExit;
        Thread t = new Thread(new Runnable() {
            @Override public void run() {
                SELF.set(pt);
                BY_JVM.put(Thread.currentThread(), pt);
                if (pool != null) pool.members.add(Thread.currentThread());
                it.denzosoft.jprolog.core.engine.ThreadSignals.bind(pt.signals);    // ISS-2025-0749
                it.denzosoft.jprolog.core.engine.ThreadWaits.enterThread();         // ISS-2025-0746
                Term status = new Atom("false");
                try {
                    status = runGoal(solver, goal);
                    if (fAtExit != null) {
                        try { runGoal(solver, fAtExit); } catch (RuntimeException ignored) { /* SWI ignores it */ }
                    }
                    // START_CHANGE: ISS-2025-0751 - 4.6 wave Q4.2: a mutex the thread still holds
                    // is released AND reported (SWI prints a warning), never silently.
                    for (Term mx : releaseMutexesOfThisThread()) {
                        warn(solver, "Thread ~p exited while holding mutex ~p (released)",
                             pt.handle(), mx);
                    }
                    // END_CHANGE: ISS-2025-0751
                } finally {
                    releaseMutexesOfThisThread();
                    it.denzosoft.jprolog.core.engine.ThreadSignals.discard(pt.signals);
                    it.denzosoft.jprolog.core.engine.ThreadSignals.bind(null);
                    it.denzosoft.jprolog.core.engine.ThreadWaits.exitThread();
                    BY_JVM.remove(Thread.currentThread(), pt);
                    SELF.remove();
                    if (pool != null) {
                        pool.members.remove(Thread.currentThread());
                        synchronized (pool) { pool.running--; pool.notifyAll(); }
                    }
                }
                finish(pt, status);
            }
        }, "prolog-thread-" + pt.id);
        t.setDaemon(true);
        pt.thread = t;
        t.start();
        return pt;
    }

    /** print_message(warning, format(Fmt, Args)) on this thread, best effort. */
    private static void warn(SolverContext solver, String fmt, Term... args) {
        if (solver == null) return;
        try {
            Term msg = new CompoundTerm(new Atom("format"), Arrays.asList(
                (Term) new Atom(fmt), it.denzosoft.jprolog.core.utils.CollectionUtils.createListTerm(Arrays.asList(args))));
            runGoal(solver, new CompoundTerm(new Atom("print_message"), Arrays.asList((Term) new Atom("warning"), msg)));
        } catch (RuntimeException ignored) {
            // a warning must not change the thread's exit status
        }
    }
    // END_CHANGE: ISS-2025-0750

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
        // START_CHANGE: ISS-2025-0698 - wave Q1.3: the join status carries an ISO error TERM.
        // Inside the query that ran out of budget the limit stays a control exception that no
        // catch/3 can see (the trust model); the join status is read by ANOTHER query, outside the
        // exhausted one, so reporting it as a term is safe and lets the parent match it with
        // exception(error(resource_error(inference_limit), _)). The context keeps the old atom.
        } catch (InferenceLimitException ile) {
            return struct("exception", it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.resourceError(
                "inference_limit", "inference_limit_exceeded"));
        } catch (QueryCancelledException qce) {
            return new Atom("cancelled");
        } catch (Throwable e) {
            return struct("exception", it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.systemError(
                e.getClass().getSimpleName() + (e.getMessage() == null ? "" : ": " + e.getMessage()), "thread"));
        }
        // END_CHANGE: ISS-2025-0698
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

    private boolean threadJoin(SolverContext solver, Term[] args, Map<String, Term> bindings,
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
                // no timeout (SWI); interruptible; ISS-2025-0749: signals are run while waiting
                while (t.isAlive()) {
                    runSignals(solver);
                    t.join(SLICE_NANOS / 1_000_000L);
                }
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

    private boolean threadSleep(SolverContext solver, Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws InterruptedException {
        double seconds = number(args[0], "thread_sleep/1");
        if (seconds > 0) {
            // ISS-2025-0749: a sleeping thread runs its signals as they arrive
            long deadline = System.nanoTime() + (long) (seconds * 1e9);
            Object mon = new Object();
            for (;;) {
                runSignals(solver);
                synchronized (mon) {
                    if (it.denzosoft.jprolog.core.engine.ThreadSignals.hasPending()) continue;
                    if (!waitSlice(mon, deadline)) break;
                }
                if (System.nanoTime() - deadline >= 0) break;
            }
            runSignals(solver);
        }
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
        int maxSize = 0;
        if (args.length == 2) {
            for (Term o : options(args[1], ctx)) {
                if (o instanceof CompoundTerm && "alias".equals(((CompoundTerm) o).getName())
                        && ((CompoundTerm) o).getArguments().size() == 1) {
                    Term v = ((CompoundTerm) o).getArguments().get(0);
                    if (v instanceof Variable) throw Errors.instantiation(ctx);
                    if (!(v instanceof Atom)) throw Errors.type("atom", v, ctx);
                    alias = ((Atom) v).getName();
                } else if (o instanceof CompoundTerm && "max_size".equals(((CompoundTerm) o).getName())
                        && ((CompoundTerm) o).getArguments().size() == 1) {           // ISS-2025-0750
                    Term v = ((CompoundTerm) o).getArguments().get(0);
                    if (v instanceof Variable) throw Errors.instantiation(ctx);
                    if (!(v instanceof Number) || !((Number) v).isInteger()) throw Errors.type("integer", v, ctx);
                    if (((Number) v).longValue() < 1) throw Errors.domain("not_less_than_one", v, ctx);
                    maxSize = (int) Math.min(Integer.MAX_VALUE, ((Number) v).longValue());
                }
            }
        }
        if (!(args[0] instanceof Variable)) throw Errors.uninstantiation(args[0], ctx);
        MQueue q = new MQueue(IDS.incrementAndGet(), alias);
        q.maxSize = maxSize;
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

    private boolean mqSend(SolverContext solver, Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws InterruptedException {
        String ctx = "thread_send_message/" + args.length;
        MQueue q = queue(args[0], ctx);
        // ISS-2025-0750: thread_send_message/3 takes timeout(T) / deadline(D) for a full queue
        long deadline = (args.length == 3) ? deadlineOf(args[2], ctx, FOREVER) : FOREVER;
        // ISS-2025-0479: a message is a TERM and it is COPIED (design B.13)
        Term msg = detach(args[1]);
        for (;;) {
            Term r;
            it.denzosoft.jprolog.core.engine.ThreadWaits.enterMessageWait();   // ISS-2025-0746
            try {
                r = q.offer(msg, deadline, ctx);
            } finally {
                it.denzosoft.jprolog.core.engine.ThreadWaits.exitMessageWait();
            }
            if (r == SIGNAL) { runSignals(solver); continue; }
            if (r == null) return false;                               // timeout: fail (SWI)
            break;
        }
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    // START_CHANGE: ISS-2025-0629 - selective receive; no fixed timeout; thread_get_message/3
    private boolean mqGet(SolverContext solver, Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws InterruptedException {
        String ctx = "thread_get_message/" + args.length;
        MQueue q = (args.length == 1) ? self().queue : queue(args[0], ctx);
        Term pattern = (args.length == 1) ? args[0] : args[1];
        long deadline = (args.length == 3) ? deadlineOf(args[2], ctx, FOREVER) : FOREVER;
        Term msg;
        for (;;) {
            // ISS-2025-0746: the load-cycle check sees a thread blocked on a message
            it.denzosoft.jprolog.core.engine.ThreadWaits.enterMessageWait();
            try {
                msg = q.take(pattern, true, deadline, ctx);
            } finally {
                it.denzosoft.jprolog.core.engine.ThreadWaits.exitMessageWait();
            }
            if (msg == SIGNAL) { runSignals(solver); continue; }   // ISS-2025-0749
            break;
        }
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

    private boolean mutexLock(SolverContext solver, Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws InterruptedException {
        acquire(solver, mutex(args[0], true, "mutex_lock/1"));
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    // START_CHANGE: ISS-2025-0746/0749 - a mutex wait is visible to the load-cycle check and runs
    // the thread's signals while it waits.
    private static void acquire(SolverContext solver, final PMutex mx) throws InterruptedException {
        if (!mx.lock.tryLock()) {
            it.denzosoft.jprolog.core.engine.ThreadWaits.enterLockWait(new java.util.function.Supplier<Thread>() {
                @Override public Thread get() { return mx.lock.holder(); }
            });
            try {
                do {
                    runSignals(solver);
                } while (!mx.lock.tryLock(SLICE_NANOS, TimeUnit.NANOSECONDS));
            } finally {
                it.denzosoft.jprolog.core.engine.ThreadWaits.exitLockWait();
            }
        }
        mx.count = mx.lock.getHoldCount();                                  // ISS-2025-0750
    }

    private static void release(PMutex mx) {
        mx.lock.unlock();
        mx.count = mx.lock.getHoldCount();                                  // ISS-2025-0750
    }
    // END_CHANGE: ISS-2025-0746/0749

    private boolean mutexTrylock(Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        PMutex mx = mutex(args[0], true, "mutex_trylock/1");
        if (!mx.lock.tryLock()) return false;
        mx.count = mx.lock.getHoldCount();                                  // ISS-2025-0750
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    private boolean mutexUnlock(Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        PMutex mx = mutex(args[0], false, "mutex_unlock/1");
        if (!mx.lock.isHeldByCurrentThread()) {
            throw Errors.permission("unlock", "mutex", args[0], "mutex_unlock/1");
        }
        release(mx);
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    private boolean mutexUnlockAll(Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        releaseMutexesOfThisThread();
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    /** Release every mutex the calling thread holds; returns their handles (ISS-2025-0751). */
    private static List<Term> releaseMutexesOfThisThread() {
        List<Term> held = new ArrayList<>();
        for (PMutex mx : new TreeMap<Integer, PMutex>(MUTEXES).values()) {
            if (mx.lock.isHeldByCurrentThread()) held.add(mx.handle());
            while (mx.lock.isHeldByCurrentThread()) release(mx);
        }
        return held;
    }

    /** with_mutex(+Mutex, :Goal): once(Goal) holding Mutex; released however Goal ends. */
    private boolean withMutex(SolverContext solver, Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws InterruptedException {
        String ctx = "with_mutex/2";
        Term goal = callable(args[1], ctx);
        PMutex mx = mutex(args[0], true, ctx);
        if (solver == null) throw Errors.existence("procedure", Errors.pi("with_mutex", 2), ctx);
        acquire(solver, mx);                                                // ISS-2025-0746/0749
        try {
            List<Map<String, Term>> sols = new ArrayList<>();
            Term once = new CompoundTerm(new Atom("once"), Collections.singletonList(goal));
            solver.solveMeta(once, new HashMap<String, Term>(bindings), sols);
            if (sols.isEmpty()) return false;
            solutions.add(sols.get(0));
            return true;
        } finally {
            release(mx);
        }
    }
    // END_CHANGE: ISS-2025-0631

    // ================================================================ 4.6 wave Q4.1

    // START_CHANGE: ISS-2025-0749 - thread_signal(+Thread, :Goal)
    /**
     * Queue {@code Goal} for {@code Thread}, which runs it at its next inference (or at once, when
     * it is blocked in a thread built-in) on its own goal stack. Signalling oneself runs the goal
     * now. {@code existence_error(thread, T)} for an unknown or finished thread.
     */
    private boolean threadSignal(SolverContext solver, Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        String ctx = "thread_signal/2";
        Term goal = callable(args[1], ctx);
        PThread pt = thread(args[0], ctx);
        if (pt == self()) {
            if (solver != null) {
                solver.solveMeta(it.denzosoft.jprolog.core.engine.v4.Machine.signalGoal(goal),
                    new HashMap<String, Term>(bindings), new ArrayList<Map<String, Term>>());
            }
        } else {
            synchronized (pt) {
                if (pt.status != null || pt.reclaimed || !pt.signals.live) {
                    throw Errors.existence("thread", args[0], ctx);
                }
            }
            it.denzosoft.jprolog.core.engine.ThreadSignals.send(pt.signals, detach(goal));
        }
        solutions.add(new HashMap<>(bindings));
        return true;
    }
    // END_CHANGE: ISS-2025-0749

    // START_CHANGE: ISS-2025-0750 - thread_statistics/3, message_queue_property/2,
    // mutex_property/2 and library(thread_pool)
    /**
     * {@code thread_statistics(+Thread, ?Key, -Value)}: {@code statistics/2} for another thread.
     * The per-thread keys ({@code cputime}, {@code runtime}, {@code inferences},
     * {@code thread_cputime}) are measured on the target; the process-wide ones are answered by
     * {@code statistics/2}.
     */
    private boolean threadStatistics(SolverContext solver, Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        String ctx = "thread_statistics/3";
        PThread pt = thread(args[0], ctx);
        Term key = args[1];
        if (key instanceof Variable) throw Errors.instantiation(ctx);
        if (!(key instanceof Atom)) throw Errors.domain("statistics_key", key, ctx);
        String k = ((Atom) key).getName();
        boolean perThread = "cputime".equals(k) || "runtime".equals(k) || "inferences".equals(k)
            || "thread_cputime".equals(k);
        if (pt == self() || !perThread) {
            if (solver == null) return false;
            List<Map<String, Term>> sols = new ArrayList<>();
            Term q = new CompoundTerm(new Atom("statistics"), Arrays.asList(key, args[2]));
            solver.solveMeta(q, new HashMap<String, Term>(bindings), sols);
            if (sols.isEmpty()) return false;
            solutions.add(sols.get(0));
            return true;
        }
        Thread t = pt.worker ? pt.thread : pt.signals.thread;
        Term value;
        if ("inferences".equals(k)) {
            it.denzosoft.jprolog.core.engine.ResourceGuard g = pt.signals.guard;
            value = Number.valueOf(g == null ? 0L : g.getSteps());
        } else {
            long nanos = 0L;
            java.lang.management.ThreadMXBean mx = java.lang.management.ManagementFactory.getThreadMXBean();
            if (t != null && t.isAlive() && mx.isThreadCpuTimeSupported()) {
                @SuppressWarnings("deprecation") long tid = t.getId();
                long v = mx.getThreadCpuTime(tid);
                if (v > 0) nanos = v;
            }
            if ("runtime".equals(k)) {
                value = CollectionUtilsBridge.list(Number.valueOf(nanos / 1_000_000L), Number.valueOf(0L));
            } else {
                value = new Number(nanos / 1e9);
            }
        }
        return unify(args[2], value, bindings, solutions);
    }

    /** Tiny list builder (keeps the imports of this file unchanged). */
    private static final class CollectionUtilsBridge {
        static Term list(Term... xs) {
            return it.denzosoft.jprolog.core.utils.CollectionUtils.createListTerm(Arrays.asList(xs));
        }
    }

    /** {@code message_queue_property(?Queue, ?Property)}: alias/1, size/1, max_size/1, waiting/1. */
    private boolean mqProperty(Term[] args, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        String ctx = "message_queue_property/2";
        Term prop = args[1];
        if (!(prop instanceof Variable) && !(prop instanceof CompoundTerm)) {
            throw Errors.domain("message_queue_property", prop, ctx);
        }
        List<MQueue> which = new ArrayList<>();
        if (args[0] instanceof Variable) which.addAll(new TreeMap<Integer, MQueue>(QUEUES).values());
        else which.add(queue(args[0], ctx));
        boolean any = false;
        for (MQueue q : which) {
            List<Term> props = new ArrayList<>();
            if (q.alias != null) props.add(struct("alias", new Atom(q.alias)));
            int size, waiting;
            synchronized (q) { size = q.size(); waiting = q.waiting; }
            props.add(struct("size", Number.valueOf(size)));
            if (q.maxSize > 0) props.add(struct("max_size", Number.valueOf(q.maxSize)));
            props.add(struct("waiting", Number.valueOf(waiting)));
            for (Term p : props) {
                Map<String, Term> nb = new HashMap<>(bindings);
                if (args[0].unify(q.handle(), nb) && prop.resolveBindings(nb).unify(p, nb)) {
                    solutions.add(nb);
                    any = true;
                }
            }
        }
        return any;
    }

    /** {@code mutex_property(?Mutex, ?Property)}: alias/1, status(unlocked | locked(Owner, Count)). */
    private boolean mutexProperty(Term[] args, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        String ctx = "mutex_property/2";
        Term prop = args[1];
        if (!(prop instanceof Variable) && !(prop instanceof CompoundTerm)) {
            throw Errors.domain("mutex_property", prop, ctx);
        }
        List<PMutex> which = new ArrayList<>();
        if (args[0] instanceof Variable) which.addAll(new TreeMap<Integer, PMutex>(MUTEXES).values());
        else which.add(mutex(args[0], false, ctx));
        boolean any = false;
        for (PMutex mx : which) {
            List<Term> props = new ArrayList<>();
            if (mx.alias != null) props.add(struct("alias", new Atom(mx.alias)));
            Thread holder = mx.lock.holder();
            Term status;
            if (holder == null) {
                status = new Atom("unlocked");
            } else {
                PThread owner = BY_JVM.get(holder);
                Term oh = (owner != null) ? owner.handle() : MAIN.handle();
                int c = Math.max(1, mx.count);
                status = new CompoundTerm(new Atom("locked"), Arrays.asList(oh, (Term) Number.valueOf(c)));
            }
            props.add(struct("status", status));
            for (Term p : props) {
                Map<String, Term> nb = new HashMap<>(bindings);
                if (args[0].unify(mx.handle(), nb) && prop.resolveBindings(nb).unify(p, nb)) {
                    solutions.add(nb);
                    any = true;
                }
            }
        }
        return any;
    }

    /** {@code thread_pool_create(+Pool, +Size, +Options)}: options backlog(N) + thread_create/3's. */
    private boolean poolCreate(Term[] args, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        String ctx = "thread_pool_create/3";
        String name = atomName(args[0], ctx);
        Term sz = args[1];
        if (sz instanceof Variable) throw Errors.instantiation(ctx);
        if (!(sz instanceof Number) || !((Number) sz).isInteger()) throw Errors.type("integer", sz, ctx);
        if (((Number) sz).longValue() < 1) throw Errors.domain("not_less_than_one", sz, ctx);
        int backlog = -1;
        List<Term> keep = new ArrayList<>();
        for (Term o : options(args[2], ctx)) {
            if (o instanceof CompoundTerm && "backlog".equals(((CompoundTerm) o).getName())
                    && ((CompoundTerm) o).getArguments().size() == 1) {
                Term v = ((CompoundTerm) o).getArguments().get(0);
                if (v instanceof Atom && "infinite".equals(((Atom) v).getName())) { backlog = -1; continue; }
                if (v instanceof Variable) throw Errors.instantiation(ctx);
                if (!(v instanceof Number) || !((Number) v).isInteger()) throw Errors.type("integer", v, ctx);
                backlog = (int) Math.max(0, Math.min(Integer.MAX_VALUE, ((Number) v).longValue()));
            } else {
                keep.add(o);
            }
        }
        new CreateOptions().parse(args[2], ctx);                 // validate the thread options now
        Pool pool = new Pool(name, (int) Math.min(Integer.MAX_VALUE, ((Number) sz).longValue()), backlog,
            detach(it.denzosoft.jprolog.core.utils.CollectionUtils.createListTerm(keep)));
        if (POOLS.putIfAbsent(name, pool) != null) {
            throw Errors.permission("create", "thread_pool", args[0], ctx);
        }
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    private boolean poolDestroy(Term[] args, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        String ctx = "thread_pool_destroy/1";
        String name = atomName(args[0], ctx);
        Pool pool = POOLS.remove(name);
        if (pool == null) throw Errors.existence("thread_pool", args[0], ctx);
        synchronized (pool) { pool.notifyAll(); }                 // waiting creators see it is gone
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    /**
     * {@code thread_create_in_pool(+Pool, :Goal, -Id, +Options)}: a thread that occupies one of the
     * pool's slots until it ends. With the pool full, {@code wait(true)} (the default) blocks until a
     * member ends; {@code wait(false)} — or a full backlog — raises
     * {@code resource_error(threads_in_pool(Pool))}.
     */
    private boolean poolCreateThread(SolverContext solver, Term[] args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws InterruptedException {
        String ctx = "thread_create_in_pool/4";
        String name = atomName(args[0], ctx);
        Term goal = callable(args[1], ctx);
        Pool pool = POOLS.get(name);
        if (pool == null) throw Errors.existence("thread_pool", args[0], ctx);
        boolean wait = true;
        List<Term> own = new ArrayList<>();
        for (Term o : options(args[3], ctx)) {
            if (o instanceof CompoundTerm && "wait".equals(((CompoundTerm) o).getName())
                    && ((CompoundTerm) o).getArguments().size() == 1) {
                wait = bool(((CompoundTerm) o).getArguments().get(0), ctx);
            } else {
                own.add(o);
            }
        }
        CreateOptions co = new CreateOptions();
        co.parse(pool.options, ctx);
        co.parse(it.denzosoft.jprolog.core.utils.CollectionUtils.createListTerm(own), ctx);   // call options win
        Term full = new CompoundTerm(new Atom("resource_error"), Collections.singletonList(
            struct("threads_in_pool", new Atom(name))));
        final Pool fpool = pool;
        it.denzosoft.jprolog.core.engine.ThreadWaits.enterAnyWait(                // ISS-2025-0746
            new java.util.function.Supplier<java.util.Collection<Thread>>() {
                @Override public java.util.Collection<Thread> get() { return new ArrayList<Thread>(fpool.members); }
            });
        try {
        for (;;) {
            synchronized (pool) {
                if (POOLS.get(name) != pool) throw Errors.existence("thread_pool", args[0], ctx);
                if (pool.running < pool.size) { pool.running++; break; }
                if (!wait || (pool.backlog >= 0 && pool.waiting >= pool.backlog)) {
                    throw Errors.error(full, "thread_create_in_pool", 4, null);
                }
                pool.waiting++;
                try {
                    waitSlice(pool, FOREVER);
                } finally {
                    pool.waiting--;
                }
            }
            runSignals(solver);
        }
        } finally {
            it.denzosoft.jprolog.core.engine.ThreadWaits.exitAnyWait();
        }
        PThread pt;
        try {
            pt = startThread(solver, goal, co, pool, ctx);
        } catch (RuntimeException e) {
            synchronized (pool) { pool.running--; pool.notifyAll(); }
            throw e;
        }
        return unify(args[2], pt.handle(), bindings, solutions);
    }

    /** {@code thread_pool_property(?Pool, ?Property)}: size/1, running/1, free/1, backlog/1, options/1. */
    private boolean poolProperty(Term[] args, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        String ctx = "thread_pool_property/2";
        List<Pool> which = new ArrayList<>();
        if (args[0] instanceof Variable) which.addAll(new TreeMap<String, Pool>(POOLS).values());
        else {
            Pool p = POOLS.get(atomName(args[0], ctx));
            if (p == null) throw Errors.existence("thread_pool", args[0], ctx);
            which.add(p);
        }
        boolean any = false;
        for (Pool p : which) {
            int running, waiting;
            synchronized (p) { running = p.running; waiting = p.waiting; }
            List<Term> props = Arrays.asList(
                struct("size", Number.valueOf(p.size)),
                struct("running", Number.valueOf(running)),
                struct("free", Number.valueOf(Math.max(0, p.size - running))),
                struct("backlog", Number.valueOf(waiting)),
                struct("options", p.options));
            for (Term pr : props) {
                Map<String, Term> nb = new HashMap<>(bindings);
                if (args[0].unify(new Atom(p.name), nb) && args[1].resolveBindings(nb).unify(pr, nb)) {
                    solutions.add(nb);
                    any = true;
                }
            }
        }
        return any;
    }

    private boolean poolCurrent(Term[] args, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        boolean any = false;
        for (String n : new TreeMap<String, Pool>(POOLS).keySet()) {
            Map<String, Term> nb = new HashMap<>(bindings);
            if (args[0].unify(new Atom(n), nb)) { solutions.add(nb); any = true; }
        }
        return any;
    }

    private static String atomName(Term t, String ctx) {
        if (t instanceof Variable) throw Errors.instantiation(ctx);
        if (!(t instanceof Atom)) throw Errors.type("atom", t, ctx);
        return ((Atom) t).getName();
    }
    // END_CHANGE: ISS-2025-0750

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
