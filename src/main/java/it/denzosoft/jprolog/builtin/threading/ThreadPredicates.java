package it.denzosoft.jprolog.builtin.threading;

// START_CHANGE: ISS-2025-0119 - Threading built-in predicates (thread-safe)
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Threading predicates (SWI-like thread support):
 *   thread_create/2    - thread_create(:Goal, -ThreadId)
 *   thread_create/3    - thread_create(:Goal, -ThreadId, +Options)   alias/1, detached/1
 *   thread_join/2      - thread_join(+ThreadId, -Status)             true | false | exception(E)
 *   thread_detach/1    - thread_detach(+ThreadId)
 *   thread_self/1      - thread_self(-ThreadId)
 *   thread_sleep/1     - thread_sleep(+Seconds)
 *   thread_is_alive/1  - thread_is_alive(+ThreadId)
 *   message_queue_create/1 - message_queue_create(-QueueId)
 *   thread_send_message/2  - thread_send_message(+QueueOrThread, +Term)
 *   thread_get_message/1   - thread_get_message(-Term)   from THIS thread's own queue
 *   thread_get_message/2   - thread_get_message(+QueueOrThread, -Term)  blocks up to 30s
 *   thread_peek_message/2  - thread_peek_message(+QueueOrThread, -Term) non-blocking
 *
 * <p>START_CHANGE: ISS-2025-0479 — engine v4 wave W8, design B.13. Until this wave
 * {@code thread_create/2} did <b>not run the goal at all</b>: it started a thread that slept 10 ms
 * and recorded {@code completed(<goal atom>)}. The goal now really runs, on a
 * <b>fresh {@code Machine} over the same {@code Engine}</b> ({@code core.engine.v4.Workers}) —
 * a shared clause store (thread-safe by generations), shared flags and operators, its own current
 * streams, its own {@code ResourceGuard} with the parent's inference budget, and a
 * {@code copy_term}'d goal so no {@code Variable} cell is shared between the two machines. On the
 * v2 and legacy engines the goal runs through {@code SolverContext.solveInWorker}'s default, i.e. the
 * shared recursive solver — the behaviour those engines have always had for
 * {@code concurrent_maplist/N} (LIM-024, closed on v4 only).
 *
 * <p>Message queues carry <b>terms</b>, not atoms, and every message is copied on the way in and on
 * the way out for the same reason. Every Prolog thread owns a queue, so
 * {@code thread_send_message/2} accepts a queue id, a thread id or a thread alias.
 *
 * <p>END_CHANGE: ISS-2025-0479
 *
 * Thread safety:
 * - All shared state uses ConcurrentHashMap and AtomicInteger
 * - Thread registration happens before thread start (no race on THREADS map)
 * - thread_join has a 60-second timeout to prevent permanent deadlocks
 * - thread_get_message has a 30-second timeout to prevent permanent blocks
 * - thread_detach marks thread as detached without calling setDaemon (which is illegal on running threads)
 * - THREAD_STATUS is cleaned up on join to prevent memory leaks
 * - InterruptedException is properly propagated via Thread.currentThread().interrupt()
 */
public class ThreadPredicates implements BuiltInWithContext {

    public enum Mode {
        THREAD_CREATE, THREAD_JOIN, THREAD_DETACH, THREAD_SELF,
        THREAD_SLEEP, THREAD_IS_ALIVE,
        MQ_CREATE, MQ_SEND, MQ_GET, MQ_PEEK
    }

    private final Mode mode;

    private static final AtomicInteger THREAD_COUNTER = new AtomicInteger(0);
    private static final AtomicInteger QUEUE_COUNTER = new AtomicInteger(0);
    private static final ConcurrentHashMap<Integer, Thread> THREADS = new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Integer, Term> THREAD_STATUS = new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Integer, Boolean> DETACHED = new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Integer, BlockingQueue<Term>> MESSAGE_QUEUES = new ConcurrentHashMap<>();
    // ISS-2025-0479: alias -> thread id, thread id -> its own message queue id, Java thread -> id
    private static final ConcurrentHashMap<String, Integer> ALIASES = new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<Integer, Integer> THREAD_QUEUE = new ConcurrentHashMap<>();
    private static final ThreadLocal<Integer> SELF = new ThreadLocal<>();

    private static final long JOIN_TIMEOUT_MS = 60_000;
    private static final long MQ_GET_TIMEOUT_MS = 30_000;

    public ThreadPredicates(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        return run(null, query, bindings, solutions);
    }

    // ISS-2025-0479: thread_create/2,3 needs the solver to run the goal on a worker machine.
    @Override
    public boolean executeWithContext(SolverContext solver, Term query,
                                      Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        return run(solver, query, bindings, solutions);
    }

    private boolean run(SolverContext solver, Term query, Map<String, Term> bindings,
                        List<Map<String, Term>> solutions) {
        try {
            switch (mode) {
                case THREAD_CREATE:   return doThreadCreate(solver, query, bindings, solutions);
                case THREAD_JOIN:     return doThreadJoin(query, bindings, solutions);
                case THREAD_DETACH:   return doThreadDetach(query, bindings, solutions);
                case THREAD_SELF:     return doThreadSelf(query, bindings, solutions);
                case THREAD_SLEEP:    return doThreadSleep(query, bindings, solutions);
                case THREAD_IS_ALIVE: return doThreadIsAlive(query, bindings, solutions);
                case MQ_CREATE:       return doMqCreate(query, bindings, solutions);
                case MQ_SEND:         return doMqSend(query, bindings, solutions);
                case MQ_GET:          return doMqGet(query, bindings, solutions);
                case MQ_PEEK:         return doMqPeek(query, bindings, solutions);
                default: return false;
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new it.denzosoft.jprolog.core.engine.QueryCancelledException();   // ISS-2025-0479
        } catch (PrologEvaluationException e) {
            throw e;
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw new PrologEvaluationException(modeName() + ": " + e.getMessage());
        }
    }

    // ================================================================ thread_create/2,3

    private boolean doThreadCreate(final SolverContext solver, Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args == null || (args.size() != 2 && args.size() != 3)) {
            throw new PrologEvaluationException("thread_create requires 2 or 3 arguments.");
        }
        final Term goal = args.get(0).resolveBindings(bindings);
        if (goal instanceof Variable) {
            throw new PrologEvaluationException("thread_create/2: goal is unbound.");
        }
        String alias = null;
        boolean detached = false;
        if (args.size() == 3) {
            for (Term opt : optionList(args.get(2).resolveBindings(bindings))) {
                Term o = opt;
                if (!(o instanceof CompoundTerm) || ((CompoundTerm) o).getArguments().size() != 1) continue;
                CompoundTerm ct = (CompoundTerm) o;
                Term v = ct.getArguments().get(0);
                if ("alias".equals(ct.getName()) && v instanceof Atom) alias = ((Atom) v).getName();
                else if ("detached".equals(ct.getName()) && v instanceof Atom) {
                    detached = "true".equals(((Atom) v).getName());
                }
                // any other option (at_exit/1, stack sizes, ...) is accepted and ignored
            }
        }

        final int id = THREAD_COUNTER.incrementAndGet();
        THREAD_STATUS.put(id, new Atom("running"));
        int qid = QUEUE_COUNTER.incrementAndGet();
        MESSAGE_QUEUES.put(qid, new LinkedBlockingQueue<Term>());
        THREAD_QUEUE.put(id, qid);
        if (alias != null) ALIASES.put(alias, id);
        if (detached) DETACHED.put(id, Boolean.TRUE);

        final boolean fDetached = detached;
        Thread t = new Thread(new Runnable() {
            @Override public void run() {
                SELF.set(id);
                Term status;
                try {
                    // ISS-2025-0479: the goal really runs — on a fresh Machine over the same Engine
                    // when the caller is the v4 facade, on the shared recursive solver otherwise.
                    List<Map<String, Term>> sols = new ArrayList<>();
                    boolean ok = (solver == null)
                        ? false
                        : solver.solveInWorker(goal, new HashMap<String, Term>(), sols, 1);
                    status = new Atom((ok && !sols.isEmpty()) ? "true" : "false");
                } catch (it.denzosoft.jprolog.core.exceptions.PrologException pe) {
                    Term ball = pe.getErrorTerm();
                    status = new CompoundTerm(new Atom("exception"),
                        Collections.singletonList(ball != null ? ball : new Atom(String.valueOf(pe.getMessage()))));
                // ISS-2025-0479: the two control exceptions are REPORTED, not swallowed — this is
                // the worker's top level, i.e. the analogue of the embedder catching them, and the
                // join status is the only way the parent can learn what happened. Untrusted
                // catch/3 inside the goal still cannot see them (they are not PrologExceptions).
                } catch (it.denzosoft.jprolog.core.engine.InferenceLimitException ile) {
                    status = new CompoundTerm(new Atom("exception"),
                        Collections.singletonList(new Atom("inference_limit_exceeded")));
                } catch (it.denzosoft.jprolog.core.engine.QueryCancelledException qce) {
                    status = new Atom("cancelled");
                } catch (Throwable e) {
                    status = new CompoundTerm(new Atom("exception"),
                        Collections.singletonList(new Atom(e.getClass().getSimpleName()
                            + (e.getMessage() == null ? "" : ": " + e.getMessage()))));
                } finally {
                    SELF.remove();
                }
                THREAD_STATUS.put(id, status);
                if (fDetached || DETACHED.remove(id) != null) cleanup(id);
            }
        }, "prolog-thread-" + id);
        t.setDaemon(true); // Set daemon BEFORE start — safe and prevents JVM hanging on exit
        THREADS.put(id, t);
        t.start();

        return unify(query.getArguments().get(1), new Number(id), bindings, solutions);
    }

    private static void cleanup(int id) {
        THREADS.remove(id);
        THREAD_STATUS.remove(id);
        Integer q = THREAD_QUEUE.remove(id);
        if (q != null) MESSAGE_QUEUES.remove(q);
        for (Map.Entry<String, Integer> e : ALIASES.entrySet()) {
            if (e.getValue().intValue() == id) ALIASES.remove(e.getKey());
        }
    }

    private List<Term> optionList(Term t) {
        List<Term> out = new ArrayList<>();
        Term cur = t;
        while (cur instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) cur;
            if (!".".equals(ct.getFunctor().getName()) || ct.getArguments().size() != 2) break;
            out.add(ct.getArguments().get(0));
            cur = ct.getArguments().get(1);
        }
        return out;
    }

    private boolean doThreadJoin(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws InterruptedException {
        checkArity(query, 2);
        int id = threadId(query.getArguments().get(0), bindings);

        if (DETACHED.containsKey(id)) {
            throw new PrologEvaluationException("thread_join: thread " + id + " is detached, cannot join.");
        }

        Thread t = THREADS.get(id);
        if (t == null) throw new PrologEvaluationException("thread_join: unknown thread " + id);

        // Use timeout to prevent permanent deadlocks
        t.join(JOIN_TIMEOUT_MS);
        if (t.isAlive()) {
            throw new PrologEvaluationException("thread_join: timeout after " + (JOIN_TIMEOUT_MS / 1000) + "s waiting for thread " + id);
        }

        Term status = THREAD_STATUS.get(id);
        if (status == null) status = new Atom("unknown");
        cleanup(id);
        return unify(query.getArguments().get(1), status, bindings, solutions);
    }

    private boolean doThreadDetach(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 1);
        int id = threadId(query.getArguments().get(0), bindings);
        Thread t = THREADS.get(id);
        if (t == null) throw new PrologEvaluationException("thread_detach: unknown thread " + id);

        // Mark as detached — the thread's finally block will clean up when it completes.
        // If already completed, clean up now.
        DETACHED.put(id, Boolean.TRUE);
        if (!t.isAlive()) {
            DETACHED.remove(id);
            cleanup(id);
        }

        solutions.add(bindings);
        return true;
    }

    private boolean doThreadSelf(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 1);
        // ISS-2025-0479: a thread created by thread_create/2,3 reports ITS Prolog id; any other
        // thread (the main one, an IDE background solve) still reports the JVM thread id.
        // ISS-2025-0487: registering here too means the id thread_self/1 reports is always a
        // usable thread_send_message/2 target, on the main thread as well as in a worker.
        long tid = selfId();
        return unify(query.getArguments().get(0), new Number(tid), bindings, solutions);
    }

    private boolean doThreadSleep(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws InterruptedException {
        checkArity(query, 1);
        Term secTerm = query.getArguments().get(0).resolveBindings(bindings);
        if (!(secTerm instanceof Number)) throw new PrologEvaluationException("thread_sleep/1: argument must be a number.");
        double seconds = ((Number) secTerm).getValue();
        if (seconds < 0) throw new PrologEvaluationException("thread_sleep/1: seconds must be non-negative.");
        long millis = (long) (seconds * 1000);
        Thread.sleep(millis);
        solutions.add(bindings);
        return true;
    }

    private boolean doThreadIsAlive(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 1);
        int id = threadId(query.getArguments().get(0), bindings);
        Thread t = THREADS.get(id);
        if (t != null && t.isAlive()) { solutions.add(bindings); return true; }
        return false;
    }

    // ================================================================ message queues

    private boolean doMqCreate(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 1);
        int id = QUEUE_COUNTER.incrementAndGet();
        MESSAGE_QUEUES.put(id, new LinkedBlockingQueue<Term>());
        return unify(query.getArguments().get(0), new Number(id), bindings, solutions);
    }

    private boolean doMqSend(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 2);
        BlockingQueue<Term> q = queueOf(query.getArguments().get(0), bindings, "thread_send_message");
        // ISS-2025-0479: a message is a TERM and it is COPIED — a cell must never be shared
        // between two machines (design B.13).
        q.add(detach(query.getArguments().get(1).resolveBindings(bindings)));
        solutions.add(bindings);
        return true;
    }

    private boolean doMqGet(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws InterruptedException {
        List<Term> args = query.getArguments();
        if (args == null || (args.size() != 1 && args.size() != 2)) {
            throw new PrologEvaluationException("thread_get_message requires 1 or 2 arguments.");
        }
        BlockingQueue<Term> q = (args.size() == 1)
            ? ownQueue("thread_get_message")
            : queueOf(args.get(0), bindings, "thread_get_message");

        // Use poll with timeout instead of take() to prevent permanent blocking
        Term msg = q.poll(MQ_GET_TIMEOUT_MS, TimeUnit.MILLISECONDS);
        if (msg == null) {
            throw new PrologEvaluationException("thread_get_message: timeout after " + (MQ_GET_TIMEOUT_MS / 1000) + "s");
        }
        return unify(args.get(args.size() - 1), detach(msg), bindings, solutions);
    }

    private boolean doMqPeek(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 2);
        BlockingQueue<Term> q = queueOf(query.getArguments().get(0), bindings, "thread_peek_message");
        Term msg = q.peek();
        if (msg == null) return false;
        return unify(query.getArguments().get(1), detach(msg), bindings, solutions);
    }

    /** The queue named by a queue id, a thread id or a thread alias. */
    private BlockingQueue<Term> queueOf(Term t, Map<String, Term> bindings, String who) {
        Term r = t.resolveBindings(bindings);
        if (r instanceof Number) {
            int n = ((Number) r).getValue().intValue();
            BlockingQueue<Term> q = MESSAGE_QUEUES.get(n);
            if (q != null) return q;
            Integer own = THREAD_QUEUE.get(n);                 // a thread id
            if (own != null) {
                q = MESSAGE_QUEUES.get(own);
                if (q != null) return q;
            }
            throw new PrologEvaluationException(who + ": unknown queue " + n);
        }
        if (r instanceof Atom) {
            String alias = ((Atom) r).getName();
            // ISS-2025-0487: `main` names the queue of the thread that runs the top-level query.
            // Registering it on demand means a worker can post to it before the main thread has
            // ever called thread_get_message/1 itself.
            if (MAIN_ALIAS.equals(alias) && !ALIASES.containsKey(MAIN_ALIAS)) selfId();
            Integer id = ALIASES.get(alias);                   // a thread alias
            if (id != null) {
                Integer own = THREAD_QUEUE.get(id);
                if (own != null) {
                    BlockingQueue<Term> q = MESSAGE_QUEUES.get(own);
                    if (q != null) return q;
                }
            }
            throw new PrologEvaluationException(who + ": unknown queue " + alias);
        }
        throw new PrologEvaluationException(who + ": queue must be an id or an alias.");
    }

    private BlockingQueue<Term> ownQueue(String who) {
        // ISS-2025-0487: every Prolog thread owns a queue, INCLUDING the one that is not a
        // thread_create/2,3 worker (the main thread, an IDE background solve). It is registered
        // lazily, on first use, and the first such thread also claims the alias `main` so
        // `thread_send_message(main, T)` from a worker reaches it — as in SWI.
        int self = selfId();
        Integer qid = THREAD_QUEUE.get(Integer.valueOf(self));
        BlockingQueue<Term> q = (qid == null) ? null : MESSAGE_QUEUES.get(qid);
        if (q == null) throw new PrologEvaluationException(who + "/1: this thread has no message queue.");
        return q;
    }

    // START_CHANGE: ISS-2025-0487 - the main thread owns a message queue too.
    /** The alias the first non-worker thread to use the message queues claims (SWI's `main`). */
    private static final String MAIN_ALIAS = "main";

    /**
     * This thread's Prolog id, registering the thread if it has none. A worker created by
     * {@code thread_create/2,3} always has one; any other thread — the embedder's, the CLI's, an
     * IDE background solve — gets one (and a queue) the first time it touches the message-queue
     * predicates or {@code thread_self/1}.
     */
    private static int selfId() {
        Integer self = SELF.get();
        if (self != null) return self.intValue();
        int id = THREAD_COUNTER.incrementAndGet();
        SELF.set(Integer.valueOf(id));
        THREADS.put(Integer.valueOf(id), Thread.currentThread());
        int qid = QUEUE_COUNTER.incrementAndGet();
        MESSAGE_QUEUES.put(Integer.valueOf(qid), new LinkedBlockingQueue<Term>());
        THREAD_QUEUE.put(Integer.valueOf(id), Integer.valueOf(qid));
        ALIASES.putIfAbsent(MAIN_ALIAS, Integer.valueOf(id));
        return id;
    }
    // END_CHANGE: ISS-2025-0487

    /**
     * A copy that shares no {@code Variable} cell with the sender (ISS-2025-0479). On the v4 engine
     * a bound cell IS the binding, so a message put on a queue would otherwise be un-bound by the
     * sender's backtracking while the receiver reads it.
     */
    private static Term detach(Term t) {
        return it.denzosoft.jprolog.core.engine.v4.Unify.copy(
            it.denzosoft.jprolog.core.engine.v4.Unify.resolve(t, null),
            new IdentityHashMap<Variable, Variable>(), null);
    }

    /** The thread named by an id or an alias. */
    private int threadId(Term t, Map<String, Term> bindings) {
        Term r = t.resolveBindings(bindings);
        if (r instanceof Number) return ((Number) r).getValue().intValue();
        if (r instanceof Atom) {
            Integer id = ALIASES.get(((Atom) r).getName());
            if (id != null) return id.intValue();
            throw new PrologEvaluationException(modeName() + ": unknown thread alias " + ((Atom) r).getName());
        }
        throw new PrologEvaluationException(modeName() + ": argument must be a thread id or alias.");
    }

    private void checkArity(Term query, int expected) {
        if (query.getArguments().size() != expected)
            throw new PrologEvaluationException(modeName() + " requires " + expected + " arguments.");
    }

    private boolean unify(Term target, Term value, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        Map<String, Term> nb = new HashMap<>(bindings);
        if (target.resolveBindings(bindings).unify(value, nb)) { solutions.add(nb); return true; }
        return false;
    }

    private String modeName() { return mode.name().toLowerCase(); }
}
// END_CHANGE: ISS-2025-0119
