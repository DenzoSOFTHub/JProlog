package it.denzosoft.jprolog.builtin.threading;

// START_CHANGE: ISS-2025-0139 - SWI-Prolog compatible concurrent execution predicates
import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.ControlFlow;
import it.denzosoft.jprolog.core.engine.QueryCancelledException;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.*;
import it.denzosoft.jprolog.core.terms.Number;

import java.util.*;
import java.util.concurrent.*;

/**
 * SWI-Prolog compatible concurrent execution predicates:
 *
 *   concurrent/3           - concurrent(+N, +Goals, +Options)
 *   concurrent_maplist/2   - concurrent_maplist(:Goal, +List)
 *   concurrent_maplist/3   - concurrent_maplist(:Goal, +List, -ResultList)
 *   concurrent_maplist/4   - concurrent_maplist(:Goal, +L1, +L2, -ResultList)
 *   concurrent_forall/2,3  - concurrent_forall(:Cond, :Action [, +Options])   (ISS-2025-0634)
 *   first_solution/3       - first_solution(-X, :Goals, +Options)
 *   concurrent_and/2       - concurrent_and(+Goals, +Options)
 *   concurrent_or/2        - concurrent_or(+Goals, -FirstSolution)
 *
 * START_CHANGE: ISS-2025-0480 - engine v4 wave W8, design B.13: every worker goal goes through
 * {@code SolverContext.solveInWorker}, which runs it on a FRESH Machine over the SAME Engine
 * (core.engine.v4.Workers) with a copy_term'd goal. Interrupting the parent (IDE Stop, embedder
 * cancel) cancels the workers. END_CHANGE: ISS-2025-0480
 *
 * START_CHANGE: ISS-2025-0622 - 4.5 wave P6.2: what a worker raises reaches the caller UNCHANGED.
 * Every predicate used to wrap a worker's failure into a message atom
 * ({@code 'first_solution: ...InferenceLimitException...'}), so the inference budget became an
 * ordinary catchable ball and {@code catch(concurrent_and([throw(foo)],[]), foo, true)} did not
 * catch. Now {@link #await} unwraps the {@code ExecutionException}: an engine-control exception
 * (budget, Stop, debugger Stop, thread_exit) is re-raised as itself, a Prolog ball is re-raised as
 * a copy (no cell of the worker's machine crosses over), and the siblings are cancelled. There is
 * no fixed timeout any more — {@code concurrent_maplist} used to give up after 60 s with
 * {@code 'concurrent_maplist_2: null'}; the wait is interruptible, so a Stop still cancels it. The
 * argument errors are ISO terms. END_CHANGE: ISS-2025-0622
 */
public class ConcurrentPredicates implements BuiltInWithContext {

    public enum OperationType {
        CONCURRENT,            // concurrent/3
        CONCURRENT_MAPLIST_2,  // concurrent_maplist/2
        CONCURRENT_MAPLIST_3,  // concurrent_maplist/3
        CONCURRENT_MAPLIST_4,  // concurrent_maplist/4
        FIRST_SOLUTION,        // first_solution/3
        CONCURRENT_AND,        // concurrent_and/2
        CONCURRENT_OR,         // concurrent_or/2
        CONCURRENT_FORALL      // concurrent_forall/2,3 (ISS-2025-0634)
    }

    private final OperationType opType;

    // Shared thread pool — daemon threads for clean JVM exit
    private static final ExecutorService POOL = Executors.newCachedThreadPool(r -> {
        Thread t = new Thread(r, "jprolog-concurrent");
        t.setDaemon(true);
        return t;
    });

    public ConcurrentPredicates(OperationType opType) {
        this.opType = opType;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("Concurrent predicates require context (SolverContext)");
    }

    @Override
    public boolean executeWithContext(SolverContext solver, Term query,
                                      Map<String, Term> bindings,
                                      List<Map<String, Term>> solutions) {
        // ISS-2025-0480: concurrent_maplist/2,3,4 are ONE registry entry; the goal's arity decides.
        OperationType op = opType;
        int n = (query.getArguments() == null) ? 0 : query.getArguments().size();
        if (op == OperationType.CONCURRENT_MAPLIST_2 || op == OperationType.CONCURRENT_MAPLIST_3
                || op == OperationType.CONCURRENT_MAPLIST_4) {
            if (n == 3) op = OperationType.CONCURRENT_MAPLIST_3;
            else if (n == 4) op = OperationType.CONCURRENT_MAPLIST_4;
            else op = OperationType.CONCURRENT_MAPLIST_2;
        }
        Term[] args = new Term[n];
        for (int i = 0; i < n; i++) args[i] = query.getArguments().get(i).resolveBindings(bindings);
        switch (op) {
            case CONCURRENT:           return doConcurrent(solver, args, bindings, solutions);
            case CONCURRENT_MAPLIST_2: return doConcurrentMaplist2(solver, args, bindings, solutions);
            case CONCURRENT_MAPLIST_3: return doConcurrentMaplistN(solver, args, 1, bindings, solutions);
            case CONCURRENT_MAPLIST_4: return doConcurrentMaplistN(solver, args, 2, bindings, solutions);
            case FIRST_SOLUTION:       return doFirstSolution(solver, args, bindings, solutions);
            case CONCURRENT_AND:       return doConcurrentAnd(solver, args, bindings, solutions);
            case CONCURRENT_OR:        return doConcurrentOr(solver, args, bindings, solutions);
            case CONCURRENT_FORALL:    return doConcurrentForall(solver, args, bindings, solutions);
            default: return false;
        }
    }

    // =========================================================================
    // concurrent(+N, +Goals, +Options): at most N workers; all goals must succeed; the bindings
    // the goals made are unified back (SWI).
    // =========================================================================
    private boolean doConcurrent(SolverContext solver, Term[] args, Map<String, Term> bindings,
                                 List<Map<String, Term>> solutions) {
        final String ctx = "concurrent/3";
        int numThreads = positiveInt(args[0], ctx);
        List<Term> goals = list(args[1], ctx);
        for (Term g : goals) callable(g, ctx);
        if (goals.isEmpty()) { solutions.add(new HashMap<>(bindings)); return true; }

        ExecutorService localPool = Executors.newFixedThreadPool(Math.min(numThreads, goals.size()), r -> {
            Thread t = new Thread(r, "jprolog-concurrent-worker");
            t.setDaemon(true);
            return t;
        });
        try {
            List<Future<Map<String, Term>>> futures = new ArrayList<>();
            for (Term g : goals) futures.add(localPool.submit(firstAnswer(solver, g)));
            List<Map<String, Term>> answers = new ArrayList<>();
            for (Future<Map<String, Term>> f : futures) {
                Map<String, Term> a = await(f, futures);
                if (a == null) { cancelAll(futures); return false; }
                answers.add(a);
            }
            return unifyBack(goals, answers, bindings, solutions);
        } finally {
            localPool.shutdownNow();
        }
    }

    // =========================================================================
    // concurrent_maplist(:Goal, +List): call(Goal, E) for every E in parallel; bindings kept.
    // =========================================================================
    private boolean doConcurrentMaplist2(SolverContext solver, Term[] args, Map<String, Term> bindings,
                                         List<Map<String, Term>> solutions) {
        final String ctx = "concurrent_maplist/2";
        Term goal = callable(args[0], ctx);
        List<Term> elements = list(args[1], ctx);
        if (elements.isEmpty()) { solutions.add(new HashMap<>(bindings)); return true; }
        List<Term> calls = new ArrayList<>();
        for (Term e : elements) calls.add(addArgs(goal, e));
        List<Future<Map<String, Term>>> futures = new ArrayList<>();
        for (Term c : calls) futures.add(POOL.submit(firstAnswer(solver, c)));
        List<Map<String, Term>> answers = new ArrayList<>();
        for (Future<Map<String, Term>> f : futures) {
            Map<String, Term> a = await(f, futures);
            if (a == null) { cancelAll(futures); return false; }
            answers.add(a);
        }
        return unifyBack(calls, answers, bindings, solutions);
    }

    // =========================================================================
    // concurrent_maplist(:Goal, +L1, ?L2)          (inputs = 1)
    // concurrent_maplist(:Goal, +L1, +L2, ?L3)     (inputs = 2)
    // call(Goal, E1 [, E2], R) per position in parallel; the results form the last list.
    // =========================================================================
    private boolean doConcurrentMaplistN(SolverContext solver, Term[] args, int inputs,
                                         Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        final String ctx = "concurrent_maplist/" + (inputs + 2);
        Term goal = callable(args[0], ctx);
        List<Term> l1 = list(args[1], ctx);
        List<Term> l2 = (inputs == 2) ? list(args[2], ctx) : null;
        if (l2 != null && l2.size() != l1.size()) return false;
        Term resultArg = args[inputs + 1];

        List<Variable> holders = new ArrayList<>();
        List<Term> calls = new ArrayList<>();
        for (int i = 0; i < l1.size(); i++) {
            Variable r = new Variable();
            holders.add(r);
            calls.add(inputs == 2 ? addArgs(goal, l1.get(i), l2.get(i), r) : addArgs(goal, l1.get(i), r));
        }
        List<Future<Map<String, Term>>> futures = new ArrayList<>();
        for (Term c : calls) futures.add(POOL.submit(firstAnswer(solver, c)));
        List<Term> results = new ArrayList<>();
        List<Map<String, Term>> answers = new ArrayList<>();
        for (int i = 0; i < futures.size(); i++) {
            Map<String, Term> a = await(futures.get(i), futures);
            if (a == null) { cancelAll(futures); return false; }
            answers.add(a);
            results.add(holders.get(i).resolveBindings(a));
        }
        Map<String, Term> nb = new HashMap<>(bindings);
        for (int i = 0; i < calls.size(); i++) {
            Term inst = calls.get(i).resolveBindings(answers.get(i));
            if (!calls.get(i).resolveBindings(nb).unify(inst, nb)) return false;
        }
        if (!resultArg.resolveBindings(nb).unify(buildPrologList(results), nb)) return false;
        solutions.add(resolved(nb));
        return true;
    }

    // =========================================================================
    // first_solution(-X, :Goals, +Options): the first goal to SUCCEED gives X; the rest are
    // cancelled. A goal that raises before any success propagates its exception.
    // =========================================================================
    private boolean doFirstSolution(SolverContext solver, Term[] args, Map<String, Term> bindings,
                                    List<Map<String, Term>> solutions) {
        final String ctx = "first_solution/3";
        final Term template = args[0];
        List<Term> goals = list(args[1], ctx);
        for (Term g : goals) callable(g, ctx);
        if (goals.isEmpty()) return false;

        CompletionService<Map<String, Term>> cs = new ExecutorCompletionService<>(POOL);
        List<Future<Map<String, Term>>> futures = new ArrayList<>();
        for (Term g : goals) futures.add(cs.submit(firstAnswer(solver, g)));
        try {
            for (int i = 0; i < futures.size(); i++) {
                Future<Map<String, Term>> done;
                try {
                    done = cs.take();
                } catch (InterruptedException ie) {
                    throw parentCancelled(futures);
                }
                Map<String, Term> result = await(done, futures);
                if (result != null) {
                    Map<String, Term> nb = new HashMap<>(bindings);
                    if (template.unify(template.resolveBindings(result), nb)) {
                        solutions.add(nb);
                        return true;
                    }
                }
            }
            return false;
        } finally {
            cancelAll(futures);
        }
    }

    // =========================================================================
    // concurrent_and(+Goals, +Options): all goals in parallel, succeed iff all succeed.
    // =========================================================================
    private boolean doConcurrentAnd(SolverContext solver, Term[] args, Map<String, Term> bindings,
                                    List<Map<String, Term>> solutions) {
        final String ctx = "concurrent_and/2";
        List<Term> goals = list(args[0], ctx);
        for (Term g : goals) callable(g, ctx);
        if (goals.isEmpty()) { solutions.add(new HashMap<>(bindings)); return true; }
        List<Future<Map<String, Term>>> futures = new ArrayList<>();
        for (Term g : goals) futures.add(POOL.submit(firstAnswer(solver, g)));
        for (Future<Map<String, Term>> f : futures) {
            if (await(f, futures) == null) { cancelAll(futures); return false; }
        }
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    // =========================================================================
    // concurrent_or(+Goals, -Index): the 1-based index of the first goal that succeeds.
    // =========================================================================
    private boolean doConcurrentOr(SolverContext solver, Term[] args, Map<String, Term> bindings,
                                   List<Map<String, Term>> solutions) {
        final String ctx = "concurrent_or/2";
        List<Term> goals = list(args[0], ctx);
        for (Term g : goals) callable(g, ctx);
        if (goals.isEmpty()) return false;
        CompletionService<Map<String, Term>> cs = new ExecutorCompletionService<>(POOL);
        List<Future<Map<String, Term>>> futures = new ArrayList<>();
        final Map<Future<Map<String, Term>>, Integer> index = new HashMap<>();
        for (int i = 0; i < goals.size(); i++) {
            Future<Map<String, Term>> f = cs.submit(firstAnswer(solver, goals.get(i)));
            futures.add(f);
            index.put(f, i + 1);
        }
        try {
            for (int i = 0; i < futures.size(); i++) {
                Future<Map<String, Term>> done;
                try {
                    done = cs.take();
                } catch (InterruptedException ie) {
                    throw parentCancelled(futures);
                }
                if (await(done, futures) != null) {
                    Map<String, Term> nb = new HashMap<>(bindings);
                    if (args[1].unify(Number.valueOf(index.get(done)), nb)) {
                        solutions.add(nb);
                        return true;
                    }
                    return false;
                }
            }
            return false;
        } finally {
            cancelAll(futures);
        }
    }

    // START_CHANGE: ISS-2025-0634 - concurrent_forall(:Cond, :Action [, +Options]) (SWI): every
    // solution of Cond (enumerated here) runs Action on a pool of threads(N) workers (default: the
    // number of processors). Succeeds iff Action succeeds for every solution; the first failure or
    // exception cancels the rest. Like forall/2 it binds nothing.
    private boolean doConcurrentForall(SolverContext solver, Term[] args, Map<String, Term> bindings,
                                       List<Map<String, Term>> solutions) {
        final String ctx = "concurrent_forall/" + args.length;
        Term cond = callable(args[0], ctx);
        Term action = callable(args[1], ctx);
        int threads = Runtime.getRuntime().availableProcessors();
        if (args.length == 3) {
            for (Term o : list(args[2], ctx)) {
                if (o instanceof CompoundTerm && "threads".equals(((CompoundTerm) o).getName())
                        && ((CompoundTerm) o).getArguments().size() == 1) {
                    threads = positiveInt(((CompoundTerm) o).getArguments().get(0), ctx);
                }
            }
        }
        List<Map<String, Term>> conds = new ArrayList<>();
        solver.solveMeta(cond, new HashMap<String, Term>(bindings), conds);
        if (conds.isEmpty()) { solutions.add(new HashMap<>(bindings)); return true; }
        ExecutorService localPool = Executors.newFixedThreadPool(Math.min(threads, conds.size()), r -> {
            Thread t = new Thread(r, "jprolog-concurrent-forall");
            t.setDaemon(true);
            return t;
        });
        try {
            List<Future<Map<String, Term>>> futures = new ArrayList<>();
            for (Map<String, Term> c : conds) {
                futures.add(localPool.submit(firstAnswer(solver, action.resolveBindings(c))));
            }
            for (Future<Map<String, Term>> f : futures) {
                if (await(f, futures) == null) { cancelAll(futures); return false; }
            }
            solutions.add(new HashMap<>(bindings));
            return true;
        } finally {
            localPool.shutdownNow();
        }
    }
    // END_CHANGE: ISS-2025-0634

    // =========================================================================
    // Utility methods
    // =========================================================================

    /** A task that runs {@code goal} on a worker machine: its first answer, or null on failure. */
    private static Callable<Map<String, Term>> firstAnswer(final SolverContext solver, final Term goal) {
        return () -> {
            List<Map<String, Term>> temp = new ArrayList<>();
            boolean ok = solver.solveInWorker(goal, new HashMap<String, Term>(), temp, 1);
            return (ok && !temp.isEmpty()) ? temp.get(0) : null;
        };
    }

    // START_CHANGE: ISS-2025-0622 - the one place a worker's outcome is collected.
    /**
     * Wait (without a time limit, interruptibly) for {@code f}. A worker's exception is re-raised
     * as itself when it is an engine-control exception, as a copied ball when it is a Prolog
     * exception; either way the sibling futures are cancelled first.
     */
    private static <T> T await(Future<T> f, List<? extends Future<?>> all) {
        try {
            return f.get();
        } catch (InterruptedException ie) {
            throw parentCancelled(all);
        } catch (CancellationException ce) {
            return null;
        } catch (ExecutionException ee) {
            cancelAll(all);
            throw rethrow(ee.getCause() != null ? ee.getCause() : ee);
        }
    }

    private static RuntimeException rethrow(Throwable cause) {
        ControlFlow.rethrowIfControl(cause);
        if (cause instanceof PrologException) {
            PrologException pe = (PrologException) cause;
            Term ball = pe.getErrorTerm();
            if (ball == null) return pe;                               // halt/1 and friends
            return new PrologException(ThreadPredicates.detach(ball));
        }
        if (cause instanceof Error) throw (Error) cause;
        return Errors.system(cause.getClass().getSimpleName()
            + (cause.getMessage() == null ? "" : ": " + cause.getMessage()), "concurrent");
    }
    // END_CHANGE: ISS-2025-0622

    // ISS-2025-0480 - cancelling the parent must cancel the workers. Future.cancel(true) interrupts
    // the worker thread; its own ResourceGuard then raises QueryCancelledException.
    private static void cancelAll(List<? extends Future<?>> futures) {
        for (Future<?> f : futures) f.cancel(true);
    }

    private static RuntimeException parentCancelled(List<? extends Future<?>> futures) {
        cancelAll(futures);
        Thread.currentThread().interrupt();
        return new QueryCancelledException();
    }

    /** Unify each goal with its instantiated copy from the worker's answer (SWI keeps them). */
    private static boolean unifyBack(List<Term> goals, List<Map<String, Term>> answers,
                                     Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        Map<String, Term> nb = new HashMap<>(bindings);
        for (int i = 0; i < goals.size(); i++) {
            Term inst = goals.get(i).resolveBindings(answers.get(i));
            if (!goals.get(i).resolveBindings(nb).unify(inst, nb)) return false;
        }
        solutions.add(resolved(nb));
        return true;
    }

    /** The map with every value resolved through it (the workers' fresh variables are chained). */
    private static Map<String, Term> resolved(Map<String, Term> nb) {
        Map<String, Term> out = new HashMap<>();
        for (Map.Entry<String, Term> e : nb.entrySet()) out.put(e.getKey(), e.getValue().resolveBindings(nb));
        return out;
    }

    private static int positiveInt(Term t, String ctx) {
        if (t instanceof Variable) throw Errors.instantiation(ctx);
        if (!(t instanceof Number) || !((Number) t).isInteger()) throw Errors.type("integer", t, ctx);
        long v = ((Number) t).longValue();
        if (v < 1) throw Errors.domain("positive_integer", t, ctx);
        return (int) Math.min(v, 1024);
    }

    private static Term callable(Term g, String ctx) {
        if (g instanceof Variable) throw Errors.instantiation(ctx);
        if (!(g instanceof Atom) && !(g instanceof CompoundTerm)) throw Errors.type("callable", g, ctx);
        return g;
    }

    /** A proper list; instantiation_error on a partial list, type_error(list, L) otherwise. */
    private static List<Term> list(Term listTerm, String ctx) {
        List<Term> result = new ArrayList<>();
        Term cur = listTerm;
        while (cur instanceof CompoundTerm && ".".equals(((CompoundTerm) cur).getName())
                && ((CompoundTerm) cur).getArguments().size() == 2) {
            result.add(((CompoundTerm) cur).getArguments().get(0));
            cur = ((CompoundTerm) cur).getArguments().get(1);
        }
        if (cur instanceof Variable) throw Errors.instantiation(ctx);
        if (!(cur instanceof Atom) || !"[]".equals(((Atom) cur).getName())) {
            throw Errors.type("list", listTerm, ctx);
        }
        return result;
    }

    /** Goal with extra arguments appended (call/N). */
    private static Term addArgs(Term goal, Term... extra) {
        if (goal instanceof Atom) return new CompoundTerm((Atom) goal, Arrays.asList(extra));
        CompoundTerm ct = (CompoundTerm) goal;
        List<Term> args = new ArrayList<>(ct.getArguments());
        args.addAll(Arrays.asList(extra));
        return new CompoundTerm(ct.getFunctor(), args);
    }

    /** Build a Prolog list from a Java list of terms. */
    private static Term buildPrologList(List<Term> terms) {
        Term list = new Atom("[]");
        for (int i = terms.size() - 1; i >= 0; i--) {
            list = new CompoundTerm(new Atom("."), Arrays.asList(terms.get(i), list));
        }
        return list;
    }
}
// END_CHANGE: ISS-2025-0139
