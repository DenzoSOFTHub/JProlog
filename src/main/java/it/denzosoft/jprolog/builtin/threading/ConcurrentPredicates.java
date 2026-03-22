package it.denzosoft.jprolog.builtin.threading;

// START_CHANGE: ISS-2025-0139 - SWI-Prolog compatible concurrent execution predicates
import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.CutStatus;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.*;
import it.denzosoft.jprolog.core.terms.Number;

import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

/**
 * SWI-Prolog compatible concurrent execution predicates:
 *
 *   concurrent/3           - concurrent(+N, +Goals, +Options)
 *   concurrent_maplist/2   - concurrent_maplist(:Goal, +List)
 *   concurrent_maplist/3   - concurrent_maplist(:Goal, +List, -ResultList)
 *   concurrent_maplist/4   - concurrent_maplist(:Goal, +L1, +L2, -ResultList)
 *   first_solution/3       - first_solution(-X, :Goals, +Options)
 *   concurrent_and/2       - concurrent_and(+Goals, +Options)
 *   concurrent_or/2        - concurrent_or(+Goals, -FirstSolution)
 *
 * All predicates use Java's ExecutorService for real thread-level parallelism.
 * Goals are executed via QuerySolver.solve() in separate threads.
 *
 * Thread safety:
 * - Each thread gets its own copy of bindings (HashMap copy)
 * - QuerySolver.solve() is reentrant for read-only KB access
 * - Results are collected via thread-safe ConcurrentLinkedQueue or Future
 * - ExecutorService uses a cached thread pool (threads recycled)
 */
public class ConcurrentPredicates implements BuiltInWithContext {

    public enum OperationType {
        CONCURRENT,            // concurrent/3
        CONCURRENT_MAPLIST_2,  // concurrent_maplist/2
        CONCURRENT_MAPLIST_3,  // concurrent_maplist/3
        CONCURRENT_MAPLIST_4,  // concurrent_maplist/4
        FIRST_SOLUTION,        // first_solution/3
        CONCURRENT_AND,        // concurrent_and/2
        CONCURRENT_OR          // concurrent_or/2
    }

    private final OperationType opType;

    // Shared thread pool — daemon threads for clean JVM exit
    private static final ExecutorService POOL = Executors.newCachedThreadPool(r -> {
        Thread t = new Thread(r, "jprolog-concurrent");
        t.setDaemon(true);
        return t;
    });

    private static final long DEFAULT_TIMEOUT_MS = 60_000;

    public ConcurrentPredicates(OperationType opType) {
        this.opType = opType;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("Concurrent predicates require context (QuerySolver)");
    }

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query,
                                      Map<String, Term> bindings,
                                      List<Map<String, Term>> solutions) {
        try {
            switch (opType) {
                case CONCURRENT:           return doConcurrent(solver, query, bindings, solutions);
                case CONCURRENT_MAPLIST_2: return doConcurrentMaplist2(solver, query, bindings, solutions);
                case CONCURRENT_MAPLIST_3: return doConcurrentMaplist3(solver, query, bindings, solutions);
                case CONCURRENT_MAPLIST_4: return doConcurrentMaplist4(solver, query, bindings, solutions);
                case FIRST_SOLUTION:       return doFirstSolution(solver, query, bindings, solutions);
                case CONCURRENT_AND:       return doConcurrentAnd(solver, query, bindings, solutions);
                case CONCURRENT_OR:        return doConcurrentOr(solver, query, bindings, solutions);
                default: return false;
            }
        } catch (PrologEvaluationException e) {
            throw e;
        } catch (Exception e) {
            throw new PrologEvaluationException(opType.name().toLowerCase() + ": " + e.getMessage());
        }
    }

    // =========================================================================
    // concurrent(+N, +Goals, +Options)
    // Execute a list of goals using at most N worker threads.
    // All goals must succeed for concurrent/3 to succeed.
    // =========================================================================
    private boolean doConcurrent(QuerySolver solver, Term query,
                                  Map<String, Term> bindings,
                                  List<Map<String, Term>> solutions) throws Exception {
        checkArity(query, 3, "concurrent/3");
        Term nTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term goalsTerm = query.getArguments().get(1).resolveBindings(bindings);

        int numThreads = toInt(nTerm, "concurrent/3: first argument must be a positive integer");
        if (numThreads < 1) throw new PrologEvaluationException("concurrent/3: thread count must be >= 1");

        List<Term> goals = termToList(goalsTerm, "concurrent/3: second argument must be a list of goals");
        if (goals.isEmpty()) {
            solutions.add(new HashMap<>(bindings));
            return true;
        }

        ExecutorService localPool = Executors.newFixedThreadPool(numThreads, r -> {
            Thread t = new Thread(r, "jprolog-concurrent-worker");
            t.setDaemon(true);
            return t;
        });

        try {
            List<Future<Boolean>> futures = new ArrayList<>();
            for (Term goal : goals) {
                final Term g = goal;
                final Map<String, Term> bindingsCopy = new HashMap<>(bindings);
                futures.add(localPool.submit(() -> {
                    List<Map<String, Term>> temp = new ArrayList<>();
                    return solver.solve(g, bindingsCopy, temp, CutStatus.notOccurred());
                }));
            }

            // All goals must succeed
            for (Future<Boolean> f : futures) {
                if (!f.get(DEFAULT_TIMEOUT_MS, TimeUnit.MILLISECONDS)) {
                    return false;
                }
            }

            solutions.add(new HashMap<>(bindings));
            return true;
        } finally {
            localPool.shutdownNow();
        }
    }

    // =========================================================================
    // concurrent_maplist(:Goal, +List)
    // Like maplist/2 but executes Goal on each element in parallel.
    // Succeeds if Goal succeeds for all elements.
    // =========================================================================
    private boolean doConcurrentMaplist2(QuerySolver solver, Term query,
                                          Map<String, Term> bindings,
                                          List<Map<String, Term>> solutions) throws Exception {
        checkArity(query, 2, "concurrent_maplist/2");
        Term goalTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term listTerm = query.getArguments().get(1).resolveBindings(bindings);

        List<Term> elements = termToList(listTerm, "concurrent_maplist/2: second argument must be a list");
        if (elements.isEmpty()) {
            solutions.add(new HashMap<>(bindings));
            return true;
        }

        List<Future<Boolean>> futures = new ArrayList<>();
        for (Term elem : elements) {
            Term callGoal = buildCallGoal(goalTerm, elem);
            Map<String, Term> bc = new HashMap<>(bindings);
            futures.add(POOL.submit(() -> {
                List<Map<String, Term>> temp = new ArrayList<>();
                return solver.solve(callGoal, bc, temp, CutStatus.notOccurred());
            }));
        }

        for (Future<Boolean> f : futures) {
            if (!f.get(DEFAULT_TIMEOUT_MS, TimeUnit.MILLISECONDS)) {
                return false;
            }
        }

        solutions.add(new HashMap<>(bindings));
        return true;
    }

    // =========================================================================
    // concurrent_maplist(:Goal, +List, -ResultList)
    // Like maplist/3 but executes Goal on each element in parallel.
    // Goal is called as call(Goal, Elem, Result) for each element.
    // =========================================================================
    private boolean doConcurrentMaplist3(QuerySolver solver, Term query,
                                          Map<String, Term> bindings,
                                          List<Map<String, Term>> solutions) throws Exception {
        checkArity(query, 3, "concurrent_maplist/3");
        Term goalTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term listTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term resultVar = query.getArguments().get(2);

        List<Term> elements = termToList(listTerm, "concurrent_maplist/3: second argument must be a list");
        if (elements.isEmpty()) {
            Map<String, Term> nb = new HashMap<>(bindings);
            Term emptyList = new Atom("[]");
            if (resultVar.resolveBindings(bindings).unify(emptyList, nb)) {
                solutions.add(nb);
                return true;
            }
            return false;
        }

        // Each task: call(Goal, Elem, Result) -> extract Result
        List<Future<Term>> futures = new ArrayList<>();
        for (Term elem : elements) {
            Variable resultHolder = new Variable("_ConcRes" + System.nanoTime() + Thread.currentThread().getId());
            Term callGoal = buildCallGoal(goalTerm, elem, resultHolder);
            Map<String, Term> bc = new HashMap<>(bindings);
            final String resVarName = resultHolder.getName();
            futures.add(POOL.submit(() -> {
                List<Map<String, Term>> temp = new ArrayList<>();
                boolean ok = solver.solve(callGoal, bc, temp, CutStatus.notOccurred());
                if (ok && !temp.isEmpty()) {
                    Term res = temp.get(0).get(resVarName);
                    return res != null ? res.resolveBindings(temp.get(0)) : null;
                }
                return null;
            }));
        }

        // Collect results in order
        List<Term> resultTerms = new ArrayList<>();
        for (Future<Term> f : futures) {
            Term res = f.get(DEFAULT_TIMEOUT_MS, TimeUnit.MILLISECONDS);
            if (res == null) return false;
            resultTerms.add(res);
        }

        // Build result list and unify
        Term resultList = buildPrologList(resultTerms);
        Map<String, Term> nb = new HashMap<>(bindings);
        if (resultVar.resolveBindings(bindings).unify(resultList, nb)) {
            solutions.add(nb);
            return true;
        }
        return false;
    }

    // =========================================================================
    // concurrent_maplist(:Goal, +L1, +L2, -ResultList)
    // Parallel maplist with two input lists.
    // =========================================================================
    private boolean doConcurrentMaplist4(QuerySolver solver, Term query,
                                          Map<String, Term> bindings,
                                          List<Map<String, Term>> solutions) throws Exception {
        checkArity(query, 4, "concurrent_maplist/4");
        Term goalTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term list1Term = query.getArguments().get(1).resolveBindings(bindings);
        Term list2Term = query.getArguments().get(2).resolveBindings(bindings);
        Term resultVar = query.getArguments().get(3);

        List<Term> list1 = termToList(list1Term, "concurrent_maplist/4: second argument must be a list");
        List<Term> list2 = termToList(list2Term, "concurrent_maplist/4: third argument must be a list");

        if (list1.size() != list2.size()) {
            throw new PrologEvaluationException("concurrent_maplist/4: input lists must have the same length");
        }

        if (list1.isEmpty()) {
            Map<String, Term> nb = new HashMap<>(bindings);
            if (resultVar.resolveBindings(bindings).unify(new Atom("[]"), nb)) {
                solutions.add(nb);
                return true;
            }
            return false;
        }

        List<Future<Term>> futures = new ArrayList<>();
        for (int i = 0; i < list1.size(); i++) {
            Variable resultHolder = new Variable("_ConcRes4_" + System.nanoTime() + "_" + i);
            Term callGoal = buildCallGoal(goalTerm, list1.get(i), list2.get(i), resultHolder);
            Map<String, Term> bc = new HashMap<>(bindings);
            final String resVarName = resultHolder.getName();
            futures.add(POOL.submit(() -> {
                List<Map<String, Term>> temp = new ArrayList<>();
                boolean ok = solver.solve(callGoal, bc, temp, CutStatus.notOccurred());
                if (ok && !temp.isEmpty()) {
                    Term res = temp.get(0).get(resVarName);
                    return res != null ? res.resolveBindings(temp.get(0)) : null;
                }
                return null;
            }));
        }

        List<Term> resultTerms = new ArrayList<>();
        for (Future<Term> f : futures) {
            Term res = f.get(DEFAULT_TIMEOUT_MS, TimeUnit.MILLISECONDS);
            if (res == null) return false;
            resultTerms.add(res);
        }

        Term resultList = buildPrologList(resultTerms);
        Map<String, Term> nb = new HashMap<>(bindings);
        if (resultVar.resolveBindings(bindings).unify(resultList, nb)) {
            solutions.add(nb);
            return true;
        }
        return false;
    }

    // =========================================================================
    // first_solution(-X, :Goals, +Options)
    // Run Goals in parallel; return the binding of X from whichever goal
    // succeeds first. Cancel remaining goals.
    // SWI-Prolog compatible: Goals is a list of goal terms.
    // =========================================================================
    private boolean doFirstSolution(QuerySolver solver, Term query,
                                     Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) throws Exception {
        checkArity(query, 3, "first_solution/3");
        Term templateVar = query.getArguments().get(0);
        Term goalsTerm = query.getArguments().get(1).resolveBindings(bindings);

        List<Term> goals = termToList(goalsTerm, "first_solution/3: second argument must be a list of goals");
        if (goals.isEmpty()) return false;

        // Use CompletionService to get the first completed result
        CompletionService<Map<String, Term>> cs = new ExecutorCompletionService<>(POOL);
        AtomicBoolean found = new AtomicBoolean(false);
        List<Future<Map<String, Term>>> futures = new ArrayList<>();

        for (Term goal : goals) {
            final Term g = goal;
            Map<String, Term> bc = new HashMap<>(bindings);
            futures.add(cs.submit(() -> {
                if (found.get()) return null;
                List<Map<String, Term>> temp = new ArrayList<>();
                boolean ok = solver.solve(g, bc, temp, CutStatus.notOccurred());
                if (ok && !temp.isEmpty() && !found.get()) {
                    return temp.get(0);
                }
                return null;
            }));
        }

        try {
            // Wait for the first successful result
            for (int i = 0; i < futures.size(); i++) {
                Future<Map<String, Term>> completed = cs.poll(DEFAULT_TIMEOUT_MS, TimeUnit.MILLISECONDS);
                if (completed == null) break;
                Map<String, Term> result = completed.get();
                if (result != null) {
                    found.set(true);
                    // Resolve template variable from the successful solution
                    Term resolvedTemplate = templateVar.resolveBindings(result);
                    Map<String, Term> nb = new HashMap<>(bindings);
                    if (templateVar.resolveBindings(bindings).unify(resolvedTemplate, nb)) {
                        // Merge relevant bindings
                        for (Map.Entry<String, Term> e : result.entrySet()) {
                            if (!nb.containsKey(e.getKey())) {
                                nb.put(e.getKey(), e.getValue());
                            }
                        }
                        solutions.add(nb);
                        return true;
                    }
                }
            }
            return false;
        } finally {
            // Cancel remaining tasks
            found.set(true);
            for (Future<?> f : futures) {
                f.cancel(true);
            }
        }
    }

    // =========================================================================
    // concurrent_and(+Goals, +Options)
    // Run all goals in parallel. Succeed only if ALL goals succeed.
    // Like concurrent/3 but uses the global thread pool.
    // =========================================================================
    private boolean doConcurrentAnd(QuerySolver solver, Term query,
                                     Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) throws Exception {
        checkArity(query, 2, "concurrent_and/2");
        Term goalsTerm = query.getArguments().get(0).resolveBindings(bindings);

        List<Term> goals = termToList(goalsTerm, "concurrent_and/2: first argument must be a list of goals");
        if (goals.isEmpty()) {
            solutions.add(new HashMap<>(bindings));
            return true;
        }

        List<Future<Boolean>> futures = new ArrayList<>();
        for (Term goal : goals) {
            Map<String, Term> bc = new HashMap<>(bindings);
            futures.add(POOL.submit(() -> {
                List<Map<String, Term>> temp = new ArrayList<>();
                return solver.solve(goal, bc, temp, CutStatus.notOccurred());
            }));
        }

        for (Future<Boolean> f : futures) {
            if (!f.get(DEFAULT_TIMEOUT_MS, TimeUnit.MILLISECONDS)) {
                return false;
            }
        }

        solutions.add(new HashMap<>(bindings));
        return true;
    }

    // =========================================================================
    // concurrent_or(+Goals, -FirstSolution)
    // Run goals in parallel; succeed with the first goal that succeeds.
    // Unifies FirstSolution with the index (1-based) of the winning goal.
    // =========================================================================
    private boolean doConcurrentOr(QuerySolver solver, Term query,
                                    Map<String, Term> bindings,
                                    List<Map<String, Term>> solutions) throws Exception {
        checkArity(query, 2, "concurrent_or/2");
        Term goalsTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term resultVar = query.getArguments().get(1);

        List<Term> goals = termToList(goalsTerm, "concurrent_or/2: first argument must be a list of goals");
        if (goals.isEmpty()) return false;

        CompletionService<Integer> cs = new ExecutorCompletionService<>(POOL);
        AtomicBoolean found = new AtomicBoolean(false);
        List<Future<Integer>> futures = new ArrayList<>();

        for (int i = 0; i < goals.size(); i++) {
            final Term g = goals.get(i);
            final int index = i + 1;
            Map<String, Term> bc = new HashMap<>(bindings);
            futures.add(cs.submit(() -> {
                if (found.get()) return -1;
                List<Map<String, Term>> temp = new ArrayList<>();
                boolean ok = solver.solve(g, bc, temp, CutStatus.notOccurred());
                return (ok && !found.get()) ? index : -1;
            }));
        }

        try {
            for (int i = 0; i < futures.size(); i++) {
                Future<Integer> completed = cs.poll(DEFAULT_TIMEOUT_MS, TimeUnit.MILLISECONDS);
                if (completed == null) break;
                int idx = completed.get();
                if (idx > 0) {
                    found.set(true);
                    Map<String, Term> nb = new HashMap<>(bindings);
                    if (resultVar.resolveBindings(bindings).unify(new Number(idx), nb)) {
                        solutions.add(nb);
                        return true;
                    }
                }
            }
            return false;
        } finally {
            found.set(true);
            for (Future<?> f : futures) {
                f.cancel(true);
            }
        }
    }

    // =========================================================================
    // Utility methods
    // =========================================================================

    private void checkArity(Term query, int expected, String name) {
        if (query.getArguments() == null || query.getArguments().size() != expected) {
            throw new PrologEvaluationException(name + " requires " + expected + " arguments");
        }
    }

    private int toInt(Term term, String errMsg) {
        if (!(term instanceof Number)) throw new PrologEvaluationException(errMsg);
        return ((Number) term).getValue().intValue();
    }

    /** Convert a Prolog list term to a Java List<Term>. */
    private List<Term> termToList(Term listTerm, String errMsg) {
        List<Term> result = new ArrayList<>();
        Term current = listTerm;
        while (current instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) current;
            if (".".equals(ct.getFunctor().getName()) && ct.getArguments().size() == 2) {
                result.add(ct.getArguments().get(0));
                current = ct.getArguments().get(1);
            } else {
                throw new PrologEvaluationException(errMsg);
            }
        }
        if (!(current instanceof Atom) || !"[]".equals(((Atom) current).getName())) {
            if (!result.isEmpty()) {
                throw new PrologEvaluationException(errMsg);
            }
            // Single term, not a list — treat as single-element list? No, error.
            throw new PrologEvaluationException(errMsg);
        }
        return result;
    }

    /** Build call(Goal, Arg1) as a compound term. */
    private Term buildCallGoal(Term goal, Term arg1) {
        if (goal instanceof Atom) {
            return new CompoundTerm((Atom) goal, Arrays.asList(arg1));
        } else if (goal instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) goal;
            List<Term> args = new ArrayList<>(ct.getArguments());
            args.add(arg1);
            return new CompoundTerm(ct.getFunctor(), args);
        }
        throw new PrologEvaluationException("Goal must be callable");
    }

    /** Build call(Goal, Arg1, Arg2) as a compound term. */
    private Term buildCallGoal(Term goal, Term arg1, Term arg2) {
        if (goal instanceof Atom) {
            return new CompoundTerm((Atom) goal, Arrays.asList(arg1, arg2));
        } else if (goal instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) goal;
            List<Term> args = new ArrayList<>(ct.getArguments());
            args.add(arg1);
            args.add(arg2);
            return new CompoundTerm(ct.getFunctor(), args);
        }
        throw new PrologEvaluationException("Goal must be callable");
    }

    /** Build call(Goal, Arg1, Arg2, Arg3) as a compound term. */
    private Term buildCallGoal(Term goal, Term arg1, Term arg2, Term arg3) {
        if (goal instanceof Atom) {
            return new CompoundTerm((Atom) goal, Arrays.asList(arg1, arg2, arg3));
        } else if (goal instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) goal;
            List<Term> args = new ArrayList<>(ct.getArguments());
            args.add(arg1);
            args.add(arg2);
            args.add(arg3);
            return new CompoundTerm(ct.getFunctor(), args);
        }
        throw new PrologEvaluationException("Goal must be callable");
    }

    /** Build a Prolog list from a Java list of terms. */
    private Term buildPrologList(List<Term> terms) {
        Term list = new Atom("[]");
        for (int i = terms.size() - 1; i >= 0; i--) {
            list = new CompoundTerm(new Atom("."), Arrays.asList(terms.get(i), list));
        }
        return list;
    }
}
// END_CHANGE: ISS-2025-0139
