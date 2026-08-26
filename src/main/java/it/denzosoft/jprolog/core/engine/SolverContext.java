package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0484 - wave W9: what a BuiltInWithContext built-in receives now that the
// recursive solver is gone. Until 4.0.0 the parameter was that concrete class, so a
// built-in that wanted a sub-solve reached the recursive algorithm by default and the v4 engine had
// to SUBCLASS it to redirect the call. The context is an
// interface now: the v4 machine's SolverFacade and the v2 engine's EngineContext implement it, and
// there is no implementation that recurses on the Java stack.
/**
 * The execution context a context-dependent built-in ({@link BuiltInWithContext}) runs in.
 *
 * <p>Three seams, and nothing else:
 * <ul>
 *   <li>{@link #solveMeta} — run a sub-goal as an independent meta-call (a cut inside it is local)
 *       and append one binding map per solution. This is <b>the</b> way a built-in runs a goal.</li>
 *   <li>{@link #solve(Term)} — the same thing for a caller that only wants the solution list.</li>
 *   <li>{@link #solveInWorker} — run a goal on a worker thread (the {@code concurrent_*} family,
 *       {@code first_solution/3}, {@code thread_create/2,3}).</li>
 * </ul>
 *
 * <p>Plus read-only access to the engine the query belongs to. Everything a built-in used to reach
 * through the recursive solver's internals — the cut status, the clause-selection loop, the
 * recursion depth — is gone: control constructs are native in both machines.
 */
public interface SolverContext {

    /**
     * Solve {@code goal} as an independent meta-call and append one map per solution to
     * {@code solutions}. Each map carries {@code bindings} plus the goal's own variables.
     *
     * @return true if at least one solution was added
     */
    boolean solveMeta(Term goal, Map<String, Term> bindings, List<Map<String, Term>> solutions);

    /** Solve {@code goal} and return its solutions. */
    List<Map<String, Term>> solve(Term goal);

    /**
     * Solve {@code goal} on behalf of a worker thread.
     *
     * @param maxSolutions stop after this many solutions (0 = all)
     */
    boolean solveInWorker(Term goal, Map<String, Term> bindings,
                          List<Map<String, Term>> solutions, int maxSolutions);

    /** The {@code Prolog} instance this query belongs to (never null in practice). */
    Prolog getPrologContext();

    /** The engine's knowledge base. */
    KnowledgeBase getKnowledgeBase();

    /** The engine's built-in registry. */
    BuiltInRegistry getBuiltInRegistry();

    /** The IDE debug controller, or null when nothing is debugging. */
    DebugController getDebugController();

    /** The query's inference budget / cancellation guard, or null when unlimited. */
    ResourceGuard getResourceGuard();
}
// END_CHANGE: ISS-2025-0484
