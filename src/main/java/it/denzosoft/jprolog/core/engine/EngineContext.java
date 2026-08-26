package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.core.terms.Term;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0484 - wave W9: the durable per-engine context. It replaces the recursive
// recursive solver in the ONE role that outlived the recursive algorithm: being the object a `Prolog`
// keeps for the whole session so the IDE can install a DebugController on it and so a query can
// publish its ResourceGuard where a nested sub-solve will find it. Everything else that class did
// (solveInternal, solveAgainstKnowledgeBase, solveBodyGoals, solveWithTabling, handleBuiltIn, the
// LCO trampoline, CutStatus threading) was the recursive engine and is deleted.
/**
 * Everything one {@link Prolog} owns that survives a query: the knowledge base and registry the
 * machines run over, the IDE debug controller, the current query's {@link ResourceGuard}, and the
 * legacy attributed-variable unification hook.
 *
 * <p>It also implements {@link SolverContext} as the <b>engine-routed</b> fallback: a caller
 * that holds only this object (rather than a per-query facade) still gets its sub-goal solved, on
 * a fresh {@code core.engine.v4.Machine}. Inside a query the per-query
 * {@code core.engine.v4.SolverFacade} is what a built-in really receives, because it shares the
 * running machine's trail, choice-point floor and guard.
 */
public class EngineContext implements SolverContext {

    private final Prolog prolog;
    private final KnowledgeBase knowledgeBase;
    private final BuiltInRegistry builtInRegistry;

    // START_CHANGE: ISS-2025-0090 - Debug controller for step-by-step execution (re-homed in W9)
    private volatile DebugController debugController;
    // END_CHANGE: ISS-2025-0090

    // START_CHANGE: ISS-2025-0431 - ENG-04: the per-query budget/cancellation guard, published here
    // by the running machine so a nested sub-solve charges the SAME counter.
    private ResourceGuard resourceGuard;
    // END_CHANGE: ISS-2025-0431

    public EngineContext(Prolog prolog, KnowledgeBase knowledgeBase, BuiltInRegistry builtInRegistry) {
        this.prolog = prolog;
        this.knowledgeBase = knowledgeBase;
        this.builtInRegistry = builtInRegistry;
    }

    // ---------------------------------------------------------------- durable state

    /** Install the IDE debug controller (null clears it). */
    public void setDebugController(DebugController debugController) {
        this.debugController = debugController;
    }

    @Override
    public DebugController getDebugController() { return debugController; }

    /** Install the per-query budget/cancellation guard (null clears it). */
    public void setResourceGuard(ResourceGuard guard) { this.resourceGuard = guard; }

    @Override
    public ResourceGuard getResourceGuard() { return resourceGuard; }

    @Override
    public Prolog getPrologContext() { return prolog; }

    @Override
    public KnowledgeBase getKnowledgeBase() { return knowledgeBase; }

    @Override
    public BuiltInRegistry getBuiltInRegistry() { return builtInRegistry; }

    // ---------------------------------------------------------------- the solve seams

    @Override
    public List<Map<String, Term>> solve(Term goal) {
        List<Map<String, Term>> out = new ArrayList<Map<String, Term>>();
        runSub(goal, out, 0);
        return out;
    }

    @Override
    public boolean solveMeta(Term goal, Map<String, Term> bindings,
                             List<Map<String, Term>> solutions) {
        Term resolved = (bindings == null || bindings.isEmpty()) ? goal : goal.resolveBindings(bindings);
        List<Map<String, Term>> sols = new ArrayList<Map<String, Term>>();
        runSub(resolved, sols, 0);
        merge(bindings, sols, solutions);
        return !sols.isEmpty();
    }

    @Override
    public boolean solveInWorker(Term goal, Map<String, Term> bindings,
                                 List<Map<String, Term>> solutions, int maxSolutions) {
        Term resolved = (bindings == null || bindings.isEmpty()) ? goal : goal.resolveBindings(bindings);
        List<Map<String, Term>> sols = new ArrayList<Map<String, Term>>();
        runSub(resolved, sols, maxSolutions);
        merge(bindings, sols, solutions);
        return !sols.isEmpty();
    }

    private static void merge(Map<String, Term> bindings, List<Map<String, Term>> sols,
                              List<Map<String, Term>> out) {
        boolean empty = (bindings == null || bindings.isEmpty());
        for (Map<String, Term> sol : sols) {
            if (empty) {
                out.add(new HashMap<String, Term>(sol));
            } else {
                Map<String, Term> merged = new HashMap<String, Term>(bindings);
                merged.putAll(sol);
                out.add(merged);
            }
        }
    }

    // START_CHANGE: ISS-2025-0491 - 4.1 wave A: one engine, so no branch.
    /**
     * Run {@code goal} on a fresh {@code Machine}, sharing this context's {@link ResourceGuard} so
     * the inference budget and the Stop interrupt apply inside the sub-solve. {@code max} > 0 stops
     * after that many solutions.
     */
    private void runSub(Term goal, final List<Map<String, Term>> out, final int max) {
        it.denzosoft.jprolog.core.engine.v4.Machine m =
            new it.denzosoft.jprolog.core.engine.v4.Machine(prolog.getV4Engine(), guardForSubSolve());
        m.solve(goal, new it.denzosoft.jprolog.core.engine.v4.Machine.SolutionSink() {
            @Override public boolean onSolution(Map<String, Term> sol) {
                out.add(sol);
                return max <= 0 || out.size() < max;
            }
        });
    }
    // END_CHANGE: ISS-2025-0491

    private ResourceGuard guardForSubSolve() {
        ResourceGuard g = resourceGuard;
        return (g != null) ? g : new ResourceGuard(prolog != null ? prolog.getInferenceBudget() : 0L);
    }

    // START_CHANGE: ISS-2025-0491 - 4.1 wave A: `handleAttributeUnification` is DELETED with the
    // v2 engine. It was the dispatcher behind {@code Variable.AttributeUnifyHook} — the legacy
    // coroutining entry point that fired freeze/when/dif and the CLP(FD) domain check from inside
    // {@code Term.unify(Term, Map)}. v4 has its own wake queue (core.engine.v4.Coroutining) and
    // uninstalled the hook for the whole query anyway, so nothing calls it any more.
    // END_CHANGE: ISS-2025-0491
}
// END_CHANGE: ISS-2025-0484
