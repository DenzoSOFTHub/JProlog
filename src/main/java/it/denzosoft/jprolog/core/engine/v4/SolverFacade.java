package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.BuiltInRegistry;
import it.denzosoft.jprolog.core.engine.DebugController;
import it.denzosoft.jprolog.core.engine.EngineContext;
import it.denzosoft.jprolog.core.engine.KnowledgeBase;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.ResourceGuard;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0443 - engine v4, design B.5 (the context seen by legacy built-ins).
/**
 * The {@link SolverContext} a legacy {@code BuiltInWithContext} built-in receives while it runs on
 * the v4 machine.
 *
 * <p>Until 4.0.0 this was a <b>subclass of the recursive solver</b>, because
 * {@code executeWithContext} was typed against that concrete class and every entry point it did not
 * override therefore still ran the recursive algorithm. Wave W9 (ISS-2025-0484) deleted the
 * recursive engine and made the context an interface, so the facade is now a plain implementation
 * with no inherited behaviour at all: every sub-goal runs on {@link Machine#runSubQuery} — one
 * nested drive sharing this query's trail, choice-point floor and {@link ResourceGuard}, which is
 * what puts the inference budget, the Stop interrupt and the v4 trace inside every meta-call.
 */
public final class SolverFacade implements SolverContext {

    private final Machine machine;

    public SolverFacade(Machine machine) {
        this.machine = machine;
    }

    @Override
    public ResourceGuard getResourceGuard() { return machine.guard(); }

    @Override
    public DebugController getDebugController() {
        EngineContext ctx = machine.engine().context();
        return (ctx != null) ? ctx.getDebugController() : null;
    }

    @Override
    public Prolog getPrologContext() { return machine.engine().prolog(); }

    @Override
    public KnowledgeBase getKnowledgeBase() { return machine.engine().kb(); }

    @Override
    public BuiltInRegistry getBuiltInRegistry() { return machine.engine().registry(); }

    /**
     * Solve {@code query} on the running machine and return its solutions.
     *
     * <p>A {@link Machine} is single-threaded by construction (one goal stack, one choice-point
     * list, and bindings that live in shared {@code Variable} cells), so a caller on another thread
     * must go through {@link #solveInWorker} instead; arriving here off-thread is a programming
     * error (ISS-2025-0480).
     */
    @Override
    public List<Map<String, Term>> solve(Term query) {
        machine.assertOwnerThread("SolverContext.solve on the v4 facade");
        final List<Map<String, Term>> out = new ArrayList<Map<String, Term>>();
        machine.runSubQuery(query, new Machine.SolutionSink() {
            @Override public boolean onSolution(Map<String, Term> sol) { out.add(sol); return true; }
        });
        return out;
    }

    /**
     * The sub-solve seam. Cut inside {@code goal} is goal-local — which is the right semantics for
     * every caller that survives on v4, because the control constructs that DO propagate a cut
     * outwards ({@code ,/2}, {@code ;/2}, {@code ->/2}, {@code \+/1}, {@code call/N},
     * {@code catch/3}) are native in {@link Machine#stepN} and never reach a built-in.
     */
    @Override
    public boolean solveMeta(Term goal, final Map<String, Term> bindings,
                             final List<Map<String, Term>> solutions) {
        machine.assertOwnerThread("SolverContext.solveMeta on the v4 facade");
        Term resolved = (bindings == null || bindings.isEmpty()) ? goal : goal.resolveBindings(bindings);
        final Map<String, Term> base = (bindings == null || bindings.isEmpty()) ? null : bindings;
        final int before = solutions.size();
        machine.runSubQuery(resolved, new Machine.SolutionSink() {
            @Override public boolean onSolution(Map<String, Term> sol) {
                Map<String, Term> merged;
                if (base == null) {
                    merged = new HashMap<String, Term>(sol);
                } else {
                    merged = new HashMap<String, Term>(base);
                    merged.putAll(sol);
                }
                solutions.add(merged);
                return true;
            }
        });
        return solutions.size() > before;
    }

    // START_CHANGE: ISS-2025-0479 - wave W8, design B.13: a worker thread gets its OWN machine over
    // the same engine. The goal is copy_term'd in and each answer copied out (Workers), so no cell
    // is shared across machines; the worker's ResourceGuard carries the same limit as this query's.
    @Override
    public boolean solveInWorker(Term goal, Map<String, Term> bindings,
                                 List<Map<String, Term>> solutions, int maxSolutions) {
        return Workers.solve(machine.engine(), machine.guard(),          // ISS-2025-0624
                             goal, bindings, solutions, maxSolutions);
    }
    // END_CHANGE: ISS-2025-0479
}
// END_CHANGE: ISS-2025-0443
