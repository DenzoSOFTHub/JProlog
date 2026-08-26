package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

/**
 * A built-in that needs the engine's execution context — typically because it runs a sub-goal.
 *
 * <p>Since 4.0.0 (wave W9, ISS-2025-0484) the context is the {@link SolverContext} <b>interface</b>,
 * not the concrete recursive solver class that used to be the only implementation. On the
 * default v4 engine the object is the per-query {@code core.engine.v4.SolverFacade}, so a sub-goal
 * runs on the running machine; on the v2 fallback it is the engine's {@link EngineContext}.
 */
public interface BuiltInWithContext extends BuiltIn {
    /**
     * Execute a built-in predicate with context.
     *
     * @param solver The execution context (sub-solve seams + the engine's knowledge base/registry)
     * @param query The query term to execute
     * @param bindings Current variable bindings
     * @param solutions List to add successful solutions to
     * @return true if execution was successful
     */
    boolean executeWithContext(SolverContext solver, Term query, Map<String, Term> bindings,
                               List<Map<String, Term>> solutions);
}
