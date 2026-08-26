package it.denzosoft.jprolog.builtin.control;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0485 - wave W9: the ISO control constructs (;/2, ->/2, \+/1, call/N,
// catch/3, ^/2) used to have Java implementations that only the recursive SolverContext ever
// dispatched — it was the one engine that looked them up in the registry. Both surviving machines
// (v4 Machine.stepN, v2 MachineSolver.stepN) implement them natively, with real choice points and
// real cut barriers, and never consult the registry for them. The implementations
// (Conjunction, IfThen, IfThenElse, NegationAsFailure, Catch, Call, Caret) are therefore deleted.
//
// What the REGISTRATION still buys us is ISO protection: BuiltInRegistry.isBuiltIn(f, a) answers
// false for an unregistered name, and that predicate is what makes assert/retract/clause raise
// permission_error(modify, static_procedure, call/1) instead of quietly redefining a control
// construct. This placeholder keeps that answer true and nothing else.
/**
 * Registry placeholder for a control construct that both engines implement natively.
 *
 * <p>Executing it is a programming error: it means a dispatcher consulted the built-in registry for
 * a construct it was supposed to handle itself.
 */
public final class ControlConstruct implements BuiltIn {

    private final String indicator;

    public ControlConstruct(String indicator) {
        this.indicator = indicator;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new IllegalStateException(
            "control construct " + indicator + " must be handled by the engine, not the registry");
    }
}
// END_CHANGE: ISS-2025-0485
