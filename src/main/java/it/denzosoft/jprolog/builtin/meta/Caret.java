package it.denzosoft.jprolog.builtin.meta;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.CutStatus;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0398 - ^/2 callable as an ordinary goal
/**
 * Implementation of '^'/2 as a callable goal.
 *
 * <p>{@code V^Goal} outside {@code bagof/3}/{@code setof/3} simply calls {@code Goal},
 * ignoring the quantified variable(s) — the SWI/SICStus/YAP consensus behaviour.
 * Inside bagof/setof the {@code ^} prefix is stripped by their own goal walker
 * ({@code CollectionUtils}) before solving, so this builtin never interferes there.
 */
public class Caret implements BuiltInWithContext {

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query,
                                      Map<String, Term> bindings,
                                      List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args.size() != 2) {
            return false;
        }
        Term goal = args.get(1).resolveBindings(bindings);
        if (goal instanceof Variable) {
            throw new PrologException(ISOErrorTerms.instantiationError("^/2"));
        }
        if (!(goal instanceof Atom) && !(goal instanceof CompoundTerm)) {
            throw new PrologException(ISOErrorTerms.typeError("callable", goal, "^/2"));
        }
        List<Map<String, Term>> goalSolutions = new ArrayList<>();
        boolean success = solver.solve(goal, new HashMap<>(bindings), goalSolutions, CutStatus.notOccurred());
        if (success) {
            solutions.addAll(goalSolutions);
            return true;
        }
        return false;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("^/2 requires context");
    }
}
// END_CHANGE: ISS-2025-0398
