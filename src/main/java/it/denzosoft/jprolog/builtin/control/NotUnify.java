package it.denzosoft.jprolog.builtin.control;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of the \\= (not unify) operator.
 * \\=(Term1, Term2) succeeds if Term1 and Term2 do not unify.
 * This is the logical negation of =/2.
 */
// START_CHANGE: ISS-2025-0007 - Implement missing inequality operator \=
public class NotUnify implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("\\=/2 requires exactly 2 arguments.");
        }

        Term term1 = query.getArguments().get(0).resolveBindings(bindings);
        Term term2 = query.getArguments().get(1).resolveBindings(bindings);

        // Create a temporary binding map to test if unification would succeed
        Map<String, Term> testBindings = new HashMap<>(bindings);
        
        // Try to unify the terms
        boolean canUnify = term1.unify(term2, testBindings);
        
        // \\= succeeds if unification fails
        if (!canUnify) {
            solutions.add(new HashMap<>(bindings)); // Add current bindings as solution
            return true;
        }
        
        return false; // Unification succeeded, so \\= fails
    }
}
// END_CHANGE: ISS-2025-0007