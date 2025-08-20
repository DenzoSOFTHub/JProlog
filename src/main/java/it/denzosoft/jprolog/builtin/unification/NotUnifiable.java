// START_CHANGE: ISS-2025-0007 - Implement missing inequality operator \=
package it.denzosoft.jprolog.builtin.unification;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of the unification inequality operator \= (not unifiable).
 * 
 * This predicate succeeds if the two terms cannot be unified.
 * It's the logical negation of the unification operator =.
 * 
 * Usage:
 * - \=(X, Y) succeeds if X and Y cannot be unified
 * - \=([1,2,3], []) succeeds (different structures)
 * - \=(X, X) fails (same variable always unifies)
 * 
 * @author JProlog Team  
 * @since ISS-2025-0007
 */
public class NotUnifiable implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            return false;
        }
        
        Term term1 = query.getArguments().get(0);
        Term term2 = query.getArguments().get(1);
        
        // Resolve bindings for both terms
        Term resolvedTerm1 = term1.resolveBindings(bindings);
        Term resolvedTerm2 = term2.resolveBindings(bindings);
        
        // Try to unify the terms in a temporary binding context
        Map<String, Term> tempBindings = new HashMap<>(bindings);
        
        try {
            // Attempt unification - if it succeeds, \= should fail
            boolean canUnify = resolvedTerm1.unify(resolvedTerm2, tempBindings);
            
            if (!canUnify) {
                // Terms cannot unify, so \= succeeds
                solutions.add(new HashMap<>(bindings));
                return true;
            } else {
                // Terms can unify, so \= fails
                return false;
            }
            
        } catch (Exception e) {
            // If unification throws an exception, consider terms as not unifiable
            solutions.add(new HashMap<>(bindings));
            return true;
        }
    }
}
// END_CHANGE: ISS-2025-0007