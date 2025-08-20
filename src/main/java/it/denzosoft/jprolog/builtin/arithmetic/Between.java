package it.denzosoft.jprolog.builtin.arithmetic;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of between/3 predicate.
 * 
 * between(+Low, +High, ?Value)
 * 
 * True if Low ≤ Value ≤ High. On backtracking, Value is unified with all 
 * integer values between Low and High (inclusive).
 * 
 * Examples:
 * ?- between(1, 3, X).
 * X = 1 ;
 * X = 2 ;
 * X = 3.
 * 
 * ?- between(1, 3, 2).
 * true.
 * 
 * ?- between(1, 3, 5).
 * false.
 */
public class Between implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 3) {
            throw new PrologEvaluationException("between/3 requires exactly 3 arguments");
        }
        
        Term lowTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term highTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term valueTerm = query.getArguments().get(2);
        
        // Low and High must be instantiated
        if (lowTerm instanceof Variable || highTerm instanceof Variable) {
            throw new PrologEvaluationException("between/3: Low and High arguments must be instantiated");
        }
        
        if (!(lowTerm instanceof it.denzosoft.jprolog.core.terms.Number) || 
            !(highTerm instanceof it.denzosoft.jprolog.core.terms.Number)) {
            throw new PrologEvaluationException("between/3: Low and High arguments must be integers");
        }
        
        double lowValue = ((it.denzosoft.jprolog.core.terms.Number) lowTerm).getValue();
        double highValue = ((it.denzosoft.jprolog.core.terms.Number) highTerm).getValue();
        
        if (lowValue != Math.floor(lowValue) || highValue != Math.floor(highValue)) {
            throw new PrologEvaluationException("between/3: Low and High arguments must be integers");
        }
        
        int low = (int) lowValue;
        int high = (int) highValue;
        
        if (low > high) {
            return false; // Empty range
        }
        
        Term resolvedValueTerm = valueTerm.resolveBindings(bindings);
        
        if (resolvedValueTerm instanceof Variable) {
            // Generate all values in the range
            boolean foundSolution = false;
            for (int i = low; i <= high; i++) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (valueTerm.unify(new it.denzosoft.jprolog.core.terms.Number((double) i), newBindings)) {
                    solutions.add(newBindings);
                    foundSolution = true;
                }
            }
            return foundSolution;
        } else {
            // Check if the given value is in range
            if (!(resolvedValueTerm instanceof it.denzosoft.jprolog.core.terms.Number)) {
                return false;
            }
            
            double value = ((it.denzosoft.jprolog.core.terms.Number) resolvedValueTerm).getValue();
            if (value != Math.floor(value)) {
                return false; // Not an integer
            }
            
            int intValue = (int) value;
            if (intValue >= low && intValue <= high) {
                solutions.add(new HashMap<>(bindings));
                return true;
            } else {
                return false;
            }
        }
    }
}