package it.denzosoft.jprolog.builtin.arithmetic;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of succ/2 predicate.
 * 
 * succ(?Int1, ?Int2)
 * 
 * True if Int2 = Int1 + 1 and both are non-negative integers.
 * Can be used to get the successor or predecessor of an integer.
 * 
 * Examples:
 * ?- succ(1, X).
 * X = 2.
 * 
 * ?- succ(X, 5).
 * X = 4.
 * 
 * ?- succ(3, 4).
 * true.
 * 
 * ?- succ(-1, 0).
 * false.
 */
public class Succ implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 2) {
            throw new PrologEvaluationException("succ/2 requires exactly 2 arguments");
        }
        
        Term int1Term = query.getArguments().get(0).resolveBindings(bindings);
        Term int2Term = query.getArguments().get(1).resolveBindings(bindings);
        
        boolean int1IsVar = int1Term instanceof Variable;
        boolean int2IsVar = int2Term instanceof Variable;
        
        if (int1IsVar && int2IsVar) {
            throw new PrologEvaluationException("succ/2: at least one argument must be instantiated");
        }
        
        if (!int1IsVar && !int2IsVar) {
            // Both are instantiated - check the relationship
            if (!(int1Term instanceof it.denzosoft.jprolog.core.terms.Number) || 
                !(int2Term instanceof it.denzosoft.jprolog.core.terms.Number)) {
                return false;
            }
            
            double value1 = ((it.denzosoft.jprolog.core.terms.Number) int1Term).getValue();
            double value2 = ((it.denzosoft.jprolog.core.terms.Number) int2Term).getValue();
            
            if (value1 != Math.floor(value1) || value2 != Math.floor(value2) ||
                value1 < 0 || value2 < 0) {
                return false;
            }
            
            // START_CHANGE: ISS-2025-0190 - Use long to avoid int overflow
            if ((long) value2 == (long) value1 + 1) {
            // END_CHANGE: ISS-2025-0190
                solutions.add(new HashMap<>(bindings));
                return true;
            } else {
                return false;
            }
        }
        
        if (int1IsVar) {
            // Int1 is variable, Int2 is instantiated
            if (!(int2Term instanceof it.denzosoft.jprolog.core.terms.Number)) {
                return false;
            }
            
            double value2 = ((it.denzosoft.jprolog.core.terms.Number) int2Term).getValue();
            // START_CHANGE: ISS-2025-0169 - Allow Int2 to be 1 (when Int1=0), so check >= 1 not > 0
            if (value2 != Math.floor(value2) || value2 < 1) {
                return false; // Int2 must be a positive integer (>= 1, since Int1 >= 0)
            }
            // END_CHANGE: ISS-2025-0169
            
            // START_CHANGE: ISS-2025-0190 - Use long to avoid int overflow
            long int2Value = (long) value2;
            long int1Value = int2Value - 1;
            // END_CHANGE: ISS-2025-0190

            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (query.getArguments().get(0).unify(new it.denzosoft.jprolog.core.terms.Number((long) int1Value)   /* ISS-2025-0424 */, newBindings)) {
                solutions.add(newBindings);
                return true;
            } else {
                return false;
            }
        } else {
            // Int2 is variable, Int1 is instantiated
            if (!(int1Term instanceof it.denzosoft.jprolog.core.terms.Number)) {
                return false;
            }
            
            double value1 = ((it.denzosoft.jprolog.core.terms.Number) int1Term).getValue();
            if (value1 != Math.floor(value1) || value1 < 0) {
                return false; // Int1 must be a non-negative integer
            }
            
            // START_CHANGE: ISS-2025-0190 - Use long to avoid int overflow
            long int1Value = (long) value1;
            long int2Value = int1Value + 1;
            // END_CHANGE: ISS-2025-0190
            
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (query.getArguments().get(1).unify(new it.denzosoft.jprolog.core.terms.Number((long) int2Value)   /* ISS-2025-0424 */, newBindings)) {
                solutions.add(newBindings);
                return true;
            } else {
                return false;
            }
        }
    }
}