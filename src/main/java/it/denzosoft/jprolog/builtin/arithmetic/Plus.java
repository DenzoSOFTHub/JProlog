package it.denzosoft.jprolog.builtin.arithmetic;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of plus/3 predicate.
 * 
 * plus(?Int1, ?Int2, ?Int3)
 * 
 * True if Int1 + Int2 = Int3. Can be used for addition, subtraction, or checking
 * arithmetic relationships between integers.
 * 
 * Examples:
 * ?- plus(1, 2, X).
 * X = 3.
 * 
 * ?- plus(X, 2, 5).
 * X = 3.
 * 
 * ?- plus(1, X, 4).
 * X = 3.
 * 
 * ?- plus(2, 3, 5).
 * true.
 */
public class Plus implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 3) {
            throw new PrologEvaluationException("plus/3 requires exactly 3 arguments");
        }
        
        Term int1Term = query.getArguments().get(0).resolveBindings(bindings);
        Term int2Term = query.getArguments().get(1).resolveBindings(bindings);
        Term int3Term = query.getArguments().get(2).resolveBindings(bindings);
        
        boolean int1IsVar = int1Term instanceof Variable;
        boolean int2IsVar = int2Term instanceof Variable;
        boolean int3IsVar = int3Term instanceof Variable;
        
        int varCount = (int1IsVar ? 1 : 0) + (int2IsVar ? 1 : 0) + (int3IsVar ? 1 : 0);
        
        if (varCount > 1) {
            throw new PrologEvaluationException("plus/3: at most one argument can be uninstantiated");
        }
        
        // START_CHANGE: ISS-2025-0193 - Use long arithmetic when all inputs are integers
        if (varCount == 0) {
            // All arguments are instantiated - check the relationship
            if (!(int1Term instanceof it.denzosoft.jprolog.core.terms.Number) ||
                !(int2Term instanceof it.denzosoft.jprolog.core.terms.Number) ||
                !(int3Term instanceof it.denzosoft.jprolog.core.terms.Number)) {
                return false;
            }

            it.denzosoft.jprolog.core.terms.Number n1 = (it.denzosoft.jprolog.core.terms.Number) int1Term;
            it.denzosoft.jprolog.core.terms.Number n2 = (it.denzosoft.jprolog.core.terms.Number) int2Term;
            it.denzosoft.jprolog.core.terms.Number n3 = (it.denzosoft.jprolog.core.terms.Number) int3Term;

            if (n1.isInteger() && n2.isInteger() && n3.isInteger()) {
                if (n1.longValue() + n2.longValue() == n3.longValue()) {
                    solutions.add(new HashMap<>(bindings));
                    return true;
                }
                return false;
            }
            if (Double.compare(n1.getValue() + n2.getValue(), n3.getValue()) == 0) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            return false;
        }

        // Exactly one variable - compute its value
        Map<String, Term> newBindings = new HashMap<>(bindings);

        if (int1IsVar) {
            if (!(int2Term instanceof it.denzosoft.jprolog.core.terms.Number) ||
                !(int3Term instanceof it.denzosoft.jprolog.core.terms.Number)) {
                return false;
            }
            it.denzosoft.jprolog.core.terms.Number n2 = (it.denzosoft.jprolog.core.terms.Number) int2Term;
            it.denzosoft.jprolog.core.terms.Number n3 = (it.denzosoft.jprolog.core.terms.Number) int3Term;
            it.denzosoft.jprolog.core.terms.Number result = (n2.isInteger() && n3.isInteger())
                ? new it.denzosoft.jprolog.core.terms.Number(n3.longValue() - n2.longValue())
                : new it.denzosoft.jprolog.core.terms.Number(n3.getValue() - n2.getValue());

            if (query.getArguments().get(0).unify(result, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        } else if (int2IsVar) {
            if (!(int1Term instanceof it.denzosoft.jprolog.core.terms.Number) ||
                !(int3Term instanceof it.denzosoft.jprolog.core.terms.Number)) {
                return false;
            }
            it.denzosoft.jprolog.core.terms.Number n1 = (it.denzosoft.jprolog.core.terms.Number) int1Term;
            it.denzosoft.jprolog.core.terms.Number n3 = (it.denzosoft.jprolog.core.terms.Number) int3Term;
            it.denzosoft.jprolog.core.terms.Number result = (n1.isInteger() && n3.isInteger())
                ? new it.denzosoft.jprolog.core.terms.Number(n3.longValue() - n1.longValue())
                : new it.denzosoft.jprolog.core.terms.Number(n3.getValue() - n1.getValue());

            if (query.getArguments().get(1).unify(result, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        } else {
            if (!(int1Term instanceof it.denzosoft.jprolog.core.terms.Number) ||
                !(int2Term instanceof it.denzosoft.jprolog.core.terms.Number)) {
                return false;
            }
            it.denzosoft.jprolog.core.terms.Number n1 = (it.denzosoft.jprolog.core.terms.Number) int1Term;
            it.denzosoft.jprolog.core.terms.Number n2 = (it.denzosoft.jprolog.core.terms.Number) int2Term;
            it.denzosoft.jprolog.core.terms.Number result = (n1.isInteger() && n2.isInteger())
                ? new it.denzosoft.jprolog.core.terms.Number(n1.longValue() + n2.longValue())
                : new it.denzosoft.jprolog.core.terms.Number(n1.getValue() + n2.getValue());

            if (query.getArguments().get(2).unify(result, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        }
        // END_CHANGE: ISS-2025-0193
    }
}