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
        
        if (varCount == 0) {
            // All arguments are instantiated - check the relationship
            if (!(int1Term instanceof it.denzosoft.jprolog.core.terms.Number) || 
                !(int2Term instanceof it.denzosoft.jprolog.core.terms.Number) ||
                !(int3Term instanceof it.denzosoft.jprolog.core.terms.Number)) {
                return false;
            }
            
            double value1 = ((it.denzosoft.jprolog.core.terms.Number) int1Term).getValue();
            double value2 = ((it.denzosoft.jprolog.core.terms.Number) int2Term).getValue();
            double value3 = ((it.denzosoft.jprolog.core.terms.Number) int3Term).getValue();
            
            if (Math.abs((value1 + value2) - value3) < 1e-10) {
                solutions.add(new HashMap<>(bindings));
                return true;
            } else {
                return false;
            }
        }
        
        // Exactly one variable - compute its value
        Map<String, Term> newBindings = new HashMap<>(bindings);
        
        if (int1IsVar) {
            // Int1 = Int3 - Int2
            if (!(int2Term instanceof it.denzosoft.jprolog.core.terms.Number) || 
                !(int3Term instanceof it.denzosoft.jprolog.core.terms.Number)) {
                return false;
            }
            
            double value2 = ((it.denzosoft.jprolog.core.terms.Number) int2Term).getValue();
            double value3 = ((it.denzosoft.jprolog.core.terms.Number) int3Term).getValue();
            double result = value3 - value2;
            
            if (query.getArguments().get(0).unify(new it.denzosoft.jprolog.core.terms.Number(result), newBindings)) {
                solutions.add(newBindings);
                return true;
            } else {
                return false;
            }
        } else if (int2IsVar) {
            // Int2 = Int3 - Int1
            if (!(int1Term instanceof it.denzosoft.jprolog.core.terms.Number) || 
                !(int3Term instanceof it.denzosoft.jprolog.core.terms.Number)) {
                return false;
            }
            
            double value1 = ((it.denzosoft.jprolog.core.terms.Number) int1Term).getValue();
            double value3 = ((it.denzosoft.jprolog.core.terms.Number) int3Term).getValue();
            double result = value3 - value1;
            
            if (query.getArguments().get(1).unify(new it.denzosoft.jprolog.core.terms.Number(result), newBindings)) {
                solutions.add(newBindings);
                return true;
            } else {
                return false;
            }
        } else {
            // int3IsVar: Int3 = Int1 + Int2
            if (!(int1Term instanceof it.denzosoft.jprolog.core.terms.Number) || 
                !(int2Term instanceof it.denzosoft.jprolog.core.terms.Number)) {
                return false;
            }
            
            double value1 = ((it.denzosoft.jprolog.core.terms.Number) int1Term).getValue();
            double value2 = ((it.denzosoft.jprolog.core.terms.Number) int2Term).getValue();
            double result = value1 + value2;
            
            if (query.getArguments().get(2).unify(new it.denzosoft.jprolog.core.terms.Number(result), newBindings)) {
                solutions.add(newBindings);
                return true;
            } else {
                return false;
            }
        }
    }
}