package it.denzosoft.jprolog.builtin.string;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of number_string/2 predicate.
 * 
 * number_string(?Number, ?String)
 * 
 * True if String is the string representation of Number.
 * At least one argument must be instantiated.
 */
public class NumberString implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<java.lang.String, Term> bindings, List<Map<java.lang.String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("number_string/2 requires exactly 2 arguments.");
        }
        
        Term numberTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term stringTerm = query.getArguments().get(1).resolveBindings(bindings);
        
        if (numberTerm.isGround() && !stringTerm.isGround()) {
            // Convert number to string
            if (!(numberTerm instanceof Number)) {
                return false;
            }
            
            double numberValue = ((Number) numberTerm).getValue();
            java.lang.String stringValue = formatNumber(numberValue);
            PrologString stringObj = new PrologString(stringValue);
            
            Map<java.lang.String, Term> newBindings = new HashMap<>(bindings);
            if (stringTerm.unify(stringObj, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            
        } else if (!numberTerm.isGround() && stringTerm.isGround()) {
            // Convert string to number
            if (!(stringTerm instanceof PrologString)) {
                return false;
            }
            
            java.lang.String stringValue = ((PrologString) stringTerm).getStringValue();
            try {
                double numberValue = Double.parseDouble(stringValue.trim());
                Number numberObj = new Number(numberValue);
                
                Map<java.lang.String, Term> newBindings = new HashMap<>(bindings);
                if (numberTerm.unify(numberObj, newBindings)) {
                    solutions.add(newBindings);
                    return true;
                }
            } catch (NumberFormatException e) {
                return false; // String is not a valid number
            }
            
        } else if (numberTerm.isGround() && stringTerm.isGround()) {
            // Both ground - check if they represent the same value
            if (!(numberTerm instanceof Number) || !(stringTerm instanceof PrologString)) {
                return false;
            }
            
            double numberValue = ((Number) numberTerm).getValue();
            java.lang.String stringValue = ((PrologString) stringTerm).getStringValue();
            
            try {
                double parsedValue = Double.parseDouble(stringValue.trim());
                if (Math.abs(numberValue - parsedValue) < 1e-10) { // Allow for floating point precision
                    solutions.add(new HashMap<>(bindings));
                    return true;
                }
            } catch (NumberFormatException e) {
                return false;
            }
            return false;
            
        } else {
            throw new PrologEvaluationException("number_string/2: at least one argument must be instantiated.");
        }
        
        return false;
    }
    
    /**
     * Formats a number for string representation.
     * Removes unnecessary decimal points for integers.
     */
    private java.lang.String formatNumber(double value) {
        if (value == Math.floor(value) && !Double.isInfinite(value)) {
            // It's an integer
            return java.lang.String.valueOf((long) value);
        } else {
            // It's a floating point number
            return java.lang.String.valueOf(value);
        }
    }
}