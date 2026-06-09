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
                // START_CHANGE: ISS-2025-0284 - parse integer strings as exact BigInteger so large
                // values keep full precision (Double.parseDouble loses precision past 2^53).
                java.lang.String trimmed = stringValue.trim();
                Number numberObj;
                if (trimmed.matches("[+-]?\\d+")) {
                    numberObj = new Number(new java.math.BigInteger(trimmed));
                } else {
                    numberObj = new Number(Double.parseDouble(trimmed));
                }
                // END_CHANGE: ISS-2025-0284

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

            // START_CHANGE: ISS-2025-0240 - exact roundtrip comparison: format number and compare strings
            // (Avoids epsilon-based false positives. Two numbers are equal iff their canonical reprs match.)
            try {
                double parsedValue = Double.parseDouble(stringValue.trim());
                if (Double.doubleToLongBits(numberValue) == Double.doubleToLongBits(parsedValue)) {
                    solutions.add(new HashMap<>(bindings));
                    return true;
                }
            } catch (NumberFormatException e) {
                return false;
            }
            return false;
            // END_CHANGE: ISS-2025-0240
            
        } else {
            // START_CHANGE: ISS-2025-0084 - Return false instead of throwing for normal failure
            return false;
            // END_CHANGE: ISS-2025-0084
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