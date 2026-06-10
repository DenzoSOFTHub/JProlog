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
            
            // START_CHANGE: ISS-2025-0399 - type-faithful formatting: floats keep float syntax
            // (1.0 -> "1.0") and big integers stay exact (the old (long) cast collapsed both)
            java.lang.String stringValue =
                it.denzosoft.jprolog.builtin.conversion.AtomNumber.formatNumberExact((Number) numberTerm);
            // END_CHANGE: ISS-2025-0399
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
            // START_CHANGE: ISS-2025-0284 - parse integer strings as exact BigInteger so large
            // values keep full precision (Double.parseDouble loses precision past 2^53).
            // START_CHANGE: ISS-2025-0399 - parse via the shared type-faithful Prolog number
            // parser, so float syntax yields a FLOAT ("1.0" -> 1.0, not the integer 1)
            Number numberObj =
                it.denzosoft.jprolog.builtin.conversion.AtomNumber.parsePrologNumber(stringValue);
            if (numberObj == null) {
                return false; // String is not a valid number
            }
            // END_CHANGE: ISS-2025-0399
            // END_CHANGE: ISS-2025-0284

            Map<java.lang.String, Term> newBindings = new HashMap<>(bindings);
            if (numberTerm.unify(numberObj, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            
        } else if (numberTerm.isGround() && stringTerm.isGround()) {
            // Both ground - check if they represent the same value
            if (!(numberTerm instanceof Number) || !(stringTerm instanceof PrologString)) {
                return false;
            }
            
            java.lang.String stringValue = ((PrologString) stringTerm).getStringValue();

            // START_CHANGE: ISS-2025-0240 - exact roundtrip comparison
            // START_CHANGE: ISS-2025-0399 - parse the string and compare type-aware (Number.equals):
            // an integer never equals a float, big integers compare exactly via BigInteger
            Number parsed =
                it.denzosoft.jprolog.builtin.conversion.AtomNumber.parsePrologNumber(stringValue);
            if (parsed != null && parsed.equals(numberTerm)) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            return false;
            // END_CHANGE: ISS-2025-0399
            // END_CHANGE: ISS-2025-0240
            
        } else {
            // START_CHANGE: ISS-2025-0084 - Return false instead of throwing for normal failure
            return false;
            // END_CHANGE: ISS-2025-0084
        }
        
        return false;
    }
    
    // START_CHANGE: ISS-2025-0399 - formatNumber removed: formatting now goes through the shared
    // type-faithful AtomNumber.formatNumberExact (the (long) cast collapsed floats and big ints)
    // END_CHANGE: ISS-2025-0399
}