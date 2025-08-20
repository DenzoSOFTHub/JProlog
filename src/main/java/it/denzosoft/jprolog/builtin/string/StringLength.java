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
 * Implementation of string_length/2 predicate.
 * 
 * string_length(+String, ?Length)
 * 
 * True if Length is the number of characters in String.
 */
public class StringLength implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<java.lang.String, Term> bindings, List<Map<java.lang.String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("string_length/2 requires exactly 2 arguments.");
        }
        
        Term stringTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term lengthTerm = query.getArguments().get(1).resolveBindings(bindings);
        
        // First argument must be a string
        if (stringTerm instanceof Variable) {
            throw new PrologEvaluationException("string_length/2: first argument must be instantiated to a string.");
        }
        
        if (!(stringTerm instanceof PrologString)) {
            return false; // Not a string, fail
        }
        
        PrologString prologString = (PrologString) stringTerm;
        int length = prologString.getStringValue().length();
        Number lengthNumber = new Number((double) length);
        
        // Try to unify with the length
        Map<java.lang.String, Term> newBindings = new HashMap<>(bindings);
        if (lengthTerm.unify(lengthNumber, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        
        return false;
    }
}