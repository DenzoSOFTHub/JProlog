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
 * Implementation of sub_string/5 predicate.
 * 
 * sub_string(+String, ?Before, ?Length, ?After, ?SubString)
 * 
 * True if SubString is a substring of String, where:
 * - Before is the number of characters before the substring
 * - Length is the length of the substring
 * - After is the number of characters after the substring
 * - Before + Length + After = length of String
 */
public class SubString implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<java.lang.String, Term> bindings, List<Map<java.lang.String, Term>> solutions) {
        if (query.getArguments().size() != 5) {
            throw new PrologEvaluationException("sub_string/5 requires exactly 5 arguments.");
        }
        
        Term stringTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term beforeTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term lengthTerm = query.getArguments().get(2).resolveBindings(bindings);
        Term afterTerm = query.getArguments().get(3).resolveBindings(bindings);
        Term subStringTerm = query.getArguments().get(4).resolveBindings(bindings);
        
        // First argument must be a string
        if (stringTerm instanceof Variable) {
            throw new PrologEvaluationException("sub_string/5: first argument must be instantiated to a string.");
        }
        
        if (!(stringTerm instanceof PrologString)) {
            return false;
        }
        
        java.lang.String mainString = ((PrologString) stringTerm).getStringValue();
        int mainLength = mainString.length();
        
        boolean foundSolution = false;
        
        // Try all possible substrings
        for (int before = 0; before <= mainLength; before++) {
            for (int length = 0; length <= mainLength - before; length++) {
                int after = mainLength - before - length;
                
                java.lang.String substring = mainString.substring(before, before + length);
                
                Map<java.lang.String, Term> newBindings = new HashMap<>(bindings);
                
                // Try to unify all the parameters
                boolean unified = true;
                
                if (!beforeTerm.unify(new Number((double) before), newBindings)) {
                    unified = false;
                }
                
                if (unified && !lengthTerm.unify(new Number((double) length), newBindings)) {
                    unified = false;
                }
                
                if (unified && !afterTerm.unify(new Number((double) after), newBindings)) {
                    unified = false;
                }
                
                if (unified && !subStringTerm.unify(new PrologString(substring), newBindings)) {
                    unified = false;
                }
                
                if (unified) {
                    solutions.add(newBindings);
                    foundSolution = true;
                }
            }
        }
        
        return foundSolution;
    }
}