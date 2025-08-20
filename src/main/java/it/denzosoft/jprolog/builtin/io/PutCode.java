package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.List;
import java.util.Map;

/**
 * Implementation of put_code/1 predicate.
 * 
 * put_code(+Code)
 * 
 * Writes a single character to standard output based on its character code.
 * Code must be an integer representing a valid character code.
 * 
 * Examples:
 * ?- put_code(97).
 * a
 * true.
 * 
 * ?- put_code(10).
 * 
 * true.
 */
public class PutCode implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 1) {
            throw new PrologEvaluationException("put_code/1 requires exactly 1 argument");
        }
        
        Term codeTerm = query.getArguments().get(0).resolveBindings(bindings);
        
        if (codeTerm instanceof Variable) {
            return false; // Fail silently for unbound variables
        }
        
        if (!(codeTerm instanceof it.denzosoft.jprolog.core.terms.Number)) {
            return false; // Fail silently for non-numbers
        }
        
        double codeValue = ((it.denzosoft.jprolog.core.terms.Number) codeTerm).getValue();
        
        if (codeValue != Math.floor(codeValue) || codeValue < 0 || codeValue > 1114111) {
            return false; // Fail silently for invalid codes
        }
        
        int charCode = (int) codeValue;
        
        // Write the character to standard output
        System.out.print((char) charCode);
        System.out.flush();
        
        solutions.add(bindings);
        return true;
    }
}