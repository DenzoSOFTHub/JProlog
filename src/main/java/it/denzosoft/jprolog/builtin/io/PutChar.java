package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.List;
import java.util.Map;

/**
 * Implementation of put_char/1 predicate.
 * 
 * put_char(+Char)
 * 
 * Writes a single character to standard output.
 * Char must be an atom representing a single character.
 * 
 * Examples:
 * ?- put_char(a).
 * a
 * true.
 * 
 * ?- put_char('\\n').
 * 
 * true.
 */
public class PutChar implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 1) {
            throw new PrologEvaluationException("put_char/1 requires exactly 1 argument");
        }
        
        Term charTerm = query.getArguments().get(0).resolveBindings(bindings);
        
        if (charTerm instanceof Variable) {
            return false; // Fail silently for unbound variables
        }
        
        if (!(charTerm instanceof Atom)) {
            return false; // Fail silently for non-atoms
        }
        
        String charString = ((Atom) charTerm).getName();
        
        if (charString.length() != 1) {
            return false; // Fail silently for multi-character atoms
        }
        
        // Write the character to standard output
        System.out.print(charString);
        System.out.flush();
        
        solutions.add(bindings);
        return true;
    }
}