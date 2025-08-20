package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Map;

/**
 * Implementation of get_char/1 predicate.
 * 
 * get_char(?Char)
 * 
 * Reads a single character from standard input and unifies it with Char.
 * If end of stream is reached, unifies with the atom 'end_of_file'.
 * 
 * Examples:
 * ?- get_char(X).
 * a
 * X = a.
 * 
 * ?- get_char(end_of_file).
 * % Succeeds if at end of input stream
 */
public class GetChar implements BuiltIn {
    
    private static BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 1) {
            throw new PrologEvaluationException("get_char/1 requires exactly 1 argument");
        }
        
        Term charTerm = query.getArguments().get(0);
        
        try {
            int charCode = reader.read();
            Term charValue;
            
            if (charCode == -1) {
                // End of file
                charValue = new Atom("end_of_file");
            } else {
                // Convert to character atom
                charValue = new Atom(String.valueOf((char) charCode));
            }
            
            // Try to unify
            if (charTerm.unify(charValue, bindings)) {
                solutions.add(bindings);
                return true;
            } else {
                return false;
            }
            
        } catch (IOException e) {
            throw new PrologEvaluationException("get_char/1: I/O error - " + e.getMessage());
        }
    }
}