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
 * Implementation of get_code/1 predicate.
 * 
 * get_code(?Code)
 * 
 * Reads a single character from standard input and unifies Code with its character code.
 * If end of stream is reached, unifies with -1.
 * 
 * Examples:
 * ?- get_code(X).
 * a
 * X = 97.
 * 
 * ?- get_code(-1).
 * % Succeeds if at end of input stream
 */
public class GetCode implements BuiltIn {
    
    private static BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 1) {
            throw new PrologEvaluationException("get_code/1 requires exactly 1 argument");
        }
        
        Term codeTerm = query.getArguments().get(0);
        
        try {
            int charCode = reader.read();
            Term codeValue;
            
            if (charCode == -1) {
                // End of file
                codeValue = new it.denzosoft.jprolog.core.terms.Number(-1.0);
            } else {
                // Character code as number
                codeValue = new it.denzosoft.jprolog.core.terms.Number((double) charCode);
            }
            
            // Try to unify
            if (codeTerm.unify(codeValue, bindings)) {
                solutions.add(bindings);
                return true;
            } else {
                return false;
            }
            
        } catch (IOException e) {
            throw new PrologEvaluationException("get_code/1: I/O error - " + e.getMessage());
        }
    }
}