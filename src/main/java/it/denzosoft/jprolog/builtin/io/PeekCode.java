package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * peek_code/1 - ISO Prolog I/O predicate
 * Reads the next character code from the current input stream without consuming it.
 */
public class PeekCode implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("peek_code/1 requires exactly 1 argument");
        }

        Term codeTerm = query.getArguments().get(0);
        
        try {
            String currentInputAlias = StreamManager.getCurrentInput();
            InputStream inputStream = StreamManager.getInputStream(currentInputAlias);
            
            if (inputStream == null) {
                throw new PrologEvaluationException("Cannot peek from current input stream: " + currentInputAlias);
            }

            // Use PushbackInputStream to peek without consuming
            PushbackInputStream pushbackStream;
            if (inputStream instanceof PushbackInputStream) {
                pushbackStream = (PushbackInputStream) inputStream;
            } else {
                pushbackStream = new PushbackInputStream(inputStream);
            }

            int charCode = pushbackStream.read();
            
            if (charCode == -1) {
                // End of stream
                Number endOfFileCode = new Number(-1);
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (codeTerm.unify(endOfFileCode, newBindings)) {
                    solutions.add(newBindings);
                    return true;
                } else {
                    return false;
                }
            } else {
                // Push the character back
                pushbackStream.unread(charCode);
                
                Number codeNumber = new Number(charCode);
                
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (codeTerm.unify(codeNumber, newBindings)) {
                    solutions.add(newBindings);
                    return true;
                } else {
                    return false;
                }
            }

        } catch (IOException e) {
            throw new PrologEvaluationException("I/O error in peek_code/1: " + e.getMessage());
        }
    }
}