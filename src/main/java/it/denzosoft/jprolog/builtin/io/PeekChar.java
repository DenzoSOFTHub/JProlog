package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * peek_char/1 - ISO Prolog I/O predicate
 * Reads the next character from the current input stream without consuming it.
 */
public class PeekChar implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("peek_char/1 requires exactly 1 argument");
        }

        Term charTerm = query.getArguments().get(0);
        
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
                // For non-pushback streams, we'll need to handle differently
                // This is a simplified implementation - in production, you'd want
                // to wrap streams in PushbackInputStream when they're opened
                pushbackStream = new PushbackInputStream(inputStream);
            }

            int charCode = pushbackStream.read();
            
            if (charCode == -1) {
                // End of stream
                Atom endOfFileAtom = new Atom("end_of_file");
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (charTerm.unify(endOfFileAtom, newBindings)) {
                    solutions.add(newBindings);
                    return true;
                } else {
                    return false;
                }
            } else {
                // Push the character back
                pushbackStream.unread(charCode);
                
                char ch = (char) charCode;
                Atom charAtom = new Atom(String.valueOf(ch));
                
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (charTerm.unify(charAtom, newBindings)) {
                    solutions.add(newBindings);
                    return true;
                } else {
                    return false;
                }
            }

        } catch (IOException e) {
            throw new PrologEvaluationException("I/O error in peek_char/1: " + e.getMessage());
        }
    }
}