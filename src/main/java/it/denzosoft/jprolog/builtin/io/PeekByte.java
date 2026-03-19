// START_CHANGE: ISS-2025-0045 - Implement peek_byte/1 and peek_byte/2
package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * peek_byte/1 and peek_byte/2 - ISO Prolog I/O predicates.
 * Reads a byte from a binary stream without consuming it.
 */
public class PeekByte implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = query.getArguments().size();

        String streamAlias;
        Term byteTerm;

        if (arity == 1) {
            streamAlias = StreamManager.getCurrentInput();
            byteTerm = query.getArguments().get(0);
        } else if (arity == 2) {
            Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);
            if (!(streamTerm instanceof Atom)) {
                throw new PrologEvaluationException("peek_byte/2: stream argument must be an atom");
            }
            streamAlias = ((Atom) streamTerm).getName();
            byteTerm = query.getArguments().get(1);
        } else {
            throw new PrologEvaluationException("peek_byte expects 1 or 2 arguments, got " + arity);
        }

        try {
            InputStream inputStream = StreamManager.getInputStream(streamAlias);
            if (inputStream == null) {
                throw new PrologEvaluationException("peek_byte: stream does not exist: " + streamAlias);
            }

            PushbackInputStream pushbackStream;
            if (inputStream instanceof PushbackInputStream) {
                pushbackStream = (PushbackInputStream) inputStream;
            } else {
                pushbackStream = new PushbackInputStream(inputStream);
            }

            int b = pushbackStream.read();
            Term byteValue;
            if (b == -1) {
                byteValue = new Number(-1, true);
            } else {
                pushbackStream.unread(b);
                byteValue = new Number(b, true);
            }

            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (byteTerm.unify(byteValue, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        } catch (IOException e) {
            throw new PrologEvaluationException("I/O error in peek_byte: " + e.getMessage());
        }
    }
}
// END_CHANGE: ISS-2025-0045
