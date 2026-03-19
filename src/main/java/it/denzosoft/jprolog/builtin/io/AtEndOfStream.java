// START_CHANGE: ISS-2025-0044 - Implement at_end_of_stream/0 and at_end_of_stream/1
package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;
import java.util.List;
import java.util.Map;

/**
 * at_end_of_stream/0 and at_end_of_stream/1 - ISO Prolog I/O predicates.
 * Tests whether the end of a stream has been reached.
 */
public class AtEndOfStream implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = (query.getArguments() == null) ? 0 : query.getArguments().size();

        String streamAlias;
        if (arity == 0) {
            streamAlias = StreamManager.getCurrentInput();
        } else if (arity == 1) {
            Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);
            if (!(streamTerm instanceof Atom)) {
                throw new PrologEvaluationException("at_end_of_stream: stream argument must be an atom");
            }
            streamAlias = ((Atom) streamTerm).getName();
        } else {
            throw new PrologEvaluationException("at_end_of_stream expects 0 or 1 arguments, got " + arity);
        }

        try {
            InputStream inputStream = StreamManager.getInputStream(streamAlias);
            if (inputStream == null) {
                throw new PrologEvaluationException("at_end_of_stream: stream does not exist: " + streamAlias);
            }

            PushbackInputStream pushbackStream;
            if (inputStream instanceof PushbackInputStream) {
                pushbackStream = (PushbackInputStream) inputStream;
            } else {
                pushbackStream = new PushbackInputStream(inputStream);
            }

            int b = pushbackStream.read();
            if (b == -1) {
                solutions.add(bindings);
                return true;
            } else {
                pushbackStream.unread(b);
                return false;
            }
        } catch (IOException e) {
            throw new PrologEvaluationException("I/O error in at_end_of_stream: " + e.getMessage());
        }
    }
}
// END_CHANGE: ISS-2025-0044
