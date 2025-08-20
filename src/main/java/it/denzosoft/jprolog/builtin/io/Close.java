package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

/**
 * close/1 - close(+Stream)
 * Closes a stream.
 */
public class Close implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("close/1 requires exactly 1 argument: close(+Stream).");
        }

        Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);

        if (!(streamTerm instanceof Atom)) {
            throw new PrologEvaluationException("close/1: Stream must be an atom.");
        }

        String streamAlias = ((Atom) streamTerm).getName();

        if (StreamManager.closeStream(streamAlias)) {
            solutions.add(bindings);
            return true;
        } else {
            throw new PrologEvaluationException("close/1: Cannot close stream '" + streamAlias + "' (not found or system stream).");
        }
    }
}