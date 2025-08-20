package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

/**
 * set_output/1 - set_output(+Stream)
 * Sets the current output stream.
 */
public class SetOutput implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("set_output/1 requires exactly 1 argument: set_output(+Stream).");
        }

        Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);

        if (!(streamTerm instanceof Atom)) {
            throw new PrologEvaluationException("set_output/1: Stream must be an atom.");
        }

        String streamAlias = ((Atom) streamTerm).getName();

        if (StreamManager.getOutputStream(streamAlias) != null) {
            StreamManager.setCurrentOutput(streamAlias);
            solutions.add(bindings);
            return true;
        } else {
            throw new PrologEvaluationException("set_output/1: Stream '" + streamAlias + "' does not exist or is not an output stream.");
        }
    }
}