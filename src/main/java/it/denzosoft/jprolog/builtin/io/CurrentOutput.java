package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * current_output/1 - current_output(-Stream)
 * Gets the current output stream.
 */
public class CurrentOutput implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("current_output/1 requires exactly 1 argument: current_output(-Stream).");
        }

        Term streamTerm = query.getArguments().get(0);
        String currentOutputAlias = StreamManager.getCurrentOutput();
        Atom currentOutputAtom = new Atom(currentOutputAlias);

        // Try to unify with the current output stream
        Term resolvedStreamTerm = streamTerm.resolveBindings(bindings);
        Map<String, Term> newBindings = new HashMap<>(bindings);
        
        if (resolvedStreamTerm.unify(currentOutputAtom, newBindings)) {
            solutions.add(newBindings);
            return true;
        } else {
            return false;
        }
    }
}