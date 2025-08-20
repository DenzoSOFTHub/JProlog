package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * current_input/1 - current_input(-Stream)
 * Gets the current input stream.
 */
public class CurrentInput implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("current_input/1 requires exactly 1 argument: current_input(-Stream).");
        }

        Term streamTerm = query.getArguments().get(0);
        String currentInputAlias = StreamManager.getCurrentInput();
        Atom currentInputAtom = new Atom(currentInputAlias);

        // Try to unify with the current input stream
        Term resolvedStreamTerm = streamTerm.resolveBindings(bindings);
        Map<String, Term> newBindings = new HashMap<>(bindings);
        
        if (resolvedStreamTerm.unify(currentInputAtom, newBindings)) {
            solutions.add(newBindings);
            return true;
        } else {
            return false;
        }
    }
}