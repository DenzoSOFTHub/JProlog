package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.PrologStream;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

/**
 * set_input/1 - set_input(+Stream): make Stream the current input of this thread (ISO 8.11.3).
 */
public class SetInput implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("set_input/1 requires exactly 1 argument");
        }
        // START_CHANGE: ISS-2025-0472 - through the engine's stream table ('$stream'(N) or an alias)
        PrologStream s = IOStreamUtils.inputStream(query.getArguments().get(0), bindings, "set_input/1");
        StreamManager.streams().setCurrentInput(s);
        solutions.add(bindings);
        return true;
        // END_CHANGE: ISS-2025-0472
    }
}
