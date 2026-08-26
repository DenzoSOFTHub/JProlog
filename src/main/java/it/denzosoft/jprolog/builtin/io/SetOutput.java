package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.PrologStream;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

/**
 * set_output/1 - set_output(+Stream): make Stream the current output of this thread (ISO 8.11.4).
 */
public class SetOutput implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("set_output/1 requires exactly 1 argument");
        }
        // START_CHANGE: ISS-2025-0472 - through the engine's stream table ('$stream'(N) or an alias)
        PrologStream s = IOStreamUtils.outputStream(query.getArguments().get(0), bindings, "set_output/1");
        StreamManager.streams().setCurrentOutput(s);
        solutions.add(bindings);
        return true;
        // END_CHANGE: ISS-2025-0472
    }
}
