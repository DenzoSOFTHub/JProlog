// START_CHANGE: ISS-2025-0044 - Implement at_end_of_stream/0 and at_end_of_stream/1
package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.PrologStream;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;

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
        if (arity != 0 && arity != 1) {
            throw new PrologEvaluationException("at_end_of_stream expects 0 or 1 arguments, got " + arity);
        }
        // START_CHANGE: ISS-2025-0472 - one-character lookahead on the engine's own stream
        Term streamArg = (arity == 1) ? query.getArguments().get(0) : null;
        PrologStream s = IOStreamUtils.inputStream(streamArg, bindings, "at_end_of_stream/" + arity);
        if (s.atEndOfStream()) {
            solutions.add(bindings);
            return true;
        }
        return false;
        // END_CHANGE: ISS-2025-0472
    }
}
// END_CHANGE: ISS-2025-0044
