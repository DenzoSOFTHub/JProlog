// START_CHANGE: LIM-007 - Stream Repositioning
package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.PrologStream;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * stream_position/2 - stream_position(+Stream, -Position): the current BYTE position.
 * The structured form is {@code stream_property(S, position(P))} plus {@code stream_position_data/3}.
 */
public class StreamPosition implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("stream_position/2 requires exactly 2 arguments");
        }

        Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term positionTerm = query.getArguments().get(1);

        // START_CHANGE: ISS-2025-0472 - read the engine's own stream position (exact on text streams)
        PrologStream s = StreamManager.stream(streamTerm);
        if (s == null) {
            throw new PrologException(ISOErrorTerms.existenceError("stream", streamTerm, "stream_position/2"));
        }
        if (!s.canReposition()) {
            throw new PrologException(
                ISOErrorTerms.permissionError("reposition", "stream", streamTerm, "stream_position/2"));
        }
        Map<String, Term> nb = new HashMap<>(bindings);
        if (positionTerm.resolveBindings(bindings).unify(new Number(s.bytePosition()), nb)) {
            solutions.add(nb);
            return true;
        }
        return false;
        // END_CHANGE: ISS-2025-0472
    }
}
// END_CHANGE: LIM-007
