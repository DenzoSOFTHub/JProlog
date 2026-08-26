// START_CHANGE: LIM-007 - Stream Repositioning
package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.PrologStream;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * set_stream_position/2 - set_stream_position(+Stream, +Position).
 *
 * <p>{@code Position} is either a plain byte offset or the opaque term
 * {@code '$stream_position'(CharCount, LineCount, LinePosition, ByteCount)} that
 * {@code stream_property(S, position(P))} hands out (ISS-2025-0473).
 */
public class SetStreamPosition implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("set_stream_position/2 requires exactly 2 arguments");
        }

        Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term positionTerm = query.getArguments().get(1).resolveBindings(bindings);

        PrologStream s = StreamManager.stream(streamTerm);
        if (s == null) {
            throw new PrologException(
                ISOErrorTerms.existenceError("stream", streamTerm, "set_stream_position/2"));
        }

        long position;
        if (positionTerm instanceof Number) {
            position = ((Number) positionTerm).longValue();
        } else if (positionTerm instanceof CompoundTerm
                && "$stream_position".equals(positionTerm.getName())
                && positionTerm.getArguments() != null && positionTerm.getArguments().size() == 4) {
            Term b = positionTerm.getArguments().get(3);
            if (!(b instanceof Number)) {
                throw new PrologException(
                    ISOErrorTerms.domainError("stream_position", positionTerm, "set_stream_position/2"));
            }
            position = ((Number) b).longValue();
        } else {
            throw new PrologException(
                ISOErrorTerms.domainError("stream_position", positionTerm, "set_stream_position/2"));
        }
        if (position < 0) {
            throw new PrologEvaluationException("set_stream_position/2: Position must be non-negative");
        }
        if (!s.canReposition()) {
            throw new PrologException(
                ISOErrorTerms.permissionError("reposition", "stream", streamTerm, "set_stream_position/2"));
        }

        // START_CHANGE: ISS-2025-0472 - flush the decode buffer so the NEXT read really starts here
        try {
            s.reposition(position);
            solutions.add(new HashMap<>(bindings));
            return true;
        } catch (PrologException pe) {
            throw pe;
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw new PrologEvaluationException("set_stream_position/2: I/O error: " + e.getMessage());
        }
        // END_CHANGE: ISS-2025-0472
    }
}
// END_CHANGE: LIM-007
