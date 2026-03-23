// START_CHANGE: LIM-007 - Stream Repositioning
package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.channels.FileChannel;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * stream_position/2 - Get the current position of a stream.
 * stream_position(+Stream, -Position)
 *
 * Unifies Position with the current byte position of the stream.
 * Only file-backed streams support position querying.
 */
public class StreamPosition implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException(
                "stream_position/2 requires exactly 2 arguments");
        }

        Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term positionTerm = query.getArguments().get(1);

        // Validate stream argument
        if (!(streamTerm instanceof Atom)) {
            throw new PrologEvaluationException(
                "stream_position/2: Stream must be an atom");
        }

        String streamAlias = ((Atom) streamTerm).getName();

        // Check stream exists
        if (!StreamManager.hasStream(streamAlias)) {
            throw new PrologException(
                ISOErrorTerms.existenceError("stream", streamTerm, "stream_position/2"));
        }

        // Standard streams: position is not available
        if ("user_input".equals(streamAlias) || "user_output".equals(streamAlias)
                || "user_error".equals(streamAlias)) {
            throw new PrologException(
                ISOErrorTerms.permissionError("reposition", "stream", streamTerm,
                    "stream_position/2"));
        }

        try {
            long position = -1;

            InputStream is = StreamManager.getInputStream(streamAlias);
            if (is instanceof FileInputStream) {
                FileChannel channel = ((FileInputStream) is).getChannel();
                position = channel.position();
            }

            if (position < 0) {
                OutputStream os = StreamManager.getOutputStream(streamAlias);
                if (os instanceof FileOutputStream) {
                    FileChannel channel = ((FileOutputStream) os).getChannel();
                    position = channel.position();
                }
            }

            if (position < 0) {
                throw new PrologException(
                    ISOErrorTerms.permissionError("reposition", "stream", streamTerm,
                        "stream_position/2"));
            }

            // Unify position with the result
            Term positionValue = new Number(position);
            Term resolvedPos = positionTerm.resolveBindings(bindings);
            Map<String, Term> newBindings = new HashMap<>(bindings);

            if (resolvedPos.unify(positionValue, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;

        } catch (PrologException pe) {
            throw pe;
        } catch (Exception e) {
            throw new PrologEvaluationException(
                "stream_position/2: I/O error: " + e.getMessage());
        }
    }
}
// END_CHANGE: LIM-007
