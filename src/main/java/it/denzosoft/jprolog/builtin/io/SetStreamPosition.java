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
 * set_stream_position/2 - ISO Prolog stream repositioning predicate.
 * set_stream_position(+Stream, +Position)
 *
 * Repositions a stream to the given byte position.
 * Only file-backed streams that support repositioning are allowed.
 * Standard streams (user_input, user_output, user_error) do not support repositioning.
 *
 * Throws permission_error(reposition, stream, Stream) if the stream does not support repositioning.
 * Throws existence_error(stream, Stream) if the stream does not exist.
 */
public class SetStreamPosition implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException(
                "set_stream_position/2 requires exactly 2 arguments");
        }

        Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term positionTerm = query.getArguments().get(1).resolveBindings(bindings);

        // Validate stream argument
        if (!(streamTerm instanceof Atom)) {
            throw new PrologEvaluationException(
                "set_stream_position/2: Stream must be an atom");
        }

        String streamAlias = ((Atom) streamTerm).getName();

        // Check stream exists
        if (!StreamManager.hasStream(streamAlias)) {
            throw new PrologException(
                ISOErrorTerms.existenceError("stream", streamTerm, "set_stream_position/2"));
        }

        // Validate position argument
        if (!(positionTerm instanceof Number)) {
            throw new PrologEvaluationException(
                "set_stream_position/2: Position must be an integer");
        }

        long position = ((Number) positionTerm).getValue().longValue();
        if (position < 0) {
            throw new PrologEvaluationException(
                "set_stream_position/2: Position must be non-negative");
        }

        // Standard streams do not support repositioning
        if ("user_input".equals(streamAlias) || "user_output".equals(streamAlias)
                || "user_error".equals(streamAlias)) {
            throw new PrologException(
                ISOErrorTerms.permissionError("reposition", "stream", streamTerm,
                    "set_stream_position/2"));
        }

        // Try to reposition the stream
        try {
            InputStream is = StreamManager.getInputStream(streamAlias);
            if (is instanceof FileInputStream) {
                FileChannel channel = ((FileInputStream) is).getChannel();
                channel.position(position);
                solutions.add(new HashMap<>(bindings));
                return true;
            }

            OutputStream os = StreamManager.getOutputStream(streamAlias);
            if (os instanceof FileOutputStream) {
                FileChannel channel = ((FileOutputStream) os).getChannel();
                channel.position(position);
                solutions.add(new HashMap<>(bindings));
                return true;
            }

            // Stream exists but doesn't support repositioning
            throw new PrologException(
                ISOErrorTerms.permissionError("reposition", "stream", streamTerm,
                    "set_stream_position/2"));

        } catch (PrologException pe) {
            throw pe;
        } catch (Exception e) {
            throw new PrologEvaluationException(
                "set_stream_position/2: I/O error: " + e.getMessage());
        }
    }
}
// END_CHANGE: LIM-007
