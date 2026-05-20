// START_CHANGE: CR-2025-0005 - seek/4 stream positioning
package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.channels.FileChannel;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * seek/4 - SWI-Prolog stream repositioning.
 *
 * seek(+Stream, +Offset, +Method, -NewLocation)
 *
 * Method = bof | current | eof
 *   bof: position = Offset (from start)
 *   current: position = currentPos + Offset
 *   eof: position = streamSize + Offset (Offset typically 0 or negative)
 *
 * NewLocation is unified with the resulting absolute position.
 */
public class Seek implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 4) {
            throw new PrologEvaluationException("seek/4 requires exactly 4 arguments");
        }
        Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term offsetTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term methodTerm = query.getArguments().get(2).resolveBindings(bindings);
        Term newLocTerm = query.getArguments().get(3);

        if (!(streamTerm instanceof Atom)) {
            throw new PrologException(ISOErrorTerms.typeError("atom", streamTerm, "seek/4"));
        }
        if (!(offsetTerm instanceof Number) || !((Number) offsetTerm).isInteger()) {
            throw new PrologException(ISOErrorTerms.typeError("integer", offsetTerm, "seek/4"));
        }
        if (!(methodTerm instanceof Atom)) {
            throw new PrologException(ISOErrorTerms.typeError("atom", methodTerm, "seek/4"));
        }

        String alias = ((Atom) streamTerm).getName();
        long offset = ((Number) offsetTerm).longValue();
        String method = ((Atom) methodTerm).getName();

        if (!StreamManager.hasStream(alias)) {
            throw new PrologException(ISOErrorTerms.existenceError("stream", streamTerm, "seek/4"));
        }
        if ("user_input".equals(alias) || "user_output".equals(alias) || "user_error".equals(alias)) {
            throw new PrologException(ISOErrorTerms.permissionError("reposition", "stream", streamTerm, "seek/4"));
        }

        try {
            FileChannel channel = null;
            InputStream is = StreamManager.getInputStream(alias);
            if (is instanceof FileInputStream) {
                channel = ((FileInputStream) is).getChannel();
            } else {
                OutputStream os = StreamManager.getOutputStream(alias);
                if (os instanceof FileOutputStream) {
                    channel = ((FileOutputStream) os).getChannel();
                }
            }
            if (channel == null) {
                throw new PrologException(ISOErrorTerms.permissionError("reposition", "stream", streamTerm, "seek/4"));
            }

            long newPos;
            switch (method) {
                case "bof": newPos = offset; break;
                case "current": newPos = channel.position() + offset; break;
                case "eof": newPos = channel.size() + offset; break;
                default:
                    throw new PrologException(ISOErrorTerms.domainError("seek_method", methodTerm, "seek/4"));
            }
            if (newPos < 0) {
                throw new PrologException(ISOErrorTerms.domainError("position", new Number(newPos), "seek/4"));
            }
            channel.position(newPos);

            Map<String, Term> nb = new HashMap<>(bindings);
            if (newLocTerm.unify(new Number(newPos), nb)) {
                solutions.add(nb);
                return true;
            }
            return false;
        } catch (PrologException pe) {
            throw pe;
        } catch (Exception e) {
            throw new PrologEvaluationException("seek/4: I/O error: " + e.getMessage());
        }
    }
}
// END_CHANGE: CR-2025-0005
