// START_CHANGE: CR-2025-0005 - seek/4 stream positioning
package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.PrologStream;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * seek/4 - seek(+Stream, +Offset, +Method, -NewLocation), Method = bof | current | eof.
 *
 * <p>START_CHANGE: ISS-2025-0472 - wave W7, limit L-07: the seek now goes through
 * {@link PrologStream#reposition(long)}, which drops the stream's decode buffer and resets the
 * decoder. Before this the channel moved but the {@code PushbackReader} that {@code get_char/2}
 * read from kept its own 8 KB buffer, so {@code get_char(S,C1), seek(S,0,bof,_), get_char(S,C2)}
 * answered {@code C2 = e} after {@code C1 = h}.
 */
public class Seek implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 4) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0697
        }
        Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term offsetTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term methodTerm = query.getArguments().get(2).resolveBindings(bindings);
        Term newLocTerm = query.getArguments().get(3);

        if (!(offsetTerm instanceof Number) || !((Number) offsetTerm).isInteger()) {
            throw new PrologException(ISOErrorTerms.typeError("integer", offsetTerm, "seek/4"));
        }
        if (!(methodTerm instanceof Atom)) {
            throw new PrologException(ISOErrorTerms.typeError("atom", methodTerm, "seek/4"));
        }

        long offset = ((Number) offsetTerm).longValue();
        String method = ((Atom) methodTerm).getName();

        PrologStream s = StreamManager.stream(streamTerm);
        if (s == null) {
            throw new PrologException(ISOErrorTerms.existenceError("stream", streamTerm, "seek/4"));
        }
        if (!s.canReposition()) {
            throw new PrologException(ISOErrorTerms.permissionError("reposition", "stream", streamTerm, "seek/4"));
        }

        try {
            long newPos;
            if ("bof".equals(method)) {
                newPos = offset;
            } else if ("current".equals(method)) {
                newPos = s.bytePosition() + offset;
            } else if ("eof".equals(method)) {
                newPos = s.size() + offset;
            } else {
                throw new PrologException(ISOErrorTerms.domainError("seek_method", methodTerm, "seek/4"));
            }
            if (newPos < 0) {
                throw new PrologException(ISOErrorTerms.domainError("position", new Number(newPos), "seek/4"));
            }
            s.reposition(newPos);

            Map<String, Term> nb = new HashMap<>(bindings);
            if (newLocTerm.unify(new Number(newPos), nb)) {
                solutions.add(nb);
                return true;
            }
            return false;
        } catch (PrologException pe) {
            throw pe;
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw Errors.host(e, "reposition", "stream", null, "seek", 4);   // ISS-2025-0697
        }
    }
}
// END_CHANGE: CR-2025-0005
