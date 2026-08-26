package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.PrologStream;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * peek_char/1 - peek_char(?Char) — look at the next character of the current input.
 * peek_char/2 - peek_char(+Stream, ?Char) — ISO 8.12.2.
 *
 * <p>The lookahead lives on the stream's decoder (ISS-2025-0472), so a peek followed by a
 * {@code get_char/2} sees the identical character and a {@code seek/4} in between discards it.
 */
public class PeekChar implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = query.getArguments() == null ? 0 : query.getArguments().size();
        if (arity != 1 && arity != 2) {
            throw new PrologEvaluationException("peek_char/1 or peek_char/2 expected");
        }
        String ctx = "peek_char/" + arity;
        Term charTerm = query.getArguments().get(arity - 1);
        Term streamArg = (arity == 2) ? query.getArguments().get(0) : null;

        PrologStream s = IOStreamUtils.inputStream(streamArg, bindings, ctx);
        try {
            int cp = IOStreamUtils.isStdin(s) ? peekStdin() : s.peekCodePoint();
            Term value = (cp < 0) ? new Atom("end_of_file") : new Atom(new String(Character.toChars(cp)));
            Map<String, Term> nb = new HashMap<>(bindings);
            if (charTerm.unify(value, nb)) {
                solutions.add(nb);
                return true;
            }
            return false;
        } catch (IOException e) {
            throw new PrologEvaluationException("I/O error in peek_char: " + e.getMessage());
        }
    }

    /** stdin keeps the legacy PushbackInputStream path so the interactive console is unaffected. */
    static int peekStdin() throws IOException {
        InputStream in = StreamManager.streams().userInput().rawInput();
        PushbackInputStream pb;
        if (in instanceof PushbackInputStream) {
            pb = (PushbackInputStream) in;
        } else {
            pb = new PushbackInputStream(in);
            StreamManager.streams().userInput().replaceRawInput(pb);
        }
        int b = pb.read();
        if (b >= 0) pb.unread(b);
        return b;
    }
}
