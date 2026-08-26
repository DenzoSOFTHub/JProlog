package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.PrologStream;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * get_char/1 - get_char(?Char) — read one character from the current input.
 * get_char/2 - get_char(+Stream, ?Char) — ISO 8.12.1.
 *
 * <p>Reads through the stream's own decoder (ISS-2025-0472, wave W7), so a preceding
 * {@code peek_char/2} or {@code seek/4} is honoured exactly.
 */
public class GetChar implements BuiltIn {

    // START_CHANGE: ISS-2025-0173 - Make stdin reader final to prevent reassignment and document non-closure
    /** Cached BufferedReader for System.in - must not be closed as that would close System.in */
    private static final BufferedReader STDIN = new BufferedReader(new InputStreamReader(System.in));
    // END_CHANGE: ISS-2025-0173

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = query.getArguments() == null ? 0 : query.getArguments().size();
        if (arity != 1 && arity != 2) {
            throw new PrologEvaluationException("get_char/1 or get_char/2 expected");
        }
        String ctx = "get_char/" + arity;
        Term charTerm = query.getArguments().get(arity - 1);
        Term streamArg = (arity == 2) ? query.getArguments().get(0) : null;

        // START_CHANGE: ISS-2025-0472 - one code path for both arities, over the engine's stream
        PrologStream s = IOStreamUtils.inputStream(streamArg, bindings, ctx);
        try {
            int cp = IOStreamUtils.isStdin(s) ? STDIN.read() : s.getCodePoint();
            Term value;
            if (cp < 0) {
                IOStreamUtils.checkPastEof(s, ctx);
                value = new Atom("end_of_file");
            } else {
                value = new Atom(new String(Character.toChars(cp)));
            }
            Map<String, Term> nb = new HashMap<>(bindings);
            if (charTerm.unify(value, nb)) {
                solutions.add(nb);
                return true;
            }
            return false;
        } catch (IOException e) {
            throw new PrologEvaluationException("get_char: I/O error - " + e.getMessage());
        }
        // END_CHANGE: ISS-2025-0472
    }
}
