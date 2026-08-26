package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.PrologStream;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * get_code/1 - get_code(?Code) — read one character code from the current input.
 * get_code/2 - get_code(+Stream, ?Code) — ISO 8.12.1; {@code -1} at end of stream.
 */
public class GetCode implements BuiltIn {

    private static final BufferedReader STDIN = new BufferedReader(new InputStreamReader(System.in));

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = query.getArguments() == null ? 0 : query.getArguments().size();
        if (arity != 1 && arity != 2) {
            throw new PrologEvaluationException("get_code/1 or get_code/2 expected");
        }
        String ctx = "get_code/" + arity;
        Term codeTerm = query.getArguments().get(arity - 1);
        Term streamArg = (arity == 2) ? query.getArguments().get(0) : null;

        // START_CHANGE: ISS-2025-0472 - read through the engine's stream decoder
        PrologStream s = IOStreamUtils.inputStream(streamArg, bindings, ctx);
        try {
            int cp = IOStreamUtils.isStdin(s) ? STDIN.read() : s.getCodePoint();
            if (cp < 0) IOStreamUtils.checkPastEof(s, ctx);
            Map<String, Term> nb = new HashMap<>(bindings);
            if (codeTerm.unify(new Number((long) cp), nb)) {
                solutions.add(nb);
                return true;
            }
            return false;
        } catch (IOException e) {
            throw new PrologEvaluationException("get_code: I/O error - " + e.getMessage());
        }
        // END_CHANGE: ISS-2025-0472
    }
}
