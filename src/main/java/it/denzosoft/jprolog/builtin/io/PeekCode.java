package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.PrologStream;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * peek_code/1 - peek_code(?Code) — look at the next character code of the current input.
 * peek_code/2 - peek_code(+Stream, ?Code) — ISO 8.12.2; {@code -1} at end of stream.
 */
public class PeekCode implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = query.getArguments() == null ? 0 : query.getArguments().size();
        if (arity != 1 && arity != 2) {
            throw new PrologEvaluationException("peek_code/1 or peek_code/2 expected");
        }
        String ctx = "peek_code/" + arity;
        Term codeTerm = query.getArguments().get(arity - 1);
        Term streamArg = (arity == 2) ? query.getArguments().get(0) : null;

        PrologStream s = IOStreamUtils.inputStream(streamArg, bindings, ctx);
        try {
            int cp = IOStreamUtils.isStdin(s) ? PeekChar.peekStdin() : s.peekCodePoint();
            Map<String, Term> nb = new HashMap<>(bindings);
            if (codeTerm.unify(new Number((long) cp), nb)) {
                solutions.add(nb);
                return true;
            }
            return false;
        } catch (IOException e) {
            throw new PrologEvaluationException("I/O error in peek_code: " + e.getMessage());
        }
    }
}
