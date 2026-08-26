// START_CHANGE: ISS-2025-0046 - Implement get_byte/1 and get_byte/2
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
 * get_byte/1 and get_byte/2 - ISO Prolog I/O predicates.
 * Reads a single byte from a binary stream; {@code -1} at end of stream.
 */
public class GetByte implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = query.getArguments().size();
        if (arity != 1 && arity != 2) {
            throw new PrologEvaluationException("get_byte expects 1 or 2 arguments, got " + arity);
        }
        String ctx = "get_byte/" + arity;
        Term byteTerm = query.getArguments().get(arity - 1);
        Term streamArg = (arity == 2) ? query.getArguments().get(0) : null;

        // START_CHANGE: ISS-2025-0472 - read through the engine's stream, so the byte position and
        // the character position of the same stream stay consistent.
        PrologStream s = IOStreamUtils.inputStream(streamArg, bindings, ctx);
        try {
            int b = s.getByte();
            if (b < 0) IOStreamUtils.checkPastEof(s, ctx);
            Map<String, Term> nb = new HashMap<>(bindings);
            if (byteTerm.unify(new Number((long) b), nb)) {
                solutions.add(nb);
                return true;
            }
            return false;
        } catch (IOException e) {
            throw new PrologEvaluationException("I/O error in get_byte: " + e.getMessage());
        }
        // END_CHANGE: ISS-2025-0472
    }
}
// END_CHANGE: ISS-2025-0046
