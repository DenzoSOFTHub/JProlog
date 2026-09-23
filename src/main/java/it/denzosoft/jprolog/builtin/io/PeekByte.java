// START_CHANGE: ISS-2025-0045 - Implement peek_byte/1 and peek_byte/2
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
 * peek_byte/1 and peek_byte/2 - ISO Prolog I/O predicates.
 * Looks at the next byte of a binary stream without consuming it.
 */
public class PeekByte implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = query.getArguments().size();
        if (arity != 1 && arity != 2) {
            throw new PrologEvaluationException("peek_byte expects 1 or 2 arguments, got " + arity);
        }
        String ctx = "peek_byte/" + arity;
        Term byteTerm = query.getArguments().get(arity - 1);
        Term streamArg = (arity == 2) ? query.getArguments().get(0) : null;

        // START_CHANGE: ISS-2025-0472 - lookahead on the engine's stream buffer
        PrologStream s = IOStreamUtils.inputStream(streamArg, bindings, ctx);
        // START_CHANGE: ISS-2025-0605 - P4.12: byte I/O on a text stream is
        // permission_error(input, text_stream, S); reading past the end with eof_action(error)
        // raises BEFORE the read, not at the first end_of_file.
        IOStreamUtils.checkStreamType(s, true, "input", ctx);
        IOStreamUtils.beforeRead(s, ctx);
        // END_CHANGE: ISS-2025-0605
        try {
            int b = s.peekByte();
            Map<String, Term> nb = new HashMap<>(bindings);
            if (byteTerm.unify(new Number((long) b), nb)) {
                solutions.add(nb);
                return true;
            }
            return false;
        } catch (IOException e) {
            throw new PrologEvaluationException("I/O error in peek_byte: " + e.getMessage());
        }
        // END_CHANGE: ISS-2025-0472
    }
}
// END_CHANGE: ISS-2025-0045
