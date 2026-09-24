// START_CHANGE: ISS-2025-0046 - Implement put_byte/1 and put_byte/2
package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.PrologStream;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.io.IOException;
import java.util.List;
import java.util.Map;

/**
 * put_byte/1 and put_byte/2 - ISO Prolog I/O predicates.
 * Writes a single byte to a binary stream.
 */
public class PutByte implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = query.getArguments().size();
        if (arity != 1 && arity != 2) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0697
        }
        String ctx = "put_byte/" + arity;
        Term byteTerm = query.getArguments().get(arity - 1).resolveBindings(bindings);
        Term streamArg = (arity == 2) ? query.getArguments().get(0) : null;

        // START_CHANGE: ISS-2025-0505 - 4.3 wave D: ISO 8.13.3.3 — instantiation_error,
        // type_error(byte, B) and representation_error(byte) instead of three message atoms that
        // catch/3 could only match with a bare variable catcher.
        if (byteTerm instanceof Variable) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError(ctx));
        }
        if (!(byteTerm instanceof Number) || !((Number) byteTerm).isInteger()) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("byte", byteTerm, ctx));
        }
        int byteValue = ((Number) byteTerm).getValue().intValue();
        if (byteValue < 0 || byteValue > 255) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("byte", byteTerm, ctx));
        }
        // END_CHANGE: ISS-2025-0505

        // START_CHANGE: ISS-2025-0472 - write through the engine's stream (counted, per engine)
        PrologStream s = IOStreamUtils.outputStream(streamArg, bindings, ctx);
        try {
            if (s.rawOutput() != null) {
                s.rawOutput().write(byteValue);
                s.rawOutput().flush();
            } else {
                StreamManager.streams().writerFor(s).write(byteValue);
                StreamManager.streams().writerFor(s).flush();
            }
            solutions.add(bindings);
            return true;
        } catch (IOException e) {
            throw Errors.host(e, "write", "stream", null, "put_byte", LibArgs.arity(query));   // ISS-2025-0697
        }
        // END_CHANGE: ISS-2025-0472
    }
}
// END_CHANGE: ISS-2025-0046
