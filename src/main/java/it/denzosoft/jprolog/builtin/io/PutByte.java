// START_CHANGE: ISS-2025-0046 - Implement put_byte/1 and put_byte/2
package it.denzosoft.jprolog.builtin.io;

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
            throw new PrologEvaluationException("put_byte expects 1 or 2 arguments, got " + arity);
        }
        String ctx = "put_byte/" + arity;
        Term byteTerm = query.getArguments().get(arity - 1).resolveBindings(bindings);
        Term streamArg = (arity == 2) ? query.getArguments().get(0) : null;

        if (byteTerm instanceof Variable) {
            throw new PrologEvaluationException("put_byte: byte argument must be instantiated");
        }
        if (!(byteTerm instanceof Number)) {
            throw new PrologEvaluationException("put_byte: byte argument must be an integer");
        }
        int byteValue = ((Number) byteTerm).getValue().intValue();
        if (byteValue < 0 || byteValue > 255) {
            throw new PrologEvaluationException("put_byte: byte value must be 0-255, got " + byteValue);
        }

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
            throw new PrologEvaluationException("I/O error in put_byte: " + e.getMessage());
        }
        // END_CHANGE: ISS-2025-0472
    }
}
// END_CHANGE: ISS-2025-0046
