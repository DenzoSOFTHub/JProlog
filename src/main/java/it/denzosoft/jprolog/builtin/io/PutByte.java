// START_CHANGE: ISS-2025-0046 - Implement put_byte/1 and put_byte/2
package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.io.IOException;
import java.io.OutputStream;
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

        String streamAlias;
        Term byteTerm;

        if (arity == 1) {
            streamAlias = StreamManager.getCurrentOutput();
            byteTerm = query.getArguments().get(0).resolveBindings(bindings);
        } else if (arity == 2) {
            Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);
            if (!(streamTerm instanceof Atom)) {
                throw new PrologEvaluationException("put_byte/2: stream argument must be an atom");
            }
            streamAlias = ((Atom) streamTerm).getName();
            byteTerm = query.getArguments().get(1).resolveBindings(bindings);
        } else {
            throw new PrologEvaluationException("put_byte expects 1 or 2 arguments, got " + arity);
        }

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

        try {
            OutputStream outputStream = StreamManager.getOutputStream(streamAlias);
            if (outputStream == null) {
                throw new PrologEvaluationException("put_byte: stream does not exist: " + streamAlias);
            }
            outputStream.write(byteValue);
            outputStream.flush();
            solutions.add(bindings);
            return true;
        } catch (IOException e) {
            throw new PrologEvaluationException("I/O error in put_byte: " + e.getMessage());
        }
    }
}
// END_CHANGE: ISS-2025-0046
