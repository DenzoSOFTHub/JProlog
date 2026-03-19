// START_CHANGE: ISS-2025-0046 - Implement get_byte/1 and get_byte/2
package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * get_byte/1 and get_byte/2 - ISO Prolog I/O predicates.
 * Reads a single byte from a binary stream.
 */
public class GetByte implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = query.getArguments().size();

        String streamAlias;
        Term byteTerm;

        if (arity == 1) {
            streamAlias = StreamManager.getCurrentInput();
            byteTerm = query.getArguments().get(0);
        } else if (arity == 2) {
            Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);
            if (!(streamTerm instanceof Atom)) {
                throw new PrologEvaluationException("get_byte/2: stream argument must be an atom");
            }
            streamAlias = ((Atom) streamTerm).getName();
            byteTerm = query.getArguments().get(1);
        } else {
            throw new PrologEvaluationException("get_byte expects 1 or 2 arguments, got " + arity);
        }

        try {
            InputStream inputStream = StreamManager.getInputStream(streamAlias);
            if (inputStream == null) {
                throw new PrologEvaluationException("get_byte: stream does not exist: " + streamAlias);
            }

            int b = inputStream.read();
            Term byteValue;
            if (b == -1) {
                byteValue = new Number(-1, true);
            } else {
                byteValue = new Number(b, true);
            }

            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (byteTerm.unify(byteValue, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        } catch (IOException e) {
            throw new PrologEvaluationException("I/O error in get_byte: " + e.getMessage());
        }
    }
}
// END_CHANGE: ISS-2025-0046
