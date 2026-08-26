package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.PrologStream;
import it.denzosoft.jprolog.core.engine.v4.Streams;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * current_input/1 - current_input(?Stream), ISO 8.11.1.
 * Unifies with {@code user_input} for the standard stream and with {@code '$stream'(N)} otherwise.
 */
public class CurrentInput implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("current_input/1 requires exactly 1 argument");
        }
        Streams st = StreamManager.streams();
        PrologStream s = st.currentInput();
        Term value = (s == st.userInput()) ? new Atom("user_input") : Streams.termFor(s);
        Map<String, Term> nb = new HashMap<>(bindings);
        if (query.getArguments().get(0).resolveBindings(bindings).unify(value, nb)) {
            solutions.add(nb);
            return true;
        }
        return false;
    }
}
