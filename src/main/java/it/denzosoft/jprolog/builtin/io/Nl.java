package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

public class Nl implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        // START_CHANGE: ISS-2025-0373 - nl/1 (ISO 8.12.3 companion): newline on the given stream
        int arity = (query.getArguments() == null) ? 0 : query.getArguments().size();
        if (arity != 0 && arity != 1) {
            throw new PrologEvaluationException("nl/0 or nl/1 expected.");
        }

        java.io.PrintStream out = (arity == 0)
            ? StreamManager.out()
            : IOStreamUtils.resolveOutputStream(query.getArguments().get(0), bindings, "nl/1");
        out.println(); // Writes a newline
        out.flush();
        // END_CHANGE: ISS-2025-0373

        solutions.add(bindings);
        return true;
    }
}
