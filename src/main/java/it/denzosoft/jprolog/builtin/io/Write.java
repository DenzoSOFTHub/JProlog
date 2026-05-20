package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

public class Write implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("write/1 requires exactly 1 argument.");
        }

        Term termToWrite = query.getArguments().get(0);
        Term resolvedTerm = termToWrite.resolveBindings(bindings);
        // START_CHANGE: ISS-2025-0242 - operator-aware formatting
        System.out.print(it.denzosoft.jprolog.core.util.TermFormatter.format(resolvedTerm, false, false, false, 1200));
        // END_CHANGE: ISS-2025-0242

        // As it's a side-effect, it always succeeds if argument is valid.
        solutions.add(bindings); // Add the unmodified bindings.
        return true;
    }
}
