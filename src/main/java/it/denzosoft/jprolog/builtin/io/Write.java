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
        // RESOLVE variables first based on current bindings for potentially clearer output.
        Term resolvedTerm = termToWrite.resolveBindings(bindings);
        System.out.print(resolvedTerm.toString()); //No trailing newline

        // As it's a side-effect, it always succeeds if argument is valid.
        solutions.add(bindings); // Add the unmodified bindings.
        return true;
    }
}
