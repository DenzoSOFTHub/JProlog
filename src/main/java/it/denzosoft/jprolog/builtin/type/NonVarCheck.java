package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.List;
import java.util.Map;

public class NonVarCheck implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("nonvar/1 requires exactly one argument.");
        }

        Term termArg = query.getArguments().get(0);

        // START_CHANGE: ISS-2025-0051 - Fix nonvar/1 to check bindings map
        // ISO Prolog: nonvar(X) succeeds if X is not an unbound variable.
        Term resolved = termArg.resolveBindings(bindings);
        boolean isNotAVariable = !(resolved instanceof Variable);
        // END_CHANGE: ISS-2025-0051

        if (isNotAVariable) {
            solutions.add(bindings);
            return true;
        } else {
            return false;
        }
    }
}
