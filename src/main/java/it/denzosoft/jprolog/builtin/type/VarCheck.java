package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.List;
import java.util.Map;

public class VarCheck implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("var/1 requires exactly one argument.");
        }

        Term termArg = query.getArguments().get(0);

        // START_CHANGE: ISS-2025-0051 - Fix var/1 to check bindings map
        // ISO Prolog: var(X) succeeds only if X is an unbound variable.
        // Must resolve through bindings first - a bound variable is not var.
        Term resolved = termArg.resolveBindings(bindings);
        boolean isVar = (resolved instanceof Variable);
        // END_CHANGE: ISS-2025-0051

        if (isVar) {
            solutions.add(bindings);
            return true;
        } else {
            return false;
        }
    }
}
