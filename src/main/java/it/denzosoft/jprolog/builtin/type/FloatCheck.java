package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;


public class FloatCheck implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("float/1 requires exactly one argument.");
        }

        // START_CHANGE: ISS-2025-0080 - Resolve bindings before type/ground checks
        Term resolvedTerm = query.getArguments().get(0).resolveBindings(bindings);
        // END_CHANGE: ISS-2025-0080

        if (resolvedTerm.isGround()) {
            // START_CHANGE: ISS-2025-0056 - Use Number.isFloat() for proper type distinction
            boolean isFloat = (resolvedTerm instanceof Number) &&
                             ((Number) resolvedTerm).isFloat();
            // END_CHANGE: ISS-2025-0056
            
            if (isFloat) {
                solutions.add(bindings);
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }
}
