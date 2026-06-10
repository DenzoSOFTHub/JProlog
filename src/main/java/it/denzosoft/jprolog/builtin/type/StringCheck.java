package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0404 - string/1 type check (SWI): true only for string terms
/**
 * string(@Term) - True if Term is a string (a PrologString, the type produced by
 * double-quoted literals under the default double_quotes=string flag). Fails for
 * atoms, numbers, compounds, and unbound variables.
 */
public class StringCheck implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("string/1 requires exactly one argument.");
        }

        Term resolvedTerm = query.getArguments().get(0).resolveBindings(bindings);

        if (resolvedTerm instanceof PrologString) {
            solutions.add(bindings); // Add current (unchanged) binding set
            return true;
        }
        return false; // Not a string (type checks fail, not throw, for any other term)
    }
}
// END_CHANGE: ISS-2025-0404
