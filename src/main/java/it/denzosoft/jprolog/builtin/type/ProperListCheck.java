package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0170 - Add proper_list/1 predicate
/**
 * proper_list/1 - Succeeds if the argument is a proper list.
 *
 * A proper list is either:
 * - The empty list [] (an Atom with name "[]")
 * - A compound term with functor ./2 where the tail (second argument)
 *   is also a proper list
 *
 * Fails for partial lists (ending in a variable) or non-list terms.
 */
public class ProperListCheck implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("proper_list/1 requires exactly one argument.");
        }

        Term termArg = query.getArguments().get(0).resolveBindings(bindings);

        boolean isProper = isProperList(termArg);

        if (isProper) {
            solutions.add(bindings);
            return true;
        } else {
            return false;
        }
    }

    // START_CHANGE: ISS-2025-0216 - iterative + cycle detection
    private boolean isProperList(Term term) {
        java.util.IdentityHashMap<Term, Boolean> visited = new java.util.IdentityHashMap<>();
        Term current = term;
        while (true) {
            if (current instanceof Atom) {
                return "[]".equals(((Atom) current).getName());
            }
            if (!(current instanceof CompoundTerm)) return false;
            CompoundTerm c = (CompoundTerm) current;
            if (!".".equals(c.getName()) || c.getArguments().size() != 2) return false;
            if (visited.put(c, Boolean.TRUE) != null) return false;
            current = c.getArguments().get(1);
        }
    }
    // END_CHANGE: ISS-2025-0216
}
// END_CHANGE: ISS-2025-0170
