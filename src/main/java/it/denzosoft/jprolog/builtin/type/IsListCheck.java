package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

/**
 * is_list/1 - Succeeds if the argument is a proper list.
 * A proper list is either [] (empty list) or a compound term with functor ./2 
 * where the second argument is also a proper list.
 */
public class IsListCheck implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("is_list/1 requires exactly one argument.");
        }

        // START_CHANGE: ISS-2025-0073 - Resolve bindings before checking list structure
        Term termArg = query.getArguments().get(0).resolveBindings(bindings);
        // END_CHANGE: ISS-2025-0073

        boolean isList = isProperList(termArg);

        if (isList) {
            solutions.add(bindings);
            return true;
        } else {
            return false;
        }
    }
    
    // START_CHANGE: ISS-2025-0216 - iterative + cycle detection to avoid SOE on cyclic terms
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
            if (visited.put(c, Boolean.TRUE) != null) return false; // cycle
            current = c.getArguments().get(1);
        }
    }
    // END_CHANGE: ISS-2025-0216
}