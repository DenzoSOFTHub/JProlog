package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.List;
import java.util.Map;

/**
 * partial_list/1 - Succeeds if the argument is a partial list.
 * A partial list is a list structure that ends with an unbound variable
 * instead of the empty list [].
 */
public class PartialListCheck implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("partial_list/1 requires exactly one argument.");
        }

        Term termArg = query.getArguments().get(0);
        
        boolean isPartialList = isPartialListTerm(termArg, bindings);

        if (isPartialList) {
            solutions.add(bindings);
            return true;
        } else {
            return false;
        }
    }
    
    /**
     * Recursively checks if a term is a partial list.
     * A partial list is a list structure that ends with an unbound variable.
     */
    // START_CHANGE: ISS-2025-0223 - iterative + cycle detection
    private boolean isPartialListTerm(Term term, Map<String, Term> bindings) {
        java.util.IdentityHashMap<Term, Boolean> visited = new java.util.IdentityHashMap<>();
        java.util.Set<String> visitedVars = new java.util.HashSet<>();
        Term current = term;
        while (true) {
            if (current instanceof Variable) {
                Variable var = (Variable) current;
                if (!visitedVars.add(var.getName())) return true; // bound cycle, treat as partial
                Term bound = bindings.get(var.getName());
                if (bound == null) return true;
                current = bound;
                continue;
            }
            if (current instanceof Atom) return false;
            if (current instanceof CompoundTerm) {
                CompoundTerm c = (CompoundTerm) current;
                if (".".equals(c.getName()) && c.getArguments().size() == 2) {
                    if (visited.put(c, Boolean.TRUE) != null) return false; // cycle of cons
                    current = c.getArguments().get(1);
                    continue;
                }
            }
            return false;
        }
    }
    // END_CHANGE: ISS-2025-0223
}