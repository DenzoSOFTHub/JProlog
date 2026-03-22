package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0170 - Add acyclic_term/1 ISO predicate
/**
 * acyclic_term/1 - Succeeds if the argument contains no cycles.
 * ISO 13211-1 standard predicate.
 *
 * A term is acyclic if it does not contain any cyclic reference.
 * In JProlog, since terms are immutable, cycles cannot normally occur,
 * but this predicate is provided for ISO compliance.
 *
 * Atoms, Numbers, and PrologStrings are always acyclic.
 * Variables are dereferenced first and then checked.
 * CompoundTerms are checked recursively using an identity-based visited set.
 */
public class AcyclicTermCheck implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("acyclic_term/1 requires exactly one argument.");
        }

        Term termArg = query.getArguments().get(0).resolveBindings(bindings);

        boolean acyclic = isAcyclic(termArg, bindings, new IdentityHashMap<>());

        if (acyclic) {
            solutions.add(bindings);
            return true;
        } else {
            return false;
        }
    }

    /**
     * Recursively checks if a term is acyclic using an identity-based visited set.
     *
     * @param term the term to check
     * @param bindings current variable bindings
     * @param visited identity-based set of already visited terms
     * @return true if the term contains no cycles
     */
    private boolean isAcyclic(Term term, Map<String, Term> bindings, IdentityHashMap<Term, Boolean> visited) {
        // Dereference variables
        if (term instanceof Variable) {
            Variable var = (Variable) term;
            Term boundValue = bindings.get(var.getName());
            if (boundValue == null) {
                return true; // Unbound variable is acyclic
            }
            return isAcyclic(boundValue, bindings, visited);
        }

        // Atoms, Numbers, PrologStrings are always acyclic
        if (!(term instanceof CompoundTerm)) {
            return true;
        }

        // Check for cycle: if we've already seen this exact term instance
        if (visited.containsKey(term)) {
            return false; // Cycle detected
        }

        // Mark as visiting
        visited.put(term, Boolean.TRUE);

        // Recursively check all arguments
        CompoundTerm compound = (CompoundTerm) term;
        for (Term arg : compound.getArguments()) {
            if (!isAcyclic(arg, bindings, visited)) {
                return false;
            }
        }

        // Remove from visited (backtrack) - allows the same subterm in different positions
        visited.remove(term);

        return true;
    }
}
// END_CHANGE: ISS-2025-0170
