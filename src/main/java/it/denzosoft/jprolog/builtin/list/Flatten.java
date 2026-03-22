package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * flatten(+NestedList, -FlatList) - Flatten a nested list.
 */
public class Flatten implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) return false;

        Term nested = query.getArguments().get(0).resolveBindings(bindings);
        Term result = query.getArguments().get(1);

        List<Term> flat = new ArrayList<>();
        // START_CHANGE: ISS-2025-0169 - Add depth limit to prevent infinite recursion on cyclic lists
        flattenTerm(nested, flat, 0);
        // END_CHANGE: ISS-2025-0169

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (result.unify(ListUtils.createList(flat), newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    // START_CHANGE: ISS-2025-0169 - Add depth limit to prevent infinite recursion on cyclic lists
    private static final int MAX_FLATTEN_DEPTH = 10000;

    private void flattenTerm(Term term, List<Term> result, int depth) {
        if (depth > MAX_FLATTEN_DEPTH) {
            throw new PrologException(ISOErrorTerms.resourceError("cyclic_term", "flatten/2"));
        }
        if (term instanceof Atom && ((Atom) term).getName().equals("[]")) {
            return;
        }
        if (term instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) term;
            if (ct.getName().equals(".") && ct.getArguments().size() == 2) {
                Term head = ct.getArguments().get(0);
                Term tail = ct.getArguments().get(1);
                // If head is itself a list, flatten it recursively
                if (isList(head)) {
                    flattenTerm(head, result, depth + 1);
                } else {
                    result.add(head);
                }
                flattenTerm(tail, result, depth + 1);
                return;
            }
        }
        // Non-list term — add as-is
        result.add(term);
    }
    // END_CHANGE: ISS-2025-0169

    private boolean isList(Term term) {
        if (term instanceof Atom && ((Atom) term).getName().equals("[]")) return true;
        if (term instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) term;
            return ct.getName().equals(".") && ct.getArguments().size() == 2;
        }
        return false;
    }
}
