package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * subtract(+Set, +Delete, -Result) - Remove from Set elements in Delete.
 */
public class Subtract implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) return false;

        Term set = query.getArguments().get(0).resolveBindings(bindings);
        Term del = query.getArguments().get(1).resolveBindings(bindings);
        Term result = query.getArguments().get(2);

        List<Term> setElems = ListUtils.extractElements(set);
        List<Term> delElems = ListUtils.extractElements(del);
        List<Term> filtered = new ArrayList<>();

        for (Term e : setElems) {
            if (!memberOf(e, delElems)) {
                filtered.add(e);
            }
        }

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (result.unify(ListUtils.createList(filtered), newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    // START_CHANGE: ISS-2025-0191 - Use structural equality (==) instead of unification for set operations
    static boolean memberOf(Term elem, List<Term> list) {
        for (Term e : list) {
            if (structurallyEqual(e, elem)) return true;
        }
        return false;
    }

    /**
     * Check structural equality of two ground terms.
     * Uses equals() for ground terms, falls back to unification with empty bindings for non-ground.
     */
    static boolean structurallyEqual(Term a, Term b) {
        if (a.isGround() && b.isGround()) {
            // START_CHANGE: ISS-2025-0266 - use type-aware term equality, not toString().
            // toString() conflated the atom '1' and the number 1 (both print "1"), so e.g.
            // subtract([1,'1'],[1]) wrongly returned [] instead of ['1'].
            return a.equals(b);
            // END_CHANGE: ISS-2025-0266
        }
        // For non-ground terms, use unification with empty bindings as approximation
        Map<String, Term> test = new HashMap<>();
        return a.unify(b, test) && test.isEmpty();
    }
    // END_CHANGE: ISS-2025-0191
}
