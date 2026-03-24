package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * intersection(+Set1, +Set2, -Intersection) - Elements in both sets.
 */
public class Intersection implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) return false;

        Term set1 = query.getArguments().get(0).resolveBindings(bindings);
        Term set2 = query.getArguments().get(1).resolveBindings(bindings);
        Term result = query.getArguments().get(2);

        List<Term> elems1 = ListUtils.extractElements(set1);
        List<Term> elems2 = ListUtils.extractElements(set2);
        // START_CHANGE: ISS-2025-0187 - Deduplicate intersection results (set semantics)
        List<Term> common = new ArrayList<>();
        java.util.Set<String> seen = new java.util.HashSet<>();

        for (Term e : elems1) {
            String key = e.toString();
            if (Subtract.memberOf(e, elems2) && seen.add(key)) {
                common.add(e);
            }
        }
        // END_CHANGE: ISS-2025-0187

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (result.unify(ListUtils.createList(common), newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }
}
