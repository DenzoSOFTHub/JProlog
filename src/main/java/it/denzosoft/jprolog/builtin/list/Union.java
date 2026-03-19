package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * union(+Set1, +Set2, -Union) - Set1 + elements of Set2 not in Set1.
 */
public class Union implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) return false;

        Term set1 = query.getArguments().get(0).resolveBindings(bindings);
        Term set2 = query.getArguments().get(1).resolveBindings(bindings);
        Term result = query.getArguments().get(2);

        List<Term> elems1 = ListUtils.extractElements(set1);
        List<Term> elems2 = ListUtils.extractElements(set2);
        List<Term> union = new ArrayList<>(elems1);

        for (Term e : elems2) {
            if (!Subtract.memberOf(e, elems1)) {
                union.add(e);
            }
        }

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (result.unify(ListUtils.createList(union), newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }
}
