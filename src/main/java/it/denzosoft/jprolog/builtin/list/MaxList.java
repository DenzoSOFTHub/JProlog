package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * max_list(+List, -Max) - Maximum numeric element.
 */
public class MaxList implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) return false;

        Term list = query.getArguments().get(0).resolveBindings(bindings);
        Term result = query.getArguments().get(1);

        List<Term> elements = ListUtils.extractElements(list);
        if (elements.isEmpty()) return false;

        // START_CHANGE: ISS-2025-0192 - Initialize max from first element instead of sentinel
        if (!(elements.get(0) instanceof Number)) return false;
        Number maxNum = (Number) elements.get(0);
        for (int i = 1; i < elements.size(); i++) {
            Term elem = elements.get(i);
            if (!(elem instanceof Number)) return false;
            Number num = (Number) elem;
            if (num.getValue() > maxNum.getValue()) {
                maxNum = num;
            }
        }

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (result.unify(maxNum, newBindings)) {
        // END_CHANGE: ISS-2025-0192
            solutions.add(newBindings);
            return true;
        }
        return false;
    }
}
