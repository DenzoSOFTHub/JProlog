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

        double max = Double.NEGATIVE_INFINITY;
        for (Term elem : elements) {
            if (!(elem instanceof Number)) return false;
            double val = ((Number) elem).getValue();
            if (val > max) max = val;
        }

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (result.unify(new Number(max), newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }
}
