package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * sum_list(+List, -Sum) / sumlist(+List, -Sum) - Sum of numeric elements.
 */
public class SumList implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) return false;

        Term list = query.getArguments().get(0).resolveBindings(bindings);
        Term result = query.getArguments().get(1);

        List<Term> elements = ListUtils.extractElements(list);
        double sum = 0;
        for (Term elem : elements) {
            if (!(elem instanceof Number)) return false;
            sum += ((Number) elem).getValue();
        }

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (result.unify(new Number(sum), newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }
}
