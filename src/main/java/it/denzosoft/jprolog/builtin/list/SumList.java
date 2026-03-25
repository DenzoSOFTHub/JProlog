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
        // START_CHANGE: ISS-2025-0192 - Use long accumulation for integer lists to avoid precision loss
        boolean allIntegers = true;
        long longSum = 0;
        double doubleSum = 0;
        for (Term elem : elements) {
            if (!(elem instanceof Number)) return false;
            Number num = (Number) elem;
            doubleSum += num.getValue();
            if (allIntegers && num.isInteger() && !num.isBigInteger()) {
                longSum += num.longValue();
            } else {
                allIntegers = false;
            }
        }

        Map<String, Term> newBindings = new HashMap<>(bindings);
        Number sumResult = allIntegers ? new Number(longSum) : new Number(doubleSum);
        if (result.unify(sumResult, newBindings)) {
        // END_CHANGE: ISS-2025-0192
            solutions.add(newBindings);
            return true;
        }
        return false;
    }
}
