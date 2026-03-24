package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * numlist(+Low, +High, -List) - Generate list [Low, Low+1, ..., High].
 */
public class Numlist implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) return false;

        Term lowTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term highTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term result = query.getArguments().get(2);

        if (!(lowTerm instanceof Number) || !(highTerm instanceof Number)) return false;

        // START_CHANGE: ISS-2025-0188 - Use long to avoid int truncation; validate integer
        Number lowNum = (Number) lowTerm;
        Number highNum = (Number) highTerm;
        if (!lowNum.isInteger() || !highNum.isInteger()) return false;
        long low = lowNum.longValue();
        long high = highNum.longValue();

        if (low > high) return false;

        List<Term> nums = new ArrayList<>();
        for (long i = low; i <= high; i++) {
            nums.add(new Number(i));
        }
        // END_CHANGE: ISS-2025-0188

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (result.unify(ListUtils.createList(nums), newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }
}
