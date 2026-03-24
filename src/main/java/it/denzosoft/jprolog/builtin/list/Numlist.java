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

        int low = ((Number) lowTerm).getValue().intValue();
        int high = ((Number) highTerm).getValue().intValue();

        // START_CHANGE: ISS-2025-0184 - Fix range validation
        if (low > high) return false;
        // END_CHANGE: ISS-2025-0184

        List<Term> nums = new ArrayList<>();
        for (int i = low; i <= high; i++) {
            nums.add(new Number(i));
        }

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (result.unify(ListUtils.createList(nums), newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }
}
