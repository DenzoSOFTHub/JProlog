package it.denzosoft.jprolog.builtin.list;

// START_CHANGE: ISS-2025-0385 - ISO errors for unbound/non-integer bounds
import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
// END_CHANGE: ISS-2025-0385
import it.denzosoft.jprolog.core.engine.BuiltIn;
// START_CHANGE: ISS-2025-0385 - ISO errors for unbound/non-integer bounds
import it.denzosoft.jprolog.core.exceptions.PrologException;
// END_CHANGE: ISS-2025-0385
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
// START_CHANGE: ISS-2025-0385 - ISO errors for unbound/non-integer bounds
import it.denzosoft.jprolog.core.terms.Variable;
// END_CHANGE: ISS-2025-0385
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

        // START_CHANGE: ISS-2025-0385 - ISO errors on bad bounds instead of silent failure:
        // instantiation_error for unbound Low/High, type_error(integer, Culprit) for
        // non-integer bounds (mode is numlist(+Low, +High, -List), matching SWI's must_be).
        if (lowTerm instanceof Variable || highTerm instanceof Variable) {
            throw new PrologException(ISOErrorTerms.instantiationError("numlist/3"));
        }
        if (!(lowTerm instanceof Number) || !((Number) lowTerm).isInteger()) {
            throw new PrologException(ISOErrorTerms.typeError("integer", lowTerm, "numlist/3"));
        }
        if (!(highTerm instanceof Number) || !((Number) highTerm).isInteger()) {
            throw new PrologException(ISOErrorTerms.typeError("integer", highTerm, "numlist/3"));
        }
        // END_CHANGE: ISS-2025-0385

        // START_CHANGE: ISS-2025-0188 - Use long to avoid int truncation; validate integer
        Number lowNum = (Number) lowTerm;
        Number highNum = (Number) highTerm;
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
