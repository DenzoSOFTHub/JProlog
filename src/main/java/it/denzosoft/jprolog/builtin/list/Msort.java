package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;
// START_CHANGE: ISS-2025-0076 - Use centralized ListUtils
import it.denzosoft.jprolog.core.util.ListUtils;
// END_CHANGE: ISS-2025-0076

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class Msort implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("msort/2 requires exactly 2 arguments.");
        }

        // START_CHANGE: ISS-2025-0080 - Resolve bindings before type/ground checks
        Term inputList = query.getArguments().get(0).resolveBindings(bindings);
        Term sortedList = query.getArguments().get(1);
        // END_CHANGE: ISS-2025-0080

        if (it.denzosoft.jprolog.core.util.ListUtils.isProperList(inputList)) {   // ISS-2025-0335: not isGround
            // START_CHANGE: ISS-2025-0076 - Use centralized ListUtils
            // Extract elements from the input list (preserving duplicates)
            List<Term> elements = ListUtils.extractElements(inputList);

            // Sort while preserving duplicates
            // START_CHANGE: ISS-2025-0184 - Use ISO standard term ordering
            Collections.sort(elements, Sort::compareTerms);
            // END_CHANGE: ISS-2025-0184

            // Create the sorted list term
            Term sortedListTerm = ListUtils.createList(elements);
            // END_CHANGE: ISS-2025-0076
            
            // Unify with the output list
            if (sortedList.unify(sortedListTerm, bindings)) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            return false;
        } else {
            // START_CHANGE: ISS-2025-0079 - Return false instead of throwing for normal failure
            return false;
            // END_CHANGE: ISS-2025-0079
        }
    }

}
