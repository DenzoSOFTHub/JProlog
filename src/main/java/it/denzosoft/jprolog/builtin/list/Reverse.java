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


public class Reverse implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("reverse/2 requires exactly 2 arguments.");
        }

        // START_CHANGE: ISS-2025-0080 - Resolve bindings before type/ground checks
        Term inputList = query.getArguments().get(0).resolveBindings(bindings);
        Term reversedList = query.getArguments().get(1);
        // END_CHANGE: ISS-2025-0080

        // START_CHANGE: ISS-2025-0349 - a list need only be PROPER (closed spine), not ground;
        // unbound elements are valid: reverse([X,Y],R) must give R=[Y,X]
        if (ListUtils.isProperList(inputList)) {
        // END_CHANGE: ISS-2025-0349
            // START_CHANGE: ISS-2025-0076 - Use centralized ListUtils
            // Extract elements from the input list
            List<Term> elements = ListUtils.extractElements(inputList);

            // Reverse the elements
            Collections.reverse(elements);

            // Create the reversed list term
            Term reversedListTerm = ListUtils.createList(elements);
            // END_CHANGE: ISS-2025-0076
            
            // Unify with the output list
            if (reversedList.unify(reversedListTerm, bindings)) {
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
