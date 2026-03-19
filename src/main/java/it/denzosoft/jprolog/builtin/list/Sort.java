package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class Sort implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("sort/2 requires exactly 2 arguments.");
        }

        // START_CHANGE: ISS-2025-0080 - Resolve bindings before type/ground checks
        Term inputList = query.getArguments().get(0).resolveBindings(bindings);
        Term sortedList = query.getArguments().get(1);
        // END_CHANGE: ISS-2025-0080

        if (inputList.isGround()) {
            // START_CHANGE: ISS-2025-0084 - Consolidate to use ListUtils
            List<Term> elements = ListUtils.extractElements(inputList);
            // END_CHANGE: ISS-2025-0084
            
            // Remove duplicates and sort
            List<Term> uniqueElements = new ArrayList<>();
            for (Term term : elements) {
                if (!uniqueElements.contains(term)) {
                    uniqueElements.add(term);
                }
            }
            
            Collections.sort(uniqueElements, (t1, t2) -> t1.toString().compareTo(t2.toString()));
            
            // Create the sorted list term
            Term sortedListTerm = ListUtils.createList(uniqueElements);
            
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
