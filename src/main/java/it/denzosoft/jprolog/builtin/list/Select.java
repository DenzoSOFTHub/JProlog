package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;
// START_CHANGE: ISS-2025-0076 - Use centralized ListUtils
import it.denzosoft.jprolog.core.util.ListUtils;
// END_CHANGE: ISS-2025-0076

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class Select implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("select/3 requires exactly 3 arguments.");
        }

        // START_CHANGE: ISS-2025-0066 - Resolve bindings before checking groundness
        Term element = query.getArguments().get(0).resolveBindings(bindings);
        Term inputList = query.getArguments().get(1).resolveBindings(bindings);
        Term remainderList = query.getArguments().get(2);
        // END_CHANGE: ISS-2025-0066

        // START_CHANGE: ISS-2025-0349 - a list need only be PROPER (closed spine), not ground;
        // unbound elements are valid: select(E,[X,b],R) must give two solutions
        if (ListUtils.isProperList(inputList)) {
        // END_CHANGE: ISS-2025-0349
            // START_CHANGE: ISS-2025-0076 - Use centralized ListUtils
            // Extract elements from the input list
            List<Term> elements = ListUtils.extractElements(inputList);
            // END_CHANGE: ISS-2025-0076
            boolean found = false;
            
            // Try removing each element from the list
            for (int i = 0; i < elements.size(); i++) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                
                // Try to unify the selected element
                if (element.unify(elements.get(i).copy(), newBindings)) {
                    // START_CHANGE: ISS-2025-0077 - Optimize Select remainder list construction
                    // Build remainder by concatenating sublists, avoiding O(n) remove
                    List<Term> remainderElements = new ArrayList<>(elements.size() - 1);
                    remainderElements.addAll(elements.subList(0, i));
                    remainderElements.addAll(elements.subList(i + 1, elements.size()));
                    // END_CHANGE: ISS-2025-0077
                    // START_CHANGE: ISS-2025-0076 - Use centralized ListUtils
                    Term remainderListTerm = ListUtils.createList(remainderElements);
                    // END_CHANGE: ISS-2025-0076
                    
                    // Unify with the remainder list
                    if (remainderList.unify(remainderListTerm, newBindings)) {
                        solutions.add(new HashMap<>(newBindings));
                        found = true;
                    }
                }
            }
            
            return found;
        } else {
            // START_CHANGE: ISS-2025-0079 - Return false instead of throwing for normal failure
            return false;
            // END_CHANGE: ISS-2025-0079
        }
    }

}
