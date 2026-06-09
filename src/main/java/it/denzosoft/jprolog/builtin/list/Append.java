package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Append extends ListPredicate {
    
    @Override
    protected int getExpectedArity() {
        return 3;
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        validateArgumentCount(query, 3);

        // START_CHANGE: ISS-2025-0070 - Resolve bindings before checking groundness
        Term list1 = query.getArguments().get(0).resolveBindings(bindings);
        Term list2 = query.getArguments().get(1).resolveBindings(bindings);
        Term result = query.getArguments().get(2).resolveBindings(bindings);
        // END_CHANGE: ISS-2025-0070

        // START_CHANGE: ISS-2025-0245 - Select mode by proper-list (closed spine) structure,
        // not deep groundness. This makes the common mode append([a],[X],R) succeed with
        // R=[a,X] instead of throwing; list elements may be unbound variables.
        if (it.denzosoft.jprolog.core.util.ListUtils.isProperList(list1)
                && it.denzosoft.jprolog.core.util.ListUtils.isProperList(list2)) {
            return handleConcatenate(list1, list2, result, bindings, solutions);
        } else if (it.denzosoft.jprolog.core.util.ListUtils.isProperList(result)) {
            return handleSplit(result, list1, list2, bindings, solutions);
        } else {
            throw new PrologEvaluationException("append/3: unsupported mode. At least List1 and List2, or List1AndList2 must be proper lists.");
        }
        // END_CHANGE: ISS-2025-0245
    }

    private boolean handleConcatenate(Term list1, Term list2, Term result, 
                                    Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        List<Term> elements1 = extractElements(list1);
        List<Term> elements2 = extractElements(list2);

        List<Term> resultElements = new ArrayList<>(elements1);
        resultElements.addAll(elements2);

        Term concatenated = createList(resultElements);
        if (result.unify(concatenated, bindings)) {
            solutions.add(new HashMap<>(bindings));
            return true;
        }
        return false;
    }

    private boolean handleSplit(Term result, Term list1, Term list2, 
                              Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        List<Term> elements = extractElements(result);
        boolean found = false;
        
        for (int i = 0; i <= elements.size(); i++) {
            Map<String, Term> newBindings = new HashMap<>(bindings);
            Term leftList = createList(elements.subList(0, i));
            Term rightList = createList(elements.subList(i, elements.size()));
            
            if (list1.unify(leftList, newBindings) && list2.unify(rightList, newBindings)) {
                solutions.add(new HashMap<>(newBindings));
                found = true;
            }
        }
        return found;
    }
}
