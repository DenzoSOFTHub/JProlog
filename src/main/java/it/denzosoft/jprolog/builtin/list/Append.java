package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.core.util.ListUtils;

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

        // START_CHANGE: ISS-2025-0379 - Support partial/open modes without throwing.
        // (+,?,?): List1 proper -> Result = [e1,...,en|List2] directly (List2 may be a
        //          variable or partial list, giving partial-list answers like Z=[1|X]).
        // (?,?,+): Result proper -> enumerate all splits (handles partial List1/List2 too).
        // (open,?,open): infinite enumeration is not representable in the eager builtin
        //          protocol; produce the first standard solution (close List1's open tail
        //          with []) instead of throwing — sound but bounded to one answer.
        if (ListUtils.isProperList(list1)) {
            return handleConcatenate(list1, list2, result, bindings, solutions);
        } else if (ListUtils.isProperList(result)) {
            return handleSplit(result, list1, list2, bindings, solutions);
        } else {
            List<Term> prefix1 = new ArrayList<>();
            Term tail1 = ListSpine.tail(list1, prefix1);
            if (!(tail1 instanceof Variable)) {
                return false;   // improper List1 (e.g. [1|foo]) can never satisfy append/3
            }
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (!tail1.unify(new Atom("[]"), newBindings)) {
                return false;
            }
            Term concatenated = ListUtils.createListWithTail(prefix1, list2);
            if (result.unify(concatenated, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        }
        // END_CHANGE: ISS-2025-0379
    }

    // START_CHANGE: ISS-2025-0379 - Build [e1,...,en|List2] directly so List2/Result may be open
    private boolean handleConcatenate(Term list1, Term list2, Term result,
                                    Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        List<Term> elements1 = extractElements(list1);

        Term concatenated = ListUtils.createListWithTail(elements1, list2);
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (result.unify(concatenated, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }
    // END_CHANGE: ISS-2025-0379

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
