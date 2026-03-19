// START_CHANGE: CR-2025-0008 - Implement permutation/2
package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * permutation/2 - Generate permutations of a list.
 * permutation(+List, ?Permutation)
 *
 * True when Permutation is a permutation of List.
 * On backtracking, generates all permutations.
 */
public class Permutation implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("permutation/2 requires exactly 2 arguments");
        }

        Term inputList = query.getArguments().get(0).resolveBindings(bindings);
        Term permList = query.getArguments().get(1);

        if (!inputList.isGround()) {
            return false;
        }

        List<Term> elements = ListUtils.extractElements(inputList);
        List<List<Term>> allPermutations = new ArrayList<>();
        generatePermutations(elements, 0, allPermutations);

        boolean found = false;
        for (List<Term> perm : allPermutations) {
            Term permTerm = ListUtils.createList(perm);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (permList.unify(permTerm, newBindings)) {
                solutions.add(newBindings);
                found = true;
            }
        }
        return found;
    }

    private void generatePermutations(List<Term> elements, int start, List<List<Term>> result) {
        if (start == elements.size()) {
            result.add(new ArrayList<>(elements));
            return;
        }
        for (int i = start; i < elements.size(); i++) {
            swap(elements, start, i);
            generatePermutations(elements, start + 1, result);
            swap(elements, start, i);
        }
    }

    private void swap(List<Term> list, int i, int j) {
        Term temp = list.get(i);
        list.set(i, list.get(j));
        list.set(j, temp);
    }
}
// END_CHANGE: CR-2025-0008
