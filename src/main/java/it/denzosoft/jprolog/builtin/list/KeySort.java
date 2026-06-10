package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0061 - Implement keysort/2 ISO predicate
/**
 * ISO Prolog keysort/2 - Sort a list of Key-Value pairs by key.
 * Pairs are represented as Key-Value (using the - operator).
 * The sort is stable (preserves order of equal keys) and does not remove duplicates.
 */
public class KeySort implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("keysort/2 requires exactly 2 arguments.");
        }

        Term inputList = query.getArguments().get(0).resolveBindings(bindings);
        Term outputList = query.getArguments().get(1);

        // START_CHANGE: ISS-2025-0350 - ISO 8.4.4: the input need only be a PROPER list of Key-Value
        // pairs (unbound keys/values are valid — the canonical keysort idiom); a partial list raises
        // instantiation_error, a non-list raises type_error(list, Culprit) instead of fabricating
        // answers from ListUtils.extractElements' truncated view (was: isGround gate + return false).
        if (!ListUtils.isProperList(inputList)) {
            throw Sort.notAProperList(inputList, "keysort/2");
        }

        List<Term> pairs = ListUtils.extractElements(inputList);

        // Validate all elements are Key-Value pairs (compound term with functor -)
        for (Term pair : pairs) {
            if (pair instanceof Variable) {
                throw new PrologException(ISOErrorTerms.instantiationError("keysort/2"));
            }
            if (!(pair instanceof CompoundTerm) ||
                !((CompoundTerm) pair).getName().equals("-") ||
                ((CompoundTerm) pair).getArguments().size() != 2) {
                throw new PrologException(ISOErrorTerms.typeError("pair", pair, "keysort/2"));
            }
        }
        // END_CHANGE: ISS-2025-0350

        // START_CHANGE: ISS-2025-0190 - Use ISO term ordering instead of toString comparison
        // Stable sort by key using standard term ordering
        List<Term> sorted = new ArrayList<>(pairs);
        sorted.sort((a, b) -> {
            Term keyA = ((CompoundTerm) a).getArguments().get(0);
            Term keyB = ((CompoundTerm) b).getArguments().get(0);
            return Sort.compareTerms(keyA, keyB);
        });
        // END_CHANGE: ISS-2025-0190

        Term sortedListTerm = ListUtils.createList(sorted);

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (outputList.unify(sortedListTerm, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

}
// END_CHANGE: ISS-2025-0061
