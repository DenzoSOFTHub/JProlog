package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * delete(+List, +Elem, -Result) - Remove all occurrences of Elem from List.
 */
public class Delete implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) return false;

        Term list = query.getArguments().get(0).resolveBindings(bindings);
        Term elem = query.getArguments().get(1).resolveBindings(bindings);
        Term result = query.getArguments().get(2);

        List<Term> elements = ListUtils.extractElements(list);
        List<Term> filtered = new ArrayList<>();

        for (Term e : elements) {
            // START_CHANGE: ISS-2025-0188 - Use current bindings for variable context
            Map<String, Term> testBindings = new HashMap<>(bindings);
            // END_CHANGE: ISS-2025-0188
            if (!e.unify(elem, testBindings)) {
                filtered.add(e);
            }
        }

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (result.unify(ListUtils.createList(filtered), newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }
}
