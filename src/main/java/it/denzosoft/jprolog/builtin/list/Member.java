package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
// START_CHANGE: ISS-2025-0076 - Use centralized ListUtils
import it.denzosoft.jprolog.core.util.ListUtils;
// END_CHANGE: ISS-2025-0076

import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class Member implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("member/2 requires exactly 2 arguments.");
        }

        Term element = query.getArguments().get(0);
        Term list = query.getArguments().get(1);

        if (list.isGround()) {
            // Find all elements in the ground list and try to unify with the given element
            // START_CHANGE: ISS-2025-0076 - Use centralized ListUtils
            List<Term> elements = ListUtils.extractElements(list);
            // END_CHANGE: ISS-2025-0076
            boolean found = false;
            
            for (Term listItem : elements) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (element.unify(listItem.copy(), newBindings)) {
                    solutions.add(new HashMap<>(newBindings));
                    found = true;
                }
            }
            
            return found;
        } else {
            // START_CHANGE: ISS-2025-0053 - Fix member/2 to support non-ground lists
            // Walk the list structure and unify element with each head
            Term current = list.resolveBindings(bindings);
            boolean found = false;

            while (current instanceof CompoundTerm) {
                CompoundTerm compound = (CompoundTerm) current;
                if (compound.getName().equals(".") && compound.getArguments().size() == 2) {
                    Term head = compound.getArguments().get(0);
                    Term tail = compound.getArguments().get(1);

                    Map<String, Term> newBindings = new HashMap<>(bindings);
                    if (element.unify(head, newBindings)) {
                        solutions.add(new HashMap<>(newBindings));
                        found = true;
                    }

                    // START_CHANGE: ISS-2025-0188 - Resolve tail with current bindings
                    current = tail.resolveBindings(newBindings);
                    // END_CHANGE: ISS-2025-0188
                } else {
                    break;
                }
            }

            return found;
            // END_CHANGE: ISS-2025-0053
        }
    }

}
