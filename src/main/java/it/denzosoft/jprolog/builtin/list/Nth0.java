package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
// START_CHANGE: ISS-2025-0076 - Use centralized ListUtils
import it.denzosoft.jprolog.core.util.ListUtils;
// END_CHANGE: ISS-2025-0076

import java.util.HashMap;
import java.util.List;
import java.util.Map;



public class Nth0 implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("nth0/3 requires exactly 3 arguments.");
        }

        // START_CHANGE: ISS-2025-0080 - Resolve bindings before type/ground checks
        Term indexTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term list = query.getArguments().get(1).resolveBindings(bindings);
        Term element = query.getArguments().get(2).resolveBindings(bindings);
        // END_CHANGE: ISS-2025-0080

        if (indexTerm.isGround() && list.isGround()) {
            // Case: nth0(GroundIndex, GroundList, Element)
            if (indexTerm instanceof Number) {
                int index = (int) Math.round(((Number) indexTerm).getValue());
                // START_CHANGE: ISS-2025-0076 - Use centralized ListUtils
                List<Term> elements = ListUtils.extractElements(list);
                // END_CHANGE: ISS-2025-0076

                if (index >= 0 && index < elements.size()) {
                    Term listElement = elements.get(index);
                    if (element.unify(listElement.copy(), bindings)) {
                        solutions.add(new HashMap<>(bindings));
                        return true;
                    }
                }
            }
            return false; // Index out of bounds or invalid index type
        } else if (list.isGround() && element.isGround()) {
            // Case: nth0(Index, GroundList, GroundElement)
            // START_CHANGE: ISS-2025-0076 - Use centralized ListUtils
            List<Term> elements = ListUtils.extractElements(list);
            // END_CHANGE: ISS-2025-0076

            for (int i = 0; i < elements.size(); i++) {
                if (elements.get(i).unify(element.copy(), new HashMap<>())) {
                    Term indexVar = new Number(i);
                    if (indexTerm.unify(indexVar, bindings)) {
                        solutions.add(new HashMap<>(bindings));
                        return true;
                    }
                }
            }
            return false;
        } else {
            throw new PrologEvaluationException("nth0/3: unsupported argument pattern.");
        }
    }

}
