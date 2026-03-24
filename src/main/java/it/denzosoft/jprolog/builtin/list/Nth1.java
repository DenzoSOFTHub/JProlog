package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;



public class Nth1 implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("nth1/3 requires exactly 3 arguments.");
        }

        // START_CHANGE: ISS-2025-0080 - Resolve bindings before type/ground checks
        Term indexTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term list = query.getArguments().get(1).resolveBindings(bindings);
        Term element = query.getArguments().get(2);
        // END_CHANGE: ISS-2025-0080

        if (indexTerm.isGround() && list.isGround()) {
            // Case: nth1(GroundIndex, GroundList, Element)
            if (indexTerm instanceof Number) {
                int index = (int) Math.round(((Number) indexTerm).getValue());
                // START_CHANGE: ISS-2025-0084 - Consolidate to use ListUtils
                List<Term> elements = ListUtils.extractElements(list);
                // END_CHANGE: ISS-2025-0084
                
                // nth1 is 1-based indexing
                if (index > 0 && index <= elements.size()) {
                    Term listElement = elements.get(index - 1);
                    if (element.unify(listElement.copy(), bindings)) {
                        solutions.add(new HashMap<>(bindings));
                        return true;
                    }
                }
            }
            return false; // Index out of bounds or invalid index type
        // START_CHANGE: ISS-2025-0186 - Debug, utility, and list predicate fixes
        } else if (list.isGround() && !indexTerm.isGround()) {
            // Enumeration mode: Index unbound, List is ground
            List<Term> elements = ListUtils.extractElements(list);
            boolean found = false;
            for (int i = 0; i < elements.size(); i++) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                Term idx = new Number(i + 1); // 1-based indexing
                // START_CHANGE: ISS-2025-0191 - Let unification handle element without pre-resolution
                if (indexTerm.unify(idx, newBindings) &&
                    element.unify(elements.get(i).copy(), newBindings)) {
                // END_CHANGE: ISS-2025-0191
                    solutions.add(new HashMap<>(newBindings));
                    found = true;
                }
            }
            return found;
        // END_CHANGE: ISS-2025-0186
        } else {
            return false;
        }
    }

}
