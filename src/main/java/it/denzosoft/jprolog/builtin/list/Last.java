package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * last(?List, ?Elem) - True if Elem is the last element of List.
 */
public class Last implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) return false;

        Term list = query.getArguments().get(0).resolveBindings(bindings);
        Term elem = query.getArguments().get(1);

        // START_CHANGE: ISS-2025-0380 - Do not succeed unsoundly on partial/improper lists.
        // A partial list ([a|T] with T unbound) gets its open tail closed with [] (the
        // first standard solution); an improper list ([a|b]) fails instead of being
        // silently truncated to its prefix.
        List<Term> elements = new ArrayList<>();
        Term tail = ListSpine.tail(list, elements);
        if (elements.isEmpty()) return false;

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (tail instanceof Variable) {
            if (!tail.unify(new Atom("[]"), newBindings)) return false;
        } else if (!ListUtils.isEmptyList(tail)) {
            return false;
        }

        Term lastElem = elements.get(elements.size() - 1);
        if (elem.unify(lastElem, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
        // END_CHANGE: ISS-2025-0380
    }
}
