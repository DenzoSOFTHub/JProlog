// START_CHANGE: ISS-2025-0221 - partition(Pred, List, Included, Excluded)
package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.CutStatus;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * partition(+Pred, +List, ?Included, ?Excluded) — split List by Pred.
 * For each X in List: if call(Pred, X) succeeds, X goes to Included; else Excluded.
 */
public class Partition implements BuiltInWithContext {

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query,
                                       Map<String, Term> bindings,
                                       List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 4) return false;
        Term goal = query.getArguments().get(0).resolveBindings(bindings);
        Term list = query.getArguments().get(1).resolveBindings(bindings);
        Term incTerm = query.getArguments().get(2);
        Term excTerm = query.getArguments().get(3);

        List<Term> elements = ListUtils.extractElements(list);
        List<Term> included = new ArrayList<>();
        List<Term> excluded = new ArrayList<>();

        Map<String, Term> currentBindings = new HashMap<>(bindings);
        for (Term elem : elements) {
            Term callGoal = new CompoundTerm(new Atom("call"), Arrays.asList(goal, elem));
            List<Map<String, Term>> temp = new ArrayList<>();
            boolean ok = solver.solve(callGoal, new HashMap<>(currentBindings), temp, CutStatus.notOccurred());
            if (ok && !temp.isEmpty()) {
                included.add(elem);
                currentBindings = new HashMap<>(temp.get(0));
            } else {
                excluded.add(elem);
            }
        }

        Map<String, Term> newBindings = new HashMap<>(currentBindings);
        if (incTerm.unify(ListUtils.createList(included), newBindings) &&
            excTerm.unify(ListUtils.createList(excluded), newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("partition/4 requires context");
    }
}
// END_CHANGE: ISS-2025-0221
