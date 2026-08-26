package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.SolverContext;
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
 * include/3 - Filter list keeping elements where Goal succeeds.
 * include(+Goal, +List, -Included)
 */
public class Include implements BuiltInWithContext {

    private final SolverContext querySolver;

    public Include(SolverContext querySolver) {
        this.querySolver = querySolver;
    }

    @Override
    public boolean executeWithContext(SolverContext solver, Term query,
                                     Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) return false;

        Term goal = query.getArguments().get(0).resolveBindings(bindings);
        Term list = query.getArguments().get(1).resolveBindings(bindings);
        Term result = query.getArguments().get(2);

        List<Term> elements = ListUtils.extractElements(list);
        List<Term> included = new ArrayList<>();

        // START_CHANGE: ISS-2025-0193 - Accumulate bindings from goal across iterations
        Map<String, Term> currentBindings = new HashMap<>(bindings);
        for (Term elem : elements) {
            Term callGoal = new CompoundTerm(new Atom("call"), Arrays.asList(goal, elem));
            List<Map<String, Term>> temp = new ArrayList<>();
            boolean ok = solver.solveMeta(callGoal, new HashMap<>(currentBindings), temp)   /* ISS-2025-0431 - ENG-04 */;
            if (ok && !temp.isEmpty()) {
                included.add(elem);
                currentBindings = new HashMap<>(temp.get(0));
            }
        }
        // END_CHANGE: ISS-2025-0193

        Map<String, Term> newBindings = new HashMap<>(currentBindings);
        if (result.unify(ListUtils.createList(included), newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("include/3 requires context");
    }
}
