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
 * exclude/3 - Filter list removing elements where Goal succeeds.
 * exclude(+Goal, +List, -Excluded)
 */
public class Exclude implements BuiltInWithContext {

    private final QuerySolver querySolver;

    public Exclude(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query,
                                     Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) return false;

        Term goal = query.getArguments().get(0).resolveBindings(bindings);
        Term list = query.getArguments().get(1).resolveBindings(bindings);
        Term result = query.getArguments().get(2);

        List<Term> elements = ListUtils.extractElements(list);
        List<Term> kept = new ArrayList<>();

        for (Term elem : elements) {
            Term callGoal = new CompoundTerm(new Atom("call"), Arrays.asList(goal, elem));
            List<Map<String, Term>> temp = new ArrayList<>();
            boolean ok = solver.solve(callGoal, new HashMap<>(bindings), temp, CutStatus.notOccurred());
            if (!ok || temp.isEmpty()) {
                kept.add(elem);
            }
        }

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (result.unify(ListUtils.createList(kept), newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("exclude/3 requires context");
    }
}
