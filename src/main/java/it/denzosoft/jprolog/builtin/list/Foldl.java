package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.CutStatus;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * foldl/4, foldl/5, foldl/6 - Left fold over list(s).
 *
 * foldl(:Goal, ?List, +V0, -V)
 * foldl(:Goal, ?List1, ?List2, +V0, -V)
 * foldl(:Goal, ?List1, ?List2, ?List3, +V0, -V)
 *
 * For foldl/4: calls call(Goal, Elem, V_i, V_i+1) for each element.
 */
public class Foldl implements BuiltInWithContext {

    private final QuerySolver querySolver;

    public Foldl(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query,
                                     Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        int arity = query.getArguments().size();
        if (arity < 4 || arity > 6) return false;

        Term goal = query.getArguments().get(0).resolveBindings(bindings);

        if (arity == 4) {
            return foldl4(solver, goal, query, bindings, solutions);
        } else if (arity == 5) {
            return foldl5(solver, goal, query, bindings, solutions);
        } else {
            return foldl6(solver, goal, query, bindings, solutions);
        }
    }

    private boolean foldl4(QuerySolver solver, Term goal, Term query,
                           Map<String, Term> bindings,
                           List<Map<String, Term>> solutions) {
        Term list = query.getArguments().get(1).resolveBindings(bindings);
        Term v0 = query.getArguments().get(2).resolveBindings(bindings);
        Term vResult = query.getArguments().get(3);

        List<Term> elements = ListUtils.extractElements(list);
        Term accumulator = v0;

        for (int i = 0; i < elements.size(); i++) {
            Variable nextAcc = new Variable("_FoldAcc_" + i);
            Term callGoal = new CompoundTerm(new Atom("call"),
                Arrays.asList(goal, elements.get(i), accumulator, nextAcc));

            List<Map<String, Term>> temp = new ArrayList<>();
            if (!solver.solve(callGoal, new HashMap<>(bindings), temp, CutStatus.notOccurred()) || temp.isEmpty()) {
                return false;
            }
            accumulator = nextAcc.resolveBindings(temp.get(0));
        }

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (vResult.unify(accumulator, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    private boolean foldl5(QuerySolver solver, Term goal, Term query,
                           Map<String, Term> bindings,
                           List<Map<String, Term>> solutions) {
        Term list1 = query.getArguments().get(1).resolveBindings(bindings);
        Term list2 = query.getArguments().get(2).resolveBindings(bindings);
        Term v0 = query.getArguments().get(3).resolveBindings(bindings);
        Term vResult = query.getArguments().get(4);

        List<Term> elems1 = ListUtils.extractElements(list1);
        List<Term> elems2 = ListUtils.extractElements(list2);
        if (elems1.size() != elems2.size()) return false;

        Term accumulator = v0;
        for (int i = 0; i < elems1.size(); i++) {
            Variable nextAcc = new Variable("_FoldAcc_" + i);
            Term callGoal = new CompoundTerm(new Atom("call"),
                Arrays.asList(goal, elems1.get(i), elems2.get(i), accumulator, nextAcc));

            List<Map<String, Term>> temp = new ArrayList<>();
            if (!solver.solve(callGoal, new HashMap<>(bindings), temp, CutStatus.notOccurred()) || temp.isEmpty()) {
                return false;
            }
            accumulator = nextAcc.resolveBindings(temp.get(0));
        }

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (vResult.unify(accumulator, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    private boolean foldl6(QuerySolver solver, Term goal, Term query,
                           Map<String, Term> bindings,
                           List<Map<String, Term>> solutions) {
        Term list1 = query.getArguments().get(1).resolveBindings(bindings);
        Term list2 = query.getArguments().get(2).resolveBindings(bindings);
        Term list3 = query.getArguments().get(3).resolveBindings(bindings);
        Term v0 = query.getArguments().get(4).resolveBindings(bindings);
        Term vResult = query.getArguments().get(5);

        List<Term> elems1 = ListUtils.extractElements(list1);
        List<Term> elems2 = ListUtils.extractElements(list2);
        List<Term> elems3 = ListUtils.extractElements(list3);
        if (elems1.size() != elems2.size() || elems1.size() != elems3.size()) return false;

        Term accumulator = v0;
        for (int i = 0; i < elems1.size(); i++) {
            Variable nextAcc = new Variable("_FoldAcc_" + i);
            Term callGoal = new CompoundTerm(new Atom("call"),
                Arrays.asList(goal, elems1.get(i), elems2.get(i), elems3.get(i), accumulator, nextAcc));

            List<Map<String, Term>> temp = new ArrayList<>();
            if (!solver.solve(callGoal, new HashMap<>(bindings), temp, CutStatus.notOccurred()) || temp.isEmpty()) {
                return false;
            }
            accumulator = nextAcc.resolveBindings(temp.get(0));
        }

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (vResult.unify(accumulator, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("foldl requires context");
    }
}
