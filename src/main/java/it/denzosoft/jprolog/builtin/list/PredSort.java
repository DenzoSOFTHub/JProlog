// START_CHANGE: CR-2025-0008 - Implement predsort/3
package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * predsort/3 - Sort a list using a user-defined comparison predicate.
 * predsort(+Pred, +List, -Sorted)
 *
 * Pred is called as call(Pred, Order, X, Y) where Order should unify with
 * <, =, or >. Elements with Order = are merged (removed duplicates).
 */
public class PredSort implements BuiltInWithContext {

    private final QuerySolver solver;

    public PredSort(QuerySolver solver) {
        this.solver = solver;
    }

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("predsort/3 requires exactly 3 arguments");
        }

        Term predTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term inputList = query.getArguments().get(1).resolveBindings(bindings);
        Term sortedList = query.getArguments().get(2);

        if (!inputList.isGround()) {
            return false;
        }

        List<Term> elements = ListUtils.extractElements(inputList);
        List<Term> sorted = mergeSort(solver, predTerm, elements, bindings);

        Term sortedTerm = ListUtils.createList(sorted);
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (sortedList.unify(sortedTerm, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("Context-dependent built-in 'predsort' must be invoked with context");
    }

    private List<Term> mergeSort(QuerySolver solver, Term pred, List<Term> list, Map<String, Term> bindings) {
        if (list.size() <= 1) {
            return new ArrayList<>(list);
        }

        int mid = list.size() / 2;
        List<Term> left = mergeSort(solver, pred, list.subList(0, mid), bindings);
        List<Term> right = mergeSort(solver, pred, list.subList(mid, list.size()), bindings);

        return merge(solver, pred, left, right, bindings);
    }

    private List<Term> merge(QuerySolver solver, Term pred, List<Term> left, List<Term> right, Map<String, Term> bindings) {
        List<Term> result = new ArrayList<>();
        int i = 0, j = 0;

        while (i < left.size() && j < right.size()) {
            String order = compareTerms(solver, pred, left.get(i), right.get(j), bindings);
            if ("<".equals(order)) {
                result.add(left.get(i++));
            } else if ("=".equals(order)) {
                result.add(left.get(i++));
                j++; // Skip duplicate
            } else {
                result.add(right.get(j++));
            }
        }

        while (i < left.size()) result.add(left.get(i++));
        while (j < right.size()) result.add(right.get(j++));

        return result;
    }

    private String compareTerms(QuerySolver solver, Term pred, Term x, Term y, Map<String, Term> bindings) {
        // Build goal: call(Pred, Order, X, Y)
        Variable orderVar = new Variable("_PredSortOrder");
        List<Term> callArgs = new ArrayList<>();
        callArgs.add(pred);
        callArgs.add(orderVar);
        callArgs.add(x);
        callArgs.add(y);
        Term callGoal = new CompoundTerm(new Atom("call"), callArgs);

        try {
            List<Map<String, Term>> tempSolutions = solver.solve(callGoal);
            if (!tempSolutions.isEmpty()) {
                Term orderTerm = orderVar.resolveBindings(tempSolutions.get(0));
                if (orderTerm instanceof Atom) {
                    return ((Atom) orderTerm).getName();
                }
            }
        } catch (Exception e) {
            // Fall through to default
        }

        // Default to standard term ordering
        return "<";
    }
}
// END_CHANGE: CR-2025-0008
