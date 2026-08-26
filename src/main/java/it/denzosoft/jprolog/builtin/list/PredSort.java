// START_CHANGE: CR-2025-0008 - Implement predsort/3
package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.core.util.ListUtils;

import it.denzosoft.jprolog.core.exceptions.PrologException;

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

    private final SolverContext solver;

    public PredSort(SolverContext solver) {
        this.solver = solver;
    }

    @Override
    public boolean executeWithContext(SolverContext solver, Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("predsort/3 requires exactly 3 arguments");
        }

        Term predTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term inputList = query.getArguments().get(1).resolveBindings(bindings);
        Term sortedList = query.getArguments().get(2);

        // START_CHANGE: ISS-2025-0419 - an unbound comparison predicate raises
        // instantiation_error, a non-callable one type_error(callable, Pred); the input list
        // need only be a PROPER list (variables are legal elements, lowest in standard order)
        if (predTerm instanceof Variable) {
            throw new PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError("predsort/3"));
        }
        if (!(predTerm instanceof Atom) && !(predTerm instanceof CompoundTerm)) {
            throw new PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("callable", predTerm, "predsort/3"));
        }
        if (!ListUtils.isProperList(inputList)) {
            return false;   // SWI: predsort fails on a non-list (its length/2 call fails)
        }

        List<Term> elements = ListUtils.extractElements(inputList);
        List<Term> sorted = mergeSort(solver, predTerm, elements, bindings);
        if (sorted == null) {
            return false;   // SWI: predsort fails when Pred fails on some pair
        }
        // END_CHANGE: ISS-2025-0419

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

    // START_CHANGE: ISS-2025-0419 - mergeSort/merge return null when the comparison predicate
    // fails (or binds Order to something other than <, =, >), making predsort/3 fail
    private List<Term> mergeSort(SolverContext solver, Term pred, List<Term> list, Map<String, Term> bindings) {
        if (list.size() <= 1) {
            return new ArrayList<>(list);
        }

        int mid = list.size() / 2;
        List<Term> left = mergeSort(solver, pred, list.subList(0, mid), bindings);
        if (left == null) return null;
        List<Term> right = mergeSort(solver, pred, list.subList(mid, list.size()), bindings);
        if (right == null) return null;

        return merge(solver, pred, left, right, bindings);
    }

    private List<Term> merge(SolverContext solver, Term pred, List<Term> left, List<Term> right, Map<String, Term> bindings) {
        List<Term> result = new ArrayList<>();
        int i = 0, j = 0;

        while (i < left.size() && j < right.size()) {
            String order = compareTerms(solver, pred, left.get(i), right.get(j), bindings);
            if (order == null) return null;
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

    // START_CHANGE: ISS-2025-0191 - Fix solver call signature, propagate system errors
    private String compareTerms(SolverContext solver, Term pred, Term x, Term y, Map<String, Term> bindings) {
        // Build goal: call(Pred, Order, X, Y)
        Variable orderVar = new Variable("_PredSortOrder");
        List<Term> callArgs = new ArrayList<>();
        callArgs.add(pred);
        callArgs.add(orderVar);
        callArgs.add(x);
        callArgs.add(y);
        Term callGoal = new CompoundTerm(new Atom("call"), callArgs);

        try {
            List<Map<String, Term>> tempSolutions = new ArrayList<>();
            boolean success = solver.solveMeta(callGoal, new HashMap<>(bindings), tempSolutions)   /* ISS-2025-0431 - ENG-04 */;
            if (success && !tempSolutions.isEmpty()) {
                Term orderTerm = orderVar.resolveBindings(tempSolutions.get(0));
                // START_CHANGE: ISS-2025-0419 - only an Order of <, =, or > counts as a
                // successful comparison; anything else makes predsort/3 fail (SWI semantics)
                if (orderTerm instanceof Atom) {
                    String name = ((Atom) orderTerm).getName();
                    if ("<".equals(name) || "=".equals(name) || ">".equals(name)) {
                        return name;
                    }
                }
                // END_CHANGE: ISS-2025-0419
            }
        } catch (PrologException e) {
            throw e; // Propagate Prolog exceptions
        } catch (RuntimeException e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw e; // Propagate system errors
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            // Fall through for checked exceptions: predsort fails
        }

        // START_CHANGE: ISS-2025-0419 - the comparison failed: predsort/3 must FAIL, never
        // silently fall back to '<' (which returned arbitrary, un-deduplicated orderings)
        return null;
        // END_CHANGE: ISS-2025-0419
    }
    // END_CHANGE: ISS-2025-0191
}
// END_CHANGE: CR-2025-0008
