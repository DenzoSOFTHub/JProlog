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
 * maplist/2, maplist/3, maplist/4
 *
 * maplist(Goal, List)           - call(Goal, Elem) for each Elem in List
 * maplist(Goal, List1, List2)   - call(Goal, E1, E2) for each pair
 * maplist(Goal, L1, L2, L3)    - call(Goal, E1, E2, E3) for each triple
 */
public class MapList implements BuiltInWithContext {

    private final QuerySolver querySolver;

    public MapList(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query,
                                     Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        int arity = query.getArguments().size();
        if (arity < 2 || arity > 4) {
            return false;
        }

        Term goal = query.getArguments().get(0).resolveBindings(bindings);
        Term list1 = query.getArguments().get(1).resolveBindings(bindings);

        if (arity == 2) {
            return maplist2(solver, goal, list1, bindings, solutions);
        } else if (arity == 3) {
            Term list2 = query.getArguments().get(2);
            return maplist3(solver, goal, list1, list2, bindings, solutions);
        } else {
            Term list2 = query.getArguments().get(2);
            Term list3 = query.getArguments().get(3);
            return maplist4(solver, goal, list1, list2, list3, bindings, solutions);
        }
    }

    private boolean maplist2(QuerySolver solver, Term goal, Term list,
                             Map<String, Term> bindings,
                             List<Map<String, Term>> solutions) {
        List<Term> elements = ListUtils.extractElements(list);
        Map<String, Term> currentBindings = new HashMap<>(bindings);

        // START_CHANGE: ISS-2025-0184 - Accumulate bindings through iterations
        for (Term elem : elements) {
            Term callGoal = buildCall(goal, elem);
            List<Map<String, Term>> tempSolutions = new ArrayList<>();
            boolean ok = solver.solve(callGoal, new HashMap<>(currentBindings), tempSolutions, CutStatus.notOccurred());
            if (!ok || tempSolutions.isEmpty()) {
                return false;
            }
            currentBindings = new HashMap<>(tempSolutions.get(0));
        }

        solutions.add(currentBindings);
        return true;
        // END_CHANGE: ISS-2025-0184
    }

    private boolean maplist3(QuerySolver solver, Term goal, Term list1, Term list2Raw,
                             Map<String, Term> bindings,
                             List<Map<String, Term>> solutions) {
        List<Term> elems1 = ListUtils.extractElements(list1);
        Term list2 = list2Raw.resolveBindings(bindings);
        boolean list2Ground = list2.isGround();

        if (list2Ground) {
            // START_CHANGE: ISS-2025-0188 - Accumulate bindings through iterations
            List<Term> elems2 = ListUtils.extractElements(list2);
            if (elems1.size() != elems2.size()) return false;
            Map<String, Term> currentBindings = new HashMap<>(bindings);
            for (int i = 0; i < elems1.size(); i++) {
                Term callGoal = buildCall(goal, elems1.get(i), elems2.get(i));
                List<Map<String, Term>> temp = new ArrayList<>();
                if (!solver.solve(callGoal, new HashMap<>(currentBindings), temp, CutStatus.notOccurred()) || temp.isEmpty()) {
                    return false;
                }
                currentBindings = new HashMap<>(temp.get(0));
            }
            solutions.add(currentBindings);
            return true;
            // END_CHANGE: ISS-2025-0188
        } else {
            // Generate output list
            // START_CHANGE: ISS-2025-0188 - Accumulate bindings in non-ground branch
            List<Term> resultElems = new ArrayList<>();
            Map<String, Term> currentBindings3 = new HashMap<>(bindings);
            for (int i = 0; i < elems1.size(); i++) {
                Variable outVar = new Variable("_MapOut_" + i);
                Term callGoal = buildCall(goal, elems1.get(i), outVar);
                List<Map<String, Term>> temp = new ArrayList<>();
                if (!solver.solve(callGoal, new HashMap<>(currentBindings3), temp, CutStatus.notOccurred()) || temp.isEmpty()) {
                    return false;
                }
                currentBindings3 = new HashMap<>(temp.get(0));
                resultElems.add(outVar.resolveBindings(currentBindings3));
            }
            // END_CHANGE: ISS-2025-0188
            Term resultList = ListUtils.createList(resultElems);
            Map<String, Term> newBindings = new HashMap<>(currentBindings3);
            if (list2Raw.unify(resultList, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        }
    }

    private boolean maplist4(QuerySolver solver, Term goal, Term list1, Term list2Raw, Term list3Raw,
                             Map<String, Term> bindings,
                             List<Map<String, Term>> solutions) {
        List<Term> elems1 = ListUtils.extractElements(list1);
        List<Term> result2 = new ArrayList<>();
        List<Term> result3 = new ArrayList<>();
        // START_CHANGE: ISS-2025-0190 - Accumulate bindings through iterations
        Map<String, Term> currentBindings = new HashMap<>(bindings);

        for (int i = 0; i < elems1.size(); i++) {
            Variable outVar2 = new Variable("_MapOut2_" + i);
            Variable outVar3 = new Variable("_MapOut3_" + i);
            Term callGoal = buildCall(goal, elems1.get(i), outVar2, outVar3);
            List<Map<String, Term>> temp = new ArrayList<>();
            if (!solver.solve(callGoal, new HashMap<>(currentBindings), temp, CutStatus.notOccurred()) || temp.isEmpty()) {
                return false;
            }
            currentBindings = new HashMap<>(temp.get(0));
            result2.add(outVar2.resolveBindings(currentBindings));
            result3.add(outVar3.resolveBindings(currentBindings));
        }
        // END_CHANGE: ISS-2025-0190

        Map<String, Term> newBindings = new HashMap<>(currentBindings);
        if (list2Raw.unify(ListUtils.createList(result2), newBindings) &&
            list3Raw.unify(ListUtils.createList(result3), newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    private Term buildCall(Term goal, Term... extraArgs) {
        List<Term> callArgs = new ArrayList<>();
        callArgs.add(goal);
        callArgs.addAll(Arrays.asList(extraArgs));
        return new CompoundTerm(new Atom("call"), callArgs);
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("maplist requires context");
    }
}
