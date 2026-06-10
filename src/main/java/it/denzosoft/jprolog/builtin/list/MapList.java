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
import java.util.concurrent.atomic.AtomicInteger;

/**
 * maplist/2, maplist/3, maplist/4, maplist/5
 *
 * maplist(Goal, List)           - call(Goal, Elem) for each Elem in List
 * maplist(Goal, List1, List2)   - call(Goal, E1, E2) for each pair
 * maplist(Goal, L1, L2, L3)    - call(Goal, E1, E2, E3) for each triple
 * maplist(Goal, L1, L2, L3, L4) - call(Goal, E1, E2, E3, E4) for each quadruple
 */
public class MapList implements BuiltInWithContext {

    private final QuerySolver querySolver;

    // START_CHANGE: ISS-2025-0381 - Collision-free fresh-variable naming across nested maplist calls
    private static final AtomicInteger FRESH = new AtomicInteger();
    // END_CHANGE: ISS-2025-0381

    public MapList(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query,
                                     Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        int arity = query.getArguments().size();
        // START_CHANGE: ISS-2025-0222 - support maplist/5
        if (arity < 2 || arity > 5) {
            return false;
        }
        // END_CHANGE: ISS-2025-0222

        // START_CHANGE: ISS-2025-0381 - Translate maplist/2..5 into ONE conjunction of call/N
        // goals and solve it once, so that (a) alternative solutions of the mapped goal are
        // enumerated instead of committing to the first one per element, and (b) the shared
        // length is derived from ANY proper list argument (fixes maplist(succ, X, [2,3])).
        // Partial lists are closed soundly ([a,b|T] -> T = []) instead of being silently
        // truncated to their prefix (ISS-2025-0380).
        Term goal = query.getArguments().get(0).resolveBindings(bindings);
        int nLists = arity - 1;
        Term[] lists = new Term[nLists];
        for (int k = 0; k < nLists; k++) {
            lists[k] = query.getArguments().get(k + 1).resolveBindings(bindings);
        }

        // Determine the shared length n from the proper list arguments (they must agree).
        int n = -1;
        int maxPrefix = 0;
        for (int k = 0; k < nLists; k++) {
            List<Term> prefix = new ArrayList<>();
            Term tail = ListSpine.tail(lists[k], prefix);
            if (ListUtils.isEmptyList(tail)) {
                if (n >= 0 && n != prefix.size()) return false;   // proper lists of differing lengths
                n = prefix.size();
            } else if (tail instanceof Variable) {
                maxPrefix = Math.max(maxPrefix, prefix.size());
            } else {
                return false;   // improper list, e.g. [a|b]
            }
        }
        if (n == -1) {
            // No proper list argument: close every open tail at the minimal consistent
            // length (the first standard solution; longer lists are not enumerable in
            // the eager builtin protocol).
            n = maxPrefix;
        } else if (maxPrefix > n) {
            return false;   // a partial list is already longer than the proper lists
        }

        // Unify every list argument with a template of fresh variables [Vk1,...,Vkn];
        // this picks up the known elements and closes open tails with [].
        Map<String, Term> current = new HashMap<>(bindings);
        Variable[][] vars = new Variable[nLists][n];
        int id = FRESH.getAndIncrement();
        for (int k = 0; k < nLists; k++) {
            List<Term> templateVars = new ArrayList<>(n);
            for (int i = 0; i < n; i++) {
                vars[k][i] = new Variable("_ML" + id + "_" + k + "_" + i);
                templateVars.add(vars[k][i]);
            }
            if (!lists[k].unify(ListUtils.createList(templateVars), current)) {
                return false;
            }
        }

        if (n == 0) {
            solutions.add(current);
            return true;
        }

        // Build call(Goal, V1i, ..., Vki) for each index and chain them with ','/2.
        Term conjunction = null;
        for (int i = n - 1; i >= 0; i--) {
            Term[] extra = new Term[nLists];
            for (int k = 0; k < nLists; k++) extra[k] = vars[k][i];
            Term callGoal = buildCall(goal, extra);
            conjunction = (conjunction == null)
                    ? callGoal
                    : new CompoundTerm(new Atom(","), Arrays.asList(callGoal, conjunction));
        }

        List<Map<String, Term>> temp = new ArrayList<>();
        boolean ok = solver.solve(conjunction, new HashMap<>(current), temp, CutStatus.notOccurred());
        if (!ok || temp.isEmpty()) {
            return false;
        }
        for (Map<String, Term> sol : temp) {
            solutions.add(new HashMap<>(sol));
        }
        return true;
        // END_CHANGE: ISS-2025-0381
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
