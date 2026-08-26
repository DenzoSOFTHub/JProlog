package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.BuiltInRegistry;
import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.ControlFlow;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0443 - engine v4, design B.5 (running the existing built-ins on v4).
/**
 * Runs every existing {@link BuiltIn} / {@link BuiltInWithContext} class on the v4 machine,
 * unchanged. This is what makes v4 usable from wave W1 instead of wave W9: the ~400 built-ins of
 * the library keep their eager {@code (goal, Map, List<Map>)} contract and are migrated to the v4
 * SPI predicate by predicate.
 *
 * <h3>The handoff</h3>
 * The built-in is given a <b>resolved</b> goal ({@link Unify#resolve}, structure-sharing) and an
 * <b>empty</b> binding map. That is a faithful view, because after resolution every variable still
 * in the goal is an unbound cell — so the built-in returns only the bindings it created, and the
 * adapter installs them by unifying each of the goal's cells with the value the built-in produced
 * for its name. Chains inside one solution map ({@code {X: Y, Y: 3}}) are followed through the map
 * before unification, so the cell ends up bound to {@code 3} and not to a stray object.
 *
 * <h3>What it does NOT do</h3>
 * It does not pass a copy of a global binding store (v3.8.0 copied every binding in the query on
 * every built-in call — O(N^2), limit L-08/ENG-11), and a deterministic built-in gets no choice
 * point at all.
 */
public final class LegacyBuiltinAdapter {

    private LegacyBuiltinAdapter() {}

    static final Term[] NO_ARGS = new Term[0];
    static final Term[] NO_FRAME = new Term[0];

    /**
     * Run {@code goal} through the legacy registry.
     *
     * @return 1 = succeeded, 0 = failed, -1 = not a built-in (try user clauses)
     */
    static int run(Machine m, Term goal, String functor, int arity) {
        BuiltInRegistry registry = m.engine().registry();
        if (registry == null || !registry.isBuiltIn(functor, arity)) return -1;
        BuiltIn b = registry.getBuiltIn(functor);
        if (b == null) return -1;

        final int dd = m.debugTraceActive() ? m.enterPort() : -1;   // ISS-2025-0482
        if (dd >= 0) m.portCall(goal, dd);

        Term callGoal = m.resolve(goal);
        Map<String, Term> inMap = new HashMap<String, Term>();
        List<Map<String, Term>> sols = new ArrayList<Map<String, Term>>();
        boolean ok;
        try {
            if (b instanceof BuiltInWithContext) {
                ok = ((BuiltInWithContext) b).executeWithContext(m.facade(), callGoal, inMap, sols);
            } else {
                ok = b.execute(callGoal, inMap, sols);
            }
        } catch (PrologException pe) {
            throw pe;                                   // a real ISO error must reach catch/3
        } catch (it.denzosoft.jprolog.core.engine.InferenceLimitException
               | it.denzosoft.jprolog.core.engine.QueryCancelledException
               | it.denzosoft.jprolog.core.engine.DebugController.DebugStopException control) {
            throw control;                              // trust model: never convertible, never caught
        } catch (it.denzosoft.jprolog.core.engine.NeedsSolverContextException nsc) {
            return -1;
        } catch (RuntimeException e) {
            ControlFlow.rethrowIfControl(e);
            if (dd >= 0) m.portFail(goal, dd);
            throw Errors.system(e.getClass().getSimpleName()
                + (e.getMessage() == null ? "" : ": " + e.getMessage()), functor + "/" + arity);
        }
        if (!ok || sols.isEmpty()) {
            if (dd >= 0) m.portFail(goal, dd);
            return 0;
        }
        return installSolutions(m, callGoal, sols, dd >= 0 ? goal : null, dd) ? 1 : 0;
    }

    /**
     * Bind the goal's cells from the built-in's solution maps: one solution deterministically, or a
     * lazy choice point over the rest.
     */
    static boolean installSolutions(Machine m, Term resolvedGoal, List<Map<String, Term>> sols,
                                    Term traceGoal, int traceDepth) {
        boolean anyBinding = false;
        for (int i = 0; i < sols.size(); i++) {
            if (!sols.get(i).isEmpty()) { anyBinding = true; break; }
        }
        // A side-effect-only built-in (write/1, nl/0, assert/1, ...) returns empty maps: skip the
        // walk of the goal entirely — on a goal holding a million-element list that walk is the
        // single most expensive thing the bridge does.
        Map<String, Variable> cells = java.util.Collections.emptyMap();
        if (anyBinding) {
            cells = new HashMap<String, Variable>();
            collectCells(resolvedGoal, cells);
        }
        if (sols.size() == 1 && traceGoal == null) {
            return apply(m, cells, sols.get(0));        // deterministic: no choice point at all
        }
        return m.pushSolutionChoice(cells, sols, traceGoal, traceDepth);
    }

    /**
     * Install one solution map. Every entry names a variable of the GOAL — a bridged built-in
     * cannot report a binding for anything else.
     *
     * <p>ISS-2025-0460 replaced {@code Machine.cellFor}, the engine-wide bounded name index of
     * waves W1-W3, with a lookup scoped to the CLP(FD) bridge's own cells, because
     * {@code exportSingletons/1} reported functionally determined variables the goal never
     * mentions. ISS-2025-0471 (wave W6) took {@code label/1} and {@code labeling/2} off that path,
     * and ISS-2025-0486 (wave W9) took the last users off it — {@code in/2}, the
     * {@code #=}/{@code #<}/… comparisons and {@code all_different/1} are cell-based v4 natives
     * now — so the fallback lookup is DELETED and this method is purely goal-scoped again.
     * An entry whose cell is already bound is skipped, as the v3.8.0 {@code applySolution} did.
     */
    static boolean apply(Machine m, Map<String, Variable> cells, Map<String, Term> sol) {
        if (sol.isEmpty()) return true;
        for (Map.Entry<String, Term> e : sol.entrySet()) {
            Variable cell = cells.get(e.getKey());
            if (cell == null || cell.ref != null) continue;
            Term value = resolveThroughMap(e.getValue(), sol);
            if (value == cell) continue;
            if (!m.unify(cell, value)) return false;
        }
        return true;
    }

    /** Follow a chain inside ONE solution map: {@code {X: Y, Y: 3}} must bind X to 3. */
    private static Term resolveThroughMap(Term v, Map<String, Term> sol) {
        Term cur = v;
        int depth = 0;
        while (cur instanceof Variable && ((Variable) cur).ref == null) {
            Term next = sol.get(((Variable) cur).getName());
            if (next == null || next == cur || ++depth > 64) break;
            cur = next;
        }
        if (cur instanceof CompoundTerm) {
            try {
                return cur.resolveBindings(sol);
            } catch (RuntimeException e) {
                ControlFlow.rethrowIfControl(e);
                return cur;
            }
        }
        return cur;
    }

    /** Index the UNBOUND cells of a resolved term by name. */
    static void collectCells(Term t, Map<String, Variable> out) {
        IdentityHashMap<Term, Boolean> seen = null;
        ArrayList<Term> work = new ArrayList<Term>();
        work.add(t);
        int n = 0;
        while (!work.isEmpty()) {
            Term x = Unify.deref(work.remove(work.size() - 1));
            if (x instanceof Variable) {
                out.put(((Variable) x).getName(), (Variable) x);
            } else if (x instanceof CompoundTerm) {
                if (++n > 4096) {
                    if (seen == null) seen = new IdentityHashMap<Term, Boolean>();
                    if (seen.put(x, Boolean.TRUE) != null) continue;
                }
                List<Term> as = ((CompoundTerm) x).getArguments();
                for (int i = as.size() - 1; i >= 0; i--) work.add(as.get(i));
            }
        }
    }

}
// END_CHANGE: ISS-2025-0443
