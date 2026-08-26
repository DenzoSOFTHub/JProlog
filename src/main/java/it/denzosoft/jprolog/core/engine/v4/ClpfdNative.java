package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.builtin.clpfd.v2.ClpfdV2Bridge;
import it.denzosoft.jprolog.builtin.clpfd.v2.Constraint;
import it.denzosoft.jprolog.builtin.clpfd.v2.Labeler;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0471 - engine v4 wave W6: label/1 and labeling/2 as v4 natives.
/**
 * {@code label/1} and {@code labeling/2} on the cell model.
 *
 * <h3>Why this exists</h3>
 * The registry implementation ({@code builtin.clpfd.v2.ClpfdV2Builtins.Label}) returns
 * {@code List<Map<String,Term>>} and calls {@code ClpfdV2Bridge.exportSingletons/1} to add the
 * functionally determined variables ({@code C in 1..3, D #= C*2+1, label([C])} must report
 * {@code D}). Those variables are named in the map but do not occur in the goal, which is why
 * {@code LegacyBuiltinAdapter} still has to ask the bridge for the cell behind a name — the last
 * name-keyed hop in the v4 engine (section 10.4 of the progress report). Here the labeler hands
 * back the <b>cells</b> directly, so the hop is not needed for labeling at all.
 *
 * <p>The generator is lazy in DELIVERY (one assignment per redo, trust-me popped on the last one)
 * but the search itself is eager, exactly as the registry version is. Making the DFS resumable
 * across redos is not possible while the CLP(FD) store rolls back with the machine's trail: the
 * machine runs the store's undo action ({@code core.engine.v4.Undo}, ISS-2025-0492) before every
 * redo, which would undo the half-finished search's own narrowing. A per-engine constraint store
 * is a later item (design B.12/L-06).
 *
 * <p>The legacy built-in stays registered; it is what a direct registry call would run.
 */
final class ClpfdNative {

    private ClpfdNative() {}

    // START_CHANGE: ISS-2025-0500 - 4.2 wave C: the CLP(FD) bridge records its backtrackable store
    // mutations through a sink it declares itself; the engine points that sink at the running
    // machine's trail. That is the last external user of core.engine.v4.Undo, which is
    // package-private again.
    static {
        it.denzosoft.jprolog.builtin.clpfd.v2.ClpfdV2Bridge.setUndoSink(
            new it.denzosoft.jprolog.builtin.clpfd.v2.ClpfdV2Bridge.UndoSink() {
                @Override public void record(Runnable undo) { Undo.record(undo); }
            });
    }
    // END_CHANGE: ISS-2025-0500

    static void register(BuiltinTable t) {
        t.register("label", 1, new Label(false));
        t.register("labeling", 2, new Label(true));
        // START_CHANGE: ISS-2025-0486 - wave W9: the posting predicates are natives too, so that
        // propagation binds CELLS instead of naming variables in a Map<String,Term> that
        // LegacyBuiltinAdapter had to translate back through ClpfdV2Bridge.cellFor/1 (LIM-037).
        t.register("in", 2, new In());
        t.register("#=", 2, new Cmp(Constraint.Rel.EQ));
        t.register("#\\=", 2, new Cmp(Constraint.Rel.NE));
        t.register("#<", 2, new Cmp(Constraint.Rel.LT));
        t.register("#>", 2, new Cmp(Constraint.Rel.GT));
        t.register("#=<", 2, new Cmp(Constraint.Rel.LE));
        t.register("#>=", 2, new Cmp(Constraint.Rel.GE));
        t.register("all_different", 1, new AllDifferent());
        t.register("all_distinct", 1, new AllDifferent());
        // END_CHANGE: ISS-2025-0486
    }

    // START_CHANGE: ISS-2025-0486 - the cell-model posting predicates.
    /** Empty: on v4 a native receives dereferenced terms, so the bridge needs no substitution. */
    private static final Map<String, Term> NO_BINDINGS = java.util.Collections.emptyMap();

    /**
     * Bind every FD cell whose domain propagation has determined ({@code X #= 2} binds
     * {@code X = 2}, and {@code C in 1..3, D #= C*2+1, C #= 1} binds {@code D = 3} even though the
     * posting goal never mentions {@code D} — ISS-2025-0357).
     */
    private static Builtin.Outcome bindDetermined(Machine m) {
        for (Map.Entry<Variable, Long> e : ClpfdV2Bridge.determinedCells().entrySet()) {
            Variable cell = e.getKey();
            if (cell.ref != null) continue;
            if (!m.unify(cell, Number.valueOf(e.getValue().longValue()))) return Builtin.Outcome.FAILURE;
        }
        return Builtin.Outcome.SUCCESS;
    }

    /** {@code Var in Lo..Hi}. */
    private static final class In implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Term dom = m.deref(args[1]);
            // START_CHANGE: ISS-2025-0510 - X in a used to fail silently; the domain argument has
            // a shape and a wrong one is a type/domain error (clpfd's own vocabulary).
            if (dom instanceof Variable) throw Errors.instantiation("in/2");
            if (!(dom instanceof CompoundTerm) || !"..".equals(((CompoundTerm) dom).getName())
                    || ((CompoundTerm) dom).getArguments().size() != 2) {
                throw Errors.type("clpfd_domain", m.resolve(dom), "in/2");
            }
            Term loT = m.deref(((CompoundTerm) dom).getArguments().get(0));
            Term hiT = m.deref(((CompoundTerm) dom).getArguments().get(1));
            if (loT instanceof Variable || hiT instanceof Variable) throw Errors.instantiation("in/2");
            if (!(loT instanceof Number) || !((Number) loT).isInteger()) {
                throw Errors.type("integer", m.resolve(loT), "in/2");
            }
            if (!(hiT instanceof Number) || !((Number) hiT).isInteger()) {
                throw Errors.type("integer", m.resolve(hiT), "in/2");
            }
            // END_CHANGE: ISS-2025-0510
            if (!ClpfdV2Bridge.postIn(m.resolve(args[0]), ((Number) loT).longValue(),
                                      ((Number) hiT).longValue(), NO_BINDINGS)) {
                return Outcome.FAILURE;
            }
            return bindDetermined(m);
        }
    }

    /** {@code Left #rel Right}. */
    private static final class Cmp implements Builtin {
        private final Constraint.Rel rel;
        Cmp(Constraint.Rel rel) { this.rel = rel; }
        @Override public Outcome call(Machine m, Term[] args) {
            if (!ClpfdV2Bridge.postCmp(m.resolve(args[0]), rel, m.resolve(args[1]), NO_BINDINGS)) {
                return Outcome.FAILURE;
            }
            return bindDetermined(m);
        }
    }

    /** {@code all_different(List)} / {@code all_distinct(List)}. */
    private static final class AllDifferent implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            List<Term> elems = list(m, args[0]);
            if (elems == null) return Outcome.FAILURE;
            for (int i = 0; i < elems.size(); i++) elems.set(i, m.resolve(elems.get(i)));
            if (!ClpfdV2Bridge.postAllDifferent(elems, NO_BINDINGS)) return Outcome.FAILURE;
            return bindDetermined(m);
        }
    }
    // END_CHANGE: ISS-2025-0486

    private static final class Label implements Builtin {
        private final boolean withOptions;
        Label(boolean withOptions) { this.withOptions = withOptions; }

        @Override public Outcome call(Machine m, Term[] args) {
            String ind = withOptions ? "labeling/2" : "label/1";
            Labeler.VarSel varSel = Labeler.VarSel.FF;
            Labeler.ValOrder valOrder = Labeler.ValOrder.UP;
            Term objective = null;
            boolean objMin = false;
            if (withOptions) {
                List<Term> opts = list(m, m.deref(args[0]));
                if (opts == null) {
                    Term o = m.deref(args[0]);
                    throw (o instanceof Variable) ? Errors.instantiation(ind)
                                                  : Errors.type("list", m.resolve(o), ind);
                }
                for (int i = 0; i < opts.size(); i++) {
                    Term opt = m.deref(opts.get(i));
                    if (opt instanceof Atom) {
                        String n = ((Atom) opt).getName();
                        if ("leftmost".equals(n)) varSel = Labeler.VarSel.LEFTMOST;
                        else if ("ff".equals(n) || "ffc".equals(n)) varSel = Labeler.VarSel.FF;
                        else if ("min".equals(n)) varSel = Labeler.VarSel.MIN;
                        else if ("max".equals(n)) varSel = Labeler.VarSel.MAX;
                        else if ("up".equals(n)) valOrder = Labeler.ValOrder.UP;
                        else if ("down".equals(n)) valOrder = Labeler.ValOrder.DOWN;
                        else if ("step".equals(n) || "enum".equals(n)) { /* our enumeration is step/enum */ }
                        else throw Errors.domain("labeling_option", m.resolve(opt), ind);
                    } else if (opt instanceof CompoundTerm
                            && ((CompoundTerm) opt).getArguments().size() == 1
                            && ("min".equals(((CompoundTerm) opt).getName())
                                || "max".equals(((CompoundTerm) opt).getName()))) {
                        objective = ((CompoundTerm) opt).getArguments().get(0);
                        objMin = "min".equals(((CompoundTerm) opt).getName());
                    } else if (opt instanceof Variable) {
                        throw Errors.instantiation(ind);
                    } else {
                        throw Errors.domain("labeling_option", m.resolve(opt), ind);
                    }
                }
            }
            Term listArg = m.deref(args[withOptions ? 1 : 0]);
            List<Term> vars = list(m, listArg);
            // START_CHANGE: ISS-2025-0510 - 4.3 wave D: label(a) / labeling(_, a) is
            // instantiation_error or type_error(list, a), the same contract labeling/2's OPTION
            // list already had; the variable list used to fail silently.
            if (vars == null) {
                throw (listArg instanceof Variable) ? Errors.instantiation(ind)
                                                    : Errors.type("list", m.resolve(listArg), ind);
            }
            // END_CHANGE: ISS-2025-0510
            for (int i = 0; i < vars.size(); i++) vars.set(i, m.deref(vars.get(i)));

            final List<Map<Variable, Long>> sols = ClpfdV2Bridge.labelCells(vars, varSel, valOrder);
            if (sols.isEmpty()) return Outcome.FAILURE;
            if (objective != null && sols.size() > 1) sortByObjective(m, sols, objective, objMin, ind);

            final int[] i = {0};
            Generator gen = new Generator() {
                @Override public boolean next(Machine mm) {
                    while (i[0] < sols.size()) {
                        Map<Variable, Long> sol = sols.get(i[0]++);
                        if (i[0] >= sols.size()) mm.lastSolution();
                        // One extent per attempt: a failed binding (the clpfd wake goal can reject
                        // it) must leave nothing behind — invariant 1's undo-then-close ordering.
                        Bindings b = mm.bindings();
                        int mark = b.mark();
                        b.forceTrail++;
                        boolean ok = true;
                        try {
                            for (Map.Entry<Variable, Long> e : sol.entrySet()) {
                                if (e.getKey().ref != null) continue;
                                if (!Unify.unify(e.getKey(), Number.valueOf(e.getValue().longValue()), b)) {
                                    ok = false;
                                    break;
                                }
                            }
                            if (!ok) b.undo(mark);
                        } finally {
                            b.forceTrail--;
                        }
                        if (ok) return true;
                    }
                    return false;
                }
            };
            return m.pushGenerator(gen) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }
    }

    /** {@code min(Expr)} / {@code max(Expr)}: optimum-first solution order, evaluated over the
     *  CELLS of each candidate assignment (no name-keyed substitution). */
    private static void sortByObjective(final Machine m, List<Map<Variable, Long>> sols,
                                        final Term objective, boolean objMin, String ind) {
        final java.util.IdentityHashMap<Map<Variable, Long>, Long> keys =
            new java.util.IdentityHashMap<Map<Variable, Long>, Long>();
        for (int i = 0; i < sols.size(); i++) {
            Long v = eval(objective, sols.get(i));
            if (v == null) throw Errors.instantiation(ind);
            keys.put(sols.get(i), v);
        }
        final int dir = objMin ? 1 : -1;
        java.util.Collections.sort(sols, new Comparator<Map<Variable, Long>>() {
            @Override public int compare(Map<Variable, Long> a, Map<Variable, Long> b) {
                return dir * Long.compare(keys.get(a).longValue(), keys.get(b).longValue());
            }
        });
    }

    /** Evaluate an objective ({@code +}, {@code -}, {@code *}, {@code abs} over integers and the
     *  labelled cells) under one assignment; null when it does not ground to an integer. */
    private static Long eval(Term t, Map<Variable, Long> sol) {
        Term r = Unify.deref(t);
        if (r instanceof Variable) return sol.get(r);
        if (r instanceof Number && ((Number) r).isInteger()) return Long.valueOf(((Number) r).longValue());
        if (r instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) r;
            List<Term> as = c.getArguments();
            if (as.size() == 1 && "abs".equals(c.getName())) {
                Long x = eval(as.get(0), sol);
                return (x == null) ? null : Long.valueOf(Math.abs(x.longValue()));
            }
            if (as.size() == 1 && "-".equals(c.getName())) {
                Long x = eval(as.get(0), sol);
                return (x == null) ? null : Long.valueOf(-x.longValue());
            }
            if (as.size() == 2) {
                Long x = eval(as.get(0), sol);
                Long y = eval(as.get(1), sol);
                if (x == null || y == null) return null;
                if ("+".equals(c.getName())) return Long.valueOf(x.longValue() + y.longValue());
                if ("-".equals(c.getName())) return Long.valueOf(x.longValue() - y.longValue());
                if ("*".equals(c.getName())) return Long.valueOf(x.longValue() * y.longValue());
            }
        }
        return null;
    }

    /** A proper list's elements, or null. */
    private static List<Term> list(Machine m, Term t) {
        List<Term> out = new ArrayList<Term>();
        Term cur = Unify.deref(t);
        int n = 0;
        while (cur instanceof CompoundTerm && ".".equals(((CompoundTerm) cur).getName())
                && ((CompoundTerm) cur).getArguments().size() == 2) {
            out.add(((CompoundTerm) cur).getArguments().get(0));
            cur = Unify.deref(((CompoundTerm) cur).getArguments().get(1));
            if ((++n & 0x3FF) == 0) m.guard().step();
        }
        return (cur instanceof Atom && "[]".equals(((Atom) cur).getName())) ? out : null;
    }
}
// END_CHANGE: ISS-2025-0471
