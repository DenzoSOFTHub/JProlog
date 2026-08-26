package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.ControlFlow;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0457 - engine v4 wave W4, design B.9: the coroutining wake queue.
// START_CHANGE: ISS-2025-0458 - the SWI attributed-variable protocol as v4 natives.
/**
 * Coroutining on the v4 machine: attributed variables, the wake queue, and the SWI
 * {@code attr_unify_hook} protocol.
 *
 * <h3>The wake queue (B.9)</h3>
 * Attributes live on the {@link Variable} cell. When {@link Unify#bindVar} binds an attributed
 * cell — to a value or to another variable — {@link #onBind} pushes one <b>wake goal</b> per
 * attribute module onto {@link Machine}'s queue, and the drive loop runs the queue <em>before the
 * next goal, in the current binding context</em>. A woken goal is therefore an ordinary goal: its
 * bindings are ordinary bindings that propagate, it is traced through the four ports, it is
 * charged to the {@code ResourceGuard}, and an exception it throws reaches the enclosing
 * {@code catch/3}. That is the whole fix for ISS-2025-0336 (a {@code when/2}-woken goal used to
 * bind names in a throw-away map, so only its side effects survived).
 *
 * <p>Every attribute change is <b>trailed</b> ({@link #putAttr} / {@link #delAttr}), and so is the
 * queue push itself, so backtracking past the binding restores the previous attribute map and
 * re-arms the suspended goals — and a head unification that binds an attributed cell and then
 * fails on a later argument leaves no stale wake behind.
 *
 * <h3>The hook protocol</h3>
 * The wake goal is the internal {@code '$attr_unify'(Module, AttValue, Other, VarName)}. It
 * dispatches to, in order:
 * <ol>
 *   <li>a user-defined {@code Module:attr_unify_hook(AttValue, Other)} clause, called through the
 *       normal goal stack (JProlog stores a module-qualified clause head as a {@code :/2}
 *       predicate, so the machine calls it with a flat lookup);</li>
 *   <li>otherwise the prelude's {@code '$attr_hook'/4}, which implements {@code freeze},
 *       {@code dif}, {@code when} and {@code clpfd} and ends in a catch-all clause, so an
 *       attribute of a module with no hook is inert data (the behaviour the legacy engines had for
 *       an unknown module).</li>
 * </ol>
 * {@code VarName} is the name of the cell that was bound; only the native {@code clpfd} hook uses
 * it (the CLP(FD) store is keyed by variable name), and it is invisible to the two-argument SWI
 * hook a user writes.
 *
 * <h3>No cross-query coroutining</h3>
 * Design decision 3 (B.17, approved): a query's variables die with the query. There is no
 * session-scoped attributed-variable splicing on the v4 route — see {@code Prolog.solveGuarded}.
 */
public final class Coroutining {

    private Coroutining() {}

    static final String FREEZE = "freeze";
    static final String DIF = "dif";
    static final String WHEN = "when";
    static final String CLPFD = it.denzosoft.jprolog.builtin.clpfd.v2.ClpfdV2Bridge.CLPFD_ATTR;

    private static final Atom ATTR_UNIFY = new Atom("$attr_unify");
    private static final Atom ATTR_HOOK = new Atom("$attr_hook");
    private static final Atom ATTR_UNIFY_HOOK = new Atom("attr_unify_hook");
    private static final Atom COLON = new Atom(":");
    private static final Atom EQ = new Atom("=");
    // ISS-2025-0486: the CLP(FD) wake goal and its cell wrapper.
    private static final Atom CLPFD_UNIFY = new Atom("$clpfd_unify_hook");
    private static final Atom ATTVAR_CELL = new Atom("$attvar_cell");

    static void register(BuiltinTable t) {
        t.register("$attr_unify", 4, new AttrUnify());
        t.register("$clpfd_unify_hook", 2, new ClpfdHook());
        t.register("put_attr", 3, new PutAttr());
        t.register("get_attr", 3, new GetAttr());
        t.register("del_attr", 2, new DelAttr());
        t.register("attvar", 1, new AttVar());
        t.register("term_attvars", 2, new TermAttvars());
        t.register("copy_term", 3, new CopyTerm3());
        t.register("unifiable", 3, new Unifiable());
    }

    // ------------------------------------------------------------------ the hook itself

    /**
     * {@link Unify.AttrHandler}: the cell {@code v} has just been bound to {@code value}. Queue one
     * wake goal per attribute module; never run anything here — we are in the middle of a term
     * walk, and a woken goal must run on the goal stack, in the machine's own order.
     */
    static boolean onBind(Machine m, Variable v, Term value, Bindings b) {
        Map<String, Term> attrs = v.getAttributes();
        if (attrs.isEmpty()) return true;
        int n = attrs.size();
        String[] mods = new String[n];
        Term[] vals = new Term[n];
        int i = 0;
        for (Map.Entry<String, Term> e : attrs.entrySet()) {
            mods[i] = e.getKey();
            vals[i] = e.getValue();
            i++;
        }
        Atom name = new Atom(v.getName());
        for (int k = 0; k < n; k++) {
            // START_CHANGE: ISS-2025-0486 - wave W9: CLP(FD) is a Java store, so its hook is native
            // and takes the CELL directly (wrapped so nothing dereferences it away: `v` is bound by
            // now). Every other module goes through the prelude's '$attr_hook'/4 dispatcher, which
            // is Prolog and can only be handed a name.
            if (CLPFD.equals(mods[k])) {
                m.wake(new CompoundTerm(CLPFD_UNIFY, Arrays.asList(
                    (Term) new CompoundTerm(ATTVAR_CELL, Arrays.asList((Term) v)), value)), b);
                continue;
            }
            // END_CHANGE: ISS-2025-0486
            m.wake(new CompoundTerm(ATTR_UNIFY,
                Arrays.asList((Term) new Atom(mods[k]), vals[k], value, (Term) name)), b);
        }
        return true;
    }

    /** Set an attribute, trailed: backtracking restores the previous value (or removes it). */
    static void putAttr(final Variable v, final String module, Term value, Bindings b) {
        final Term old = v.getAttribute(module);
        v.putAttribute(module, value);
        b.pushUndo(new Runnable() {
            @Override public void run() {
                if (old == null) v.removeAttribute(module); else v.putAttribute(module, old);
            }
        });
    }

    /** Remove an attribute, trailed. */
    static void delAttr(final Variable v, final String module, Bindings b) {
        final Term old = v.getAttribute(module);
        if (old == null) return;
        v.removeAttribute(module);
        b.pushUndo(new Runnable() {
            @Override public void run() { v.putAttribute(module, old); }
        });
    }

    // ------------------------------------------------------------------ residual goals (B.9 / W7)

    /**
     * The residual goals of every attributed variable reachable from {@code roots}: the frozen
     * goals, the {@code dif/2} constraints, the pending {@code when/2} conditions and the CLP(FD)
     * domains, in that order of discovery. {@code put_attr/3} is the fallback for a module with no
     * known rendering, so nothing is ever lost.
     *
     * <p>This is what an answer printer needs (design B.12, limit L-11); wave W7 wires it into the
     * CLI and the IDE. Nothing prints it yet.
     */
    public static List<Term> residualGoals(List<Term> roots) {
        List<Term> out = new ArrayList<Term>();
        List<Variable> vars = new ArrayList<Variable>();
        for (int i = 0; i < roots.size(); i++) Unify.termVariables(roots.get(i), vars, null);
        IdentityHashMap<Variable, Boolean> seen = new IdentityHashMap<Variable, Boolean>();
        for (int i = 0; i < vars.size(); i++) {
            Variable v = vars.get(i);
            if (seen.put(v, Boolean.TRUE) != null) continue;
            if (v.hasAttributes()) attributeGoals(v, out);
        }
        return out;
    }

    /** The residual goals of one attributed cell, appended to {@code out}. */
    static void attributeGoals(Variable v, List<Term> out) {
        for (Map.Entry<String, Term> e : v.getAttributes().entrySet()) {
            String mod = e.getKey();
            Term val = e.getValue();
            if (FREEZE.equals(mod)) {
                out.add(new CompoundTerm(new Atom("freeze"), Arrays.asList((Term) v, val)));
            } else if (DIF.equals(mod)) {
                for (Term c : listElements(val)) {
                    if (c instanceof CompoundTerm && "$dif".equals(((CompoundTerm) c).getName())
                            && ((CompoundTerm) c).getArguments().size() == 2) {
                        out.add(new CompoundTerm(new Atom("dif"), ((CompoundTerm) c).getArguments()));
                    }
                }
            } else if (WHEN.equals(mod)) {
                for (Term c : listElements(val)) {
                    if (c instanceof CompoundTerm && "$when".equals(((CompoundTerm) c).getName())
                            && ((CompoundTerm) c).getArguments().size() == 3) {
                        List<Term> as = ((CompoundTerm) c).getArguments();
                        if (!(Unify.deref(as.get(0)) instanceof Variable)) continue;   // already fired
                        out.add(new CompoundTerm(new Atom("when"), Arrays.asList(as.get(1), as.get(2))));
                    }
                }
            } else if (CLPFD.equals(mod)) {
                Term dom = null;
                try {
                    dom = it.denzosoft.jprolog.builtin.clpfd.v2.ClpfdV2Bridge.domainTermForCell(v);
                } catch (RuntimeException ex) {
                    ControlFlow.rethrowIfControl(ex);
                }
                if (dom != null) out.add(new CompoundTerm(new Atom("in"), Arrays.asList((Term) v, dom)));
            } else {
                out.add(new CompoundTerm(new Atom("put_attr"),
                    Arrays.asList((Term) v, (Term) new Atom(mod), val)));
            }
        }
    }

    private static List<Term> listElements(Term t) {
        List<Term> out = new ArrayList<Term>();
        Term cur = Unify.deref(t);
        int n = 0;
        while (cur instanceof CompoundTerm && ".".equals(((CompoundTerm) cur).getName())
                && ((CompoundTerm) cur).getArguments().size() == 2 && ++n < 1000000) {
            out.add(Unify.deref(((CompoundTerm) cur).getArguments().get(0)));
            cur = Unify.deref(((CompoundTerm) cur).getArguments().get(1));
        }
        if (out.isEmpty() && !(cur instanceof Atom && "[]".equals(((Atom) cur).getName()))) out.add(cur);
        return out;
    }

    // ------------------------------------------------------------------ natives

    /** {@code '$attr_unify'(Module, AttValue, Other, VarName)} — the wake goal (see the header). */
    private static final class AttrUnify implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Term mod = m.deref(args[0]);
            if (mod instanceof Atom && m.hasQualifiedHook(((Atom) mod).getName())) {
                Term hook = new CompoundTerm(ATTR_UNIFY_HOOK, Arrays.asList(args[1], args[2]));
                Term q = new CompoundTerm(COLON, Arrays.asList(mod, hook));
                return m.callQualified(q) ? Outcome.SUSPENDED : Outcome.FAILURE;
            }
            m.pushGoal(new CompoundTerm(ATTR_HOOK, Arrays.asList(args[0], args[1], args[2], args[3])));
            return Outcome.SUSPENDED;
        }
    }

    // START_CHANGE: ISS-2025-0460 - CLP(FD) on the v4 attribute hook.
    /**
     * {@code '$clpfd_unify_hook'('$attvar_cell'(Cell), Other)} — the CLP(FD) v2 store is a Java
     * constraint store, so its hook is native rather than Prolog, and since wave W9
     * (ISS-2025-0486) it receives the attributed CELL rather than its name: the machine has the
     * cell when it queues the wake goal, and the wrapper keeps anything from dereferencing it away
     * (by wake time the cell is bound). That removes the last name-to-cell lookup in the engine.
     */
    private static final class ClpfdHook implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Term w = m.deref(args[0]);
            if (!(w instanceof CompoundTerm)
                    || !"$attvar_cell".equals(((CompoundTerm) w).getName())
                    || ((CompoundTerm) w).getArguments().size() != 1) {
                return Outcome.SUCCESS;
            }
            Term cell = ((CompoundTerm) w).getArguments().get(0);   // raw: NOT dereferenced
            if (!(cell instanceof Variable)) return Outcome.SUCCESS;
            Term other = m.deref(args[1]);
            return it.denzosoft.jprolog.builtin.clpfd.v2.ClpfdV2Bridge
                .onBindCell((Variable) cell, other) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }
    // END_CHANGE: ISS-2025-0460

    /** {@code put_attr(-Var, +Module, +Value)}. */
    private static final class PutAttr implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Term v = m.deref(args[0]);
            if (!(v instanceof Variable)) throw Errors.type("variable", m.resolve(v), "put_attr/3");
            Term mod = m.deref(args[1]);
            if (mod instanceof Variable) throw Errors.instantiation("put_attr/3");
            if (!(mod instanceof Atom)) throw Errors.type("atom", mod, "put_attr/3");
            putAttr((Variable) v, ((Atom) mod).getName(), m.resolve(args[2]), m.bindings());
            return Outcome.SUCCESS;
        }
    }

    /** {@code get_attr(+Var, +Module, ?Value)}. */
    private static final class GetAttr implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Term v = m.deref(args[0]);
            if (!(v instanceof Variable)) return Outcome.FAILURE;
            Term mod = m.deref(args[1]);
            if (mod instanceof Variable) throw Errors.instantiation("get_attr/3");
            if (!(mod instanceof Atom)) throw Errors.type("atom", mod, "get_attr/3");
            Term value = ((Variable) v).getAttribute(((Atom) mod).getName());
            if (value == null) return Outcome.FAILURE;
            return m.unify(args[2], value) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    /** {@code del_attr(+Var, +Module)} — succeeds even when there is no such attribute (SWI). */
    private static final class DelAttr implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Term v = m.deref(args[0]);
            if (!(v instanceof Variable)) return Outcome.SUCCESS;
            Term mod = m.deref(args[1]);
            if (mod instanceof Variable) throw Errors.instantiation("del_attr/2");
            if (!(mod instanceof Atom)) throw Errors.type("atom", mod, "del_attr/2");
            delAttr((Variable) v, ((Atom) mod).getName(), m.bindings());
            return Outcome.SUCCESS;
        }
    }

    /** {@code attvar(@Term)}. */
    private static final class AttVar implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Term v = m.deref(args[0]);
            return (v instanceof Variable && ((Variable) v).hasAttributes())
                ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    /** {@code term_attvars(+Term, -AttVars)}. */
    private static final class TermAttvars implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            List<Variable> vars = new ArrayList<Variable>();
            Unify.termVariables(args[0], vars, m.guard());
            List<Term> out = new ArrayList<Term>();
            for (int i = 0; i < vars.size(); i++) {
                if (vars.get(i).hasAttributes()) out.add(vars.get(i));
            }
            return m.unify(args[1], Machine.makeList(out)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    /**
     * {@code copy_term(+Term, -Copy, -Goals)}: the copy carries NO attributes; {@code Goals} is the
     * list of residual goals that would restore them, expressed over the copy's variables (they are
     * copied through the same variable map, so a frozen goal in {@code Goals} talks about
     * {@code Copy}'s cells and not the original's).
     */
    private static final class CopyTerm3 implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            IdentityHashMap<Variable, Variable> map = new IdentityHashMap<Variable, Variable>();
            Term copy = Unify.copy(args[0], map, m.guard());
            List<Variable> vars = new ArrayList<Variable>();
            Unify.termVariables(args[0], vars, m.guard());
            List<Term> goals = new ArrayList<Term>();
            for (int i = 0; i < vars.size(); i++) {
                if (vars.get(i).hasAttributes()) attributeGoals(vars.get(i), goals);
            }
            Term goalList = Unify.copy(Machine.makeList(goals), map, m.guard());
            if (!m.unify(args[1], copy)) return Outcome.FAILURE;
            return m.unify(args[2], goalList) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    /**
     * {@code unifiable(@X, @Y, -Unifier)}: the bindings {@code X = Y} would make, as a list of
     * {@code Var = Value}, without making them. This is what {@code dif/2} suspends on — the
     * <em>remaining unifier variables</em> of a partially instantiated pair, rather than every
     * variable of both terms.
     */
    private static final class Unifiable implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Bindings b = m.bindings();
            int mark = b.mark();
            b.forceTrail++;
            List<Term> eqs = null;
            boolean ok;
            try {
                ok = Unify.unify(args[0], args[1], b);
                if (ok) {
                    List<Variable> bound = b.boundSince(mark);
                    eqs = new ArrayList<Term>(bound.size());
                    for (int i = bound.size() - 1; i >= 0; i--) {
                        Variable v = bound.get(i);
                        if (v.ref == null) continue;
                        eqs.add(new CompoundTerm(EQ, Arrays.asList((Term) v, m.resolve(v.ref))));
                    }
                }
            } finally {
                // ISS-2025-0448 ordering: undo INSIDE the forced-trail extent, then close it.
                b.undo(mark);
                b.forceTrail--;
            }
            if (!ok) return Outcome.FAILURE;
            return m.unify(args[2], Machine.makeList(eqs)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }
}
// END_CHANGE: ISS-2025-0458
// END_CHANGE: ISS-2025-0457
