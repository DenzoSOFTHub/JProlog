package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.DebugEvent;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.Arrays;

/**
 * 4.6 wave Q6.2 (ISS-2025-0780): library(apply)'s {@code maplist/2..7}, {@code foldl/4..7},
 * {@code include/3}, {@code exclude/3} and {@code partition/4,5} as NATIVE FRAMES.
 *
 * <p>The predicates stay defined by {@code prelude/apply.pl} — resolution, autoloading, the
 * meta-argument qualification and a user's own definition of the same name are untouched — but
 * the first clause of each carries an {@link Op} ({@link #tag}), and when the machine activates
 * that two-clause window it runs {@link #enter} instead of the clauses. {@code enter} reproduces
 * exactly what the two clauses do:
 * <ul>
 *   <li>the same four ports: a Call port per level (the recursive call's goal), the closure's own
 *       ports one level deeper, and the Exit ports of the levels in reverse order;</li>
 *   <li>the same choice points: the base clause ({@code []}) and the recursive clause
 *       ({@code [X|Xs]}) are the two alternatives, and an alternative whose list arguments
 *       clash (the same per-argument look-ahead the clause frame does, ISS-2025-0715) is never a
 *       choice point — so a proper list is deterministic and an open list enumerates lengths, as
 *       before. A traced level keeps its frame until its Exit port, as a traced clause frame
 *       does;</li>
 *   <li>the closure runs as a goal on the goal stack (never in a nested drive), so its choice
 *       points, cuts (local, as for call/N), exceptions and coroutines are the ordinary ones; the
 *       recursion is a {@link Machine.Step} pushed after it.</li>
 * </ul>
 * What it saves is everything around the closure: clause selection, head unification against a
 * skeleton, the meta-argument re-qualification of every recursive call, the {@code call/N} goal
 * and its {@code '$mctx'} unwrapping step.
 */
// START_CHANGE: ISS-2025-0780 - 4.6 wave Q6.2
final class NativeApply {

    private NativeApply() {}

    static final int MAPLIST = 0, FOLDL = 1, INCLUDE = 2, EXCLUDE = 3, PARTITION4 = 4, PARTITION5 = 5;

    private static final Atom NIL = new Atom("[]");
    private static final Atom DOT = new Atom(".");
    private static final Atom CALL = new Atom("call");
    private static final Atom ITE = new Atom("->");
    private static final Atom OR = new Atom(";");
    private static final Atom AND = new Atom(",");
    private static final Atom EQ = new Atom("=");
    private static final Atom PORDER = new Atom("$partition_order");

    /** One iteration predicate: its kind, name, arity and number of list arguments. */
    static final class Op {
        final int kind;
        final Atom name;
        final int arity;
        final int lists;
        Op(int kind, String name, int arity, int lists) {
            this.kind = kind;
            this.name = new Atom(name);
            this.arity = arity;
            this.lists = lists;
        }
    }

    /** Called when library(apply) is loaded: mark the first clause of each iteration predicate. */
    static void tag(String key, Clause[] cs) {
        if (cs.length != 2) return;
        int slash = key.lastIndexOf('/');
        if (slash < 0) return;
        String name = key.substring(0, slash);
        int n;
        try { n = Integer.parseInt(key.substring(slash + 1)); } catch (NumberFormatException e) { return; }
        Op op = null;
        if ("maplist".equals(name) && n >= 2 && n <= 7) op = new Op(MAPLIST, name, n, n - 1);
        else if ("foldl".equals(name) && n >= 4 && n <= 7) op = new Op(FOLDL, name, n, n - 3);
        else if ("include".equals(name) && n == 3) op = new Op(INCLUDE, name, n, 1);
        else if ("exclude".equals(name) && n == 3) op = new Op(EXCLUDE, name, n, 1);
        else if ("partition".equals(name) && n == 4) op = new Op(PARTITION4, name, n, 1);
        else if ("partition".equals(name) && n == 5) op = new Op(PARTITION5, name, n, 1);
        if (op != null) cs[0].apply = op;
    }

    // ------------------------------------------------------------------ head tests

    private static boolean isCons(Term t) {
        return t instanceof CompoundTerm && ((CompoundTerm) t).arity() == 2
            && ".".equals(((CompoundTerm) t).getName());
    }

    /** Could the argument (dereferenced) unify with {@code []}? (no principal-functor clash) */
    private static boolean mayBeNil(Term t) {
        return t instanceof Variable || (t instanceof Atom && "[]".equals(((Atom) t).getName()));
    }

    /** Could it unify with {@code [_|_]}? */
    private static boolean mayBeCons(Term t) {
        return t instanceof Variable || isCons(t);
    }

    /** The arguments that are {@code []} in the base clause's head. */
    private static int baseNilArgs(Op op) {
        switch (op.kind) {
            case MAPLIST: case FOLDL: return op.lists;          // the lists (foldl: V0, V are A, A)
            case INCLUDE: case EXCLUDE: return 2;               // include(_, [], [])
            case PARTITION4: return 3;                          // partition(_, [], [], [])
            default: return 4;                                  // partition(_, [], [], [], [])
        }
    }

    /** Can alternative {@code alt} (0 = base clause, 1 = recursive clause) match {@code a}? */
    private static boolean possible(Op op, Term[] a, int alt) {
        if (alt == 0) {
            int k = baseNilArgs(op);
            for (int i = 1; i <= k; i++) if (!mayBeNil(Unify.deref(a[i]))) return false;
            return true;
        }
        for (int i = 1; i <= op.lists; i++) if (!mayBeCons(Unify.deref(a[i]))) return false;
        return true;
    }

    /** Head unification of the base clause. */
    private static boolean unifyBase(Machine m, Op op, Term[] a) {
        int k = baseNilArgs(op);
        for (int i = 1; i <= k; i++) if (!m.unify(a[i], NIL)) return false;
        if (op.kind == FOLDL) return m.unify(a[a.length - 1], a[a.length - 2]);   // foldl(_, [], A, A)
        return true;
    }

    /**
     * Head unification of the recursive clause: every list argument becomes {@code [X|Xs]}.
     * {@code heads}/{@code tails} receive the element and the rest; false when it cannot unify.
     */
    private static boolean unifyRec(Machine m, Op op, Term[] a, Term[] heads, Term[] tails) {
        for (int i = 0; i < op.lists; i++) {
            Term l = Unify.deref(a[i + 1]);
            if (isCons(l)) {
                heads[i] = ((CompoundTerm) l).arg(0);
                tails[i] = ((CompoundTerm) l).arg(1);
            } else if (l instanceof Variable) {
                Variable x = new Variable(), t = new Variable();
                heads[i] = x;
                tails[i] = t;
                if (!m.unify(l, new CompoundTerm(DOT, new Term[] {x, t}))) return false;
            } else {
                return false;
            }
        }
        return true;
    }

    // ------------------------------------------------------------------ the frame

    /**
     * Activate one level: {@code a} are the goal's arguments (the closure first), {@code traceGoal}
     * the goal the ports report (built on demand), {@code defMod} the module the clauses would
     * run in. Mirrors {@code Machine.activate} for a two-clause window.
     */
    static boolean enter(Machine m, Op op, Term goal, Term traceGoal, String defMod) {
        CompoundTerm gc = (CompoundTerm) goal;
        Term[] a = new Term[op.arity];
        for (int i = 0; i < a.length; i++) a[i] = gc.arg(i);
        return enter(m, op, a, traceGoal, defMod);
    }

    static boolean enter(final Machine m, final Op op, final Term[] a, Term traceGoal, final String defMod) {
        if (it.denzosoft.jprolog.core.engine.Profiler.isEnabled()) {
            it.denzosoft.jprolog.core.engine.Profiler.recordCall(op.name.getName(), op.arity);
        }
        final boolean tracing = it.denzosoft.jprolog.builtin.debug.Trace.isTracingEnabled();
        final boolean debugging = m.debugPortsActive();
        m.applyLevels++;
        final boolean p0 = possible(op, a, 0), p1 = possible(op, a, 1);

        if (!tracing && !debugging && !(p0 && p1)) {
            // one alternative at most: no choice point (the frame would be trust-me popped)
            if (p0) return unifyBase(m, op, a);
            if (!p1) return false;
            return recBody(m, op, a, m.cpHeight(), defMod, true, null);
        }

        final int tdepth = (tracing || debugging) ? m.enterPort() : 0;
        final Term g = (tracing || debugging)
            ? (traceGoal != null ? traceGoal : new CompoundTerm(op.name, a.clone())) : null;
        if (tracing || debugging) m.noteOpen(g, tdepth);
        if (tracing) m.tracePort("Call", g, tdepth);
        if (debugging) m.debugPort(DebugEvent.Port.CALL, g, tdepth);

        final Machine.Goal cont = m.goalStack;
        final int barrier = m.cpHeight();
        Machine.CP cp = new Machine.CP(Machine.CP.CLAUSES, m.bindings().mark());
        cp.goal = g;
        cp.cont = cont;
        cp.barrier = barrier;
        cp.gen = new Machine.Gen() {
            int alt = 0;
            @Override public Machine.Goal next(final Machine.CP self) {
                while (alt < 2) {
                    int k = alt++;
                    if (k == 0 ? !p0 : !possible(op, a, 1)) continue;
                    if (k == 1 || !p1) { self.genExhausted = true; alt = 2; }
                    Machine.Goal after = self.cont;
                    if (tracing || debugging) {
                        after = new Machine.Goal(new Runnable() {
                            @Override public void run() {
                                m.setPortDepth(tdepth);
                                if (tracing) m.tracePort("Exit", g, tdepth);
                                if (debugging) m.debugPort(DebugEvent.Port.EXIT, g, tdepth);
                                m.popIfDeterministicTop(self);
                            }
                        }, self.cont);
                    }
                    if (k == 0) {
                        if (!unifyBase(m, op, a)) return Machine.FAILED;
                        return after;
                    }
                    Machine.Goal saved = m.goalStack;
                    m.goalStack = after;
                    boolean ok = recBody(m, op, a, self.barrier, defMod, false, g);
                    Machine.Goal body = m.goalStack;
                    m.goalStack = saved;
                    return ok ? body : Machine.FAILED;
                }
                return Machine.EXHAUSTED;
            }
        };
        if (tracing || debugging) { cp.traceGoal = g; cp.traceDepth = tdepth; cp.traceDebug = debugging; }
        m.pushCP(cp);
        if (m.advance(cp)) return true;
        m.popCP();
        if (tracing || debugging) m.setPortDepth(tdepth);
        if (tracing) m.tracePort("Fail", g, tdepth);
        if (debugging) m.debugPort(DebugEvent.Port.FAIL, g, tdepth);
        return false;
    }

    /**
     * Unify the recursive clause's head and push its body in front of {@code m.goalStack}:
     * the per-element goal(s), then the recursion (a Step). {@code direct}: the closure is pushed
     * as the goal call/N would push (its cut barrier is the current height, which is right
     * because nothing runs in between); otherwise it is pushed as the literal {@code call/N}
     * term, which takes its barrier when it is reached, exactly like the clause body.
     */
    private static boolean recBody(Machine m, Op op, Term[] a, int barrier, String defMod,
                                   boolean direct, Term traceGoal) {
        int nl = op.lists;
        Term[] heads = new Term[nl], tails = new Term[nl];
        if (!unifyRec(m, op, a, heads, tails)) return false;
        Term g = a[0];
        Term[] next = new Term[op.arity];
        next[0] = g;
        for (int i = 0; i < nl; i++) next[i + 1] = tails[i];
        switch (op.kind) {
            case MAPLIST: {
                m.goalStack = new Machine.Goal(new Level(op, next, defMod), m.goalStack);
                pushClosure(m, g, heads, barrier, defMod, direct);
                return true;
            }
            case FOLDL: {
                Variable v1 = new Variable();
                next[nl + 1] = v1;
                next[nl + 2] = a[nl + 2];
                Term[] extra = Arrays.copyOf(heads, nl + 2);
                extra[nl] = a[nl + 1];
                extra[nl + 1] = v1;
                m.goalStack = new Machine.Goal(new Level(op, next, defMod), m.goalStack);
                pushClosure(m, g, extra, barrier, defMod, direct);
                return true;
            }
            case INCLUDE:
            case EXCLUDE: {
                Variable ys1 = new Variable();
                next[2] = ys1;
                Term x = heads[0], ys = a[2];
                Term keep = new CompoundTerm(EQ, new Term[] {ys, cons(x, ys1)});
                Term drop = new CompoundTerm(EQ, new Term[] {ys, ys1});
                String[] cmod = {defMod};
                Term cond = closure(m, g, new Term[] {x}, cmod);
                Term body = ite(cond, op.kind == INCLUDE ? keep : drop, op.kind == INCLUDE ? drop : keep);
                m.goalStack = new Machine.Goal(new Level(op, next, defMod), m.goalStack);
                m.goalStack = Machine.mg(body, barrier, m.goalStack, cmod[0]);
                return true;
            }
            case PARTITION4: {
                Variable i1 = new Variable(), e1 = new Variable();
                next[2] = i1;
                next[3] = e1;
                Term x = heads[0], incl = a[2], excl = a[3];
                Term yes = new CompoundTerm(AND, new Term[] {
                    new CompoundTerm(EQ, new Term[] {incl, cons(x, i1)}),
                    new CompoundTerm(EQ, new Term[] {excl, e1})});
                Term no = new CompoundTerm(AND, new Term[] {
                    new CompoundTerm(EQ, new Term[] {incl, i1}),
                    new CompoundTerm(EQ, new Term[] {excl, cons(x, e1)})});
                String[] cmod = {defMod};
                Term body = ite(closure(m, g, new Term[] {x}, cmod), yes, no);
                m.goalStack = new Machine.Goal(new Level(op, next, defMod), m.goalStack);
                m.goalStack = Machine.mg(body, barrier, m.goalStack, cmod[0]);
                return true;
            }
            default: {                                       // PARTITION5
                Variable l1 = new Variable(), e1 = new Variable(), g1 = new Variable(), order = new Variable();
                next[2] = l1;
                next[3] = e1;
                next[4] = g1;
                Term x = heads[0];
                Term po = new CompoundTerm(PORDER, new Term[] {order, x, a[2], a[3], a[4], l1, e1, g1});
                m.goalStack = new Machine.Goal(new Level(op, next, defMod), m.goalStack);
                m.goalStack = Machine.mg(po, barrier, m.goalStack, defMod);
                pushClosure(m, g, new Term[] {x, order}, barrier, defMod, direct);
                return true;
            }
        }
    }

    private static Term cons(Term h, Term t) { return new CompoundTerm(DOT, new Term[] {h, t}); }

    private static Term ite(Term c, Term t, Term e) {
        return new CompoundTerm(OR, new Term[] {new CompoundTerm(ITE, new Term[] {c, t}), e});
    }

    /** Push {@code call(G, Extra...)} — directly as the goal call/N builds, or as the call/N term. */
    private static void pushClosure(Machine m, Term g, Term[] extra, int barrier, String defMod, boolean direct) {
        if (!direct) {
            Term[] ca = new Term[extra.length + 1];
            ca[0] = g;
            System.arraycopy(extra, 0, ca, 1, extra.length);
            m.goalStack = Machine.mg(new CompoundTerm(CALL, ca), barrier, m.goalStack, defMod);
            return;
        }
        String[] mod = {defMod};
        Term goal = closure(m, g, extra, mod);
        m.goalStack = Machine.mg(goal, m.cpHeight(), m.goalStack, mod[0]);
    }

    /**
     * The goal {@code call(G, Extra...)} runs, and (in {@code mod[0]}) the context it runs in —
     * the work call/N does when it is reached, done here. {@code call('$mctx'(M, G), X...)} is
     * {@code '$mctx'(M, G(X...))}, which runs {@code G(X...)} in M: that next step is taken here
     * too (a yall lambda inside is then expanded by the machine, as it would be after that step).
     */
    private static Term closure(Machine m, Term g, Term[] extra, String[] mod) {
        Term callee = Unify.deref(g);
        if (callee instanceof CompoundTerm && ((CompoundTerm) callee).arity() == 2
                && Modules.MCTX.equals(((CompoundTerm) callee).getName())
                && Unify.deref(((CompoundTerm) callee).arg(0)) instanceof Atom) {
            mod[0] = Machine.modKey(((Atom) Unify.deref(((CompoundTerm) callee).arg(0))).getName());
            return addArgs(m, Unify.deref(((CompoundTerm) callee).arg(1)), extra);
        }
        Term goal;
        if (Lambdas.isLambda(callee)) {
            goal = Lambdas.expand(m, callee, Arrays.asList(extra));
            if (goal == null) goal = addArgs(m, callee, extra);
        } else {
            goal = addArgs(m, callee, extra);
        }
        m.checkBody(goal, "call", extra.length + 1);
        return goal;
    }

    private static Term addArgs(Machine m, Term callee, Term[] extra) {
        return m.addArgs(callee, new CompoundTerm(CALL, extra), 0, extra.length);
    }

    /** The recursive call: the next level, run when the goal stack reaches it. */
    private static final class Level extends Machine.Step {
        final Op op;
        final Term[] args;
        final String defMod;
        Level(Op op, Term[] args, String defMod) { this.op = op; this.args = args; this.defMod = defMod; }
        @Override boolean step(Machine m) {
            return enter(m, op, args, null, defMod);
        }
    }
}
// END_CHANGE: ISS-2025-0780
