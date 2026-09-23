package it.denzosoft.jprolog.builtin.clpfd.v2;

import it.denzosoft.jprolog.builtin.clpfd.v2.ClpStore.FdVar;
import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Bridges the engine's term/binding world to the clean-room {@link ClpStore}. A per-query
 * context (a {@link ClpStore} plus a name→{@link FdVar} map) is held in a {@link ThreadLocal}
 * and reset at the start of every top-level query, so constraints never leak across queries —
 * the defect of the legacy global singleton store. Linear arithmetic (`X + 2*Y - 3`) is compiled
 * into the v2 {@link Constraint.Linear} form; comparisons map onto `=`/`=<`/`>=`/`<`/`>`/`\=`.
 */
public final class ClpfdV2Bridge {

    private static final class Ctx {
        final ClpStore store = new ClpStore();
        final Map<String, FdVar> vars = new HashMap<>();
        // START_CHANGE: ISS-2025-0460 - engine v4 wave W4: the ENGINE CELL behind each FD variable.
        // On v4 a variable is an object, so a solution map keyed by name (exportSingletons/1 reports
        // functionally-determined variables the goal never mentions) needs a way back to the cell.
        // The bridge owns that mapping because the bridge is what attributed the cell in the first
        // place; it replaces Machine.nameIndex, the engine-wide shim of waves W1-W3, and it is reset
        // with the rest of the context at every top-level query.
        final Map<String, Variable> cells = new HashMap<>();
        // START_CHANGE: ISS-2025-0642 - the same (cell, FdVar) registrations as two parallel lists
        // in registration order, so determinedCells() is an array scan instead of a hash-map walk
        // per labeling node. Truncated by the same (LIFO) undo actions that unregister the names.
        final ArrayList<Variable> cellList = new ArrayList<>();
        final ArrayList<FdVar> fdList = new ArrayList<>();
        void register(Variable cell, FdVar fv) { cellList.add(cell); fdList.add(fv); }
        void truncate(int n) {
            while (cellList.size() > n) { cellList.remove(cellList.size() - 1); fdList.remove(fdList.size() - 1); }
        }
        // END_CHANGE: ISS-2025-0642
        // END_CHANGE: ISS-2025-0460
        // START_CHANGE: ISS-2025-0358 - counter for auxiliary difference variables
        int aux = 0;
        // END_CHANGE: ISS-2025-0358
    }

    // START_CHANGE: ISS-2025-0355 - attribute marking an engine variable as FD-constrained, so the
    // engines' attribute-unify hooks fire when it is bound (unification must respect the domain).
    public static final String CLPFD_ATTR = "clpfd";
    private static final Term FD_MARKER = new Atom("true");
    // END_CHANGE: ISS-2025-0355

    private static final ThreadLocal<Ctx> CTX = new ThreadLocal<Ctx>() {
        @Override protected Ctx initialValue() { return new Ctx(); }
    };

    private ClpfdV2Bridge() {}

    // START_CHANGE: ISS-2025-0500 - 4.2 wave C: the bridge no longer reaches into
    // core.engine.v4.Undo. It declares WHERE a backtrackable side effect goes and the engine
    // installs the destination (ClpfdNative.register -> the running Machine's trail), which is what
    // lets core.engine.v4.Undo stop being public API: the trail is the machine's business, and the
    // one client outside the engine package now depends on an interface it owns itself.
    //
    // With no engine installed — a directly-instantiated store in a unit test, a bridge call with
    // no machine running on the thread — the sink is a no-op, exactly what Undo.record did when
    // there was no current machine: nothing can backtrack over the mutation, so it is permanent.
    /** Where a backtrackable CLP(FD) side effect is recorded; installed by the v4 engine. */
    public interface UndoSink { void record(Runnable undo); }

    private static final UndoSink NO_TRAIL = new UndoSink() { public void record(Runnable undo) {} };

    private static volatile UndoSink undoSink = NO_TRAIL;

    /** Called once per engine by {@code core.engine.v4.ClpfdNative}. */
    public static void setUndoSink(UndoSink sink) { undoSink = (sink == null) ? NO_TRAIL : sink; }

    private static void recordUndo(Runnable undo) {
        if (undo != null) undoSink.record(undo);
    }
    // END_CHANGE: ISS-2025-0500

    /** Reset the per-query CLP state (call at the start of each top-level solve). */
    public static void reset() { CTX.set(new Ctx()); }

    private static Ctx ctx() { return CTX.get(); }

    /** Get (or create with a wide default domain) the FdVar for an engine variable. */
    private static FdVar varFor(Variable v) {
        Ctx c = ctx();
        FdVar fv = c.vars.get(v.getName());
        if (fv == null) {
            // START_CHANGE: ISS-2025-0644 - SWI: an unconstrained CLP(FD) variable is inf..sup (it used
            // to be -10^8..10^8, which made X #> Y, Y #> X grind through 2*10^8 propagation rounds)
            fv = c.store.newVar(v.getName(), IntervalDomain.ALL);
            // END_CHANGE: ISS-2025-0644
            c.vars.put(v.getName(), fv);
            c.cells.put(v.getName(), v);                 // ISS-2025-0460
            final int reg = c.cellList.size();           // ISS-2025-0642
            c.register(v, fv);
            // START_CHANGE: ISS-2025-0355 - mark the engine variable as FD-constrained so binding it
            // fires onBind/onAlias; the registration is trailed, so backtracking out of the goal that
            // created the FdVar restores plain-variable semantics.
            v.putAttribute(CLPFD_ATTR, FD_MARKER);
            final Ctx fc = c; final String name = v.getName(); final Variable fvv = v;
            recordUndo(() -> {
                fc.vars.remove(name);
                fc.cells.remove(name);                   // ISS-2025-0460
                fc.truncate(reg);                        // ISS-2025-0642
                fvv.removeAttribute(CLPFD_ATTR);
            });
            // END_CHANGE: ISS-2025-0355
        }
        return fv;
    }

    // START_CHANGE: ISS-2025-0356 - posted constraints must be undone when the engine backtracks past
    // the posting goal. Every store mutation is bracketed: snapshot (domain mark + constraint count)
    // before, self-undo immediately on failure (a failed post leaves no narrowing behind), and on
    // success register a rollback with core.engine.v4.Undo — the machine runs it from its own
    // trail when it backtracks past the posting goal, so abandoning a branch retracts its posts
    // (ISS-2025-0492; it used to be a second, parallel trail marked by CP.legacyMark).
    private static boolean guardedPost(java.util.function.BooleanSupplier post) {
        final ClpStore store = ctx().store;
        final int dm = store.mark();
        final int cm = store.constraintMark();
        boolean ok = false;
        try {
            ok = post.getAsBoolean();
        } finally {
            if (!ok) store.rollbackTo(dm, cm);            // failed (or threw): leave no trace
        }
        if (!ok) return false;
        recordUndo(() -> store.rollbackTo(dm, cm));
        return true;
    }
    // END_CHANGE: ISS-2025-0356

    // START_CHANGE: ISS-2025-0355 - unification hooks: narrow the domain when an FD variable is bound
    /** Engine hook: the FD-constrained variable {@code v} is being bound to {@code value} by
     *  unification. An integer inside the domain narrows it to the singleton (and propagates);
     *  anything else — an integer outside the domain or a non-integer term — fails the unification.
     *  Variables unknown to the store are not FD-constrained: succeed. */
    public static boolean onBind(Variable v, Term value) {
        final FdVar fv = ctx().vars.get(v.getName());
        if (fv == null) return true;                      // not (or no longer) FD-constrained
        if (value instanceof Variable) return onAlias(v, (Variable) value);
        if (!(value instanceof Number) || !((Number) value).isInteger()) return false;
        if (IntervalDomain.isInfinite(Constraint.clamp(((Number) value).bigIntegerValue()))) return bindBig(fv, (Number) value);   // ISS-2025-0644
        final long val = ((Number) value).longValue();
        return guardedPost(() -> ctx().store.narrow(fv, IntervalDomain.singleton(val))
                              && ctx().store.propagate());
    }

    /** Engine hook: the variable {@code bound} was aliased to the variable {@code to} by var-var
     *  unification. If only {@code bound} is FD-constrained, {@code to} inherits its FdVar (trailed);
     *  if both are, an equality constraint intersects the domains and keeps them synced. */
    public static boolean onAlias(Variable bound, Variable to) {
        final Ctx c = ctx();
        final FdVar fa = c.vars.get(bound.getName());
        if (fa == null) return true;                      // the bound variable is not FD-constrained
        final FdVar fb = c.vars.get(to.getName());
        if (fb == null) {
            c.vars.put(to.getName(), fa);
            c.cells.put(to.getName(), to);               // ISS-2025-0460
            final int reg = c.cellList.size();           // ISS-2025-0642
            c.register(to, fa);
            to.putAttribute(CLPFD_ATTR, FD_MARKER);
            final String name = to.getName(); final Variable tv = to;
            recordUndo(() -> {
                c.vars.remove(name);
                c.cells.remove(name);                    // ISS-2025-0460
                c.truncate(reg);                         // ISS-2025-0642
                tv.removeAttribute(CLPFD_ATTR);
            });
            return true;
        }
        if (fa == fb) return true;
        return guardedPost(() -> c.store.addConstraint(new Constraint.Cmp(fa, Constraint.Rel.EQ, fb)));
    }
    // END_CHANGE: ISS-2025-0355

    // START_CHANGE: ISS-2025-0460 - engine v4 wave W4 (design B.9): the bridge is an ordinary
    // attributed-cell client of the v4 machine. The attribute on the cell is what makes the machine
    // wake '$clpfd_unify_hook'(VarName, Other) when the cell is bound; these three entry points are
    // what that hook, and the v4 solution installer, call.
    // START_CHANGE: ISS-2025-0486 - wave W9: the v4 hook takes the CELL, not a name. `cellFor(String)`
    // and `onBindByName(String, Term)` are deleted with the last name-keyed hop in the engine: the
    // machine already has the attributed cell when it queues the wake goal, so it hands it over.
    /** v4 attribute hook: the FD-constrained cell {@code self} was bound to {@code value}. */
    public static boolean onBindCell(Variable self, Term value) {
        final Ctx c = ctx();
        final FdVar fv = c.vars.get(self.getName());
        if (fv == null) return true;                      // not (or no longer) FD-constrained
        if (value instanceof Variable && ((Variable) value).ref == null) {
            return onAlias(self, (Variable) value);
        }
        if (!(value instanceof Number) || !((Number) value).isInteger()) return false;
        // START_CHANGE: ISS-2025-0644 - an integer beyond the representable finite range
        if (IntervalDomain.isInfinite(Constraint.clamp(((Number) value).bigIntegerValue()))) return bindBig(fv, (Number) value);
        // END_CHANGE: ISS-2025-0644
        final long val = ((Number) value).longValue();
        return guardedPost(() -> ctx().store.narrow(fv, IntervalDomain.singleton(val))
                              && ctx().store.propagate());
    }
    // END_CHANGE: ISS-2025-0486

    // START_CHANGE: ISS-2025-0644 - binding an FD variable to a big integer. A domain that is
    // bounded on that side rejects it (sound failure); a constraint that solves the variable
    // exactly to another value refutes it; otherwise the value is recorded in the domain.
    private static boolean bindBig(FdVar fv, Number value) {
        IntervalDomain d = ctx().store.dom(fv);
        boolean positive = value.bigIntegerValue().signum() > 0;
        if (positive ? d.max() != IntervalDomain.SUP : d.min() != IntervalDomain.INF) return false;
        // every constraint on it already entailed by the domains (X #> 3, X = 10^23), or solved
        // exactly for it by the other (fixed) variables: nothing left to propagate (SWI)
        int verdict = bigVerdict(fv, value.bigIntegerValue());
        if (verdict == 0) return false;
        // otherwise record the exact value (a degenerate sup..sup / inf..inf domain that remembers
        // it) and propagate: the other constraints see an infinite bound, which is sound, and a
        // constraint whose other variables are all fixed solves its last one exactly
        final BigInteger v = value.bigIntegerValue();
        return guardedPost(() -> ctx().store.assignBig(fv, v) && ctx().store.propagate());
    }

    /** 1 = every constraint on {@code fv} accepts {@code value}, 0 = one refutes it, -1 = unknown. */
    private static int bigVerdict(FdVar fv, BigInteger value) {
        ClpStore st = ctx().store;
        List<Constraint> all = new ArrayList<>(fv.watchers);
        all.addAll(fv.fixWatchers);
        for (Constraint c : all) {
            if (c.entailment(st) == Constraint.Entail.TRUE) continue;
            BigInteger exact = c.solveFor(fv, st);
            if (exact == null) return -1;
            if (!exact.equals(value)) return 0;
        }
        return 1;
    }
    // END_CHANGE: ISS-2025-0644

    /** The current domain of an FD-constrained cell as a term, or null when it is not one. This is
     *  the residual goal an answer printer shows for a CLP(FD) variable (design B.12 / W7). */
    public static Term domainTermForCell(Variable v) {
        FdVar fv = ctx().vars.get(v.getName());
        if (fv == null) return null;
        IntervalDomain d = ctx().store.dom(fv);
        return (d == null) ? null : domainToTerm(d);
    }
    // END_CHANGE: ISS-2025-0460

    // START_CHANGE: ISS-2025-0471 - engine v4 wave W6: labeling over CELLS.
    /**
     * Label {@code varTerms} (already dereferenced by the machine) and return one
     * cell -> value assignment per solution.
     *
     * <p>This is the cell-model twin of {@link #label(List, Map, Labeler.VarSel, Labeler.ValOrder)}:
     * it never builds a {@code Map<String,Term>} and therefore never needs
     * {@link #exportSingletons(Map)} or the {@link #cellFor(String)} hop back from a name to an
     * engine variable. Functionally determined variables are still reported — every FD variable of
     * the query whose domain is a singleton under the assignment is included, which is what
     * {@code C in 1..3, D #= C*2+1, label([C])} needs (ISS-2025-0357) — but they are reported as
     * the cells the bridge itself created, not as names.
     *
     * @return one insertion-ordered {@code cell -> value} map per solution
     */
    public static List<Map<Variable, Long>> labelCells(List<Term> varTerms,
                                                       Labeler.VarSel varSel,
                                                       Labeler.ValOrder valOrder) {
        final Ctx c = ctx();
        List<FdVar> fdVars = new ArrayList<>();
        for (Term r : varTerms) {
            if (r instanceof Variable) {
                fdVars.add(varFor((Variable) r));
            } else if (!(r instanceof Number) || !((Number) r).isInteger()) {
                throw new PrologException(ISOErrorTerms.typeError("integer", r, "label/1"));
            }
        }
        final List<Map<Variable, Long>> out = new ArrayList<>();
        try {
            Labeler.label(c.store, fdVars, varSel, valOrder, sol -> {
                Map<Variable, Long> one = new java.util.LinkedHashMap<>();
                for (Map.Entry<String, FdVar> e : c.vars.entrySet()) {
                    Variable cell = c.cells.get(e.getKey());
                    if (cell == null || cell.ref != null) continue;      // already bound
                    Long v = sol.get(e.getValue());
                    if (v == null) {
                        IntervalDomain d = c.store.dom(e.getValue());
                        if (d == null || !d.isSingleton()) continue;
                        v = Long.valueOf(d.value());
                    }
                    one.put(cell, v);
                }
                out.add(one);
                return true;
            });
        } catch (Labeler.TooLargeToLabel e) {
            throw new PrologException(ISOErrorTerms.resourceError("clpfd_label_domain_too_large", "label/1"));
        }
        return out;
    }
    // END_CHANGE: ISS-2025-0471

    // START_CHANGE: ISS-2025-0357 - propagation that fixes a domain must bind the Prolog variable
    /** Add a binding for every engine variable whose domain is a singleton and which is not already
     *  bound (SWI behavior: {@code X #= 2} binds {@code X = 2}). */
    public static void exportSingletons(Map<String, Term> bindings) {
        Ctx c = ctx();
        for (Map.Entry<String, FdVar> e : c.vars.entrySet()) {
            if (bindings.containsKey(e.getKey())) continue;        // already bound (or aliased away)
            IntervalDomain d = c.store.dom(e.getValue());
            if (d != null && d.isSingleton()) bindings.put(e.getKey(), new Number(d.value()));
        }
    }
    // END_CHANGE: ISS-2025-0357

    // START_CHANGE: ISS-2025-0486 - wave W9: the cell-model twin of exportSingletons/1. A v4 native
    // posts a constraint and then binds the FD CELLS whose domain propagation has determined,
    // instead of naming them in a Map<String,Term> that LegacyBuiltinAdapter had to translate back
    // into cells through cellFor/1. That translation was the last name-keyed hop in the engine.
    /**
     * Every FD cell that is still unbound and whose domain is now a singleton, in registration
     * order. This is what makes {@code C in 1..3, D #= C*2+1, C #= 1} bind {@code D} even though
     * the goal {@code #=(C, 1)} never mentions it (ISS-2025-0357).
     */
    public static Map<Variable, Number> determinedCells() {
        Ctx c = ctx();
        // ISS-2025-0642: a list scan, and no allocation when nothing is determined
        Map<Variable, Number> out = null;
        ArrayList<Variable> cl = c.cellList;
        ArrayList<FdVar> fl = c.fdList;
        for (int i = 0, n = cl.size(); i < n; i++) {
            Variable cell = cl.get(i);
            if (cell.ref != null) continue;                     // already bound
            IntervalDomain d = fl.get(i).dom;
            Number value = null;
            if (d.isSingleton()) {
                value = Number.valueOf(d.value());
            } else if (!d.isFinite() && !d.isEmpty()) {
                // ISS-2025-0644: an unbounded (or degenerate inf..inf / sup..sup) domain may hide
                // a value beyond the 64-bit range; bind it when a constraint determines it exactly
                BigInteger big = c.store.exactValue(fl.get(i));
                if (big != null) value = intTerm(big);
            }
            if (value != null) {
                if (out == null) out = new java.util.LinkedHashMap<>();
                out.put(cell, value);
            }
        }
        return out == null ? java.util.Collections.<Variable, Number>emptyMap() : out;
    }

    // START_CHANGE: ISS-2025-0644
    private static Number intTerm(BigInteger v) {
        return v.bitLength() <= 63 ? Number.valueOf(v.longValue()) : new Number(v);
    }
    // END_CHANGE: ISS-2025-0644
    // END_CHANGE: ISS-2025-0486


    // ----------------------------------------------------------------- domain posting

    /** Post {@code Var in Lo..Hi}. Returns false on inconsistency. */
    public static boolean postIn(Term varTerm, long lo, long hi, Map<String, Term> bindings) {
        return postDomain(varTerm.resolveBindings(bindings), IntervalDomain.interval(lo, hi));
    }

    // START_CHANGE: ISS-2025-0641 - post an arbitrary domain (unions, holes, inf/sup bounds)
    /** Post {@code Var in Dom} for a parsed domain. Returns false on inconsistency. */
    public static boolean postDomain(Term t, final IntervalDomain dom) {
        if (t instanceof Number) {                       // a constant must lie in the domain
            Number n = (Number) t;
            if (!n.isInteger()) throw new PrologException(ISOErrorTerms.typeError("integer", t, "in/2"));
            if (n.bigIntegerValue().bitLength() > 62) {
                return n.bigIntegerValue().signum() > 0 ? dom.max() == IntervalDomain.SUP
                                                        : dom.min() == IntervalDomain.INF;
            }
            return dom.contains(n.longValue());
        }
        if (!(t instanceof Variable)) throw new PrologException(ISOErrorTerms.typeError("integer", t, "in/2"));
        final Term ft = t;
        return guardedPost(() -> {
            FdVar fv = varFor((Variable) ft);
            return ctx().store.narrow(fv, dom) && ctx().store.propagate();
        });
    }

    /**
     * Parse a CLP(FD) domain term: {@code N}, {@code Lo..Hi} (bounds integers, {@code inf} or
     * {@code sup}) or {@code D1 \/ D2}. Unbound parts raise instantiation_error, anything else
     * type_error(clpfd_domain, D).
     */
    public static IntervalDomain parseDomain(Term d, String ctxName) {
        d = deref(d);
        if (d instanceof Variable) throw new PrologException(ISOErrorTerms.instantiationError(ctxName));
        if (d instanceof Number) {
            Number n = (Number) d;
            if (!n.isInteger()) throw new PrologException(ISOErrorTerms.typeError("clpfd_domain", d, ctxName));
            if (n.bigIntegerValue().bitLength() > 62) {
                throw new PrologException(ISOErrorTerms.representationError("max_integer", ctxName));
            }
            return IntervalDomain.singleton(n.longValue());
        }
        if (d instanceof CompoundTerm && ((CompoundTerm) d).getArguments().size() == 2) {
            CompoundTerm c = (CompoundTerm) d;
            if ("..".equals(c.getName())) {
                long lo = bound(c.getArguments().get(0), true, d, ctxName);
                long hi = bound(c.getArguments().get(1), false, d, ctxName);
                return IntervalDomain.interval(lo, hi);
            }
            if ("\\/".equals(c.getName())) {
                return parseDomain(c.getArguments().get(0), ctxName)
                    .union(parseDomain(c.getArguments().get(1), ctxName));
            }
        }
        throw new PrologException(ISOErrorTerms.typeError("clpfd_domain", d, ctxName));
    }

    private static long bound(Term b, boolean lower, Term whole, String ctxName) {
        b = deref(b);
        if (b instanceof Variable) throw new PrologException(ISOErrorTerms.instantiationError(ctxName));
        if (b instanceof Atom) {
            String n = ((Atom) b).getName();
            if ("inf".equals(n)) return IntervalDomain.INF;
            if ("sup".equals(n)) return IntervalDomain.SUP;
        }
        if (b instanceof Number && ((Number) b).isInteger()) {
            BigInteger v = ((Number) b).bigIntegerValue();
            long l = Constraint.clamp(v);
            return l;
        }
        throw new PrologException(ISOErrorTerms.typeError("integer", b, ctxName));
    }
    // END_CHANGE: ISS-2025-0641

    private static Term deref(Term t) {
        while (t instanceof Variable && ((Variable) t).ref != null) t = ((Variable) t).ref;
        return t;
    }

    // ----------------------------------------------------------------- comparison posting

    /** Post a comparison {@code Left <rel> Right} between arithmetic expressions. */
    public static boolean postCmp(Term left, Constraint.Rel rel, Term right, Map<String, Term> bindings) {
        final Term l = left.resolveBindings(bindings);
        final Term r = right.resolveBindings(bindings);
        return guardedPost(() -> {
            try {
                Constraint c = buildCmp(l, rel, r);
                if (c == null) return true;                          // a ground truth
                if (c == FALSE_CONSTRAINT) return false;
                // ISS-2025-0645: the cycle check runs BEFORE the constraint propagates -- on a
                // huge finite domain the propagation itself is the one-round-per-value crawl
                if (negativeDifferenceCycle(c)) return false;
                return ctx().store.addConstraint(c);
            } catch (Unsat e) {
                return false;
            }
        });
    }

    /** Marker for a ground comparison that is false. */
    private static final Constraint FALSE_CONSTRAINT = new Constraint.Linear(new long[0], new FdVar[0],
        Constraint.Rel.EQ, BigInteger.ONE);

    /**
     * Compile {@code l rel r} into ONE constraint (auxiliary constraints for non-linear sub-terms are
     * posted on the way). Returns null for a ground comparison that holds, FALSE_CONSTRAINT for one
     * that does not.
     */
    private static Constraint buildCmp(Term l, Constraint.Rel rel, Term r) {
        LinExpr le = new LinExpr();
        compile(l, 1, le);
        compile(r, -1, le);
        le.dropZeros();
        BigInteger k = le.constant.negate();                           // sum(ci*xi) rel k
        int n = le.terms.size();
        if (n == 0) {
            int cmp = BigInteger.ZERO.compareTo(k);
            boolean holds;
            switch (rel) {
                case EQ: holds = cmp == 0; break;
                case NE: holds = cmp != 0; break;
                case LT: holds = cmp < 0; break;
                case LE: holds = cmp <= 0; break;
                case GT: holds = cmp > 0; break;
                default: holds = cmp >= 0; break;
            }
            return holds ? null : FALSE_CONSTRAINT;
        }
        long[] coeffs = new long[n];
        FdVar[] vars = new FdVar[n];
        int i = 0;
        for (Map.Entry<FdVar, Long> e : le.terms.entrySet()) { coeffs[i] = e.getValue(); vars[i] = e.getKey(); i++; }
        switch (rel) {
            case EQ:
                // X #= Y keeps holes: the domains are intersected, not just their bounds
                if (n == 2 && k.signum() == 0 && coeffs[0] == -coeffs[1] && Math.abs(coeffs[0]) == 1) {
                    return new Constraint.Cmp(vars[0], Constraint.Rel.EQ, vars[1]);
                }
                return new Constraint.Linear(coeffs, vars, Constraint.Rel.EQ, k);
            case NE: return new Constraint.LinearNE(coeffs, vars, k);         // ISS-2025-0641
            case LE: return new Constraint.Linear(coeffs, vars, Constraint.Rel.LE, k);
            case LT: return new Constraint.Linear(coeffs, vars, Constraint.Rel.LE, k.subtract(BigInteger.ONE));
            case GE: return new Constraint.Linear(coeffs, vars, Constraint.Rel.GE, k);
            default: return new Constraint.Linear(coeffs, vars, Constraint.Rel.GE, k.add(BigInteger.ONE));
        }
    }

    // START_CHANGE: ISS-2025-0645 - X #> Y, Y #> X on unbounded (or huge) domains: bounds
    // propagation alone either never prunes (inf..sup) or needs one round per value (a huge finite
    // domain, 29 s and resource_error(memory) in 4.4.0). The difference constraints (x - y =< c)
    // posted so far form a graph; a negative cycle in it means the system is unsatisfiable, and
    // Bellman-Ford finds one in O(V*E). The check runs only when the new constraint is a difference
    // constraint over a variable whose domain is infinite or larger than 10^6 values, where the
    // slow convergence can happen, and gives up (no answer = no pruning, still sound) past a budget.
    private static final long HUGE_DOMAIN = 1_000_000L;
    private static final long CYCLE_BUDGET = 4_000_000L;

    private static boolean negativeDifferenceCycle(Constraint c) {
        List<Object[]> newEdges = new ArrayList<>(2);
        differenceEdges(c, newEdges);
        if (newEdges.isEmpty()) return false;
        ClpStore st = ctx().store;
        boolean huge = false;
        for (Object[] e : newEdges) {
            for (int j = 0; j < 2; j++) {
                IntervalDomain d = st.dom((FdVar) e[j]);
                if (d.size() > HUGE_DOMAIN) huge = true;
            }
        }
        if (!huge) return false;
        List<Object[]> edges = new ArrayList<>(newEdges);            // c is not posted yet
        for (Constraint k : st.constraints()) differenceEdges(k, edges);
        java.util.IdentityHashMap<FdVar, Integer> ids = new java.util.IdentityHashMap<>();
        for (Object[] e : edges) {
            for (int j = 0; j < 2; j++) if (!ids.containsKey(e[j])) ids.put((FdVar) e[j], ids.size());
        }
        int nv = ids.size();
        int ne = edges.size();
        int[] from = new int[ne], to = new int[ne];
        long[] w = new long[ne];
        for (int i = 0; i < ne; i++) {
            from[i] = ids.get(edges.get(i)[0]);
            to[i] = ids.get(edges.get(i)[1]);
            w[i] = (Long) edges.get(i)[2];
        }
        // The system without c had no negative cycle (every earlier post was checked, or could
        // not close one), so a new negative cycle must run through a new edge u -> v: it exists
        // iff the shortest path v ~> u plus w(u -> v) is negative. Single-source shortest paths
        // from v over the part of the graph reachable from v (Bellman-Ford, queue-based): a chain
        // X1 #< X2 #< ... costs one reachability sweep per post instead of V rounds over E edges.
        int[] headOf = new int[nv];
        java.util.Arrays.fill(headOf, -1);
        int[] nextEdge = new int[ne];
        for (int i = 0; i < ne; i++) { nextEdge[i] = headOf[from[i]]; headOf[from[i]] = i; }
        long work = 0;
        for (int e = 0; e < newEdges.size(); e++) {
            int u = from[e], v = to[e];
            if (u == v) { if (w[e] < 0) return true; continue; }
            long[] dist = new long[nv];
            java.util.Arrays.fill(dist, Long.MAX_VALUE);
            int[] relaxed = new int[nv];
            boolean[] inQueue = new boolean[nv];
            java.util.ArrayDeque<Integer> queue = new java.util.ArrayDeque<>();
            dist[v] = 0;
            queue.add(v);
            inQueue[v] = true;
            while (!queue.isEmpty()) {
                int x = queue.poll();
                inQueue[x] = false;
                if (++relaxed[x] > nv) return true;                 // a negative cycle elsewhere
                for (int k = headOf[x]; k >= 0; k = nextEdge[k]) {
                    if (++work > CYCLE_BUDGET) return false;       // give up: no pruning, sound
                    long nd = Constraint.add(dist[x], w[k]);
                    int y = to[k];
                    if (nd < dist[y]) {
                        dist[y] = nd;
                        if (y == u && Constraint.add(nd, w[e]) < 0) return true;
                        if (!inQueue[y]) { inQueue[y] = true; queue.add(y); }
                    }
                }
            }
        }
        return false;
    }

    /** Edges {from, to, weight} meaning {@code to =< from + weight}. */
    private static void differenceEdges(Constraint c, List<Object[]> out) {
        if (c instanceof Constraint.Linear) {
            Constraint.Linear l = (Constraint.Linear) c;
            long[] co = l.coeffs();
            FdVar[] vs = l.vars();
            if (co.length != 2 || co[0] != -co[1] || Math.abs(co[0]) != 1) return;
            if (l.constant().bitLength() > 62) return;
            long k = l.constant().longValue();
            FdVar pos = co[0] == 1 ? vs[0] : vs[1];                   // pos - neg  rel  k
            FdVar neg = co[0] == 1 ? vs[1] : vs[0];
            switch (l.rel()) {
                case LE: out.add(new Object[]{neg, pos, k}); break;                 // pos =< neg + k
                case GE: out.add(new Object[]{pos, neg, -k}); break;                // neg =< pos - k
                case EQ: out.add(new Object[]{neg, pos, k}); out.add(new Object[]{pos, neg, -k}); break;
                default: break;
            }
        } else if (c instanceof Constraint.Cmp) {
            ((Constraint.Cmp) c).differenceEdges(out);
        }
    }
    // END_CHANGE: ISS-2025-0645

    /** all_different(List). */
    public static boolean postAllDifferent(List<Term> elems, Map<String, Term> bindings) {
        return postAll(elems, bindings, false);
    }

    // START_CHANGE: ISS-2025-0651 - all_distinct/1 posts the matching-based propagator
    /** all_distinct(List): all_different with generalised arc consistency. */
    public static boolean postAllDistinct(List<Term> elems) {
        return postAll(elems, java.util.Collections.<String, Term>emptyMap(), true);
    }

    private static boolean postAll(List<Term> elems, Map<String, Term> bindings, boolean strong) {
        return guardedPost(() -> {
            List<FdVar> vs = new ArrayList<>();
            for (Term e : elems) vs.add(operandVar(e.resolveBindings(bindings)));
            return ctx().store.addConstraint(strong ? new Constraint.AllDistinct(vs) : new Constraint.AllDifferent(vs));
        });
    }
    // END_CHANGE: ISS-2025-0651

    // START_CHANGE: ISS-2025-0647 - reification: #<==>, #==>, #<==, #\/, #\, #/\ over reifiable
    // constraints (the six comparisons over linear/non-linear expressions, X in Dom, 0/1 integers
    // and variables). Each sub-formula becomes a 0/1 variable; the formula posted at top level
    // must be true.
    /** Post a boolean CLP(FD) formula (the whole goal term, e.g. {@code B #<==> (X #= 3)}). */
    public static boolean postBoolean(final Term formula) {
        return guardedPost(() -> {
            try {
                FdVar b = reify(formula);
                return ctx().store.assign(b, 1) && ctx().store.propagate();
            } catch (Unsat e) {
                return false;
            }
        });
    }

    private static FdVar reify(Term t) {
        t = deref(t);
        ClpStore st = ctx().store;
        if (t instanceof Variable) {
            FdVar fv = varFor((Variable) t);
            if (!st.narrow(fv, IntervalDomain.interval(0, 1)) || !st.propagate()) throw new Unsat();
            return fv;
        }
        if (t instanceof Number && ((Number) t).isInteger()
                && (((Number) t).longValue() == 0 || ((Number) t).longValue() == 1)
                && ((Number) t).bigIntegerValue().bitLength() <= 1) {
            return st.newVar("_b", IntervalDomain.singleton(((Number) t).longValue()));
        }
        if (t instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) t;
            String f = c.getName();
            List<Term> a = c.getArguments();
            if (a.size() == 1 && "#\\".equals(f)) {
                FdVar x = reify(a.get(0));
                FdVar z = boolVar();
                post(new Constraint.Linear(new long[]{1, 1}, new FdVar[]{x, z}, Constraint.Rel.EQ, 1));
                return z;
            }
            if (a.size() == 2) {
                Constraint.BoolOp op = null;
                boolean swap = false;
                switch (f) {
                    case "#/\\": op = Constraint.BoolOp.AND; break;
                    case "#\\/": op = Constraint.BoolOp.OR; break;
                    case "#\\": op = Constraint.BoolOp.XOR; break;
                    case "#==>": op = Constraint.BoolOp.IMPL; break;
                    case "#<==": op = Constraint.BoolOp.IMPL; swap = true; break;
                    case "#<==>": op = Constraint.BoolOp.EQUIV; break;
                    default: break;
                }
                if (op != null) {
                    FdVar x = reify(a.get(swap ? 1 : 0));
                    FdVar y = reify(a.get(swap ? 0 : 1));
                    FdVar z = boolVar();
                    post(new Constraint.Bool(op, x, y, z));
                    return z;
                }
                Constraint.Rel rel = relOf(f);
                if (rel != null) {
                    Constraint cmp = buildCmp(a.get(0), rel, a.get(1));
                    if (cmp == null) return st.newVar("_b", IntervalDomain.singleton(1));
                    if (cmp == FALSE_CONSTRAINT) return st.newVar("_b", IntervalDomain.singleton(0));
                    FdVar z = boolVar();
                    post(new Constraint.Reified(z, cmp));
                    return z;
                }
                if ("in".equals(f)) {
                    IntervalDomain dom = parseDomain(a.get(1), "in/2");
                    FdVar x = operandVar(a.get(0));
                    FdVar z = boolVar();
                    post(new Constraint.Reified(z, new Constraint.InDomain(x, dom)));
                    return z;
                }
            }
        }
        throw new PrologException(ISOErrorTerms.domainError("clpfd_reifiable_expression", t, "clpfd"));
    }

    private static FdVar boolVar() {
        return ctx().store.newVar("_b" + (ctx().aux++), IntervalDomain.interval(0, 1));
    }

    private static void post(Constraint c) {
        if (!ctx().store.addConstraint(c)) throw new Unsat();
    }

    /** The comparison relation named by a CLP(FD) operator, or null. */
    public static Constraint.Rel relOf(String op) {
        switch (op) {
            case "#=": return Constraint.Rel.EQ;
            case "#\\=": return Constraint.Rel.NE;
            case "#<": return Constraint.Rel.LT;
            case "#>": return Constraint.Rel.GT;
            case "#=<": return Constraint.Rel.LE;
            case "#>=": return Constraint.Rel.GE;
            default: return null;
        }
    }
    // END_CHANGE: ISS-2025-0647

    // START_CHANGE: ISS-2025-0650 - element/3, tuples_in/2, global_cardinality/2
    /** {@code element(I, List, V)}. */
    public static boolean postElement(final Term index, final List<Term> list, final Term value) {
        return guardedPost(() -> {
            FdVar[] xs = new FdVar[list.size()];
            for (int i = 0; i < xs.length; i++) xs[i] = operandVar(list.get(i));
            return ctx().store.addConstraint(new Constraint.Element(operandVar(index), xs, operandVar(value)));
        });
    }

    /** {@code tuples_in(Tuples, Relation)}: one table constraint per tuple. */
    public static boolean postTuples(final List<List<Term>> tuples, final long[][] rows) {
        return guardedPost(() -> {
            for (List<Term> tuple : tuples) {
                FdVar[] xs = new FdVar[tuple.size()];
                for (int i = 0; i < xs.length; i++) xs[i] = operandVar(tuple.get(i));
                if (!ctx().store.addConstraint(new Constraint.Table(xs, rows))) return false;
            }
            return true;
        });
    }

    /** {@code global_cardinality(Vs, [K-C, ...])}. */
    public static boolean postGcc(final List<Term> vars, final long[] keys, final List<Term> counts) {
        return guardedPost(() -> {
            FdVar[] xs = new FdVar[vars.size()];
            for (int i = 0; i < xs.length; i++) xs[i] = operandVar(vars.get(i));
            FdVar[] cs = new FdVar[counts.size()];
            for (int i = 0; i < cs.length; i++) cs[i] = operandVar(counts.get(i));
            return ctx().store.addConstraint(new Constraint.Gcc(xs, keys, cs));
        });
    }
    // END_CHANGE: ISS-2025-0650

    // ----------------------------------------------------------------- labeling support

    // START_CHANGE: ISS-2025-0642 - what the lazy v4 labeling generator needs from the store
    /** The domain of an FD cell, or null when the cell is not an FD variable. */
    public static IntervalDomain cellDomain(Variable v) {
        FdVar fv = ctx().vars.get(v.getName());
        return fv == null ? null : ctx().store.dom(fv);
    }

    /** Number of constraints on an FD cell (0 when it is not one). */
    public static int cellDegree(Variable v) {
        FdVar fv = ctx().vars.get(v.getName());
        return fv == null ? 0 : ctx().store.degree(fv);
    }

    /** Intersect an FD cell's domain with {@code d} and propagate (a labeling branch). */
    public static boolean narrowCell(Variable v, final IntervalDomain d) {
        final FdVar fv = varFor(v);
        return guardedPost(() -> ctx().store.narrow(fv, d) && ctx().store.propagate());
    }
    // END_CHANGE: ISS-2025-0642

    // START_CHANGE: ISS-2025-0643 - labeling/2 min(Expr)/max(Expr): branch and bound. Each round
    // searches (depth-first, inside the store, with the store's own trail) for the first
    // assignment whose objective beats the incumbent, tightening the bound on the objective's
    // auxiliary variable; the last improvement is the optimum. Everything the search posts is
    // rolled back before returning, so the store is exactly as it was.
    /**
     * The optimum of {@code expr} over the labelings of {@code varTerms}, or null when there is no
     * solution at all.
     */
    public static BigInteger optimum(List<Term> varTerms, Term expr, boolean minimize,
                                     Labeler.VarSel varSel, Labeler.ValOrder valOrder) {
        final ClpStore st = ctx().store;
        final int dm = st.mark();
        final int cm = st.constraintMark();
        try {
            FdVar obj;
            try {
                obj = exprVar(expr);
                if (!st.propagate()) return null;
            } catch (Unsat e) {
                return null;
            }
            List<FdVar> fdVars = new ArrayList<>();
            for (Term t : varTerms) {
                Term r = deref(t);
                if (r instanceof Variable) fdVars.add(varFor((Variable) r));
            }
            Long best = null;
            while (true) {
                int m2 = st.mark();
                int c2 = st.constraintMark();
                boolean ok = true;
                if (best != null) {
                    ok = minimize ? st.removeAbove(obj, best - 1) : st.removeBelow(obj, best + 1);
                    ok = ok && st.propagate();
                }
                final Long[] found = {null};
                final FdVar fobj = obj;
                if (ok) {
                    try {
                        Labeler.label(st, fdVars, varSel, valOrder, sol -> {
                            IntervalDomain od = st.dom(fobj);
                            if (!od.isSingleton()) {
                                throw new PrologException(ISOErrorTerms.instantiationError("labeling/2"));
                            }
                            found[0] = od.value();
                            return false;                                 // first solution only
                        });
                    } catch (Labeler.TooLargeToLabel e) {
                        throw new PrologException(ISOErrorTerms.resourceError("clpfd_label_domain_too_large", "labeling/2"));
                    }
                }
                st.rollbackTo(m2, c2);
                if (found[0] == null) break;
                best = found[0];
            }
            return best == null ? null : BigInteger.valueOf(best);
        } finally {
            st.rollbackTo(dm, cm);
        }
    }
    // END_CHANGE: ISS-2025-0643

    // ----------------------------------------------------------------- labeling (registry path)

    /** Label the given variables; returns one binding map per solution (bound to the values). */
    public static List<Map<String, Term>> label(List<Term> varTerms, Map<String, Term> bindings) {
        // START_CHANGE: ISS-2025-0422 - delegate to the strategy-aware variant with the defaults
        return label(varTerms, bindings, Labeler.VarSel.FF, Labeler.ValOrder.UP);
    }

    /** Label the given variables under the given strategies (labeling/2 options). */
    public static List<Map<String, Term>> label(List<Term> varTerms, Map<String, Term> bindings,
                                                Labeler.VarSel varSel, Labeler.ValOrder valOrder) {
        // END_CHANGE: ISS-2025-0422
        List<FdVar> fdVars = new ArrayList<>();
        List<Variable> engineVars = new ArrayList<>();
        for (Term t : varTerms) {
            Term r = t.resolveBindings(bindings);
            if (r instanceof Variable) {
                engineVars.add((Variable) r);
                fdVars.add(varFor((Variable) r));
            }
            // START_CHANGE: ISS-2025-0422 - already-ground integers need no labeling, but any other
            // term must raise type_error(integer, T): label([a]) used to silently succeed.
            else if (!(r instanceof Number) || !((Number) r).isInteger()) {
                throw new PrologException(ISOErrorTerms.typeError("integer", r, "label/1"));
            }
            // END_CHANGE: ISS-2025-0422
        }
        List<Map<String, Term>> out = new ArrayList<>();
        try {
            // START_CHANGE: ISS-2025-0357 - snapshot the bindings while the labeled assignment is in
            // the store, so EVERY variable whose domain is (now) a singleton comes out bound.
            Labeler.label(ctx().store, fdVars, varSel, valOrder, sol -> {       // ISS-2025-0298
                Map<String, Term> b = new HashMap<>(bindings);
                for (int i = 0; i < engineVars.size(); i++) {
                    b.put(engineVars.get(i).getName(), new Number(sol.get(fdVars.get(i))));
                }
                exportSingletons(b);
                out.add(b);
                return true;
            });
            // END_CHANGE: ISS-2025-0357
        } catch (Labeler.TooLargeToLabel e) {
            throw new PrologException(ISOErrorTerms.resourceError("clpfd_label_domain_too_large", "label/1"));
        }
        return out;
    }

    // ----------------------------------------------------------------- domain inspection

    /** fd_dom(Var, Dom): the current domain as a term (N, Lo..Hi, or A \/ B unions). */
    public static Term domainTerm(Term varTerm, Map<String, Term> bindings) {
        Term t = varTerm.resolveBindings(bindings);
        IntervalDomain d;
        if (t instanceof Number) {
            d = IntervalDomain.singleton(((Number) t).longValue());
        } else if (t instanceof Variable) {
            d = ctx().store.dom(varFor((Variable) t));
        } else {
            return null;
        }
        return domainToTerm(d);
    }

    /** {@code N}, {@code Lo..Hi} or {@code A \\/ B} for a domain; infinite bounds print as
     *  {@code inf}/{@code sup} (ISS-2025-0460: shared with {@link #domainTermForCell}). */
    public static Term domainToTerm(IntervalDomain d) {
        if (d.isEmpty()) return new Atom("{}");
        long[][] rs = d.rangeArray();
        Term acc = null;
        for (long[] r : rs) {
            Term part = (r[0] == r[1] && !IntervalDomain.isInfinite(r[0]))
                ? new Number(r[0])
                : new CompoundTerm(new Atom(".."), java.util.Arrays.asList(boundTerm(r[0]), boundTerm(r[1])));
            acc = (acc == null) ? part : new CompoundTerm(new Atom("\\/"), java.util.Arrays.asList(acc, part));
        }
        return acc;
    }

    /** ISS-2025-0644: an infinite bound is the atom inf/sup. */
    private static Term boundTerm(long v) {
        if (v == IntervalDomain.INF) return new Atom("inf");
        if (v == IntervalDomain.SUP) return new Atom("sup");
        return new Number(v);
    }

    /** fd_size(Var, N): the number of values in the domain (Long.MAX_VALUE when infinite). */
    public static long domainSize(Term varTerm, Map<String, Term> bindings) {
        Term t = varTerm.resolveBindings(bindings);
        if (t instanceof Number) return 1;
        if (t instanceof Variable) return ctx().store.dom(varFor((Variable) t)).size();
        return 0;
    }

    // ----------------------------------------------------------------- expression compiler

    /** A linear expression:  sum(coeff_i * var_i) + constant (the constant is exact). */
    private static final class LinExpr {
        final Map<FdVar, Long> terms = new java.util.LinkedHashMap<>();
        BigInteger constant = BigInteger.ZERO;
        void addVar(FdVar v, long c) {
            Long old = terms.get(v);
            terms.put(v, old == null ? c : exact(() -> Math.addExact(old, c)));
        }
        void dropZeros() { terms.values().removeIf(co -> co == 0); }
    }

    private interface LongOp { long run(); }

    /** Run an exact long operation; overflow of a COEFFICIENT is a representation error. */
    private static long exact(LongOp op) {
        try {
            return op.run();
        } catch (ArithmeticException e) {
            throw new PrologException(ISOErrorTerms.representationError("max_integer", "clpfd"));
        }
    }

    // START_CHANGE: ISS-2025-0644 / ISS-2025-0648 - the expression compiler. Ground sub-terms are
    // evaluated EXACTLY (BigInteger), so 1000000000000*1000000000000 is 10^24 and not a saturated
    // long; + - * by constants stay linear; everything else (var*var, abs, min, max, //, div, rem,
    // mod, ^) is folded into an auxiliary variable backed by its propagator. An unsupported functor
    // raises type_error(evaluable, F/N) (ISS-2025-0421).
    private static void compile(Term term, long scale, LinExpr into) {
        Term t = deref(term);
        if (t instanceof Number) {
            requireIntNum((Number) t);
            into.constant = into.constant.add(BigInteger.valueOf(scale).multiply(((Number) t).bigIntegerValue()));
            return;
        }
        if (t instanceof Variable) {
            into.addVar(varFor((Variable) t), scale);
            return;
        }
        if (t instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) t;
            String f = c.getName();
            List<Term> a = c.getArguments();
            if (isGround(t)) {
                BigInteger v = evalGround(t);
                if (v == null) throw new Unsat();                  // undefined, e.g. 1 // 0
                into.constant = into.constant.add(BigInteger.valueOf(scale).multiply(v));
                return;
            }
            if (a.size() == 2 && "+".equals(f)) {
                compile(a.get(0), scale, into);
                compile(a.get(1), scale, into);
                return;
            }
            if (a.size() == 2 && "-".equals(f)) {
                compile(a.get(0), scale, into);
                compile(a.get(1), exact(() -> Math.negateExact(scale)), into);
                return;
            }
            if (a.size() == 1 && "-".equals(f)) {
                compile(a.get(0), exact(() -> Math.negateExact(scale)), into);
                return;
            }
            if (a.size() == 1 && "+".equals(f)) {
                compile(a.get(0), scale, into);
                return;
            }
            if (a.size() == 2 && "*".equals(f)) {
                Term x = a.get(0), y = a.get(1);
                if (isGround(y) && !isGround(x)) { Term tmp = x; x = y; y = tmp; }
                if (isGround(x)) {
                    BigInteger cst = evalGround(x);
                    if (cst == null) throw new Unsat();
                    if (cst.bitLength() > 62) {
                        throw new PrologException(ISOErrorTerms.representationError("max_integer", "clpfd"));
                    }
                    final long cl = cst.longValue();
                    compile(y, exact(() -> Math.multiplyExact(scale, cl)), into);
                    return;
                }
                FdVar va = exprVar(x);
                FdVar vb = exprVar(y);
                FdVar p = auxVar("_p");
                post(va == vb ? new Constraint.Square(va, p) : new Constraint.Mul(va, vb, p));
                into.addVar(p, scale);
                return;
            }
            if (a.size() == 1 && "abs".equals(f)) {
                FdVar vx = exprVar(a.get(0));
                FdVar y = ctx().store.newVar("_a" + (ctx().aux++), IntervalDomain.interval(0, IntervalDomain.SUP));
                post(new Constraint.Abs(vx, y));
                into.addVar(y, scale);
                return;
            }
            if (a.size() == 2 && ("min".equals(f) || "max".equals(f))) {
                FdVar vx = exprVar(a.get(0));
                FdVar vy = exprVar(a.get(1));
                FdVar z = auxVar("_m");
                post("min".equals(f) ? new Constraint.Min(vx, vy, z) : new Constraint.Max(vx, vy, z));
                into.addVar(z, scale);
                return;
            }
            Constraint.Fn fn = null;
            if (a.size() == 2) {
                switch (f) {
                    case "//": fn = Constraint.Fn.TDIV; break;
                    case "div": fn = Constraint.Fn.FDIV; break;
                    case "rem": fn = Constraint.Fn.REM; break;
                    case "mod": fn = Constraint.Fn.MOD; break;
                    case "^": fn = Constraint.Fn.POW; break;
                    default: break;
                }
            }
            if (fn != null) {
                FdVar vx = exprVar(a.get(0));
                if (fn == Constraint.Fn.MOD && isGround(a.get(1))) {
                    BigInteger m = evalGround(a.get(1));
                    if (m == null) throw new Unsat();
                    if (m.signum() > 0 && m.bitLength() <= 62) {       // the stronger fixed-modulus form
                        long ml = m.longValue();
                        FdVar z = ctx().store.newVar("_r" + (ctx().aux++), IntervalDomain.interval(0, ml - 1));
                        post(new Constraint.Mod(vx, ml, z));
                        into.addVar(z, scale);
                        return;
                    }
                }
                FdVar vy = exprVar(a.get(1));
                FdVar z = auxVar("_f");
                post(new Constraint.ArithFn(fn, vx, vy, z));
                into.addVar(z, scale);
                return;
            }
        }
        throw unsupportedExpr(t);
    }
    // END_CHANGE: ISS-2025-0644 / ISS-2025-0648

    /** True when {@code t} contains no unbound variable. */
    public static boolean isGround(Term t) {
        t = deref(t);
        if (t instanceof Variable) return false;
        if (t instanceof CompoundTerm) {
            for (Term a : ((CompoundTerm) t).getArguments()) if (!isGround(a)) return false;
        }
        return true;
    }

    // START_CHANGE: ISS-2025-0644 - exact evaluation of a ground CLP(FD) expression
    /**
     * Evaluate a ground CLP(FD) arithmetic expression exactly. Returns null when the value is
     * undefined (division by zero); raises type_error(integer, F) for a float and
     * type_error(evaluable, F/N) for an unsupported functor.
     */
    public static BigInteger evalGround(Term term) {
        Term t = deref(term);
        if (t instanceof Number) {
            requireIntNum((Number) t);
            return ((Number) t).bigIntegerValue();
        }
        if (t instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) t;
            String f = c.getName();
            List<Term> a = c.getArguments();
            if (a.size() == 1) {
                if ("-".equals(f) || "+".equals(f) || "abs".equals(f)) {
                    BigInteger x = evalGround(a.get(0));
                    if (x == null) return null;
                    return "-".equals(f) ? x.negate() : ("abs".equals(f) ? x.abs() : x);
                }
            } else if (a.size() == 2) {
                Constraint.Fn fn = null;
                switch (f) {
                    case "+": case "-": case "*": case "min": case "max": break;
                    case "//": fn = Constraint.Fn.TDIV; break;
                    case "div": fn = Constraint.Fn.FDIV; break;
                    case "rem": fn = Constraint.Fn.REM; break;
                    case "mod": fn = Constraint.Fn.MOD; break;
                    case "^": fn = Constraint.Fn.POW; break;
                    default: throw unsupportedExpr(t);
                }
                BigInteger x = evalGround(a.get(0));
                BigInteger y = evalGround(a.get(1));
                if (x == null || y == null) return null;
                if (fn != null) return Constraint.ArithFn.eval(fn, x, y);
                switch (f) {
                    case "+": return x.add(y);
                    case "-": return x.subtract(y);
                    case "*": return x.multiply(y);
                    case "min": return x.min(y);
                    default: return x.max(y);
                }
            }
        }
        throw unsupportedExpr(t);
    }
    // END_CHANGE: ISS-2025-0644

    // START_CHANGE: ISS-2025-0421 - helpers for folding non-linear subterms into auxiliary FdVars

    /** Marker: an auxiliary constraint post wiped a domain — the enclosing comparison must FAIL
     *  (the constraint is unsatisfiable), not error; caught by the posting entry points. */
    private static final class Unsat extends RuntimeException {
        Unsat() { super(null, null, false, false); }
    }

    /** Fresh auxiliary variable (inf..sup since ISS-2025-0644). */
    private static FdVar auxVar(String prefix) {
        return ctx().store.newVar(prefix + (ctx().aux++), IntervalDomain.ALL);
    }

    /** Compile an arbitrary supported expression down to a single FdVar. */
    private static FdVar exprVar(Term term) {
        Term r = deref(term);
        if (r instanceof Variable) return varFor((Variable) r);
        if (r instanceof Number || isGround(r)) {
            BigInteger v = evalGround(r);
            if (v == null) throw new Unsat();
            return constVar(v);
        }
        LinExpr le = new LinExpr();
        compile(r, 1, le);
        le.dropZeros();
        if (le.constant.signum() == 0 && le.terms.size() == 1) {
            Map.Entry<FdVar, Long> e = le.terms.entrySet().iterator().next();
            if (e.getValue() == 1) return e.getKey();             // the expression IS a variable
        }
        long[] coeffs = new long[le.terms.size() + 1];
        FdVar[] vars = new FdVar[le.terms.size() + 1];
        int i = 0;
        for (Map.Entry<FdVar, Long> e : le.terms.entrySet()) { coeffs[i] = e.getValue(); vars[i] = e.getKey(); i++; }
        FdVar d = auxVar("_e");
        coeffs[i] = -1; vars[i] = d;                              // sum(ci*xi) - D = -k0  <=>  D = expr
        post(new Constraint.Linear(coeffs, vars, Constraint.Rel.EQ, le.constant.negate()));
        return d;
    }

    private static FdVar constVar(BigInteger v) {
        if (v.bitLength() > 62) {
            throw new PrologException(ISOErrorTerms.representationError(
                v.signum() > 0 ? "max_integer" : "min_integer", "clpfd"));
        }
        return ctx().store.newVar("_c" + v, IntervalDomain.singleton(v.longValue()));
    }

    /** type_error(evaluable, F/N) for an unsupported functor (SWI parity); type_error(integer, T)
     *  for terms that are not arithmetic at all. */
    private static PrologException unsupportedExpr(Term t) {
        if (t instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) t;
            Term ind = new CompoundTerm(new Atom("/"), java.util.Arrays.asList(
                (Term) new Atom(c.getName()), new Number((long) c.getArguments().size())));
            return new PrologException(ISOErrorTerms.typeError("evaluable", ind, "clpfd"));
        }
        if (t instanceof Atom) {
            Term ind = new CompoundTerm(new Atom("/"), java.util.Arrays.asList(t, new Number(0L)));
            return new PrologException(ISOErrorTerms.typeError("evaluable", ind, "clpfd"));
        }
        return new PrologException(ISOErrorTerms.typeError("integer", t, "clpfd"));
    }
    // END_CHANGE: ISS-2025-0421

    /** Map a simple operand (variable or integer) to an FdVar; constants become singleton vars. */
    private static FdVar operandVar(Term t) {
        Term r = deref(t);
        if (r instanceof Variable) return varFor((Variable) r);
        if (r instanceof Number) {
            requireIntNum((Number) r);                           // ISS-2025-0299
            return constVar(((Number) r).bigIntegerValue());
        }
        throw new PrologException(ISOErrorTerms.typeError("integer", r, "clpfd"));
    }

    /** CLP(FD) is over integers: reject floats (type_error). */
    private static void requireIntNum(Number n) {
        if (!n.isInteger()) throw new PrologException(ISOErrorTerms.typeError("integer", n, "clpfd"));
    }
}
