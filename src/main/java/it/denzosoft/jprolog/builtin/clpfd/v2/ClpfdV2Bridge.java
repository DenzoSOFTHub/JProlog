package it.denzosoft.jprolog.builtin.clpfd.v2;

import it.denzosoft.jprolog.builtin.clpfd.v2.ClpStore.FdVar;
import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

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

    /** Reset the per-query CLP state (call at the start of each top-level solve). */
    public static void reset() { CTX.set(new Ctx()); }

    private static Ctx ctx() { return CTX.get(); }

    /** Get (or create with a wide default domain) the FdVar for an engine variable. */
    private static FdVar varFor(Variable v) {
        Ctx c = ctx();
        FdVar fv = c.vars.get(v.getName());
        if (fv == null) {
            // default domain: a wide but finite interval (CLP(FD) requires bounded domains here)
            fv = c.store.newVar(v.getName(), IntervalDomain.interval(-100_000_000L, 100_000_000L));
            c.vars.put(v.getName(), fv);
            c.cells.put(v.getName(), v);                 // ISS-2025-0460
            // START_CHANGE: ISS-2025-0355 - mark the engine variable as FD-constrained so binding it
            // fires onBind/onAlias; the registration is trailed, so backtracking out of the goal that
            // created the FdVar restores plain-variable semantics.
            v.putAttribute(CLPFD_ATTR, FD_MARKER);
            final Ctx fc = c; final String name = v.getName(); final Variable fvv = v;
            it.denzosoft.jprolog.core.engine.Trail.record(() -> {
                fc.vars.remove(name);
                fc.cells.remove(name);                   // ISS-2025-0460
                fvv.removeAttribute(CLPFD_ATTR);
            });
            // END_CHANGE: ISS-2025-0355
        }
        return fv;
    }

    // START_CHANGE: ISS-2025-0356 - posted constraints must be undone when the engine backtracks past
    // the posting goal. Every store mutation is bracketed: snapshot (domain mark + constraint count)
    // before, self-undo immediately on failure (a failed post leaves no narrowing behind), and on
    // success register a rollback on the legacy Trail — the v2 engine already rolls that Trail back
    // at every choice point (MachineSolver CP.legacyMark), so abandoning a branch retracts its posts.
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
        it.denzosoft.jprolog.core.engine.Trail.record(() -> store.rollbackTo(dm, cm));
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
        if (((Number) value).bigIntegerValue().bitLength() > 63) return false;   // outside FD range
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
            to.putAttribute(CLPFD_ATTR, FD_MARKER);
            final String name = to.getName(); final Variable tv = to;
            it.denzosoft.jprolog.core.engine.Trail.record(() -> {
                c.vars.remove(name);
                c.cells.remove(name);                    // ISS-2025-0460
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
        if (((Number) value).bigIntegerValue().bitLength() > 63) return false;   // outside FD range
        final long val = ((Number) value).longValue();
        return guardedPost(() -> ctx().store.narrow(fv, IntervalDomain.singleton(val))
                              && ctx().store.propagate());
    }
    // END_CHANGE: ISS-2025-0486

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
    public static Map<Variable, Long> determinedCells() {
        Ctx c = ctx();
        Map<Variable, Long> out = new java.util.LinkedHashMap<>();
        for (Map.Entry<String, FdVar> e : c.vars.entrySet()) {
            Variable cell = c.cells.get(e.getKey());
            if (cell == null || cell.ref != null) continue;      // already bound
            IntervalDomain d = c.store.dom(e.getValue());
            if (d != null && d.isSingleton()) out.put(cell, Long.valueOf(d.value()));
        }
        return out;
    }
    // END_CHANGE: ISS-2025-0486

    // ----------------------------------------------------------------- domain posting

    /** Post {@code Var in Lo..Hi}. Returns false on inconsistency. */
    public static boolean postIn(Term varTerm, long lo, long hi, Map<String, Term> bindings) {
        Term t = varTerm.resolveBindings(bindings);
        if (t instanceof Number) {                       // a constant must lie in the range
            long val = ((Number) t).longValue();
            return val >= lo && val <= hi;
        }
        if (!(t instanceof Variable)) return false;
        // START_CHANGE: ISS-2025-0356 - bracket the post so backtracking undoes it
        final Term ft = t;
        return guardedPost(() -> {
            FdVar fv = varFor((Variable) ft);
            return ctx().store.narrow(fv, IntervalDomain.interval(lo, hi)) && ctx().store.propagate();
        });
        // END_CHANGE: ISS-2025-0356
    }

    // ----------------------------------------------------------------- comparison posting

    /** Post a comparison {@code Left <rel> Right} where each side is a linear expression. */
    public static boolean postCmp(Term left, Constraint.Rel rel, Term right, Map<String, Term> bindings) {
        // START_CHANGE: ISS-2025-0356 - bracket the post so backtracking undoes it
        // START_CHANGE: ISS-2025-0421 - an auxiliary post that wipes a domain means the whole
        // comparison is unsatisfiable: fail the goal (guardedPost rolls the store back).
        return guardedPost(() -> {
            try {
                return doPostCmp(left, rel, right, bindings);
            } catch (Unsat e) {
                return false;
            }
        });
        // END_CHANGE: ISS-2025-0421
        // END_CHANGE: ISS-2025-0356
    }

    private static boolean doPostCmp(Term left, Constraint.Rel rel, Term right, Map<String, Term> bindings) {
        // \= : compile (left - right) into a linear form so expression operands work
        // (ISS-2025-0301), e.g. X+1 #\= 5 -> X #\= 4. Handles the 0/1-variable cases exactly and the
        // multi-variable case via an auxiliary difference variable (ISS-2025-0358); falls back to a
        // direct Cmp NE only when the expression is genuinely non-linear.
        if (rel == Constraint.Rel.NE) {
            LinExpr le = new LinExpr();
            if (compile(left, 1, le, bindings) && compile(right, -1, le, bindings)) {
                // START_CHANGE: ISS-2025-0358 - drop cancelled-out terms (e.g. X #\= X -> 0*X)
                le.terms.values().removeIf(co -> co == 0);
                // END_CHANGE: ISS-2025-0358
                if (le.terms.isEmpty()) return le.constant != 0;          // const #\= 0
                if (le.terms.size() == 1) {
                    Map.Entry<FdVar, Long> e = le.terms.entrySet().iterator().next();
                    long c = e.getValue();
                    long rhs = -le.constant;                              // c*X #\= rhs
                    if (rhs % c != 0) return true;                        // never equal -> always holds
                    long val = rhs / c;
                    FdVar cv = ctx().store.newVar("_c" + val, IntervalDomain.singleton(val));
                    return ctx().store.addConstraint(new Constraint.Cmp(e.getKey(), Constraint.Rel.NE, cv));
                }
                // START_CHANGE: ISS-2025-0358 - multi-variable disequality: introduce the auxiliary
                // difference D = left - right via a Linear EQ constraint, then post D #\= 0 (the
                // previous operandVar fallback returned null for any compound side -> silent failure
                // of a satisfiable constraint).
                long[] coeffs = new long[le.terms.size() + 1];
                FdVar[] vars = new FdVar[le.terms.size() + 1];
                int i = 0;
                for (Map.Entry<FdVar, Long> e : le.terms.entrySet()) { coeffs[i] = e.getValue(); vars[i] = e.getKey(); i++; }
                FdVar d = ctx().store.newVar("_d" + (ctx().aux++),
                    IntervalDomain.interval(Long.MIN_VALUE / 2, Long.MAX_VALUE / 2));
                coeffs[i] = -1; vars[i] = d;                              // sum(ci*xi) - D = -k0  <=>  D = left - right
                if (!ctx().store.addConstraint(new Constraint.Linear(coeffs, vars, Constraint.Rel.EQ, -le.constant))) return false;
                FdVar zero = ctx().store.newVar("_c0", IntervalDomain.singleton(0));
                return ctx().store.addConstraint(new Constraint.Cmp(d, Constraint.Rel.NE, zero));
                // END_CHANGE: ISS-2025-0358
            }
            FdVar a = operandVar(left, bindings);                         // non-linear fallback
            FdVar b = operandVar(right, bindings);
            if (a == null || b == null) return false;
            return ctx().store.addConstraint(new Constraint.Cmp(a, Constraint.Rel.NE, b));
        }

        // Non-linear hook: X mod M #= R  (M a positive integer constant).  ISS-2025-0303.
        if (rel == Constraint.Rel.EQ) {
            Constraint mod = tryMod(left, right, bindings);
            if (mod == null) mod = tryMod(right, left, bindings);
            if (mod != null) return ctx().store.addConstraint(mod);
        }

        // Linear: compile (Left - Right) into  sum(ci*xi) + k0,  then  sum(ci*xi) <rel'> (-k0).
        LinExpr le = new LinExpr();
        if (!compile(left, 1, le, bindings)) return false;
        if (!compile(right, -1, le, bindings)) return false;

        long[] coeffs = new long[le.terms.size()];
        FdVar[] vars = new FdVar[le.terms.size()];
        int i = 0;
        for (Map.Entry<FdVar, Long> e : le.terms.entrySet()) { coeffs[i] = e.getValue(); vars[i] = e.getKey(); i++; }

        long k;
        Constraint.Rel lrel;
        switch (rel) {
            case EQ: lrel = Constraint.Rel.EQ; k = -le.constant; break;
            case LE: lrel = Constraint.Rel.LE; k = -le.constant; break;
            case GE: lrel = Constraint.Rel.GE; k = -le.constant; break;
            case LT: lrel = Constraint.Rel.LE; k = -le.constant - 1; break;   // x < y  <=>  x =< y-1
            case GT: lrel = Constraint.Rel.GE; k = -le.constant + 1; break;
            default: return false;
        }
        if (vars.length == 0) {                          // constant relation, no variables
            switch (lrel) { case EQ: return 0 == k; case LE: return 0 <= k; case GE: return 0 >= k; default: return false; }
        }
        return ctx().store.addConstraint(new Constraint.Linear(coeffs, vars, lrel, k));
    }

    /** If {@code modSide} is {@code mod(X, Mconst)}, build {@code Z = X mod M} with Z = {@code other}. */
    private static Constraint tryMod(Term modSide, Term other, Map<String, Term> bindings) {
        Term t = modSide.resolveBindings(bindings);
        if (!(t instanceof CompoundTerm)) return null;
        CompoundTerm c = (CompoundTerm) t;
        if (!"mod".equals(c.getName()) || c.getArguments().size() != 2) return null;
        Term mT = c.getArguments().get(1).resolveBindings(bindings);
        if (!(mT instanceof Number) || !((Number) mT).isInteger()) return null;
        long m = ((Number) mT).longValue();
        if (m <= 0) return null;
        FdVar x = operandVar(c.getArguments().get(0), bindings);
        FdVar z = operandVar(other, bindings);
        if (x == null || z == null) return null;
        return new Constraint.Mod(x, m, z);
    }

    /** all_different(List). */
    public static boolean postAllDifferent(List<Term> elems, Map<String, Term> bindings) {
        // START_CHANGE: ISS-2025-0356 - bracket the post so backtracking undoes it
        return guardedPost(() -> {
            List<FdVar> vs = new ArrayList<>();
            for (Term e : elems) {
                FdVar fv = operandVar(e, bindings);
                if (fv == null) return false;
                vs.add(fv);
            }
            return ctx().store.addConstraint(new Constraint.AllDifferent(vs));
        });
        // END_CHANGE: ISS-2025-0356
    }

    // ----------------------------------------------------------------- labeling

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
            // the store, so EVERY variable whose domain is (now) a singleton comes out bound — not
            // just the ones in the label list (functionally-determined vars, e.g. D #= C*2+1).
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

    /** {@code N}, {@code Lo..Hi} or {@code A \\/ B} for a domain (ISS-2025-0460: shared with
     *  {@link #domainTermForCell}). */
    private static Term domainToTerm(IntervalDomain d) {
        if (d.isEmpty()) return new Atom("{}");
        long[][] rs = d.rangeArray();
        Term acc = null;
        for (long[] r : rs) {
            Term part = (r[0] == r[1])
                ? new Number(r[0])
                : new CompoundTerm(new Atom(".."), java.util.Arrays.asList(new Number(r[0]), new Number(r[1])));
            acc = (acc == null) ? part : new CompoundTerm(new Atom("\\/"), java.util.Arrays.asList(acc, part));
        }
        return acc;
    }

    /** fd_size(Var, N): the number of values in the domain (capped at Long.MAX_VALUE). */
    public static long domainSize(Term varTerm, Map<String, Term> bindings) {
        Term t = varTerm.resolveBindings(bindings);
        if (t instanceof Number) return 1;
        if (t instanceof Variable) return ctx().store.dom(varFor((Variable) t)).size();
        return 0;
    }

    // ----------------------------------------------------------------- expression compiler

    /** A linear expression:  sum(coeff_i * var_i) + constant. */
    private static final class LinExpr {
        final Map<FdVar, Long> terms = new HashMap<>();
        long constant = 0;
        void addVar(FdVar v, long c) { terms.merge(v, c, Long::sum); }
    }

    /** Compile {@code term} (scaled by {@code sign}) into {@code into}. Non-linear subterms
     *  (products, abs, min, max, mod) are folded into auxiliary FD variables backed by their
     *  dedicated propagators; a genuinely unsupported expression raises
     *  {@code type_error(evaluable, F/N)} instead of silently failing (ISS-2025-0421). */
    private static boolean compile(Term term, long sign, LinExpr into, Map<String, Term> bindings) {
        Term t = term.resolveBindings(bindings);
        if (t instanceof Number) {
            requireInt((Number) t);                          // ISS-2025-0299: float -> type_error(integer)
            into.constant += sign * ((Number) t).longValue();
            return true;
        }
        if (t instanceof Variable) {
            into.addVar(varFor((Variable) t), sign);
            return true;
        }
        if (t instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) t;
            String f = c.getName();
            List<Term> a = c.getArguments();
            if ("+".equals(f) && a.size() == 2) {
                return compile(a.get(0), sign, into, bindings) && compile(a.get(1), sign, into, bindings);
            }
            if ("-".equals(f) && a.size() == 2) {
                return compile(a.get(0), sign, into, bindings) && compile(a.get(1), -sign, into, bindings);
            }
            if ("-".equals(f) && a.size() == 1) {
                return compile(a.get(0), -sign, into, bindings);
            }
            if ("*".equals(f) && a.size() == 2) {
                Long cst = constOf(a.get(0), bindings);
                Term other = a.get(1);
                if (cst == null) { cst = constOf(a.get(1), bindings); other = a.get(0); }
                if (cst != null) return compileScaled(other, sign * cst, into, bindings);
                // START_CHANGE: ISS-2025-0421 - var*var products: P = A*B via interval propagation
                // (previously "non-linear -> return false" made X*X #= 16 silently fail though
                // satisfiable). The X*X case gets the tighter Square propagator.
                FdVar va = exprVar(a.get(0), bindings);
                FdVar vb = exprVar(a.get(1), bindings);
                FdVar p = auxVar("_p");
                Constraint prod = (va == vb) ? new Constraint.Square(va, p) : new Constraint.Mul(va, vb, p);
                if (!ctx().store.addConstraint(prod)) throw new Unsat();
                into.addVar(p, sign);
                return true;
                // END_CHANGE: ISS-2025-0421
            }
            // START_CHANGE: ISS-2025-0421 - abs/min/max/mod folded onto their existing propagators
            if ("abs".equals(f) && a.size() == 1) {
                FdVar vx = exprVar(a.get(0), bindings);
                FdVar y = ctx().store.newVar("_a" + (ctx().aux++),
                    IntervalDomain.interval(0, Long.MAX_VALUE / 2));
                if (!ctx().store.addConstraint(new Constraint.Abs(vx, y))) throw new Unsat();
                into.addVar(y, sign);
                return true;
            }
            if (("min".equals(f) || "max".equals(f)) && a.size() == 2) {
                FdVar vx = exprVar(a.get(0), bindings);
                FdVar vy = exprVar(a.get(1), bindings);
                FdVar z = auxVar("_m");
                Constraint mm = "min".equals(f) ? new Constraint.Min(vx, vy, z)
                                                : new Constraint.Max(vx, vy, z);
                if (!ctx().store.addConstraint(mm)) throw new Unsat();
                into.addVar(z, sign);
                return true;
            }
            if ("mod".equals(f) && a.size() == 2) {
                Long m = constOf(a.get(1), bindings);
                if (m != null && m > 0) {                 // same support as tryMod, but composable
                    FdVar vx = exprVar(a.get(0), bindings);
                    FdVar z = ctx().store.newVar("_r" + (ctx().aux++),
                        IntervalDomain.interval(0, m - 1));
                    if (!ctx().store.addConstraint(new Constraint.Mod(vx, m, z))) throw new Unsat();
                    into.addVar(z, sign);
                    return true;
                }
            }
            // END_CHANGE: ISS-2025-0421
        }
        // START_CHANGE: ISS-2025-0421 - unsupported arithmetic must raise a clear error: a
        // constraint system answering "false" to a satisfiable query is unsound.
        throw unsupportedExpr(t);
        // END_CHANGE: ISS-2025-0421
    }

    // START_CHANGE: ISS-2025-0421 - helpers for folding non-linear subterms into auxiliary FdVars

    /** Marker: an auxiliary constraint post wiped a domain — the enclosing comparison must FAIL
     *  (the constraint is unsatisfiable), not error; caught in {@link #postCmp}. */
    private static final class Unsat extends RuntimeException {
        Unsat() { super(null, null, false, false); }
    }

    /** Fresh auxiliary variable on a domain wide enough to hold any product/difference of two
     *  default-domain variables (the propagators saturate instead of overflowing). */
    private static FdVar auxVar(String prefix) {
        return ctx().store.newVar(prefix + (ctx().aux++),
            IntervalDomain.interval(Long.MIN_VALUE / 2, Long.MAX_VALUE / 2));
    }

    /** Compile an arbitrary supported expression down to a single FdVar: variables and integer
     *  constants map directly; anything else becomes an auxiliary variable D with D = expr posted
     *  as a Linear EQ (which may itself recurse through {@link #compile} for nested operators). */
    private static FdVar exprVar(Term t, Map<String, Term> bindings) {
        Term r = t.resolveBindings(bindings);
        if (r instanceof Variable) return varFor((Variable) r);
        if (r instanceof Number) {
            requireInt((Number) r);
            long v = ((Number) r).longValue();
            return ctx().store.newVar("_c" + v, IntervalDomain.singleton(v));
        }
        LinExpr le = new LinExpr();
        if (!compile(r, 1, le, bindings)) throw new Unsat();      // defensive: compile errors instead
        le.terms.values().removeIf(co -> co == 0);
        if (le.constant == 0 && le.terms.size() == 1) {
            Map.Entry<FdVar, Long> e = le.terms.entrySet().iterator().next();
            if (e.getValue() == 1) return e.getKey();             // the expression IS a variable
        }
        long[] coeffs = new long[le.terms.size() + 1];
        FdVar[] vars = new FdVar[le.terms.size() + 1];
        int i = 0;
        for (Map.Entry<FdVar, Long> e : le.terms.entrySet()) { coeffs[i] = e.getValue(); vars[i] = e.getKey(); i++; }
        FdVar d = auxVar("_e");
        coeffs[i] = -1; vars[i] = d;                              // sum(ci*xi) - D = -k0  <=>  D = expr
        if (!ctx().store.addConstraint(new Constraint.Linear(coeffs, vars, Constraint.Rel.EQ, -le.constant))) {
            throw new Unsat();
        }
        return d;
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

    private static boolean compileScaled(Term t, long scale, LinExpr into, Map<String, Term> bindings) {
        Term r = t.resolveBindings(bindings);
        if (r instanceof Number) { into.constant += scale * ((Number) r).longValue(); return true; }
        if (r instanceof Variable) { into.addVar(varFor((Variable) r), scale); return true; }
        return compile(r, scale, into, bindings); // nested expression
    }

    private static Long constOf(Term t, Map<String, Term> bindings) {
        Term r = t.resolveBindings(bindings);
        if (!(r instanceof Number)) return null;
        // START_CHANGE: ISS-2025-0421 - 2.5*X must raise type_error(integer, 2.5), not truncate to 2*X
        requireInt((Number) r);
        // END_CHANGE: ISS-2025-0421
        return ((Number) r).longValue();
    }

    /** Map a simple operand (variable or constant) to an FdVar; constants become singleton vars. */
    private static FdVar operandVar(Term t, Map<String, Term> bindings) {
        Term r = t.resolveBindings(bindings);
        if (r instanceof Variable) return varFor((Variable) r);
        if (r instanceof Number) {
            requireInt((Number) r);                          // ISS-2025-0299
            long v = ((Number) r).longValue();
            return ctx().store.newVar("_c" + v, IntervalDomain.singleton(v));
        }
        return null;
    }

    /** CLP(FD) is over integers within long range: reject floats (type_error) and out-of-range
     *  big integers (representation_error) instead of silently truncating. */
    private static void requireInt(Number n) {
        if (!n.isInteger()) throw new PrologException(ISOErrorTerms.typeError("integer", n, "clpfd"));
        if (n.bigIntegerValue().bitLength() > 63) {              // ISS-2025-0299: would truncate
            throw new PrologException(ISOErrorTerms.representationError("max_integer", "clpfd"));
        }
    }
}
