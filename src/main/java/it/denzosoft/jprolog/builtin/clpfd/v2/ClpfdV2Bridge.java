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
    }

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
        }
        return fv;
    }

    // ----------------------------------------------------------------- domain posting

    /** Post {@code Var in Lo..Hi}. Returns false on inconsistency. */
    public static boolean postIn(Term varTerm, long lo, long hi, Map<String, Term> bindings) {
        Term t = varTerm.resolveBindings(bindings);
        if (t instanceof Number) {                       // a constant must lie in the range
            long val = ((Number) t).longValue();
            return val >= lo && val <= hi;
        }
        if (!(t instanceof Variable)) return false;
        FdVar fv = varFor((Variable) t);
        return ctx().store.narrow(fv, IntervalDomain.interval(lo, hi)) && ctx().store.propagate();
    }

    // ----------------------------------------------------------------- comparison posting

    /** Post a comparison {@code Left <rel> Right} where each side is a linear expression. */
    public static boolean postCmp(Term left, Constraint.Rel rel, Term right, Map<String, Term> bindings) {
        // \= : compile (left - right) into a linear form so expression operands work
        // (ISS-2025-0301), e.g. X+1 #\= 5 -> X #\= 4. Handles 0/1-variable cases exactly; falls
        // back to a direct Cmp NE for the general (multi-variable) case.
        if (rel == Constraint.Rel.NE) {
            LinExpr le = new LinExpr();
            if (compile(left, 1, le, bindings) && compile(right, -1, le, bindings)) {
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
            }
            FdVar a = operandVar(left, bindings);                         // general / non-linear fallback
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
        List<FdVar> vs = new ArrayList<>();
        for (Term e : elems) {
            FdVar fv = operandVar(e, bindings);
            if (fv == null) return false;
            vs.add(fv);
        }
        return ctx().store.addConstraint(new Constraint.AllDifferent(vs));
    }

    // ----------------------------------------------------------------- labeling

    /** Label the given variables; returns one binding map per solution (bound to the values). */
    public static List<Map<String, Term>> label(List<Term> varTerms, Map<String, Term> bindings) {
        List<FdVar> fdVars = new ArrayList<>();
        List<Variable> engineVars = new ArrayList<>();
        for (Term t : varTerms) {
            Term r = t.resolveBindings(bindings);
            if (r instanceof Variable) {
                engineVars.add((Variable) r);
                fdVars.add(varFor((Variable) r));
            }
            // already-ground numbers need no labeling
        }
        List<Map<FdVar, Long>> sols;
        try {
            sols = Labeler.labelAll(ctx().store, fdVars);     // ISS-2025-0298
        } catch (Labeler.TooLargeToLabel e) {
            throw new PrologException(ISOErrorTerms.resourceError("clpfd_label_domain_too_large", "label/1"));
        }
        List<Map<String, Term>> out = new ArrayList<>();
        for (Map<FdVar, Long> sol : sols) {
            Map<String, Term> b = new HashMap<>(bindings);
            for (int i = 0; i < engineVars.size(); i++) {
                b.put(engineVars.get(i).getName(), new Number(sol.get(fdVars.get(i))));
            }
            out.add(b);
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

    /** Compile {@code term} (scaled by {@code sign}) into {@code into}. Returns false if non-linear. */
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
                if (cst == null) return false;            // var*var is non-linear
                return compileScaled(other, sign * cst, into, bindings);
            }
        }
        return false;
    }

    private static boolean compileScaled(Term t, long scale, LinExpr into, Map<String, Term> bindings) {
        Term r = t.resolveBindings(bindings);
        if (r instanceof Number) { into.constant += scale * ((Number) r).longValue(); return true; }
        if (r instanceof Variable) { into.addVar(varFor((Variable) r), scale); return true; }
        return compile(r, scale, into, bindings); // nested expression
    }

    private static Long constOf(Term t, Map<String, Term> bindings) {
        Term r = t.resolveBindings(bindings);
        return (r instanceof Number) ? ((Number) r).longValue() : null;
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
