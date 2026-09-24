package it.denzosoft.jprolog.builtin.clpfd.v2;

import it.denzosoft.jprolog.builtin.clpfd.v2.ClpStore.FdVar;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

/**
 * START_CHANGE: ISS-2025-0760 - 4.6 wave Q5.1: the residual goals of the CLP(FD) store
 * (SWI's {@code clpfd:attribute_goals//1}).
 *
 * <p>One instance renders the constraints still alive on a set of FD cells, each constraint
 * ONCE, in SWI-Prolog 9's printed forms where they are known:
 * <ul>
 *   <li>{@code X in Dom} for a cell whose domain is not {@code inf..sup}, or that has no live
 *       constraint left (SWI: {@code default_domain(Dom), \+ all_dead_(Ps)} suppresses it);</li>
 *   <li>a two-variable inequality as {@code Y#=<X+ -1} / {@code X#>=Y} (SWI's
 *       {@code x_leq_y_plus_c} / {@code pgeq}); {@code Y+Z#=X} for {@code X #= Y+Z} (pplus);
 *       {@code Y+2#=X}; {@code 2*Y#=X}; {@code X#\=Y}, {@code X#\=Y+C}; a longer linear
 *       constraint by SWI's {@code scalar_product_left_right/4} layout (positive coefficients
 *       left, negated negative ones right, the constant on the side that keeps it positive);</li>
 *   <li>the non-linear propagators as SWI prints them ({@code X*Y#=Z}, {@code Z#=max(X,Y)},
 *       {@code X mod 3#=Z}, ...), reified comparisons as {@code X#>=4#<==>B}, the connectives
 *       as {@code X#\/Y#<==>B}, and the globals by name ({@code all_different/1},
 *       {@code all_distinct/1}, {@code element/3}, {@code tuples_in/2},
 *       {@code global_cardinality/2}, {@code circuit/1}, {@code cumulative/2}).</li>
 * </ul>
 * Variables bound (or fixed by propagation) print as their values and are folded into the
 * constant of a linear form, as SWI's printer does. An auxiliary variable of the compiled
 * store (the product in {@code X #= Y*Z+W}) prints as a fresh variable, and its own domain and
 * defining constraint follow, which is also what SWI shows ({@code _A}).
 */
public final class Residuals {

    private final ClpStore store;
    private final IdentityHashMap<FdVar, Variable> cellOf;
    private final IdentityHashMap<FdVar, Variable> auxOf = new IdentityHashMap<>();
    private final List<FdVar> auxQueue = new ArrayList<>();
    private final IdentityHashMap<Constraint, Boolean> printed = new IdentityHashMap<>();
    private final IdentityHashMap<FdVar, Boolean> visited = new IdentityHashMap<>();
    private final IdentityHashMap<FdVar, Variable> reached = new IdentityHashMap<>();
    private IdentityHashMap<Constraint, Integer> order;

    Residuals(ClpStore store, IdentityHashMap<FdVar, Variable> cellOf) {
        this.store = store;
        this.cellOf = cellOf;
    }

    ClpStore store() { return store; }

    // ------------------------------------------------------------------ per variable

    /** The goals of one FD variable: its domain (when SWI shows it) and its unprinted constraints. */
    void goalsOf(FdVar fv, Term self, List<Term> out) {
        if (visited.put(fv, Boolean.TRUE) != null) return;
        IntervalDomain d = store.dom(fv);
        if (d.isEmpty()) return;
        List<Constraint> cs = liveConstraints(fv);
        boolean defaultDom = d.min() == IntervalDomain.INF && d.max() == IntervalDomain.SUP
            && d.intervalCount() == 1 && d.exactBig() == null;
        if (!(defaultDom && !cs.isEmpty()) && fixedValue(fv) == null) {
            out.add(new CompoundTerm(new Atom("in"), Arrays.asList(self, ClpfdV2Bridge.domainToTerm(d))));
        }
        for (Constraint c : cs) {
            if (printed.put(c, Boolean.TRUE) != null) continue;
            if (c.form == Constraint.FORM_HIDDEN) continue;
            c.render(this, out);
        }
    }

    /** Goals of the auxiliary variables reached while rendering (drains the queue). */
    void finish(List<Term> out) {
        for (int i = 0; i < auxQueue.size(); i++) {
            FdVar fv = auxQueue.get(i);
            if (fixedValue(fv) != null || visited.containsKey(fv)) continue;
            IntervalDomain d = store.dom(fv);
            List<Term> tmp = new ArrayList<>();
            Variable cell = reached.get(fv);
            if (cell != null) {                          // an FD cell outside the answer's terms
                goalsOf(fv, cell, tmp);
                out.addAll(tmp);
                continue;
            }
            goalsOf(fv, auxOf.get(fv), tmp);
            // an auxiliary variable with the default domain is shown only through its constraints
            if (!tmp.isEmpty() && d.min() == IntervalDomain.INF && d.max() == IntervalDomain.SUP
                    && isInGoal(tmp.get(0))) {
                tmp.remove(0);
            }
            out.addAll(tmp);
        }
    }

    private static boolean isInGoal(Term t) {
        return t instanceof CompoundTerm && "in".equals(((CompoundTerm) t).getName())
            && ((CompoundTerm) t).getArguments().size() == 2;
    }

    /** The constraints on {@code fv} that are still alive, in posting order. */
    List<Constraint> liveConstraints(FdVar fv) {
        List<Constraint> cs = new ArrayList<>(fv.watchers.size() + fv.fixWatchers.size());
        for (Constraint c : fv.watchers) if (c.alive(store)) cs.add(c);
        for (Constraint c : fv.fixWatchers) if (c.alive(store)) cs.add(c);
        if (cs.size() > 1) {
            if (order == null) {
                order = new IdentityHashMap<>();
                List<Constraint> all = store.constraints();
                for (int i = 0; i < all.size(); i++) order.put(all.get(i), i);
            }
            Collections.sort(cs, new Comparator<Constraint>() {
                @Override public int compare(Constraint a, Constraint b) {
                    Integer ia = order.get(a), ib = order.get(b);
                    return Integer.compare(ia == null ? Integer.MAX_VALUE : ia, ib == null ? Integer.MAX_VALUE : ib);
                }
            });
        }
        return cs;
    }

    // ------------------------------------------------------------------ terms

    /** The exact value of a fixed variable, or null. */
    BigInteger fixedValue(FdVar v) {
        IntervalDomain d = store.dom(v);
        if (d.isSingleton()) return BigInteger.valueOf(d.value());
        return d.exactBig();
    }

    /** The term a store variable prints as: its value, its engine cell, or a fresh variable. */
    Term term(FdVar v) {
        BigInteger x = fixedValue(v);
        if (x != null) return num(x);
        Variable cell = cellOf.get(v);
        if (cell != null) {
            Term t = deref(cell);
            // SWI's term_attvars/2 follows the constraints: an FD cell reached through one is
            // rendered too (after the answer's own variables)
            if (t instanceof Variable && !visited.containsKey(v) && !reached.containsKey(v)) {
                reached.put(v, (Variable) t);
                auxQueue.add(v);
            }
            return t;
        }
        Variable aux = auxOf.get(v);
        if (aux == null) {
            aux = new Variable("_");
            auxOf.put(v, aux);
            auxQueue.add(v);
        }
        return aux;
    }

    List<Term> terms(List<FdVar> vs) {
        List<Term> out = new ArrayList<>(vs.size());
        for (FdVar v : vs) out.add(term(v));
        return out;
    }

    static Term deref(Term t) {
        while (t instanceof Variable && ((Variable) t).ref != null) t = ((Variable) t).ref;
        return t;
    }

    static Term num(BigInteger v) {
        return v.bitLength() <= 63 ? Number.valueOf(v.longValue()) : new Number(v);
    }

    static Term num(long v) { return Number.valueOf(v); }

    static Term op(String name, Term a, Term b) {
        return new CompoundTerm(new Atom(name), Arrays.asList(a, b));
    }

    static Term op(String name, Term a) {
        return new CompoundTerm(new Atom(name), Arrays.asList(a));
    }

    static Term list(List<Term> xs) {
        Term acc = new Atom("[]");
        for (int i = xs.size() - 1; i >= 0; i--) acc = op(".", xs.get(i), acc);
        return acc;
    }

    // ------------------------------------------------------------------ linear normal forms

    /** A term with a coefficient, for sorting by variable age. */
    private static final class CV {
        BigInteger c;
        final Term v;
        CV(BigInteger c, Term v) { this.c = c; this.v = v; }
    }

    private static long age(Term t) {
        return (t instanceof Variable) ? ((Variable) t).serial : Long.MAX_VALUE;
    }

    /**
     * {@code sum(cs[i]*vs[i]) rel k} in SWI's printed form, or null when nothing is left to say
     * (every variable fixed, or a single variable whose domain already says it).
     *
     * @param reified the inner form of a reified constraint (SWI prints it un-normalised:
     *                {@code X#>=4#<==>B})
     */
    Term linear(BigInteger[] cs, FdVar[] vs, Constraint.Rel rel, BigInteger k, boolean reified) {
        List<CV> ts = new ArrayList<>(vs.length);
        IdentityHashMap<Term, CV> byVar = new IdentityHashMap<>();
        for (int i = 0; i < vs.length; i++) {
            if (cs[i].signum() == 0) continue;
            BigInteger x = fixedValue(vs[i]);
            if (x != null) { k = k.subtract(cs[i].multiply(x)); continue; }
            Term t = term(vs[i]);
            CV old = byVar.get(t);
            if (old != null) { old.c = old.c.add(cs[i]); continue; }
            CV cv = new CV(cs[i], t);
            byVar.put(t, cv);
            ts.add(cv);
        }
        ts.removeIf(cv -> cv.c.signum() == 0);
        if (ts.isEmpty()) return null;
        Collections.sort(ts, new Comparator<CV>() {
            @Override public int compare(CV a, CV b) { return Long.compare(age(a.v), age(b.v)); }
        });
        if (reified) return scalarLeftRight(ts, k, opName(rel));
        if (ts.size() == 1) return null;
        switch (rel) {
            case LE: {                                           // negate into GE
                for (CV cv : ts) cv.c = cv.c.negate();
                return geq(ts, k.negate());
            }
            case LT: {
                for (CV cv : ts) cv.c = cv.c.negate();
                return geq(ts, k.negate().add(BigInteger.ONE));
            }
            case GT: return geq(ts, k.add(BigInteger.ONE));
            case GE: return geq(ts, k);
            case EQ: return eq(ts, k);
            default: return neq(ts, k);
        }
    }

    private static String opName(Constraint.Rel rel) {
        switch (rel) {
            case EQ: return "#=";
            case NE: return "#\\=";
            case LT: return "#<";
            case LE: return "#=<";
            case GT: return "#>";
            default: return "#>=";
        }
    }

    private static boolean unit(CV cv, int sign) { return cv.c.equals(BigInteger.valueOf(sign)); }

    /** SWI's clpfd_geq_/scalar_product_ cases for {@code sum >= k}. */
    private static Term geq(List<CV> ts, BigInteger k) {
        if (ts.size() == 2) {
            CV a = ts.get(0), b = ts.get(1);
            if (unit(a, 1) && unit(b, -1)) {                     // A - B >= k
                return k.signum() == 0 ? op("#>=", a.v, b.v)
                                       : op("#=<", b.v, op("+", a.v, num(k.negate())));
            }
            if (unit(a, -1) && unit(b, 1)) {                     // B - A >= k
                return k.signum() == 0 ? op("#>=", b.v, a.v)
                                       : op("#=<", a.v, op("+", b.v, num(k.negate())));
            }
        }
        for (CV cv : ts) cv.c = cv.c.negate();                   // scalar_product_leq
        return scalarLeftRight(ts, k.negate(), "#=<");
    }

    /** SWI's clpfd_equal_/scalar_product_ cases for {@code sum = k}. */
    private static Term eq(List<CV> ts, BigInteger k) {
        BigInteger g = BigInteger.ZERO;
        for (CV cv : ts) g = g.gcd(cv.c);
        if (g.signum() > 0 && !g.equals(BigInteger.ONE) && k.mod(g).signum() == 0) {
            for (CV cv : ts) cv.c = cv.c.divide(g);
            k = k.divide(g);
        }
        if (ts.size() == 2) {
            CV a = ts.get(0), b = ts.get(1);
            if (unit(a, 1) && unit(b, 1)) return op("#=", op("+", a.v, b.v), num(k));
            if (unit(a, -1) && unit(b, -1)) return op("#=", op("+", a.v, b.v), num(k.negate()));
            if ((unit(a, 1) && unit(b, -1)) || (unit(a, -1) && unit(b, 1))) {   // U - W = k
                if (k.signum() == 0) return op("#=", a.v, b.v);
                Term u = a.c.signum() > 0 ? a.v : b.v, w = a.c.signum() > 0 ? b.v : a.v;
                // SWI's pplus form W+k#=U (X #= Y+2 prints Y+2#=X), with the constant kept
                // positive: X #= Y-2 prints X+2#=Y
                return k.signum() > 0 ? op("#=", op("+", w, num(k)), u) : op("#=", op("+", u, num(k.negate())), w);
            }
            if (k.signum() == 0) {                               // A = c*B (SWI ptimes: c*B#=A)
                if (a.c.abs().equals(BigInteger.ONE)) return op("#=", op("*", num(b.c.multiply(a.c).negate()), b.v), a.v);
                if (b.c.abs().equals(BigInteger.ONE)) return op("#=", op("*", num(a.c.multiply(b.c).negate()), a.v), b.v);
            }
        }
        if (ts.size() == 3 && k.signum() == 0) {
            Term plus = pplus(ts);
            if (plus != null) return plus;
        }
        return scalarLeftRight(ts, k, "#=");
    }

    /** {@code A+B#=C} (SWI pplus) for three unit coefficients split two against one. */
    private static Term pplus(List<CV> ts) {
        int pos = 0;
        for (CV cv : ts) {
            if (!cv.c.abs().equals(BigInteger.ONE)) return null;
            if (cv.c.signum() > 0) pos++;
        }
        if (pos != 1 && pos != 2) return null;
        int oddSign = (pos == 1) ? 1 : -1;
        CV odd = null;
        List<Term> two = new ArrayList<>(2);
        for (CV cv : ts) {
            if (cv.c.signum() == oddSign) odd = cv; else two.add(cv.v);
        }
        return op("#=", op("+", two.get(0), two.get(1)), odd.v);
    }

    /** SWI's clpfd_neq cases for {@code sum =\= k}. */
    private static Term neq(List<CV> ts, BigInteger k) {
        if (ts.size() == 2) {
            CV a = ts.get(0), b = ts.get(1);
            if (unit(a, 1) && unit(b, -1)) {
                return k.signum() == 0 ? op("#\\=", a.v, b.v) : op("#\\=", a.v, op("+", b.v, num(k)));
            }
            if (unit(a, -1) && unit(b, 1)) {
                return k.signum() == 0 ? op("#\\=", a.v, b.v) : op("#\\=", b.v, op("+", a.v, num(k)));
            }
        }
        if (ts.size() == 3 && k.signum() == 0) {
            Term plus = pplus(ts);
            if (plus != null) {                                  // X #\= Y+Z (x_neq_y_plus_z)
                List<Term> as = ((CompoundTerm) plus).getArguments();
                return op("#\\=", as.get(1), as.get(0));
            }
        }
        return scalarLeftRight(ts, k, "#\\=");
    }

    /**
     * SWI's {@code scalar_product_left_right/4}: {@code sum(c*v) Op k} as
     * {@code Positives Op Negatives} with the constant on the side that keeps it positive.
     */
    private static Term scalarLeftRight(List<CV> ts, BigInteger k, String opName) {
        List<CV> pos = new ArrayList<>(), neg = new ArrayList<>();
        for (CV cv : ts) {
            if (cv.c.signum() > 0) pos.add(cv); else neg.add(new CV(cv.c.negate(), cv.v));
        }
        Term left0 = plusTerm(pos), right0 = plusTerm(neg);
        Term left, right;
        if (k.signum() == 0) {
            left = left0 == null ? num(0) : left0;
            right = right0 == null ? num(0) : right0;
        } else if (right0 == null) {
            left = left0; right = num(k);
        } else if (left0 == null) {
            left = num(k.negate()); right = right0;
        } else if (k.signum() > 0) {
            left = left0; right = op("+", right0, num(k));
        } else {
            left = op("+", left0, num(k.negate())); right = right0;
        }
        return op(opName, left, right);
    }

    private static Term plusTerm(List<CV> cvs) {
        Term acc = null;
        for (CV cv : cvs) {
            Term t = cv.c.equals(BigInteger.ONE) ? cv.v : op("*", num(cv.c), cv.v);
            acc = (acc == null) ? t : op("+", acc, t);
        }
        return acc;
    }
}
// END_CHANGE: ISS-2025-0760
