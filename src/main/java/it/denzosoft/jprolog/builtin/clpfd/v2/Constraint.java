package it.denzosoft.jprolog.builtin.clpfd.v2;

import it.denzosoft.jprolog.builtin.clpfd.v2.ClpStore.FdVar;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static it.denzosoft.jprolog.builtin.clpfd.v2.IntervalDomain.INF;
import static it.denzosoft.jprolog.builtin.clpfd.v2.IntervalDomain.SUP;

/**
 * A CLP(FD) constraint. {@link #propagate(ClpStore)} narrows the domains of its variables to
 * (a superset of) the consistent assignments and returns false on a wipeout. Constraints are
 * re-awoken by the store's propagation queue whenever a watched variable narrows.
 *
 * <p>START_CHANGE: ISS-2025-0644 - every propagator is infinity-aware: a bound equal to
 * {@link IntervalDomain#INF}/{@link IntervalDomain#SUP} is an infinity, arithmetic on it stays
 * infinite, and a finite result that does not fit a {@code long} saturates to the matching
 * infinity (which can only lose pruning, never a solution). END_CHANGE: ISS-2025-0644
 */
public abstract class Constraint {

    /** Entailment status of a constraint given the current domains (for reification). */
    public enum Entail { TRUE, FALSE, UNKNOWN }

    /** In the owning store's propagation queue (ISS-2025-0642: replaces a HashSet lookup). */
    boolean queued;

    /**
     * True when the constraint can only prune once one of its variables becomes FIXED, so a
     * narrowing that leaves the variable unfixed need not wake it (ISS-2025-0642: that is what
     * made every N-queens assignment wake every disequality of every narrowed column).
     */
    protected boolean wakesOnFixOnly() { return false; }

    public abstract List<FdVar> variables();

    /** Narrow domains; return false if the constraint is unsatisfiable in the current store. */
    public abstract boolean propagate(ClpStore s);

    /** Is the constraint already entailed/disentailed by the current domains? (Default: unknown.) */
    public Entail entailment(ClpStore s) { return Entail.UNKNOWN; }

    // START_CHANGE: ISS-2025-0644 - exact values beyond the 64-bit domain range. When every other
    // variable of a functional constraint is fixed, the remaining one has ONE exact value, which may
    // not fit a long bound (X #= Y*10^12, Y = 10^12): the store keeps a clamped, degenerate domain
    // for it and the bridge asks the constraint for the exact value to bind (SWI answers 10^24).
    /** The exact value {@code v} must take when every other variable is fixed, or null. */
    public BigInteger solveFor(FdVar v, ClpStore s) { return null; }

    /** The exact value of a fixed (finite singleton) domain, or null. */
    static BigInteger fixedValue(ClpStore s, FdVar v) {
        return s.exactValue(v);
    }
    // END_CHANGE: ISS-2025-0644

    // START_CHANGE: ISS-2025-0760 - residual goals (SWI's attribute_goals//1)
    /** Rendering hint for a constraint that stands for a different source form (#\ X). */
    public static final int FORM_NOT = 1;
    /** A constraint that is part of another one's residual goal (circuit/1's all_distinct). */
    public static final int FORM_HIDDEN = 2;
    int form;

    /**
     * Is the constraint still telling something the domains do not? SWI kills a propagator once it
     * is entailed; a dead constraint is not printed as a residual goal. Default: some variable is
     * not fixed and the constraint is not entailed by the current domains.
     */
    public boolean alive(ClpStore s) {
        for (FdVar v : variables()) {
            IntervalDomain d = s.dom(v);
            if (!d.isSingleton() && d.exactBig() == null) return entailment(s) != Entail.TRUE;
        }
        return false;
    }

    /** Append this constraint's residual goal(s), in SWI's printed form, to {@code out}. */
    void render(Residuals r, List<it.denzosoft.jprolog.core.terms.Term> out) { }

    /** The constraint as the inner goal of a reification ({@code X#>=4} in {@code X#>=4#<==>B}). */
    it.denzosoft.jprolog.core.terms.Term reifiedForm(Residuals r) { return null; }

    static final BigInteger[] UNIT_PAIR = {BigInteger.ONE, BigInteger.ONE.negate()};

    static void emit(List<it.denzosoft.jprolog.core.terms.Term> out, it.denzosoft.jprolog.core.terms.Term t) {
        if (t != null) out.add(t);
    }
    // END_CHANGE: ISS-2025-0760

    /** The logical negation of this constraint (override where reification is supported). */
    public Constraint negation() {
        throw new UnsupportedOperationException("negation not supported for " + getClass().getSimpleName());
    }

    // START_CHANGE: ISS-2025-0644 - infinity-aware, saturating long arithmetic
    static boolean isInf(long v) { return v == INF || v == SUP; }

    static long neg(long v) {
        if (v == INF) return SUP;
        if (v == SUP) return INF;
        return -v;
    }

    static long add(long a, long b) {
        if (isInf(a)) return a;
        if (isInf(b)) return b;
        long r = a + b;
        if (((a ^ r) & (b ^ r)) < 0) return a > 0 ? SUP : INF; // overflow -> saturate to infinity
        return r;
    }

    static long sub(long a, long b) {
        return add(a, neg(b));
    }

    static long mul(long a, long b) {
        if (a == 0 || b == 0) return 0;
        if (isInf(a) || isInf(b)) return ((a > 0) == (b > 0)) ? SUP : INF;
        try {
            return Math.multiplyExact(a, b);
        } catch (ArithmeticException e) {
            return ((a > 0) == (b > 0)) ? SUP : INF;
        }
    }

    /** Overflow-safe |v| (an infinity has infinite magnitude). */
    static long absSat(long v) { return isInf(v) ? SUP : Math.abs(v); }

    static long minOf(long a, long b, long c, long d) { return Math.min(Math.min(a, b), Math.min(c, d)); }
    static long maxOf(long a, long b, long c, long d) { return Math.max(Math.max(a, b), Math.max(c, d)); }

    /** Clamp an exact value to a long bound; beyond the representable range means infinite. */
    static long clamp(BigInteger v) {
        if (v.compareTo(BIG_SUP_M1) > 0) return SUP;
        if (v.compareTo(BIG_INF_P1) < 0) return INF;
        return v.longValue();
    }
    static final BigInteger BIG_SUP_M1 = BigInteger.valueOf(SUP - 1);
    static final BigInteger BIG_INF_P1 = BigInteger.valueOf(INF + 1);
    // END_CHANGE: ISS-2025-0644

    // =====================================================================================
    // Binary comparison: a <rel> b   (LT, LE, GT, GE, EQ, NE)
    // =====================================================================================
    public enum Rel { LT, LE, GT, GE, EQ, NE }

    public static final class Cmp extends Constraint {
        private final FdVar a, b;
        private final Rel rel;
        public Cmp(FdVar a, Rel rel, FdVar b) { this.a = a; this.rel = rel; this.b = b; }
        // START_CHANGE: ISS-2025-0760 - residual goal
        @Override void render(Residuals r, List<it.denzosoft.jprolog.core.terms.Term> out) {
            emit(out, r.linear(UNIT_PAIR, new FdVar[]{a, b}, rel, BigInteger.ZERO, false));
        }
        @Override it.denzosoft.jprolog.core.terms.Term reifiedForm(Residuals r) {
            return r.linear(UNIT_PAIR, new FdVar[]{a, b}, rel, BigInteger.ZERO, true);
        }
        // END_CHANGE: ISS-2025-0760

        @Override public List<FdVar> variables() { return Arrays.asList(a, b); }

        @Override public BigInteger solveFor(FdVar v, ClpStore s) {      // ISS-2025-0644
            if (rel != Rel.EQ || a == b) return null;
            return v == a ? fixedValue(s, b) : v == b ? fixedValue(s, a) : null;
        }

        @Override public boolean propagate(ClpStore s) {
            IntervalDomain da = s.dom(a), db = s.dom(b);
            if (da.isEmpty() || db.isEmpty()) return false;
            switch (rel) {
                case LT: // a < b
                    if (!s.removeAbove(a, sub(db.max(), 1))) return false;
                    if (!s.removeBelow(b, add(s.dom(a).min(), 1))) return false;
                    return true;
                case LE: // a <= b
                    if (!s.removeAbove(a, db.max())) return false;
                    if (!s.removeBelow(b, s.dom(a).min())) return false;
                    return true;
                case GT: // a > b  ==  b < a
                    if (!s.removeBelow(a, add(db.min(), 1))) return false;
                    if (!s.removeAbove(b, sub(s.dom(a).max(), 1))) return false;
                    return true;
                case GE: // a >= b
                    if (!s.removeBelow(a, db.min())) return false;
                    if (!s.removeAbove(b, s.dom(a).max())) return false;
                    return true;
                case EQ: // a == b: domains must be equal -> intersect both (holes included)
                    if (!s.narrow(a, db)) return false;
                    if (!s.narrow(b, s.dom(a))) return false;
                    return true;
                case NE: // a != b: when one side is fixed, remove that value from the other
                    da = s.dom(a); db = s.dom(b);
                    if (da.isSingleton()) {
                        if (!s.removeValue(b, da.value())) return false;
                    }
                    db = s.dom(b);
                    if (db.isSingleton()) {
                        if (!s.removeValue(a, db.value())) return false;
                    }
                    return true;
                default:
                    return true;
            }
        }

        @Override public Entail entailment(ClpStore s) {
            IntervalDomain da = s.dom(a), db = s.dom(b);
            if (da.isEmpty() || db.isEmpty()) return Entail.FALSE;
            boolean disjoint = da.intersect(db).isEmpty();
            boolean equalFixed = da.isSingleton() && db.isSingleton() && da.value() == db.value();
            switch (rel) {
                case LT: if (da.max() < db.min()) return Entail.TRUE; if (da.min() >= db.max()) return Entail.FALSE; break;
                case LE: if (da.max() <= db.min()) return Entail.TRUE; if (da.min() > db.max()) return Entail.FALSE; break;
                case GT: if (da.min() > db.max()) return Entail.TRUE; if (da.max() <= db.min()) return Entail.FALSE; break;
                case GE: if (da.min() >= db.max()) return Entail.TRUE; if (da.max() < db.min()) return Entail.FALSE; break;
                case EQ: if (equalFixed) return Entail.TRUE; if (disjoint) return Entail.FALSE; break;
                case NE: if (disjoint) return Entail.TRUE; if (equalFixed) return Entail.FALSE; break;
            }
            return Entail.UNKNOWN;
        }

        // START_CHANGE: ISS-2025-0645 - the difference-constraint view used by the cycle check
        /** Append {from, to, weight} edges meaning {@code to =< from + weight}. */
        void differenceEdges(List<Object[]> out) {
            switch (rel) {
                case LT: out.add(new Object[]{b, a, -1L}); break;
                case LE: out.add(new Object[]{b, a, 0L}); break;
                case GT: out.add(new Object[]{a, b, -1L}); break;
                case GE: out.add(new Object[]{a, b, 0L}); break;
                case EQ: out.add(new Object[]{b, a, 0L}); out.add(new Object[]{a, b, 0L}); break;
                default: break;
            }
        }
        // END_CHANGE: ISS-2025-0645

        @Override public Constraint negation() {
            Rel n;
            switch (rel) {
                case LT: n = Rel.GE; break;
                case LE: n = Rel.GT; break;
                case GT: n = Rel.LE; break;
                case GE: n = Rel.LT; break;
                case EQ: n = Rel.NE; break;
                default: n = Rel.EQ; break; // NE -> EQ
            }
            return new Cmp(a, n, b);
        }
    }

    // =====================================================================================
    // Sum:  z = x + y   (bounds consistency, both directions)
    // =====================================================================================
    public static final class Sum extends Constraint {
        private final FdVar x, y, z;
        public Sum(FdVar x, FdVar y, FdVar z) { this.x = x; this.y = y; this.z = z; }
        // START_CHANGE: ISS-2025-0760 - residual goal
        @Override void render(Residuals r, List<it.denzosoft.jprolog.core.terms.Term> out) {
            emit(out, r.linear(new BigInteger[]{BigInteger.ONE, BigInteger.ONE, BigInteger.ONE.negate()},
                new FdVar[]{x, y, z}, Rel.EQ, BigInteger.ZERO, false));
        }
        // END_CHANGE: ISS-2025-0760

        @Override public List<FdVar> variables() { return Arrays.asList(x, y, z); }

        @Override public boolean propagate(ClpStore s) {
            IntervalDomain dx = s.dom(x), dy = s.dom(y), dz = s.dom(z);
            if (dx.isEmpty() || dy.isEmpty() || dz.isEmpty()) return false;
            if (!s.narrow(z, IntervalDomain.interval(add(dx.min(), dy.min()), add(dx.max(), dy.max())))) return false;
            dz = s.dom(z);
            if (!s.narrow(x, IntervalDomain.interval(sub(dz.min(), dy.max()), sub(dz.max(), dy.min())))) return false;
            dx = s.dom(x);
            if (!s.narrow(y, IntervalDomain.interval(sub(dz.min(), dx.max()), sub(dz.max(), dx.min())))) return false;
            return true;
        }
    }

    // =====================================================================================
    // all_different: singleton elimination + pigeonhole infeasibility (SWI's weak variant)
    // =====================================================================================
    public static class AllDifferent extends Constraint {
        protected final List<FdVar> vars;
        public AllDifferent(List<FdVar> vars) { this.vars = new ArrayList<>(vars); }
        // START_CHANGE: ISS-2025-0760 - residual goal
        /** Alive while two variables are free (one free variable is pruned by value elimination). */
        @Override public boolean alive(ClpStore s) {
            int free = 0;
            for (FdVar v : vars) if (!s.dom(v).isSingleton() && ++free >= 2) return true;
            return false;
        }
        protected String residualName() { return "all_different"; }
        @Override void render(Residuals r, List<it.denzosoft.jprolog.core.terms.Term> out) {
            out.add(Residuals.op(residualName(), Residuals.list(r.terms(vars))));
        }
        // END_CHANGE: ISS-2025-0760

        @Override public List<FdVar> variables() { return vars; }

        /** SWI's all_different/1 only reacts to instantiation (ISS-2025-0642). */
        @Override protected boolean wakesOnFixOnly() { return true; }

        @Override public boolean propagate(ClpStore s) {
            if (!eliminateSingletons(s)) return false;
            return pigeonhole(s);
        }

        /** Remove each fixed value from the other variables, to a fixpoint. */
        protected final boolean eliminateSingletons(ClpStore s) {
            boolean changed = true;
            while (changed) {
                changed = false;
                for (int i = 0; i < vars.size(); i++) {
                    FdVar fixed = vars.get(i);
                    IntervalDomain df = s.dom(fixed);
                    if (!df.isSingleton()) continue;
                    long val = df.value();
                    for (int j = 0; j < vars.size(); j++) {
                        FdVar other = vars.get(j);
                        if (other == fixed) {
                            if (j != i) return false;             // the same variable twice, fixed
                            continue;
                        }
                        if (s.dom(other).contains(val)) {
                            if (!s.removeValue(other, val)) return false;
                            changed = true;
                        }
                    }
                }
            }
            return true;
        }

        /** N variables cannot fit in fewer than N distinct values. */
        protected final boolean pigeonhole(ClpStore s) {
            IntervalDomain union = IntervalDomain.EMPTY;
            for (FdVar v : vars) {
                IntervalDomain d = s.dom(v);
                if (d.isEmpty()) return false;
                if (!d.isFinite()) return true;
                union = union.union(d);
            }
            long size = union.size();
            return size >= vars.size();
        }
    }

    // START_CHANGE: ISS-2025-0651 - all_distinct/1: generalised arc consistency (Regin's matching
    // algorithm), strictly stronger than all_different/1's value elimination. Falls back to the weak
    // propagation when the value universe is too large to build the bipartite graph cheaply.
    // =====================================================================================
    // all_distinct: Regin GAC via maximum matching + SCC of the alternating graph
    // =====================================================================================
    public static final class AllDistinct extends AllDifferent {
        /** Value universes larger than this fall back to the weak propagation. */
        static final long MAX_SPAN = 4096;
        private int[] warm;                                   // previous matching (hint only)

        public AllDistinct(List<FdVar> vars) { super(vars); }
        @Override protected String residualName() { return "all_distinct"; }   // ISS-2025-0760

        /** Regin filtering needs every narrowing, not just fixings. */
        @Override protected boolean wakesOnFixOnly() { return false; }

        @Override public boolean propagate(ClpStore s) {
            if (!eliminateSingletons(s)) return false;
            int n = vars.size();
            if (n <= 1) return true;
            long lo = SUP, hi = INF;
            for (FdVar v : vars) {
                IntervalDomain d = s.dom(v);
                if (d.isEmpty()) return false;
                if (!d.isFinite()) return pigeonhole(s);
                lo = Math.min(lo, d.min());
                hi = Math.max(hi, d.max());
            }
            long spanL = hi - lo + 1;
            if (spanL <= 0 || spanL > MAX_SPAN) return pigeonhole(s);
            int m = (int) spanL;
            if (m < n) {
                // fewer candidate values than variables: only possible if pigeonhole already fails
                if (!pigeonhole(s)) return false;
            }
            // adjacency: var -> values (as offsets)
            int[][] adj = new int[n][];
            for (int i = 0; i < n; i++) {
                IntervalDomain d = s.dom(vars.get(i));
                int cnt = (int) d.size();
                int[] a = new int[cnt];
                int k = 0;
                for (long[] r : d.rangeArray()) {
                    for (long v = r[0]; v <= r[1]; v++) a[k++] = (int) (v - lo);
                }
                adj[i] = a;
            }
            int[] matchVar = new int[n];
            int[] matchVal = new int[m];
            Arrays.fill(matchVar, -1);
            Arrays.fill(matchVal, -1);
            // warm start from the previous matching when it is still valid
            if (warm != null && warm.length == n) {
                for (int i = 0; i < n; i++) {
                    long v = warm[i];
                    if (v == Long.MIN_VALUE) continue;
                    long off = warm[i] - lo;
                    if (off < 0 || off >= m) continue;
                    if (matchVal[(int) off] != -1) continue;
                    if (!s.dom(vars.get(i)).contains(warm[i])) continue;
                    matchVar[i] = (int) off;
                    matchVal[(int) off] = i;
                }
            }
            for (int i = 0; i < n; i++) {
                if (matchVar[i] != -1) continue;
                boolean[] seen = new boolean[m];
                if (!augment(i, adj, matchVar, matchVal, seen)) return false;   // no perfect matching
            }
            int[] w = new int[n];
            for (int i = 0; i < n; i++) w[i] = (int) (matchVar[i] + lo);
            warm = w;

            // directed alternating graph: nodes 0..n-1 vars, n..n+m-1 values.
            // matched edge var -> value, unmatched edge value -> var.
            int nodes = n + m;
            int[] outCount = new int[nodes];
            for (int i = 0; i < n; i++) {
                outCount[i] = 1;
                for (int v : adj[i]) if (v != matchVar[i]) outCount[n + v]++;
            }
            int[][] out = new int[nodes][];
            for (int k = 0; k < nodes; k++) out[k] = new int[outCount[k]];
            int[] fill = new int[nodes];
            for (int i = 0; i < n; i++) {
                out[i][fill[i]++] = n + matchVar[i];
                for (int v : adj[i]) if (v != matchVar[i]) out[n + v][fill[n + v]++] = i;
            }
            int[] comp = scc(out, nodes);
            // values reachable from a free value (present in some domain but unmatched)
            boolean[] reach = new boolean[nodes];
            int[] stack = new int[nodes];
            int sp = 0;
            for (int v = 0; v < m; v++) {
                if (matchVal[v] == -1 && outCount[n + v] > 0) { reach[n + v] = true; stack[sp++] = n + v; }
            }
            while (sp > 0) {
                int u = stack[--sp];
                for (int t : out[u]) if (!reach[t]) { reach[t] = true; stack[sp++] = t; }
            }
            // prune every unmatched edge that is neither on an alternating cycle nor on an even
            // alternating path from a free value
            for (int i = 0; i < n; i++) {
                long[] keep = new long[adj[i].length];
                int kept = 0;
                boolean pruned = false;
                for (int v : adj[i]) {
                    if (v == matchVar[i] || comp[i] == comp[n + v] || reach[n + v]) keep[kept++] = v + lo;
                    else pruned = true;
                }
                if (pruned) {
                    if (!s.narrow(vars.get(i), IntervalDomain.fromValues(Arrays.copyOf(keep, kept)))) return false;
                }
            }
            return true;
        }

        private static boolean augment(int i, int[][] adj, int[] matchVar, int[] matchVal, boolean[] seen) {
            for (int v : adj[i]) {
                if (seen[v]) continue;
                seen[v] = true;
                if (matchVal[v] == -1 || augment(matchVal[v], adj, matchVar, matchVal, seen)) {
                    matchVar[i] = v;
                    matchVal[v] = i;
                    return true;
                }
            }
            return false;
        }

        /** Iterative Tarjan: the SCC id of every node. */
        private static int[] scc(int[][] out, int nodes) {
            int[] index = new int[nodes], low = new int[nodes], comp = new int[nodes];
            Arrays.fill(index, -1);
            boolean[] onStack = new boolean[nodes];
            int[] stack = new int[nodes];
            int sp = 0, counter = 0, compCount = 0;
            int[] callNode = new int[nodes], callEdge = new int[nodes];
            for (int root = 0; root < nodes; root++) {
                if (index[root] != -1) continue;
                int csp = 0;
                callNode[csp] = root; callEdge[csp] = 0; csp++;
                index[root] = low[root] = counter++;
                stack[sp++] = root; onStack[root] = true;
                while (csp > 0) {
                    int u = callNode[csp - 1];
                    if (callEdge[csp - 1] < out[u].length) {
                        int t = out[u][callEdge[csp - 1]++];
                        if (index[t] == -1) {
                            index[t] = low[t] = counter++;
                            stack[sp++] = t; onStack[t] = true;
                            callNode[csp] = t; callEdge[csp] = 0; csp++;
                        } else if (onStack[t]) {
                            low[u] = Math.min(low[u], index[t]);
                        }
                    } else {
                        if (low[u] == index[u]) {
                            int x;
                            do {
                                x = stack[--sp];
                                onStack[x] = false;
                                comp[x] = compCount;
                            } while (x != u);
                            compCount++;
                        }
                        csp--;
                        if (csp > 0) {
                            int p = callNode[csp - 1];
                            low[p] = Math.min(low[p], low[u]);
                        }
                    }
                }
            }
            return comp;
        }
    }
    // END_CHANGE: ISS-2025-0651

    // =====================================================================================
    // Product:  z = x * y   (interval multiplication; backward division)
    // =====================================================================================
    public static final class Mul extends Constraint {
        private final FdVar x, y, z;
        public Mul(FdVar x, FdVar y, FdVar z) { this.x = x; this.y = y; this.z = z; }
        // START_CHANGE: ISS-2025-0760 - residual goal
        @Override void render(Residuals r, List<it.denzosoft.jprolog.core.terms.Term> out) {
            out.add(Residuals.op("#=", Residuals.op("*", r.term(x), r.term(y)), r.term(z)));
        }
        // END_CHANGE: ISS-2025-0760

        @Override public List<FdVar> variables() { return Arrays.asList(x, y, z); }

        @Override public BigInteger solveFor(FdVar v, ClpStore s) {      // ISS-2025-0644
            if (v != z || x == z || y == z) return null;
            BigInteger a = fixedValue(s, x), b = fixedValue(s, y);
            return (a == null || b == null) ? null : a.multiply(b);
        }

        @Override public boolean propagate(ClpStore s) {
            IntervalDomain dx = s.dom(x), dy = s.dom(y);
            if (dx.isEmpty() || dy.isEmpty() || s.dom(z).isEmpty()) return false;
            long p1 = mul(dx.min(), dy.min()), p2 = mul(dx.min(), dy.max());
            long p3 = mul(dx.max(), dy.min()), p4 = mul(dx.max(), dy.max());
            if (!s.narrow(z, IntervalDomain.interval(minOf(p1, p2, p3, p4), maxOf(p1, p2, p3, p4)))) return false;
            if (!divideInto(s, y, x)) return false;
            if (!divideInto(s, x, y)) return false;
            // z = 0 with the other factor certainly non-zero forces this factor to 0 (and back)
            return true;
        }

        /** Narrow {@code target} so that target * divisor lies in z (divisor not straddling 0). */
        private boolean divideInto(ClpStore s, FdVar divisor, FdVar target) {
            IntervalDomain dd = s.dom(divisor), dz = s.dom(z);
            if (!dd.isFinite() || !dz.isFinite()) return true;
            if (dd.min() <= 0 && dd.max() >= 0) {
                // divisor may be 0: only a non-zero z excludes it
                if (dd.isSingleton() && !dz.contains(0)) return false;
                return true;
            }
            // target in hull of z/d over the corners (d does not contain 0)
            BigInteger zl = BigInteger.valueOf(dz.min()), zh = BigInteger.valueOf(dz.max());
            BigInteger dl = BigInteger.valueOf(dd.min()), dh = BigInteger.valueOf(dd.max());
            BigInteger[] qs = {
                ceilDiv(zl, dl), ceilDiv(zl, dh), ceilDiv(zh, dl), ceilDiv(zh, dh),
                floorDiv(zl, dl), floorDiv(zl, dh), floorDiv(zh, dl), floorDiv(zh, dh)
            };
            BigInteger lo = qs[0], hi = qs[0];
            for (BigInteger q : qs) { if (q.compareTo(lo) < 0) lo = q; if (q.compareTo(hi) > 0) hi = q; }
            // the true quotient range is contained in [min ceil, max floor] of the corner quotients
            BigInteger cLo = qs[0], fHi = qs[4];
            for (int k = 0; k < 4; k++) if (qs[k].compareTo(cLo) < 0) cLo = qs[k];
            for (int k = 4; k < 8; k++) if (qs[k].compareTo(fHi) > 0) fHi = qs[k];
            if (!s.narrow(target, IntervalDomain.interval(clamp(cLo), clamp(fHi)))) return false;
            if (dd.isSingleton()) {
                // exact: target must divide z; keep only values t with t*d in z (small domains)
                IntervalDomain dt = s.dom(target);
                if (dt.isFinite() && dt.size() <= 4096) {
                    long d = dd.value();
                    long[] keep = new long[(int) dt.size()];
                    int k = 0;
                    for (long[] r : dt.rangeArray()) {
                        for (long t = r[0]; t <= r[1]; t++) {
                            long p = mul(t, d);
                            if (!isInf(p) && dz.contains(p)) keep[k++] = t;
                        }
                    }
                    if (!s.narrow(target, IntervalDomain.fromValues(Arrays.copyOf(keep, k)))) return false;
                }
            }
            return true;
        }
    }

    static BigInteger floorDiv(BigInteger a, BigInteger b) {
        BigInteger[] qr = a.divideAndRemainder(b);
        if (qr[1].signum() != 0 && qr[1].signum() != b.signum()) return qr[0].subtract(BigInteger.ONE);
        return qr[0];
    }

    static BigInteger ceilDiv(BigInteger a, BigInteger b) {
        return floorDiv(a.negate(), b).negate();
    }

    // START_CHANGE: ISS-2025-0421 - square propagator (z = x*x)
    // =====================================================================================
    // Square:  z = x * x
    // =====================================================================================
    public static final class Square extends Constraint {
        private final FdVar x, z;
        public Square(FdVar x, FdVar z) { this.x = x; this.z = z; }
        // START_CHANGE: ISS-2025-0760 - residual goal
        @Override void render(Residuals r, List<it.denzosoft.jprolog.core.terms.Term> out) {
            it.denzosoft.jprolog.core.terms.Term tx = r.term(x);
            out.add(Residuals.op("#=", Residuals.op("*", tx, tx), r.term(z)));
        }
        // END_CHANGE: ISS-2025-0760

        @Override public List<FdVar> variables() { return Arrays.asList(x, z); }

        @Override public BigInteger solveFor(FdVar v, ClpStore s) {      // ISS-2025-0644
            if (v != z || x == z) return null;
            BigInteger a = fixedValue(s, x);
            return a == null ? null : a.multiply(a);
        }

        @Override public boolean propagate(ClpStore s) {
            IntervalDomain dx = s.dom(x), dz = s.dom(z);
            if (dx.isEmpty() || dz.isEmpty()) return false;
            long lo = dx.min(), hi = dx.max();
            long sLo = mul(lo, lo), sHi = mul(hi, hi);
            long zmin = (lo <= 0 && hi >= 0) ? 0 : Math.min(sLo, sHi);
            long zmax = Math.max(sLo, sHi);
            if (!s.narrow(z, IntervalDomain.interval(zmin, zmax))) return false;
            dz = s.dom(z);
            if (dz.max() == SUP) return true;                  // ISS-2025-0644: nothing to invert
            long r = floorSqrt(dz.max());
            long c = ceilSqrt(Math.max(dz.min(), 0));
            dx = s.dom(x);
            if (dx.min() >= 0) return s.narrow(x, IntervalDomain.interval(c, r));
            if (dx.max() <= 0) return s.narrow(x, IntervalDomain.interval(-r, -c));
            IntervalDomain both = IntervalDomain.interval(-r, -c).union(IntervalDomain.interval(c, r));
            return s.narrow(x, both);
        }

        /** Largest r >= 0 with r*r <= v (v >= 0). */
        static long floorSqrt(long v) {
            if (v <= 0) return 0;
            if (v >= 3037000499L * 3037000499L) return 3037000499L;   // floorSqrt(Long.MAX_VALUE)
            long r = (long) Math.sqrt((double) v);
            while (r > 0 && r * r > v) r--;
            while ((r + 1) * (r + 1) <= v) r++;
            return r;
        }

        /** Smallest r >= 0 with r*r >= v (v >= 0). */
        static long ceilSqrt(long v) {
            if (v <= 0) return 0;
            return floorSqrt(v - 1) + 1;
        }
    }

    // =====================================================================================
    // Minimum / maximum:  z = min(x, y)  /  z = max(x, y)   (bounds consistency)
    // =====================================================================================
    public static final class Min extends Constraint {
        private final FdVar x, y, z;
        public Min(FdVar x, FdVar y, FdVar z) { this.x = x; this.y = y; this.z = z; }
        // START_CHANGE: ISS-2025-0760 - residual goal
        @Override void render(Residuals r, List<it.denzosoft.jprolog.core.terms.Term> out) {
            out.add(Residuals.op("#=", r.term(z), Residuals.op("min", r.term(x), r.term(y))));
        }
        // END_CHANGE: ISS-2025-0760

        @Override public List<FdVar> variables() { return Arrays.asList(x, y, z); }

        @Override public boolean propagate(ClpStore s) {
            IntervalDomain dx = s.dom(x), dy = s.dom(y);
            if (dx.isEmpty() || dy.isEmpty() || s.dom(z).isEmpty()) return false;
            if (!s.narrow(z, IntervalDomain.interval(Math.min(dx.min(), dy.min()),
                                                     Math.min(dx.max(), dy.max())))) return false;
            long zmin = s.dom(z).min();
            if (!s.removeBelow(x, zmin)) return false;
            if (!s.removeBelow(y, zmin)) return false;
            if (s.dom(x).min() > s.dom(y).max()) {
                if (!s.narrow(z, s.dom(y)) || !s.narrow(y, s.dom(z))) return false;
            } else if (s.dom(y).min() > s.dom(x).max()) {
                if (!s.narrow(z, s.dom(x)) || !s.narrow(x, s.dom(z))) return false;
            }
            return true;
        }
    }

    public static final class Max extends Constraint {
        private final FdVar x, y, z;
        public Max(FdVar x, FdVar y, FdVar z) { this.x = x; this.y = y; this.z = z; }
        // START_CHANGE: ISS-2025-0760 - residual goal
        @Override void render(Residuals r, List<it.denzosoft.jprolog.core.terms.Term> out) {
            out.add(Residuals.op("#=", r.term(z), Residuals.op("max", r.term(x), r.term(y))));
        }
        // END_CHANGE: ISS-2025-0760

        @Override public List<FdVar> variables() { return Arrays.asList(x, y, z); }

        @Override public boolean propagate(ClpStore s) {
            IntervalDomain dx = s.dom(x), dy = s.dom(y);
            if (dx.isEmpty() || dy.isEmpty() || s.dom(z).isEmpty()) return false;
            if (!s.narrow(z, IntervalDomain.interval(Math.max(dx.min(), dy.min()),
                                                     Math.max(dx.max(), dy.max())))) return false;
            long zmax = s.dom(z).max();
            if (!s.removeAbove(x, zmax)) return false;
            if (!s.removeAbove(y, zmax)) return false;
            if (s.dom(x).max() < s.dom(y).min()) {
                if (!s.narrow(z, s.dom(y)) || !s.narrow(y, s.dom(z))) return false;
            } else if (s.dom(y).max() < s.dom(x).min()) {
                if (!s.narrow(z, s.dom(x)) || !s.narrow(x, s.dom(z))) return false;
            }
            return true;
        }
    }
    // END_CHANGE: ISS-2025-0421

    // =====================================================================================
    // Absolute value:  y = |x|
    // =====================================================================================
    public static final class Abs extends Constraint {
        private final FdVar x, y;
        public Abs(FdVar x, FdVar y) { this.x = x; this.y = y; }
        // START_CHANGE: ISS-2025-0760 - residual goal
        @Override void render(Residuals r, List<it.denzosoft.jprolog.core.terms.Term> out) {
            out.add(Residuals.op("#=", r.term(y), Residuals.op("abs", r.term(x))));
        }
        // END_CHANGE: ISS-2025-0760

        @Override public List<FdVar> variables() { return Arrays.asList(x, y); }

        @Override public boolean propagate(ClpStore s) {
            IntervalDomain dx = s.dom(x);
            if (dx.isEmpty()) return false;
            long lo = dx.min(), hi = dx.max();
            long ymin = (lo <= 0 && hi >= 0) ? 0 : Math.min(absSat(lo), absSat(hi));
            long ymax = Math.max(absSat(lo), absSat(hi));
            if (!s.narrow(y, IntervalDomain.interval(ymin, ymax))) return false;
            IntervalDomain dy = s.dom(y);
            long ay = dy.max();
            long by = dy.min();
            // x in [-ay .. -by] \/ [by .. ay]
            IntervalDomain allowed = IntervalDomain.interval(neg(ay), neg(by))
                .union(IntervalDomain.interval(by, ay));
            return s.narrow(x, allowed);
        }
    }

    // =====================================================================================
    // Modulo:  Z = X mod M   (M a fixed positive modulus; floor semantics, ISO §9.1.7)
    // =====================================================================================
    public static final class Mod extends Constraint {
        private final FdVar x, z;
        private final long m;
        public Mod(FdVar x, long m, FdVar z) { this.x = x; this.m = m; this.z = z; }
        // START_CHANGE: ISS-2025-0760 - residual goal
        @Override void render(Residuals r, List<it.denzosoft.jprolog.core.terms.Term> out) {
            out.add(Residuals.op("#=", Residuals.op("mod", r.term(x), Residuals.num(m)), r.term(z)));
        }
        // END_CHANGE: ISS-2025-0760

        @Override public List<FdVar> variables() { return Arrays.asList(x, z); }

        @Override public boolean propagate(ClpStore s) {
            if (m <= 0) return false;
            if (!s.narrow(z, IntervalDomain.interval(0, m - 1))) return false;
            IntervalDomain dx = s.dom(x);
            if (dx.isEmpty()) return false;
            if (dx.isSingleton()) {
                return s.narrow(z, IntervalDomain.singleton(Math.floorMod(dx.value(), m)));
            }
            // ISS-2025-0648: small domains get full support filtering in both directions
            if (dx.isFinite() && dx.size() <= 4096) {
                IntervalDomain dz = s.dom(z);
                long[] keepX = new long[(int) dx.size()];
                boolean[] zSeen = new boolean[(int) Math.min(m, 4096)];
                long[] keepZ = new long[zSeen.length];
                int kx = 0, kz = 0;
                for (long[] r : dx.rangeArray()) {
                    for (long v = r[0]; v <= r[1]; v++) {
                        long md = Math.floorMod(v, m);
                        if (dz.contains(md)) {
                            keepX[kx++] = v;
                            if (md < zSeen.length && !zSeen[(int) md]) { zSeen[(int) md] = true; keepZ[kz++] = md; }
                        }
                    }
                }
                if (!s.narrow(x, IntervalDomain.fromValues(Arrays.copyOf(keepX, kx)))) return false;
                if (m <= 4096 && !s.narrow(z, IntervalDomain.fromValues(Arrays.copyOf(keepZ, kz)))) return false;
            }
            return true;
        }
    }

    // START_CHANGE: ISS-2025-0648 - the remaining CLP(FD) arithmetic functions: // (truncating),
    // div (floored), rem, mod with a variable modulus, and ^ (power). Each is sound (it never removes
    // a supported value) and exact once the operands are fixed.
    // =====================================================================================
    // Arithmetic function:  z = x OP y
    // =====================================================================================
    public enum Fn { TDIV, FDIV, REM, MOD, POW }

    public static final class ArithFn extends Constraint {
        private final Fn op;
        private final FdVar x, y, z;
        public ArithFn(Fn op, FdVar x, FdVar y, FdVar z) { this.op = op; this.x = x; this.y = y; this.z = z; }
        // START_CHANGE: ISS-2025-0760 - residual goal
        @Override void render(Residuals r, List<it.denzosoft.jprolog.core.terms.Term> out) {
            String f;
            switch (op) {
                case TDIV: f = "//"; break;
                case FDIV: f = "div"; break;
                case REM: f = "rem"; break;
                case MOD: f = "mod"; break;
                default: f = "^"; break;
            }
            out.add(Residuals.op("#=", Residuals.op(f, r.term(x), r.term(y)), r.term(z)));
        }
        // END_CHANGE: ISS-2025-0760

        @Override public List<FdVar> variables() { return Arrays.asList(x, y, z); }

        @Override public BigInteger solveFor(FdVar v, ClpStore s) {      // ISS-2025-0644
            if (v != z || x == z || y == z) return null;
            BigInteger a = fixedValue(s, x), b = fixedValue(s, y);
            return (a == null || b == null) ? null : eval(op, a, b);
        }

        /** Exact evaluation over integers; null when undefined (division by zero, negative power). */
        public static BigInteger eval(Fn op, BigInteger a, BigInteger b) {
            switch (op) {
                case TDIV:
                    if (b.signum() == 0) return null;
                    return a.divide(b);
                case FDIV:
                    if (b.signum() == 0) return null;
                    return floorDiv(a, b);
                case REM:
                    if (b.signum() == 0) return null;
                    return a.remainder(b);
                case MOD: {
                    if (b.signum() == 0) return null;
                    BigInteger r = a.mod(b.abs());
                    if (b.signum() < 0 && r.signum() != 0) r = r.add(b);
                    return r;
                }
                case POW: {
                    if (b.signum() < 0) {
                        if (a.equals(BigInteger.ONE)) return BigInteger.ONE;
                        if (a.equals(BigInteger.ONE.negate())) return b.testBit(0) ? a : BigInteger.ONE;
                        return null;
                    }
                    if (b.bitLength() > 31) {
                        if (a.abs().compareTo(BigInteger.ONE) <= 0) return a.signum() == 0 ? BigInteger.ZERO
                            : (a.signum() > 0 || !b.testBit(0) ? BigInteger.ONE : a);
                        return null;
                    }
                    int e = b.intValue();
                    if (a.bitLength() * (long) e > 1_000_000L) return null;    // absurdly large
                    return a.pow(e);
                }
                default: return null;
            }
        }

        @Override public boolean propagate(ClpStore s) {
            IntervalDomain dx = s.dom(x), dy = s.dom(y), dz = s.dom(z);
            if (dx.isEmpty() || dy.isEmpty() || dz.isEmpty()) return false;
            if (op != Fn.POW && dy.contains(0)) {
                if (!s.removeValue(y, 0)) return false;           // division by zero is undefined
                dy = s.dom(y);
            }
            if (dx.isSingleton() && dy.isSingleton()) {
                BigInteger r = eval(op, BigInteger.valueOf(dx.value()), BigInteger.valueOf(dy.value()));
                if (r == null) return false;
                long rl = clamp(r);
                if (isInf(rl)) return s.narrow(z, IntervalDomain.interval(rl, rl));   // out of range
                return s.narrow(z, IntervalDomain.singleton(rl));
            }
            switch (op) {
                case TDIV: case FDIV: return propagateDiv(s, dx, dy);
                case REM: return propagateRem(s, dx, dy);
                case MOD: return propagateMod(s, dx, dy);
                case POW: return propagatePow(s, dx, dy);
                default: return true;
            }
        }

        private boolean propagateDiv(ClpStore s, IntervalDomain dx, IntervalDomain dy) {
            if (!dx.isFinite() || !dy.isFinite()) return true;
            // hull over the non-zero parts of y (negative part and positive part separately)
            BigInteger lo = null, hi = null;
            long[][] parts = {
                {dy.min(), Math.min(dy.max(), -1)},
                {Math.max(dy.min(), 1), dy.max()}
            };
            for (long[] p : parts) {
                if (p[0] > p[1]) continue;
                long[] xs = {dx.min(), dx.max()};
                long[] ys = {p[0], p[1]};
                for (long a : xs) {
                    for (long b : ys) {
                        BigInteger q = eval(op, BigInteger.valueOf(a), BigInteger.valueOf(b));
                        if (q == null) continue;
                        if (lo == null || q.compareTo(lo) < 0) lo = q;
                        if (hi == null || q.compareTo(hi) > 0) hi = q;
                    }
                }
            }
            if (lo == null) return false;
            if (!s.narrow(z, IntervalDomain.interval(clamp(lo), clamp(hi)))) return false;
            // backward, divisor fixed: |x - z*c| < |c| (truncating) / x in [z*c, z*c + c - 1] (floored)
            IntervalDomain dz = s.dom(z);
            if (dy.isSingleton() && dz.isFinite()) {
                long c = dy.value();
                long ac = Math.abs(c) - 1;
                long a1 = mul(dz.min(), c), a2 = mul(dz.max(), c);
                long lo2 = sub(Math.min(a1, a2), ac), hi2 = add(Math.max(a1, a2), ac);
                if (!s.narrow(x, IntervalDomain.interval(lo2, hi2))) return false;
            }
            return true;
        }

        private boolean propagateRem(ClpStore s, IntervalDomain dx, IntervalDomain dy) {
            long my = Math.max(absSat(dy.min()), absSat(dy.max()));
            long bound = (my == SUP) ? SUP : my - 1;                  // |z| < |y|
            long zlo = dx.min() >= 0 ? 0 : neg(Math.min(bound, absSat(dx.min())));
            long zhi = dx.max() <= 0 ? 0 : Math.min(bound, dx.max() == SUP ? SUP : dx.max());
            return s.narrow(z, IntervalDomain.interval(zlo, zhi));
        }

        private boolean propagateMod(ClpStore s, IntervalDomain dx, IntervalDomain dy) {
            if (dy.min() > 0) {
                long hiZ = sub(dy.max(), 1);
                if (dx.min() >= 0) hiZ = Math.min(hiZ, dx.max());
                return s.narrow(z, IntervalDomain.interval(0, hiZ));
            }
            if (dy.max() < 0) {
                long loZ = add(dy.min(), 1);
                if (dx.max() <= 0) loZ = Math.max(loZ, dx.min());
                return s.narrow(z, IntervalDomain.interval(loZ, 0));
            }
            long my = Math.max(absSat(dy.min()), absSat(dy.max()));
            long bound = (my == SUP) ? SUP : my - 1;
            return s.narrow(z, IntervalDomain.interval(neg(bound), bound));
        }

        private boolean propagatePow(ClpStore s, IntervalDomain dx, IntervalDomain dy) {
            if (!dy.isSingleton()) {
                // variable exponent: only the trivial sign facts are cheap and sound
                if (dy.min() >= 0 && dx.min() >= 0) return s.removeBelow(z, 0);
                return true;
            }
            long e = dy.value();
            if (e < 0) return true;
            if (e == 0) return s.narrow(z, IntervalDomain.singleton(1));
            if (e == 1) {
                if (!s.narrow(z, s.dom(x))) return false;
                return s.narrow(x, s.dom(z));
            }
            if (e > 64) return true;
            int ei = (int) e;
            boolean even = (ei % 2) == 0;
            long lo = dx.min(), hi = dx.max();
            long pLo = pow(lo, ei), pHi = pow(hi, ei);
            long zlo, zhi;
            if (even) {
                if (lo >= 0) { zlo = pLo; zhi = pHi; }
                else if (hi <= 0) { zlo = pHi; zhi = pLo; }
                else { zlo = 0; zhi = Math.max(pLo, pHi); }
            } else {
                zlo = pLo; zhi = pHi;
            }
            if (!s.narrow(z, IntervalDomain.interval(zlo, zhi))) return false;
            IntervalDomain dz = s.dom(z);
            if (even) {
                if (dz.max() == SUP) return true;
                long r = root(dz.max(), ei, false);
                long c = dz.min() <= 0 ? 0 : root(dz.min(), ei, true);
                IntervalDomain allowed = IntervalDomain.interval(-r, -c).union(IntervalDomain.interval(c, r));
                return s.narrow(x, allowed);
            }
            long rlo = dz.min() == INF ? INF : signedRoot(dz.min(), ei, true);
            long rhi = dz.max() == SUP ? SUP : signedRoot(dz.max(), ei, false);
            return s.narrow(x, IntervalDomain.interval(rlo, rhi));
        }

        /** base^e with infinity/saturation (e >= 2). */
        static long pow(long base, int e) {
            if (base == INF) return (e % 2 == 0) ? SUP : INF;
            if (base == SUP) return SUP;
            return clamp(BigInteger.valueOf(base).pow(e));
        }

        /** floor (ceil=false) or ceil (ceil=true) of the e-th root of v >= 0. */
        static long root(long v, int e, boolean ceil) {
            if (v <= 0) return 0;
            long lo = 0, hi = 1;
            while (pow(hi, e) <= v && hi < (1L << 62)) hi <<= 1;
            while (lo < hi) {                                    // largest r with r^e <= v
                long mid = lo + (hi - lo + 1) / 2;
                if (pow(mid, e) <= v) lo = mid; else hi = mid - 1;
            }
            if (ceil && pow(lo, e) != v) return lo + 1;
            return lo;
        }

        /** Signed root for odd e: floor/ceil of v^(1/e). */
        static long signedRoot(long v, int e, boolean ceil) {
            if (v >= 0) return root(v, e, ceil);
            long r = root(-v, e, !ceil);
            return -r;
        }
    }
    // END_CHANGE: ISS-2025-0648

    // =====================================================================================
    // Linear:  sum(coeff_i * var_i)  <rel>  constant   (bounds consistency)
    // =====================================================================================
    public static final class Linear extends Constraint {
        private final long[] coeffs;
        private final FdVar[] vars;
        private final Rel rel;       // EQ, LE, GE supported
        private final BigInteger kBig;
        private final boolean kFits;
        private final long k;
        // START_CHANGE: ISS-2025-0768 - 4.6 wave Q5.3: exact coefficients beyond 64 bits. Null (the
        // normal case) keeps the long fast path untouched; when a coefficient does not fit, this
        // holds every coefficient exactly, coeffs[] holds only their signs, and the constraint runs
        // on the exact (BigInteger, infinity-aware) path.
        private final BigInteger[] bigC;
        // END_CHANGE: ISS-2025-0768

        public Linear(long[] coeffs, FdVar[] vars, Rel rel, long k) {
            this(coeffs, vars, rel, BigInteger.valueOf(k));
        }

        // START_CHANGE: ISS-2025-0644 - the constant is exact (BigInteger)
        public Linear(long[] coeffs, FdVar[] vars, Rel rel, BigInteger k) {
            if (coeffs.length != vars.length) throw new IllegalArgumentException("coeffs/vars length mismatch");
            this.coeffs = coeffs.clone();
            this.vars = vars.clone();
            this.rel = rel;
            this.kBig = k;
            this.kFits = k.bitLength() <= 62;
            this.k = kFits ? k.longValue() : 0;
            this.bigC = null;
        }

        // START_CHANGE: ISS-2025-0768
        /** Exact coefficients: the long representation is used whenever every one fits. */
        public Linear(BigInteger[] coeffs, FdVar[] vars, Rel rel, BigInteger k) {
            if (coeffs.length != vars.length) throw new IllegalArgumentException("coeffs/vars length mismatch");
            boolean fits = true;
            for (BigInteger c : coeffs) if (c.bitLength() > 62) { fits = false; break; }
            this.coeffs = new long[coeffs.length];
            for (int i = 0; i < coeffs.length; i++) this.coeffs[i] = fits ? coeffs[i].longValue() : coeffs[i].signum();
            this.bigC = fits ? null : coeffs.clone();
            this.vars = vars.clone();
            this.rel = rel;
            this.kBig = k;
            this.kFits = k.bitLength() <= 62;
            this.k = kFits ? k.longValue() : 0;
        }

        /** True when a coefficient does not fit a long (the exact path only). */
        public boolean isBig() { return bigC != null; }

        private BigInteger coefB(int i) { return bigC != null ? bigC[i] : BigInteger.valueOf(coeffs[i]); }

        BigInteger[] exactCoeffs() {
            if (bigC != null) return bigC.clone();
            BigInteger[] out = new BigInteger[coeffs.length];
            for (int i = 0; i < out.length; i++) out[i] = BigInteger.valueOf(coeffs[i]);
            return out;
        }
        // END_CHANGE: ISS-2025-0768

        public long[] coeffs() { return coeffs.clone(); }
        public FdVar[] vars() { return vars.clone(); }
        public Rel rel() { return rel; }
        public BigInteger constant() { return kBig; }
        // END_CHANGE: ISS-2025-0644

        @Override public List<FdVar> variables() { return Arrays.asList(vars); }

        // START_CHANGE: ISS-2025-0760 - residual goal
        @Override void render(Residuals r, List<it.denzosoft.jprolog.core.terms.Term> out) {
            if (form == FORM_NOT && vars.length == 2) {                 // #\ X  as  X + Z = 1
                out.add(Residuals.op("#<==>", Residuals.op("#\\", r.term(vars[0])), r.term(vars[1])));
                return;
            }
            emit(out, r.linear(exactCoeffs(), vars, rel, kBig, false));
        }
        @Override it.denzosoft.jprolog.core.terms.Term reifiedForm(Residuals r) {
            return r.linear(exactCoeffs(), vars, rel, kBig, true);
        }
        // END_CHANGE: ISS-2025-0760

        @Override public BigInteger solveFor(FdVar v, ClpStore s) {      // ISS-2025-0644
            if (rel != Rel.EQ) return null;
            BigInteger rest = kBig;
            BigInteger cv = BigInteger.ZERO;                                // ISS-2025-0768: exact
            for (int i = 0; i < vars.length; i++) {
                if (vars[i] == v) { cv = cv.add(coefB(i)); continue; }
                if (coeffs[i] == 0) continue;
                BigInteger fx = fixedValue(s, vars[i]);
                if (fx == null) return null;
                rest = rest.subtract(coefB(i).multiply(fx));
            }
            if (cv.signum() == 0) return null;
            BigInteger[] qr = rest.divideAndRemainder(cv);
            return qr[1].signum() == 0 ? qr[0] : null;
        }

        @Override public boolean propagate(ClpStore s) {
            // START_CHANGE: ISS-2025-0644 - exact long fast path, BigInteger + infinities otherwise
            if (kFits && bigC == null) {                                    // ISS-2025-0768
                try {
                    int r = propagateLong(s);
                    if (r >= 0) return r == 1;
                } catch (ArithmeticException overflow) {
                    // fall through to the exact path
                }
            }
            return propagateBig(s);
            // END_CHANGE: ISS-2025-0644
        }

        /** 1 = ok, 0 = wipeout, -1 = not applicable (an infinite bound). */
        private int propagateLong(ClpStore s) {
            int n = vars.length;
            long lower = 0, upper = 0;
            long[] lo = new long[n], hi = new long[n];
            for (int i = 0; i < n; i++) {
                IntervalDomain d = s.dom(vars[i]);
                if (d.isEmpty()) return 0;
                long dmin = d.min(), dmax = d.max();
                if (isInf(dmin) || isInf(dmax)) return -1;
                long a = Math.multiplyExact(coeffs[i], dmin);
                long b = Math.multiplyExact(coeffs[i], dmax);
                lo[i] = Math.min(a, b); hi[i] = Math.max(a, b);
                lower = Math.addExact(lower, lo[i]);
                upper = Math.addExact(upper, hi[i]);
            }
            if (rel == Rel.EQ && (lower > k || upper < k)) return 0;
            if (rel == Rel.LE && lower > k) return 0;
            if (rel == Rel.GE && upper < k) return 0;
            if (rel == Rel.LE && upper <= k) return 1;            // entailed: nothing to prune
            if (rel == Rel.GE && lower >= k) return 1;
            for (int i = 0; i < n; i++) {
                long c = coeffs[i];
                if (c == 0) continue;
                long restLow = lower - lo[i];
                long restHigh = upper - hi[i];
                boolean hasLo = rel != Rel.LE, hasHi = rel != Rel.GE;
                long termLo = hasLo ? Math.subtractExact(k, restHigh) : 0;
                long termHi = hasHi ? Math.subtractExact(k, restLow) : 0;
                long xLo, xHi;
                if (c > 0) {
                    xLo = hasLo ? ceilDivL(termLo, c) : INF;
                    xHi = hasHi ? Math.floorDiv(termHi, c) : SUP;
                } else {
                    xLo = hasHi ? ceilDivL(termHi, c) : INF;
                    xHi = hasLo ? Math.floorDiv(termLo, c) : SUP;
                }
                if (xLo > xHi) return 0;
                IntervalDomain d = s.dom(vars[i]);
                if (xLo > d.min() || xHi < d.max()) {
                    if (!s.narrow(vars[i], IntervalDomain.interval(xLo, xHi))) return 0;
                }
            }
            return 1;
        }

        private static long ceilDivL(long a, long b) {
            return -Math.floorDiv(-a, b);
        }

        /** The exact lower (or upper) bound of a domain, or null when it is infinite (ISS-2025-0768:
         *  a variable fixed to an out-of-range value contributes that value exactly). */
        private static BigInteger exactBound(IntervalDomain d, boolean lower) {
            if (d.exactBig() != null) return d.exactBig();
            long b = lower ? d.min() : d.max();
            return isInf(b) ? null : BigInteger.valueOf(b);
        }

        private boolean propagateBig(ClpStore s) {
            int n = vars.length;
            BigInteger[] lo = new BigInteger[n], hi = new BigInteger[n];     // null = infinite
            BigInteger lowerF = BigInteger.ZERO, upperF = BigInteger.ZERO;
            int loInf = 0, hiInf = 0;
            for (int i = 0; i < n; i++) {
                IntervalDomain d = s.dom(vars[i]);
                if (d.isEmpty()) return false;
                BigInteger c = coefB(i);
                if (c.signum() == 0) { lo[i] = BigInteger.ZERO; hi[i] = BigInteger.ZERO; continue; }
                BigInteger dmin = exactBound(d, true);
                BigInteger dmax = exactBound(d, false);
                if (c.signum() > 0) {
                    lo[i] = dmin == null ? null : c.multiply(dmin);
                    hi[i] = dmax == null ? null : c.multiply(dmax);
                } else {
                    lo[i] = dmax == null ? null : c.multiply(dmax);
                    hi[i] = dmin == null ? null : c.multiply(dmin);
                }
                if (lo[i] == null) loInf++; else lowerF = lowerF.add(lo[i]);
                if (hi[i] == null) hiInf++; else upperF = upperF.add(hi[i]);
            }
            BigInteger K = kBig;
            boolean lowerFinite = loInf == 0, upperFinite = hiInf == 0;
            if (rel == Rel.EQ && ((lowerFinite && lowerF.compareTo(K) > 0) || (upperFinite && upperF.compareTo(K) < 0))) return false;
            if (rel == Rel.LE && lowerFinite && lowerF.compareTo(K) > 0) return false;
            if (rel == Rel.GE && upperFinite && upperF.compareTo(K) < 0) return false;

            for (int i = 0; i < n; i++) {
                BigInteger c = coefB(i);
                if (c.signum() == 0) continue;
                if (s.dom(vars[i]).exactBig() != null) continue;             // fixed exactly
                BigInteger restLow = (loInf - (lo[i] == null ? 1 : 0)) > 0 ? null
                    : (lo[i] == null ? lowerF : lowerF.subtract(lo[i]));
                BigInteger restHigh = (hiInf - (hi[i] == null ? 1 : 0)) > 0 ? null
                    : (hi[i] == null ? upperF : upperF.subtract(hi[i]));
                BigInteger termLo = null, termHi = null;               // null = unbounded
                if (rel != Rel.LE && restHigh != null) termLo = K.subtract(restHigh);
                if (rel != Rel.GE && restLow != null) termHi = K.subtract(restLow);
                BigInteger xLo, xHi;
                if (c.signum() > 0) {
                    xLo = (termLo == null) ? null : ceilDiv(termLo, c);
                    xHi = (termHi == null) ? null : floorDiv(termHi, c);
                } else {
                    xLo = (termHi == null) ? null : ceilDiv(termHi, c);
                    xHi = (termLo == null) ? null : floorDiv(termLo, c);
                }
                if (xLo != null && xHi != null && xLo.compareTo(xHi) > 0) return false;
                long loL = (xLo == null) ? INF : clamp(xLo);
                long hiL = (xHi == null) ? SUP : clamp(xHi);
                if (loL == INF && hiL == SUP) continue;
                if (!s.narrow(vars[i], IntervalDomain.interval(loL, hiL))) return false;
            }
            return true;
        }

        // START_CHANGE: ISS-2025-0647 - reification of linear comparisons
        @Override public Entail entailment(ClpStore s) {
            BigInteger lower = BigInteger.ZERO, upper = BigInteger.ZERO;
            boolean lowerInf = false, upperInf = false;
            for (int i = 0; i < vars.length; i++) {
                IntervalDomain d = s.dom(vars[i]);
                if (d.isEmpty()) return Entail.FALSE;
                BigInteger c = coefB(i);                                      // ISS-2025-0768
                if (c.signum() == 0) continue;
                BigInteger a = exactBound(d, c.signum() > 0);
                BigInteger b = exactBound(d, c.signum() < 0);
                if (a == null) lowerInf = true; else lower = lower.add(c.multiply(a));
                if (b == null) upperInf = true; else upper = upper.add(c.multiply(b));
            }
            switch (rel) {
                case EQ:
                    if (!lowerInf && lower.compareTo(kBig) > 0) return Entail.FALSE;
                    if (!upperInf && upper.compareTo(kBig) < 0) return Entail.FALSE;
                    if (!lowerInf && !upperInf && lower.equals(upper) && lower.equals(kBig)) return Entail.TRUE;
                    if (vars.length == 1 && coeffs[0] != 0) {
                        BigInteger[] qr = kBig.divideAndRemainder(coefB(0));
                        if (qr[1].signum() != 0) return Entail.FALSE;
                        long v = clamp(qr[0]);
                        if (!isInf(v) && !s.dom(vars[0]).contains(v)) return Entail.FALSE;
                    }
                    return Entail.UNKNOWN;
                case LE:
                    if (!upperInf && upper.compareTo(kBig) <= 0) return Entail.TRUE;
                    if (!lowerInf && lower.compareTo(kBig) > 0) return Entail.FALSE;
                    return Entail.UNKNOWN;
                case GE:
                    if (!lowerInf && lower.compareTo(kBig) >= 0) return Entail.TRUE;
                    if (!upperInf && upper.compareTo(kBig) < 0) return Entail.FALSE;
                    return Entail.UNKNOWN;
                default:
                    return Entail.UNKNOWN;
            }
        }

        @Override public Constraint negation() {
            BigInteger[] cs = exactCoeffs();                                  // ISS-2025-0768
            switch (rel) {
                case EQ: return new LinearNE(cs, vars, kBig);
                case LE: return new Linear(cs, vars, Rel.GE, kBig.add(BigInteger.ONE));
                case GE: return new Linear(cs, vars, Rel.LE, kBig.subtract(BigInteger.ONE));
                default: throw new UnsupportedOperationException("negation of " + rel);
            }
        }
        // END_CHANGE: ISS-2025-0647
    }

    // START_CHANGE: ISS-2025-0641 - sum(ci*xi) #\= k with forward checking: as soon as all but one
    // variable are fixed, the one value that would make the sum equal k is removed from the last
    // variable's domain (an interior value, so the domain gets a hole). This is what makes
    // X #\= Y, X #= 2 remove 2 from Y, and what gives N-queens its forward checking.
    // =====================================================================================
    // Linear disequality:  sum(coeff_i * var_i)  =\=  constant
    // =====================================================================================
    public static final class LinearNE extends Constraint {
        private final long[] coeffs;
        private final FdVar[] vars;
        private final BigInteger k;
        private final boolean kFits;
        private final long kL;
        private final BigInteger[] bigC;                                      // ISS-2025-0768

        public LinearNE(long[] coeffs, FdVar[] vars, BigInteger k) {
            this.coeffs = coeffs.clone();
            this.vars = vars.clone();
            this.k = k;
            this.kFits = k.bitLength() <= 62;
            this.kL = kFits ? k.longValue() : 0;
            this.bigC = null;
        }

        // START_CHANGE: ISS-2025-0768 - exact coefficients (see Linear)
        public LinearNE(BigInteger[] coeffs, FdVar[] vars, BigInteger k) {
            boolean fits = true;
            for (BigInteger c : coeffs) if (c.bitLength() > 62) { fits = false; break; }
            this.coeffs = new long[coeffs.length];
            for (int i = 0; i < coeffs.length; i++) this.coeffs[i] = fits ? coeffs[i].longValue() : coeffs[i].signum();
            this.bigC = fits ? null : coeffs.clone();
            this.vars = vars.clone();
            this.k = k;
            this.kFits = k.bitLength() <= 62;
            this.kL = kFits ? k.longValue() : 0;
        }

        private BigInteger coefB(int i) { return bigC != null ? bigC[i] : BigInteger.valueOf(coeffs[i]); }

        BigInteger[] exactCoeffs() {
            if (bigC != null) return bigC.clone();
            BigInteger[] out = new BigInteger[coeffs.length];
            for (int i = 0; i < out.length; i++) out[i] = BigInteger.valueOf(coeffs[i]);
            return out;
        }
        // END_CHANGE: ISS-2025-0768

        @Override public List<FdVar> variables() { return Arrays.asList(vars); }

        @Override protected boolean wakesOnFixOnly() { return true; }

        // START_CHANGE: ISS-2025-0760 - residual goal
        @Override void render(Residuals r, List<it.denzosoft.jprolog.core.terms.Term> out) {
            emit(out, r.linear(exactCoeffs(), vars, Rel.NE, k, false));
        }
        @Override it.denzosoft.jprolog.core.terms.Term reifiedForm(Residuals r) {
            return r.linear(exactCoeffs(), vars, Rel.NE, k, true);
        }
        // END_CHANGE: ISS-2025-0760

        @Override public boolean propagate(ClpStore s) {
            if (kFits && bigC == null) {                                    // ISS-2025-0768
                // exact long fast path (the N-queens hot path); overflow falls back below
                try {
                    int free = -1;
                    long sum = 0;
                    for (int i = 0; i < vars.length; i++) {
                        IntervalDomain d = vars[i].dom;
                        if (d.isEmpty()) return false;
                        if (coeffs[i] == 0) continue;
                        if (d.isSingleton()) {
                            sum = Math.addExact(sum, Math.multiplyExact(coeffs[i], d.value()));
                            continue;
                        }
                        if (free != -1) return true;               // two free variables: wait
                        free = i;
                    }
                    long rest = Math.subtractExact(kL, sum);
                    if (free == -1) return rest != 0;
                    long c = coeffs[free];
                    if (rest % c != 0) return true;
                    return s.removeValue(vars[free], rest / c);
                } catch (ArithmeticException overflow) {
                    // exact path below
                }
            }
            int free = -1;
            BigInteger sum = BigInteger.ZERO;
            for (int i = 0; i < vars.length; i++) {
                IntervalDomain d = s.dom(vars[i]);
                if (d.isEmpty()) return false;
                if (coeffs[i] == 0) continue;
                BigInteger fx = d.isSingleton() ? BigInteger.valueOf(d.value()) : d.exactBig();
                if (fx != null) {
                    sum = sum.add(coefB(i).multiply(fx));
                    continue;
                }
                if (free != -1) return true;                       // two free variables: wait
                free = i;
            }
            BigInteger rest = k.subtract(sum);                      // coeff_free * x != rest
            if (free == -1) return rest.signum() != 0;
            BigInteger[] qr = rest.divideAndRemainder(coefB(free));
            if (qr[1].signum() != 0) return true;                   // never equal
            long v = clamp(qr[0]);
            if (isInf(v)) return true;
            return s.removeValue(vars[free], v);
        }

        @Override public Entail entailment(ClpStore s) {
            Entail e = new Linear(exactCoeffs(), vars, Rel.EQ, k).entailment(s);
            if (e == Entail.TRUE) return Entail.FALSE;
            if (e == Entail.FALSE) return Entail.TRUE;
            return Entail.UNKNOWN;
        }

        @Override public Constraint negation() { return new Linear(exactCoeffs(), vars, Rel.EQ, k); }
    }
    // END_CHANGE: ISS-2025-0641

    // START_CHANGE: ISS-2025-0783 - 4.6 wave Q6 (extra 2): |X - Y| =\= C as ONE propagator
    // (SWI's absdiff_neq). Posted through the general path, abs(X-Y) #\= C became an auxiliary
    // D = X - Y, an auxiliary A = abs(D) and A =\= C: nothing was pruned until BOTH X and Y were
    // fixed, so N-queens written with abs/1 lost all its forward checking (20 queens: 3.9 s).
    // Here a fixed X removes Y = X - C and Y = X + C at once, and symmetrically.
    public static final class AbsDiffNE extends Constraint {
        private final FdVar x, y;
        private final long c;
        public AbsDiffNE(FdVar x, FdVar y, long c) { this.x = x; this.y = y; this.c = c; }

        @Override public List<FdVar> variables() { return Arrays.asList(x, y); }

        @Override protected boolean wakesOnFixOnly() { return true; }

        @Override void render(Residuals r, List<it.denzosoft.jprolog.core.terms.Term> out) {
            out.add(Residuals.op("#\\=", Residuals.op("abs", Residuals.op("-", r.term(x), r.term(y))),
                it.denzosoft.jprolog.core.terms.Number.valueOf(c)));
        }

        @Override public boolean propagate(ClpStore s) {
            IntervalDomain dx = s.dom(x), dy = s.dom(y);
            if (dx.isEmpty() || dy.isEmpty()) return false;
            boolean fx = dx.isSingleton(), fy = dy.isSingleton();
            if (fx && fy) {
                long d = sub(dx.value(), dy.value());
                return isInf(d) || absSat(d) != c;
            }
            if (fx) return removeBoth(s, y, dx.value());
            if (fy) return removeBoth(s, x, dy.value());
            return true;
        }

        private boolean removeBoth(ClpStore s, FdVar v, long at) {
            long lo = sub(at, c), hi = add(at, c);
            if (!isInf(lo) && !s.removeValue(v, lo)) return false;
            return isInf(hi) || s.removeValue(v, hi);
        }

        @Override public Entail entailment(ClpStore s) {
            IntervalDomain dx = s.dom(x), dy = s.dom(y);
            if (dx.isSingleton() && dy.isSingleton()) {
                long d = sub(dx.value(), dy.value());
                return (isInf(d) || absSat(d) != c) ? Entail.TRUE : Entail.FALSE;
            }
            // one side fixed and both forbidden values already gone: nothing left to say (SWI
            // kills the propagator then, so it is no residual goal either)
            if (dx.isSingleton()) return excludes(dy, dx.value()) ? Entail.TRUE : Entail.UNKNOWN;
            if (dy.isSingleton()) return excludes(dx, dy.value()) ? Entail.TRUE : Entail.UNKNOWN;
            return Entail.UNKNOWN;
        }

        private boolean excludes(IntervalDomain d, long at) {
            long lo = sub(at, c), hi = add(at, c);
            return (isInf(lo) || !d.contains(lo)) && (isInf(hi) || !d.contains(hi));
        }
    }
    // END_CHANGE: ISS-2025-0783

    // =====================================================================================
    // Reification:  B #<==> C    (B is 0/1; B=1 iff C holds)
    // =====================================================================================
    public static final class Reified extends Constraint {
        private final FdVar b;
        private final Constraint c;
        private final Constraint notC;

        public Reified(FdVar b, Constraint c) {
            this.b = b;
            this.c = c;
            this.notC = c.negation();   // requires C to support negation
        }
        // START_CHANGE: ISS-2025-0760 - residual goal
        @Override public boolean alive(ClpStore s) {
            IntervalDomain db = s.dom(b);
            if (db.isSingleton()) return (db.value() == 1 ? c : notC).alive(s);
            return c.entailment(s) == Entail.UNKNOWN;
        }
        @Override void render(Residuals r, List<it.denzosoft.jprolog.core.terms.Term> out) {
            IntervalDomain db = r.store().dom(b);
            if (db.isSingleton()) { (db.value() == 1 ? c : notC).render(r, out); return; }
            it.denzosoft.jprolog.core.terms.Term inner = c.reifiedForm(r);
            if (inner != null) out.add(Residuals.op("#<==>", inner, r.term(b)));
        }
        // END_CHANGE: ISS-2025-0760

        @Override public List<FdVar> variables() {
            List<FdVar> vs = new ArrayList<>();
            vs.add(b);
            vs.addAll(c.variables());
            return vs;
        }

        @Override public boolean propagate(ClpStore s) {
            if (!s.narrow(b, IntervalDomain.interval(0, 1))) return false;  // B is boolean
            IntervalDomain db = s.dom(b);
            if (db.isSingleton()) {
                return db.value() == 1 ? c.propagate(s) : notC.propagate(s);
            }
            Entail e = c.entailment(s);
            if (e == Entail.TRUE) return s.assign(b, 1);
            if (e == Entail.FALSE) return s.assign(b, 0);
            return true;
        }
    }

    // START_CHANGE: ISS-2025-0647 - boolean connectives and reifiable domain membership
    // =====================================================================================
    // Boolean connective over 0/1 variables:  z = a OP b   (generalised arc consistency)
    // =====================================================================================
    public enum BoolOp { AND, OR, XOR, IMPL, EQUIV }

    public static final class Bool extends Constraint {
        private final BoolOp op;
        private final FdVar a, b, z;
        public Bool(BoolOp op, FdVar a, FdVar b, FdVar z) { this.op = op; this.a = a; this.b = b; this.z = z; }
        // START_CHANGE: ISS-2025-0760 - residual goal
        /** Dead once every combination the domains allow satisfies it. */
        @Override public boolean alive(ClpStore s) {
            IntervalDomain da = s.dom(a), db = s.dom(b), dz = s.dom(z);
            for (int x = 0; x <= 1; x++) {
                if (!da.contains(x)) continue;
                for (int y = 0; y <= 1; y++) {
                    if (!db.contains(y)) continue;
                    for (int w = 0; w <= 1; w++) {
                        if (dz.contains(w) && (apply(op, x == 1, y == 1) ? 1 : 0) != w) return true;
                    }
                }
            }
            return false;
        }
        @Override void render(Residuals r, List<it.denzosoft.jprolog.core.terms.Term> out) {
            String f;
            switch (op) {
                case AND: f = "#/\\"; break;
                case OR: f = "#\\/"; break;
                case XOR: f = "#\\"; break;
                case IMPL: f = "#==>"; break;
                default: f = "#<==>"; break;
            }
            it.denzosoft.jprolog.core.terms.Term body = Residuals.op(f, r.term(a), r.term(b));
            IntervalDomain dz = r.store().dom(z);
            if (dz.isSingleton()) out.add(dz.value() == 1 ? body : Residuals.op("#\\", body));
            else out.add(Residuals.op("#<==>", body, r.term(z)));
        }
        // END_CHANGE: ISS-2025-0760

        @Override public List<FdVar> variables() { return Arrays.asList(a, b, z); }

        static boolean apply(BoolOp op, boolean x, boolean y) {
            switch (op) {
                case AND: return x && y;
                case OR: return x || y;
                case XOR: return x != y;
                case IMPL: return !x || y;
                default: return x == y;
            }
        }

        @Override public boolean propagate(ClpStore s) {
            IntervalDomain bool = IntervalDomain.interval(0, 1);
            if (!s.narrow(a, bool) || !s.narrow(b, bool) || !s.narrow(z, bool)) return false;
            IntervalDomain da = s.dom(a), db = s.dom(b), dz = s.dom(z);
            boolean[] sa = new boolean[2], sb = new boolean[2], sz = new boolean[2];
            for (int x = 0; x <= 1; x++) {
                if (!da.contains(x)) continue;
                for (int y = 0; y <= 1; y++) {
                    if (!db.contains(y)) continue;
                    int r = apply(op, x == 1, y == 1) ? 1 : 0;
                    if (!dz.contains(r)) continue;
                    sa[x] = true; sb[y] = true; sz[r] = true;
                }
            }
            return keep(s, a, sa) && keep(s, b, sb) && keep(s, z, sz);
        }

        private static boolean keep(ClpStore s, FdVar v, boolean[] sup) {
            if (!sup[0] && !sup[1]) return false;
            if (!sup[0]) return s.narrow(v, IntervalDomain.singleton(1));
            if (!sup[1]) return s.narrow(v, IntervalDomain.singleton(0));
            return true;
        }
    }

    /** {@code X in D} as a reifiable constraint. */
    public static final class InDomain extends Constraint {
        private final FdVar x;
        private final IntervalDomain d;
        public InDomain(FdVar x, IntervalDomain d) { this.x = x; this.d = d; }
        // START_CHANGE: ISS-2025-0760 - residual goal
        @Override it.denzosoft.jprolog.core.terms.Term reifiedForm(Residuals r) {
            return Residuals.op("in", r.term(x), ClpfdV2Bridge.domainToTerm(d));
        }
        // END_CHANGE: ISS-2025-0760

        @Override public List<FdVar> variables() { return Arrays.asList(x); }
        @Override public boolean propagate(ClpStore s) { return s.narrow(x, d); }

        @Override public Entail entailment(ClpStore s) {
            IntervalDomain dx = s.dom(x);
            if (dx.subsetOf(d)) return Entail.TRUE;
            if (dx.intersect(d).isEmpty()) return Entail.FALSE;
            return Entail.UNKNOWN;
        }

        @Override public Constraint negation() { return new InDomain(x, d.complement()); }
    }
    // END_CHANGE: ISS-2025-0647

    // START_CHANGE: ISS-2025-0650 - element/3, tuples_in/2 and global_cardinality/2
    // =====================================================================================
    // element(I, [X1..Xn], V):  V = X_I   (1-based)
    // =====================================================================================
    public static final class Element extends Constraint {
        private final FdVar index, value;
        private final FdVar[] list;
        public Element(FdVar index, FdVar[] list, FdVar value) {
            this.index = index; this.list = list.clone(); this.value = value;
        }
        // START_CHANGE: ISS-2025-0760 - residual goal
        @Override void render(Residuals r, List<it.denzosoft.jprolog.core.terms.Term> out) {
            out.add(new it.denzosoft.jprolog.core.terms.CompoundTerm(new it.denzosoft.jprolog.core.terms.Atom("element"),
                Arrays.asList(r.term(index), Residuals.list(r.terms(Arrays.asList(list))), r.term(value))));
        }
        // END_CHANGE: ISS-2025-0760

        @Override public List<FdVar> variables() {
            List<FdVar> vs = new ArrayList<>(list.length + 2);
            vs.add(index);
            vs.addAll(Arrays.asList(list));
            vs.add(value);
            return vs;
        }

        @Override public boolean propagate(ClpStore s) {
            if (!s.narrow(index, IntervalDomain.interval(1, list.length))) return false;
            IntervalDomain di = s.dom(index), dv = s.dom(value);
            long[] keep = new long[list.length];
            int k = 0;
            IntervalDomain reach = IntervalDomain.EMPTY;
            for (int i = 1; i <= list.length; i++) {
                if (!di.contains(i)) continue;
                IntervalDomain de = s.dom(list[i - 1]);
                IntervalDomain both = de.intersect(dv);
                if (both.isEmpty()) continue;
                keep[k++] = i;
                reach = reach.union(both);
            }
            if (k == 0) return false;
            if (!s.narrow(index, IntervalDomain.fromValues(Arrays.copyOf(keep, k)))) return false;
            if (!s.narrow(value, reach)) return false;
            di = s.dom(index);
            if (di.isSingleton()) {
                FdVar e = list[(int) di.value() - 1];
                if (!s.narrow(e, s.dom(value))) return false;
                if (!s.narrow(value, s.dom(e))) return false;
            }
            return true;
        }
    }

    // =====================================================================================
    // tuples_in: (X1..Xk) must be one of the rows of a fixed relation (GAC by row filtering)
    // =====================================================================================
    public static final class Table extends Constraint {
        private final FdVar[] vars;
        private final long[][] rows;
        public Table(FdVar[] vars, long[][] rows) { this.vars = vars.clone(); this.rows = rows; }
        // START_CHANGE: ISS-2025-0760 - residual goal
        @Override void render(Residuals r, List<it.denzosoft.jprolog.core.terms.Term> out) {
            List<it.denzosoft.jprolog.core.terms.Term> rs = new ArrayList<>(rows.length);
            for (long[] row : rows) {
                List<it.denzosoft.jprolog.core.terms.Term> vs = new ArrayList<>(row.length);
                for (long v : row) vs.add(Residuals.num(v));
                rs.add(Residuals.list(vs));
            }
            it.denzosoft.jprolog.core.terms.Term tuple = Residuals.list(r.terms(Arrays.asList(vars)));
            out.add(Residuals.op("tuples_in", Residuals.list(Arrays.asList(tuple)), Residuals.list(rs)));
        }
        // END_CHANGE: ISS-2025-0760

        @Override public List<FdVar> variables() { return Arrays.asList(vars); }

        @Override public boolean propagate(ClpStore s) {
            int k = vars.length;
            IntervalDomain[] ds = new IntervalDomain[k];
            for (int j = 0; j < k; j++) ds[j] = s.dom(vars[j]);
            long[][] sup = new long[k][rows.length];
            int[] cnt = new int[k];
            int alive = 0;
            for (long[] row : rows) {
                boolean ok = true;
                for (int j = 0; j < k && ok; j++) ok = ds[j].contains(row[j]);
                if (!ok) continue;
                alive++;
                for (int j = 0; j < k; j++) sup[j][cnt[j]++] = row[j];
            }
            if (alive == 0) return false;
            for (int j = 0; j < k; j++) {
                if (!s.narrow(vars[j], IntervalDomain.fromValues(Arrays.copyOf(sup[j], cnt[j])))) return false;
            }
            return true;
        }
    }

    // =====================================================================================
    // global_cardinality(Vs, [K1-C1, ..]): every V is one of the keys, and key Ki occurs Ci times
    // =====================================================================================
    public static final class Gcc extends Constraint {
        private final FdVar[] vars;
        private final long[] keys;
        private final FdVar[] counts;
        public Gcc(FdVar[] vars, long[] keys, FdVar[] counts) {
            this.vars = vars.clone(); this.keys = keys.clone(); this.counts = counts.clone();
        }
        // START_CHANGE: ISS-2025-0760 - residual goal
        @Override void render(Residuals r, List<it.denzosoft.jprolog.core.terms.Term> out) {
            List<it.denzosoft.jprolog.core.terms.Term> pairs = new ArrayList<>(keys.length);
            for (int j = 0; j < keys.length; j++) pairs.add(Residuals.op("-", Residuals.num(keys[j]), r.term(counts[j])));
            out.add(Residuals.op("global_cardinality", Residuals.list(r.terms(Arrays.asList(vars))), Residuals.list(pairs)));
        }
        // END_CHANGE: ISS-2025-0760

        @Override public List<FdVar> variables() {
            List<FdVar> vs = new ArrayList<>(Arrays.asList(vars));
            vs.addAll(Arrays.asList(counts));
            return vs;
        }

        @Override public boolean propagate(ClpStore s) {
            IntervalDomain keyDom = IntervalDomain.fromValues(keys);
            for (FdVar v : vars) if (!s.narrow(v, keyDom)) return false;
            long sumMin = 0, sumMax = 0;
            for (int j = 0; j < keys.length; j++) {
                long fixed = 0, possible = 0;
                for (FdVar v : vars) {
                    IntervalDomain d = s.dom(v);
                    if (!d.contains(keys[j])) continue;
                    possible++;
                    if (d.isSingleton()) fixed++;
                }
                if (!s.narrow(counts[j], IntervalDomain.interval(fixed, possible))) return false;
                IntervalDomain dc = s.dom(counts[j]);
                if (dc.max() == fixed && possible > fixed) {
                    for (FdVar v : vars) {                             // the key is saturated
                        IntervalDomain d = s.dom(v);
                        if (!d.isSingleton() && d.contains(keys[j])) {
                            if (!s.removeValue(v, keys[j])) return false;
                        }
                    }
                } else if (dc.min() == possible && possible > fixed) {
                    for (FdVar v : vars) {                             // every candidate must take it
                        IntervalDomain d = s.dom(v);
                        if (!d.isSingleton() && d.contains(keys[j])) {
                            if (!s.assign(v, keys[j])) return false;
                        }
                    }
                }
                sumMin = add(sumMin, s.dom(counts[j]).min());
                sumMax = add(sumMax, s.dom(counts[j]).max());
            }
            return sumMin <= vars.length && sumMax >= vars.length;
        }
    }
    // END_CHANGE: ISS-2025-0650
}
