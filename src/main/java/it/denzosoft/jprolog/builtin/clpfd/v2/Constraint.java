package it.denzosoft.jprolog.builtin.clpfd.v2;

import it.denzosoft.jprolog.builtin.clpfd.v2.ClpStore.FdVar;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * A CLP(FD) constraint. {@link #propagate(ClpStore)} narrows the domains of its variables to
 * (a superset of) the consistent assignments and returns false on a wipeout. Constraints are
 * re-awoken by the store's propagation queue whenever a watched variable narrows.
 */
public abstract class Constraint {

    /** Entailment status of a constraint given the current domains (for reification). */
    public enum Entail { TRUE, FALSE, UNKNOWN }

    public abstract List<FdVar> variables();

    /** Narrow domains; return false if the constraint is unsatisfiable in the current store. */
    public abstract boolean propagate(ClpStore s);

    /** Is the constraint already entailed/disentailed by the current domains? (Default: unknown.) */
    public Entail entailment(ClpStore s) { return Entail.UNKNOWN; }

    /** The logical negation of this constraint (override where reification is supported). */
    public Constraint negation() {
        throw new UnsupportedOperationException("negation not supported for " + getClass().getSimpleName());
    }

    // ------------------------------------------------------------- overflow-safe long math
    static long add(long a, long b) {
        long r = a + b;
        if (((a ^ r) & (b ^ r)) < 0) return a > 0 ? Long.MAX_VALUE : Long.MIN_VALUE; // overflow -> saturate
        return r;
    }
    static long sub(long a, long b) {
        return add(a, b == Long.MIN_VALUE ? Long.MAX_VALUE : -b);
    }

    // =====================================================================================
    // Binary comparison: a <rel> b   (LT, LE, GT, GE, EQ, NE)
    // =====================================================================================
    public enum Rel { LT, LE, GT, GE, EQ, NE }

    public static final class Cmp extends Constraint {
        private final FdVar a, b;
        private final Rel rel;
        public Cmp(FdVar a, Rel rel, FdVar b) { this.a = a; this.rel = rel; this.b = b; }

        @Override public List<FdVar> variables() { return Arrays.asList(a, b); }

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
                case EQ: // a == b: domains must be equal -> intersect both
                    if (!s.narrow(a, db)) return false;
                    if (!s.narrow(b, s.dom(a))) return false;
                    return true;
                case NE: // a != b: when one side is fixed, remove that value from the other
                    da = s.dom(a); db = s.dom(b);
                    if (da.isSingleton()) {
                        if (!s.removeValue(b, da.value())) return false;
                    }
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

        @Override public List<FdVar> variables() { return Arrays.asList(x, y, z); }

        @Override public boolean propagate(ClpStore s) {
            IntervalDomain dx = s.dom(x), dy = s.dom(y), dz = s.dom(z);
            if (dx.isEmpty() || dy.isEmpty() || dz.isEmpty()) return false;
            // z in [xmin+ymin .. xmax+ymax]
            if (!s.narrow(z, IntervalDomain.interval(add(dx.min(), dy.min()), add(dx.max(), dy.max())))) return false;
            dz = s.dom(z);
            // x in [zmin-ymax .. zmax-ymin]
            if (!s.narrow(x, IntervalDomain.interval(sub(dz.min(), dy.max()), sub(dz.max(), dy.min())))) return false;
            dx = s.dom(x);
            // y in [zmin-xmax .. zmax-xmin]
            if (!s.narrow(y, IntervalDomain.interval(sub(dz.min(), dx.max()), sub(dz.max(), dx.min())))) return false;
            return true;
        }
    }

    // =====================================================================================
    // all_different: singleton elimination + pigeonhole (Hall-interval-lite) infeasibility
    // =====================================================================================
    public static final class AllDifferent extends Constraint {
        private final List<FdVar> vars;
        public AllDifferent(List<FdVar> vars) { this.vars = new ArrayList<>(vars); }

        @Override public List<FdVar> variables() { return vars; }

        @Override public boolean propagate(ClpStore s) {
            // 1) singleton elimination: remove each fixed value from the other variables.
            boolean changed = true;
            while (changed) {
                changed = false;
                for (FdVar fixed : vars) {
                    IntervalDomain df = s.dom(fixed);
                    if (!df.isSingleton()) continue;
                    long val = df.value();
                    for (FdVar other : vars) {
                        if (other == fixed) continue;
                        IntervalDomain dOld = s.dom(other);
                        if (dOld.contains(val)) {
                            if (!s.removeValue(other, val)) return false;
                            changed = true;
                        }
                    }
                }
            }
            // 2) pigeonhole: N variables cannot fit in fewer than N distinct values.
            long lo = Long.MAX_VALUE, hi = Long.MIN_VALUE;
            for (FdVar v : vars) {
                IntervalDomain d = s.dom(v);
                if (d.isEmpty()) return false;
                lo = Math.min(lo, d.min());
                hi = Math.max(hi, d.max());
            }
            // ISS-2025-0297: overflow-safe span (hi-lo can overflow for MIN..MAX)
            long span = sub(hi, lo);
            long unionSpan = (span == Long.MAX_VALUE) ? Long.MAX_VALUE : span + 1;
            if (unionSpan >= 0 && unionSpan < vars.size()) return false; // not enough distinct values
            return true;
        }
    }

    // ------------------------------------------------------------- overflow-safe multiply / division
    static long mul(long a, long b) {
        // START_CHANGE: ISS-2025-0297 - -1 * MIN_VALUE overflows but a*a/-check misses it
        if ((a == -1 && b == Long.MIN_VALUE) || (b == -1 && a == Long.MIN_VALUE)) return Long.MAX_VALUE;
        // END_CHANGE: ISS-2025-0297
        long r = a * b;
        if (a != 0 && (r / a != b)) return (a > 0) == (b > 0) ? Long.MAX_VALUE : Long.MIN_VALUE;
        return r;
    }
    /** Overflow-safe |v| (|MIN_VALUE| saturates to MAX_VALUE). */
    static long absSat(long v) { return v == Long.MIN_VALUE ? Long.MAX_VALUE : Math.abs(v); }
    static long minOf(long a, long b, long c, long d) { return Math.min(Math.min(a, b), Math.min(c, d)); }
    static long maxOf(long a, long b, long c, long d) { return Math.max(Math.max(a, b), Math.max(c, d)); }

    // =====================================================================================
    // Product:  z = x * y   (interval multiplication; backward division when an operand is fixed)
    // =====================================================================================
    public static final class Mul extends Constraint {
        private final FdVar x, y, z;
        public Mul(FdVar x, FdVar y, FdVar z) { this.x = x; this.y = y; this.z = z; }

        @Override public List<FdVar> variables() { return Arrays.asList(x, y, z); }

        @Override public boolean propagate(ClpStore s) {
            IntervalDomain dx = s.dom(x), dy = s.dom(y);
            if (dx.isEmpty() || dy.isEmpty() || s.dom(z).isEmpty()) return false;
            long p1 = mul(dx.min(), dy.min()), p2 = mul(dx.min(), dy.max());
            long p3 = mul(dx.max(), dy.min()), p4 = mul(dx.max(), dy.max());
            if (!s.narrow(z, IntervalDomain.interval(minOf(p1, p2, p3, p4), maxOf(p1, p2, p3, p4)))) return false;

            // backward: if y is a fixed non-zero constant, x in [zmin/c .. zmax/c] (sign-aware)
            if (!divideInto(s, y, x)) return false;
            if (!divideInto(s, x, y)) return false;
            return true;
        }

        /** If {@code divisor} is a fixed non-zero value c, narrow {@code target} so target = z / c. */
        private boolean divideInto(ClpStore s, FdVar divisor, FdVar target) {
            IntervalDomain dd = s.dom(divisor);
            if (!dd.isSingleton()) return true;
            long c = dd.value();
            if (c == 0) return true;
            IntervalDomain dz = s.dom(z);
            long a = Math.floorDiv(dz.min(), c), b = Math.floorDiv(dz.max(), c);
            long lo = Math.min(a, b), hi = Math.max(a, b);
            // tighten to exact multiples is not required for bounds consistency
            return s.narrow(target, IntervalDomain.interval(lo, hi));
        }
    }

    // START_CHANGE: ISS-2025-0421 - square propagator (z = x*x): tighter than Mul(x,x,z) because
    // it knows z >= 0 and can invert through the integer square root in both sign cases.
    // =====================================================================================
    // Square:  z = x * x
    // =====================================================================================
    public static final class Square extends Constraint {
        private final FdVar x, z;
        public Square(FdVar x, FdVar z) { this.x = x; this.z = z; }

        @Override public List<FdVar> variables() { return Arrays.asList(x, z); }

        @Override public boolean propagate(ClpStore s) {
            IntervalDomain dx = s.dom(x), dz = s.dom(z);
            if (dx.isEmpty() || dz.isEmpty()) return false;
            long lo = dx.min(), hi = dx.max();
            long sLo = mul(lo, lo), sHi = mul(hi, hi);
            long zmin = (lo <= 0 && hi >= 0) ? 0 : Math.min(sLo, sHi);
            long zmax = Math.max(sLo, sHi);
            if (!s.narrow(z, IntervalDomain.interval(zmin, zmax))) return false;
            dz = s.dom(z);
            // backward: |x| in [ceil(sqrt(zmin)) .. floor(sqrt(zmax))]
            long r = floorSqrt(dz.max());
            long c = ceilSqrt(Math.max(dz.min(), 0));
            dx = s.dom(x);
            if (dx.min() >= 0) return s.narrow(x, IntervalDomain.interval(c, r));
            if (dx.max() <= 0) return s.narrow(x, IntervalDomain.interval(-r, -c));
            return s.narrow(x, IntervalDomain.interval(-r, r));
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

        @Override public List<FdVar> variables() { return Arrays.asList(x, y, z); }

        @Override public boolean propagate(ClpStore s) {
            IntervalDomain dx = s.dom(x), dy = s.dom(y);
            if (dx.isEmpty() || dy.isEmpty() || s.dom(z).isEmpty()) return false;
            if (!s.narrow(z, IntervalDomain.interval(Math.min(dx.min(), dy.min()),
                                                     Math.min(dx.max(), dy.max())))) return false;
            long zmin = s.dom(z).min();
            if (!s.removeBelow(x, zmin)) return false;    // both operands are >= the minimum
            if (!s.removeBelow(y, zmin)) return false;
            // if one operand is certainly larger, z equals the other
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

        @Override public List<FdVar> variables() { return Arrays.asList(x, y, z); }

        @Override public boolean propagate(ClpStore s) {
            IntervalDomain dx = s.dom(x), dy = s.dom(y);
            if (dx.isEmpty() || dy.isEmpty() || s.dom(z).isEmpty()) return false;
            if (!s.narrow(z, IntervalDomain.interval(Math.max(dx.min(), dy.min()),
                                                     Math.max(dx.max(), dy.max())))) return false;
            long zmax = s.dom(z).max();
            if (!s.removeAbove(x, zmax)) return false;    // both operands are =< the maximum
            if (!s.removeAbove(y, zmax)) return false;
            // if one operand is certainly smaller, z equals the other
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

        @Override public List<FdVar> variables() { return Arrays.asList(x, y); }

        @Override public boolean propagate(ClpStore s) {
            IntervalDomain dx = s.dom(x);
            if (dx.isEmpty()) return false;
            long lo = dx.min(), hi = dx.max();
            long ymin = (lo <= 0 && hi >= 0) ? 0 : Math.min(absSat(lo), absSat(hi));
            long ymax = Math.max(absSat(lo), absSat(hi));
            if (!s.narrow(y, IntervalDomain.interval(ymin, ymax))) return false;
            // backward: x in [-ymax .. ymax]
            long ay = s.dom(y).max();
            if (!s.narrow(x, IntervalDomain.interval(-ay, ay))) return false;
            return true;
        }
    }

    // =====================================================================================
    // Modulo:  Z = X mod M   (M a fixed positive modulus; floor semantics, ISO §9.1.7)
    // =====================================================================================
    public static final class Mod extends Constraint {
        private final FdVar x, z;
        private final long m;
        public Mod(FdVar x, long m, FdVar z) { this.x = x; this.m = m; this.z = z; }

        @Override public List<FdVar> variables() { return Arrays.asList(x, z); }

        @Override public boolean propagate(ClpStore s) {
            if (m <= 0) return false;                                  // only positive moduli supported
            if (!s.narrow(z, IntervalDomain.interval(0, m - 1))) return false;   // Z in 0..M-1
            IntervalDomain dx = s.dom(x);
            if (dx.isEmpty()) return false;
            if (dx.isSingleton()) {                                    // X fixed -> Z fixed
                if (!s.narrow(z, IntervalDomain.singleton(Math.floorMod(dx.value(), m)))) return false;
            }
            return true;
        }
    }

    // =====================================================================================
    // Linear:  sum(coeff_i * var_i)  <rel>  constant   (bounds consistency)
    // =====================================================================================
    public static final class Linear extends Constraint {
        private final long[] coeffs;
        private final FdVar[] vars;
        private final Rel rel;       // EQ, LE, GE supported
        private final long k;

        public Linear(long[] coeffs, FdVar[] vars, Rel rel, long k) {
            if (coeffs.length != vars.length) throw new IllegalArgumentException("coeffs/vars length mismatch");
            this.coeffs = coeffs.clone();
            this.vars = vars.clone();
            this.rel = rel;
            this.k = k;
        }

        @Override public List<FdVar> variables() { return Arrays.asList(vars); }

        @Override public boolean propagate(ClpStore s) {
            // START_CHANGE: ISS-2025-0296 - bounds in EXACT BigInteger (no saturating-long overflow
            // that previously rejected satisfiable constraints / pruned valid solutions). Clamp to
            // long only at the final narrow() call.
            int n = vars.length;
            java.math.BigInteger lower = java.math.BigInteger.ZERO, upper = java.math.BigInteger.ZERO;
            java.math.BigInteger[] lo = new java.math.BigInteger[n], hi = new java.math.BigInteger[n];
            for (int i = 0; i < n; i++) {
                IntervalDomain d = s.dom(vars[i]);
                if (d.isEmpty()) return false;
                java.math.BigInteger c = java.math.BigInteger.valueOf(coeffs[i]);
                java.math.BigInteger a = c.multiply(java.math.BigInteger.valueOf(d.min()));
                java.math.BigInteger b = c.multiply(java.math.BigInteger.valueOf(d.max()));
                lo[i] = a.min(b); hi[i] = a.max(b);
                lower = lower.add(lo[i]); upper = upper.add(hi[i]);
            }
            java.math.BigInteger K = java.math.BigInteger.valueOf(k);
            if (rel == Rel.EQ && (lower.compareTo(K) > 0 || upper.compareTo(K) < 0)) return false;
            if (rel == Rel.LE && lower.compareTo(K) > 0) return false;
            if (rel == Rel.GE && upper.compareTo(K) < 0) return false;

            for (int i = 0; i < n; i++) {
                if (coeffs[i] == 0) continue;
                java.math.BigInteger restLow = lower.subtract(lo[i]);   // true sum of the OTHER terms
                java.math.BigInteger restHigh = upper.subtract(hi[i]);
                java.math.BigInteger termLo, termHi;                    // null = unbounded
                switch (rel) {
                    case EQ: termLo = K.subtract(restHigh); termHi = K.subtract(restLow); break;
                    case LE: termLo = null;                 termHi = K.subtract(restLow); break;
                    case GE: termLo = K.subtract(restHigh); termHi = null;                break;
                    default: continue;
                }
                java.math.BigInteger c = java.math.BigInteger.valueOf(coeffs[i]);
                java.math.BigInteger xLo, xHi;
                if (coeffs[i] > 0) {
                    xLo = (termLo == null) ? null : ceilDivBig(termLo, c);
                    xHi = (termHi == null) ? null : floorDivBig(termHi, c);
                } else {
                    xLo = (termHi == null) ? null : ceilDivBig(termHi, c);
                    xHi = (termLo == null) ? null : floorDivBig(termLo, c);
                }
                long loL = clampLong(xLo, Long.MIN_VALUE);
                long hiL = clampLong(xHi, Long.MAX_VALUE);
                if (loL > hiL) return false;
                if (!s.narrow(vars[i], IntervalDomain.interval(loL, hiL))) return false;
            }
            return true;
            // END_CHANGE: ISS-2025-0296
        }

        private static java.math.BigInteger floorDivBig(java.math.BigInteger a, java.math.BigInteger b) {
            java.math.BigInteger[] qr = a.divideAndRemainder(b);
            if (qr[1].signum() != 0 && qr[1].signum() != b.signum()) return qr[0].subtract(java.math.BigInteger.ONE);
            return qr[0];
        }
        private static java.math.BigInteger ceilDivBig(java.math.BigInteger a, java.math.BigInteger b) {
            return floorDivBig(a.negate(), b).negate();
        }
        private static long clampLong(java.math.BigInteger v, long unboundedDefault) {
            if (v == null) return unboundedDefault;
            if (v.compareTo(java.math.BigInteger.valueOf(Long.MAX_VALUE)) > 0) return Long.MAX_VALUE;
            if (v.compareTo(java.math.BigInteger.valueOf(Long.MIN_VALUE)) < 0) return Long.MIN_VALUE;
            return v.longValue();
        }
    }

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
            this.notC = c.negation();   // requires C to support negation (e.g. Cmp)
        }

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
            // B unknown: let C's entailment decide B.
            Entail e = c.entailment(s);
            if (e == Entail.TRUE) return s.assign(b, 1);
            if (e == Entail.FALSE) return s.assign(b, 0);
            return true;
        }
    }
}
