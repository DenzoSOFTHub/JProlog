package it.denzosoft.jprolog.builtin.clpfd.v2;

import java.util.ArrayList;
import java.util.List;

/**
 * An immutable finite-domain value set represented as a sorted list of disjoint,
 * non-adjacent integer intervals {@code [lo, hi]} (inclusive, {@code long} bounds).
 *
 * <p>This is the core of the clean-room CLP(FD) rewrite: domains are stored as O(#intervals)
 * ranges rather than O(#values) boxed integers, so a legal goal like {@code X in 1..2147483647}
 * is a single interval — no {@code OutOfMemoryError} and no billion-element {@code TreeSet}.
 *
 * <p>All narrowing operations are pure (return a new domain), which makes trail-based
 * backtracking trivial (record the previous domain object and restore it).
 */
public final class IntervalDomain {

    /** Flattened sorted disjoint intervals: ranges.get(i) = {lo, hi}. */
    private final List<long[]> ranges;

    public static final IntervalDomain EMPTY = new IntervalDomain(new ArrayList<>());

    // START_CHANGE: ISS-2025-0644 - unbounded domains (SWI inf..sup). Long.MIN_VALUE stands for
    // -infinity and Long.MAX_VALUE for +infinity; representable finite values are strictly between.
    // A bound that is computed beyond the representable range is clamped to the matching infinity,
    // which only ever LOSES pruning (sound), and a domain whose only "value" is an infinity is never
    // a singleton, so propagation can never bind a variable to a wrong clamped value.
    /** -infinity marker ({@code inf}). */
    public static final long INF = Long.MIN_VALUE;
    /** +infinity marker ({@code sup}). */
    public static final long SUP = Long.MAX_VALUE;
    /** The unconstrained domain {@code inf..sup}. */
    public static final IntervalDomain ALL = interval(INF, SUP);

    /** True when {@code v} is one of the two infinity markers. */
    public static boolean isInfinite(long v) { return v == INF || v == SUP; }
    // END_CHANGE: ISS-2025-0644

    // START_CHANGE: ISS-2025-0644 - a value beyond the 64-bit range: the degenerate domain sup..sup
    // (or inf..inf) that ALSO remembers the exact value, so a constraint whose other variables are
    // all fixed can solve for its last variable exactly (X #= Y + 1, Y = 2^63 binds X = 2^63+1).
    // Propagation sees only the infinite bound (sound: it can only lose pruning).
    private final java.math.BigInteger exact;

    private IntervalDomain(List<long[]> ranges) {
        this(ranges, null);
    }

    private IntervalDomain(List<long[]> ranges, java.math.BigInteger exact) {
        this.ranges = ranges;
        this.exact = exact;
    }

    /** The degenerate domain of an integer beyond the representable range. */
    public static IntervalDomain bigValue(java.math.BigInteger v) {
        long end = v.signum() > 0 ? SUP : INF;
        List<long[]> r = new ArrayList<>(1);
        r.add(new long[]{end, end});
        return new IntervalDomain(r, v);
    }

    /** The exact out-of-range value this domain stands for, or null. */
    public java.math.BigInteger exactBig() { return exact; }
    // END_CHANGE: ISS-2025-0644

    /** A single contiguous interval [lo, hi]; empty if lo > hi. */
    public static IntervalDomain interval(long lo, long hi) {
        if (lo > hi) return EMPTY;
        List<long[]> r = new ArrayList<>(1);
        r.add(new long[]{lo, hi});
        return new IntervalDomain(r);
    }

    /** A singleton {v}. */
    public static IntervalDomain singleton(long v) {
        return interval(v, v);
    }

    /** Build from an arbitrary set of values (merged into intervals). */
    public static IntervalDomain fromValues(long... values) {
        if (values.length == 0) return EMPTY;
        long[] sorted = values.clone();
        java.util.Arrays.sort(sorted);
        List<long[]> r = new ArrayList<>();
        long lo = sorted[0], hi = sorted[0];
        for (int i = 1; i < sorted.length; i++) {
            if (sorted[i] == hi) continue;
            if (sorted[i] == hi + 1) { hi = sorted[i]; continue; }
            r.add(new long[]{lo, hi});
            lo = hi = sorted[i];
        }
        r.add(new long[]{lo, hi});
        return new IntervalDomain(r);
    }

    public boolean isEmpty() { return ranges.isEmpty(); }
    // START_CHANGE: ISS-2025-0644 - an infinity marker is not a value
    public boolean isSingleton() {
        if (ranges.size() != 1) return false;
        long[] r = ranges.get(0);
        return r[0] == r[1] && !isInfinite(r[0]);
    }

    /** Both bounds finite (labeling needs this; SWI raises an instantiation error otherwise). */
    public boolean isFinite() {
        return !isEmpty() && ranges.get(0)[0] != INF && ranges.get(ranges.size() - 1)[1] != SUP;
    }

    /** Number of disjoint intervals. */
    public int intervalCount() { return ranges.size(); }
    // END_CHANGE: ISS-2025-0644

    /** The disjoint intervals as {lo, hi} pairs (a copy), sorted ascending. */
    public long[][] rangeArray() {
        long[][] out = new long[ranges.size()][];
        for (int i = 0; i < ranges.size(); i++) out[i] = ranges.get(i).clone();
        return out;
    }

    public long min() {
        if (isEmpty()) throw new IllegalStateException("min of empty domain");
        return ranges.get(0)[0];
    }
    public long max() {
        if (isEmpty()) throw new IllegalStateException("max of empty domain");
        return ranges.get(ranges.size() - 1)[1];
    }

    /** The singleton value (call only when {@link #isSingleton()}). */
    public long value() { return min(); }

    /** Number of values, capped at {@link Long#MAX_VALUE} (intervals can be huge). */
    public long size() {
        if (!isEmpty() && !isFinite()) return Long.MAX_VALUE;    // ISS-2025-0644: unbounded
        long n = 0;
        for (long[] r : ranges) {
            // ISS-2025-0297: overflow-safe (MIN..MAX would wrap with r[1]-r[0]+1)
            long span = r[1] - r[0];
            if (span < 0 || span == Long.MAX_VALUE) return Long.MAX_VALUE;
            long cnt = span + 1;
            if (n > Long.MAX_VALUE - cnt) return Long.MAX_VALUE;
            n += cnt;
        }
        return n;
    }

    public boolean contains(long v) {
        for (long[] r : ranges) {
            if (v < r[0]) return false;
            if (v <= r[1]) return true;
        }
        return false;
    }

    /** Remove all values below {@code bound} (keep >= bound). */
    public IntervalDomain removeBelow(long bound) {
        List<long[]> out = new ArrayList<>(ranges.size());
        for (long[] r : ranges) {
            if (r[1] < bound) continue;
            out.add(new long[]{Math.max(r[0], bound), r[1]});
        }
        return out.isEmpty() ? EMPTY : new IntervalDomain(out);
    }

    /** Remove all values above {@code bound} (keep <= bound). */
    public IntervalDomain removeAbove(long bound) {
        List<long[]> out = new ArrayList<>(ranges.size());
        for (long[] r : ranges) {
            if (r[0] > bound) continue;
            out.add(new long[]{r[0], Math.min(r[1], bound)});
        }
        return out.isEmpty() ? EMPTY : new IntervalDomain(out);
    }

    /** Remove a single value (may split an interval). */
    public IntervalDomain removeValue(long v) {
        if (isInfinite(v)) return this;                           // ISS-2025-0644: not a value
        if (!contains(v)) return this;
        List<long[]> out = new ArrayList<>(ranges.size() + 1);
        for (long[] r : ranges) {
            if (v < r[0] || v > r[1]) { out.add(r); continue; }
            if (r[0] < v) out.add(new long[]{r[0], v - 1});
            if (v < r[1]) out.add(new long[]{v + 1, r[1]});
        }
        return out.isEmpty() ? EMPTY : new IntervalDomain(out);
    }

    /** Intersect with another domain. */
    public IntervalDomain intersect(IntervalDomain other) {
        // ISS-2025-0642: the common "restriction contains this domain" case allocates nothing
        if (other.ranges.size() == 1 && !ranges.isEmpty()) {
            long[] o = other.ranges.get(0);
            if (o[0] <= ranges.get(0)[0] && o[1] >= ranges.get(ranges.size() - 1)[1]) return this;
        }
        List<long[]> out = new ArrayList<>(ranges.size() + other.ranges.size());
        int i = 0, j = 0;
        while (i < ranges.size() && j < other.ranges.size()) {
            long[] a = ranges.get(i), b = other.ranges.get(j);
            long lo = Math.max(a[0], b[0]);
            long hi = Math.min(a[1], b[1]);
            if (lo <= hi) out.add(new long[]{lo, hi});
            if (a[1] < b[1]) i++; else j++;
        }
        return out.isEmpty() ? EMPTY : new IntervalDomain(out);
    }

    // START_CHANGE: ISS-2025-0641 - domains with holes: unions and complements (X in 1..3 \/ 5..7,
    // the negation of a reified X in D).
    /** The union of two domains (sorted, disjoint, adjacent intervals merged). */
    public IntervalDomain union(IntervalDomain other) {
        if (other.isEmpty()) return this;
        if (isEmpty()) return other;
        List<long[]> all = new ArrayList<>(ranges.size() + other.ranges.size());
        int i = 0, j = 0;
        while (i < ranges.size() || j < other.ranges.size()) {
            long[] next;
            if (j >= other.ranges.size() || (i < ranges.size() && ranges.get(i)[0] <= other.ranges.get(j)[0])) {
                next = ranges.get(i++);
            } else {
                next = other.ranges.get(j++);
            }
            if (!all.isEmpty()) {
                long[] last = all.get(all.size() - 1);
                if (last[1] == SUP || next[0] <= last[1] + 1) {    // overlapping or adjacent
                    if (next[1] > last[1]) last[1] = next[1];
                    continue;
                }
            }
            all.add(new long[]{next[0], next[1]});
        }
        return new IntervalDomain(all);
    }

    /** Every value of {@code inf..sup} NOT in this domain. */
    public IntervalDomain complement() {
        List<long[]> out = new ArrayList<>();
        long from = INF;                                          // start of the next uncovered gap
        boolean open = true;                                      // false once a range reaches sup
        for (long[] r : ranges) {
            if (r[0] != INF && from <= r[0] - 1) out.add(new long[]{from, r[0] - 1});
            if (r[1] == SUP) { open = false; break; }
            from = r[1] + 1;
        }
        if (open) out.add(new long[]{from, SUP});
        return out.isEmpty() ? EMPTY : new IntervalDomain(out);
    }

    /** True when every value of this domain is also in {@code other}. */
    public boolean subsetOf(IntervalDomain other) {
        return intersect(other).equals(this);
    }

    /** The largest value strictly below {@code v} in the domain, or {@link #INF} if none. */
    public long prevBelow(long v) {
        for (int i = ranges.size() - 1; i >= 0; i--) {
            long[] r = ranges.get(i);
            if (r[0] < v) return Math.min(r[1], v - 1);
        }
        return INF;
    }
    // END_CHANGE: ISS-2025-0641

    /** All values in ascending order (use only for labeling a reasonably-sized domain). */
    public Iterable<Long> values() {
        List<Long> vs = new ArrayList<>();
        for (long[] r : ranges) {
            for (long v = r[0]; v <= r[1]; v++) {
                vs.add(v);
                if (vs.size() > 100_000_000) throw new IllegalStateException("domain too large to enumerate");
            }
        }
        return vs;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof IntervalDomain)) return false;
        IntervalDomain d = (IntervalDomain) o;
        if (ranges.size() != d.ranges.size()) return false;
        if (exact == null ? d.exact != null : !exact.equals(d.exact)) return false;   // ISS-2025-0644
        for (int i = 0; i < ranges.size(); i++) {
            if (ranges.get(i)[0] != d.ranges.get(i)[0] || ranges.get(i)[1] != d.ranges.get(i)[1]) return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        int h = 1;
        for (long[] r : ranges) h = 31 * h + (int) (r[0] ^ r[1]);
        return h;
    }

    @Override
    public String toString() {
        if (isEmpty()) return "{}";
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < ranges.size(); i++) {
            if (i > 0) sb.append(" \\/ ");
            long[] r = ranges.get(i);
            // ISS-2025-0644: infinity markers print as inf/sup
            String lo = r[0] == INF ? "inf" : String.valueOf(r[0]);
            String hi = r[1] == SUP ? "sup" : String.valueOf(r[1]);
            if (r[0] == r[1]) sb.append(lo);
            else sb.append(lo).append("..").append(hi);
        }
        return sb.toString();
    }
}
