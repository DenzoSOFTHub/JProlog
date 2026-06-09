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

    private IntervalDomain(List<long[]> ranges) {
        this.ranges = ranges;
    }

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
    public boolean isSingleton() { return ranges.size() == 1 && ranges.get(0)[0] == ranges.get(0)[1]; }

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
        List<long[]> out = new ArrayList<>();
        for (long[] r : ranges) {
            if (r[1] < bound) continue;
            out.add(new long[]{Math.max(r[0], bound), r[1]});
        }
        return out.isEmpty() ? EMPTY : new IntervalDomain(out);
    }

    /** Remove all values above {@code bound} (keep <= bound). */
    public IntervalDomain removeAbove(long bound) {
        List<long[]> out = new ArrayList<>();
        for (long[] r : ranges) {
            if (r[0] > bound) continue;
            out.add(new long[]{r[0], Math.min(r[1], bound)});
        }
        return out.isEmpty() ? EMPTY : new IntervalDomain(out);
    }

    /** Remove a single value (may split an interval). */
    public IntervalDomain removeValue(long v) {
        if (!contains(v)) return this;
        List<long[]> out = new ArrayList<>();
        for (long[] r : ranges) {
            if (v < r[0] || v > r[1]) { out.add(r); continue; }
            if (r[0] < v) out.add(new long[]{r[0], v - 1});
            if (v < r[1]) out.add(new long[]{v + 1, r[1]});
        }
        return out.isEmpty() ? EMPTY : new IntervalDomain(out);
    }

    /** Intersect with another domain. */
    public IntervalDomain intersect(IntervalDomain other) {
        List<long[]> out = new ArrayList<>();
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
            if (r[0] == r[1]) sb.append(r[0]);
            else sb.append(r[0]).append("..").append(r[1]);
        }
        return sb.toString();
    }
}
