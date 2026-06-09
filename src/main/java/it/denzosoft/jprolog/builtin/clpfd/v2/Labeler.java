package it.denzosoft.jprolog.builtin.clpfd.v2;

import it.denzosoft.jprolog.builtin.clpfd.v2.ClpStore.FdVar;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

/**
 * Depth-first labeling for the clean-room CLP(FD) store. After each value assignment it
 * propagates the full constraint store, so every emitted solution satisfies all constraints
 * (this is the soundness the legacy {@code indomain/labeling} lacked). Backtracking uses the
 * store's trail ({@link ClpStore#mark()}/{@link ClpStore#undo(int)}), not full snapshots, and
 * solutions are streamed through a sink so a caller can stop after the first one.
 */
public final class Labeler {

    private Labeler() {}

    /** Refuse to enumerate a domain larger than this (e.g. a var left on the default wide domain). */
    public static final long MAX_LABEL_DOMAIN = 10_000_000L;

    /** Thrown when a variable's domain is too large to enumerate during labeling. */
    public static final class TooLargeToLabel extends RuntimeException {
        public final long size;
        public TooLargeToLabel(long size) { super("CLP(FD) domain too large to label: " + size + " values"); this.size = size; }
    }

    /** Receives complete assignments; return false to stop the search. */
    public interface SolutionSink {
        boolean onSolution(Map<FdVar, Long> assignment);
    }

    /** Enumerate solutions for {@code vars}, streaming each to {@code sink}. */
    public static void label(ClpStore s, List<FdVar> vars, SolutionSink sink) {
        labelRec(s, vars, sink);
    }

    /** Convenience: collect all solutions (use only when the search space is bounded). */
    public static List<Map<FdVar, Long>> labelAll(ClpStore s, List<FdVar> vars) {
        List<Map<FdVar, Long>> out = new ArrayList<>();
        label(s, vars, sol -> { out.add(sol); return true; });
        return out;
    }

    /** Convenience: first solution or null. */
    public static Map<FdVar, Long> labelFirst(ClpStore s, List<FdVar> vars) {
        Map<FdVar, Long>[] holder = new Map[1];
        label(s, vars, sol -> { holder[0] = sol; return false; });
        return holder[0];
    }

    private static boolean labelRec(ClpStore s, List<FdVar> vars, SolutionSink sink) {
        // first-fail: pick the unassigned variable with the smallest domain
        FdVar chosen = null;
        long best = Long.MAX_VALUE;
        for (FdVar v : vars) {
            IntervalDomain d = s.dom(v);
            if (d.isEmpty()) return true;             // inconsistent branch; nothing to do
            if (!d.isSingleton()) {
                long sz = d.size();
                if (sz < best) { best = sz; chosen = v; }
            }
        }
        if (chosen == null) {
            Map<FdVar, Long> sol = new IdentityHashMap<>();
            for (FdVar v : vars) sol.put(v, s.dom(v).value());
            return sink.onSolution(sol);             // false => stop the whole search
        }

        // START_CHANGE: ISS-2025-0298 - iterate candidate values LAZILY over the interval ranges
        // (no list materialization), and refuse to enumerate an unreasonably large domain (e.g. a
        // variable left on the default wide domain) instead of allocating gigabytes / OOM.
        if (s.dom(chosen).size() > MAX_LABEL_DOMAIN) {
            throw new TooLargeToLabel(s.dom(chosen).size());
        }
        long[][] ranges = s.dom(chosen).rangeArray();   // snapshot; safe to iterate while the store mutates
        for (long[] r : ranges) {
            for (long val = r[0]; val <= r[1]; val++) {
                int mark = s.mark();
                if (s.assign(chosen, val) && s.propagate()) {
                    if (!labelRec(s, vars, sink)) {     // stop requested deeper down
                        s.undo(mark);
                        return false;
                    }
                }
                s.undo(mark);
                if (val == Long.MAX_VALUE) break;       // guard the val++ overflow
            }
        }
        return true;
        // END_CHANGE: ISS-2025-0298
    }
}
