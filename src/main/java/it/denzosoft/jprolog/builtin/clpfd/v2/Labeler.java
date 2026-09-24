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

    // START_CHANGE: ISS-2025-0422 - labeling strategies (variable selection + value order)
    /** Variable-selection strategy (SWI labeling/2 option names). */
    public enum VarSel {
        /** First unassigned variable in list order. */          LEFTMOST,
        /** First-fail: smallest domain first. */                FF,
        /** Smallest lower bound first (SWI's bare {@code min}). */ MIN,
        /** Largest upper bound first (SWI's bare {@code max}). */  MAX,
        /** First-fail, ties broken by most constraints (ISS-2025-0642). */ FFC
    }

    /** Value-enumeration order within a domain. */
    public enum ValOrder { UP, DOWN }
    // END_CHANGE: ISS-2025-0422

    /** Enumerate solutions for {@code vars}, streaming each to {@code sink}. */
    public static void label(ClpStore s, List<FdVar> vars, SolutionSink sink) {
        labelRec(s, vars, VarSel.FF, ValOrder.UP, sink);
    }

    // START_CHANGE: ISS-2025-0422 - strategy-aware entry point for labeling/2 options
    /** Enumerate solutions for {@code vars} under the given strategies. */
    public static void label(ClpStore s, List<FdVar> vars, VarSel varSel, ValOrder valOrder, SolutionSink sink) {
        labelRec(s, vars, varSel, valOrder, sink);
    }
    // END_CHANGE: ISS-2025-0422

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

    // START_CHANGE: ISS-2025-0422 - thread the (varSel, valOrder) strategies through the search
    private static boolean labelRec(ClpStore s, List<FdVar> vars, VarSel varSel, ValOrder valOrder,
                                    SolutionSink sink) {
        // pick the next unassigned variable per the selection strategy (default: first-fail)
        FdVar chosen = null;
        for (FdVar v : vars) {
            IntervalDomain d = s.dom(v);
            if (d.isEmpty()) return true;             // inconsistent branch; nothing to do
            if (d.isSingleton()) continue;
            if (chosen == null) { chosen = v; continue; }
            boolean better;
            switch (varSel) {
                case FF:  better = d.size() < s.dom(chosen).size(); break;
                // START_CHANGE: ISS-2025-0642
                case FFC: {
                    long a = d.size(), b = s.dom(chosen).size();
                    better = a < b || (a == b && s.degree(v) > s.degree(chosen));
                    break;
                }
                // END_CHANGE: ISS-2025-0642
                case MIN: better = d.min() < s.dom(chosen).min(); break;
                case MAX: better = d.max() > s.dom(chosen).max(); break;
                default:  better = false; break;      // LEFTMOST keeps the first one found
            }
            if (better) chosen = v;
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
        if (valOrder == ValOrder.UP) {
            for (long[] r : ranges) {
                for (long val = r[0]; val <= r[1]; val++) {
                    if (!tryValue(s, vars, varSel, valOrder, sink, chosen, val)) return false;
                    if (val == Long.MAX_VALUE) break;   // guard the val++ overflow
                }
            }
        } else {                                        // DOWN: descending values
            for (int ri = ranges.length - 1; ri >= 0; ri--) {
                long[] r = ranges[ri];
                for (long val = r[1]; val >= r[0]; val--) {
                    if (!tryValue(s, vars, varSel, valOrder, sink, chosen, val)) return false;
                    if (val == Long.MIN_VALUE) break;   // guard the val-- underflow
                }
            }
        }
        return true;
        // END_CHANGE: ISS-2025-0298
    }

    /** Try one value for {@code chosen}; returns false when the search should stop entirely. */
    private static boolean tryValue(ClpStore s, List<FdVar> vars, VarSel varSel, ValOrder valOrder,
                                    SolutionSink sink, FdVar chosen, long val) {
        int mark = s.mark();
        boolean keepGoing = true;
        if (s.assign(chosen, val) && s.propagate()) {
            keepGoing = labelRec(s, vars, varSel, valOrder, sink);   // false: stop requested deeper down
        }
        s.undo(mark);
        return keepGoing;
    }
    // END_CHANGE: ISS-2025-0422

    // START_CHANGE: ISS-2025-0769 - 4.6 wave Q5.4: branch and bound, ITERATIVE. The optimum of
    // labeling/2's min(Expr)/max(Expr) used to be found by re-running the recursive labeler once per
    // improvement (Java recursion one level per variable, a restart from the root each round). This
    // is one depth-first search with an explicit frame stack: every solution tightens the bound on
    // the objective, and the bound is re-imposed on every later branch, so the search never
    // revisits a region that cannot improve and uses O(1) Java stack whatever the number of
    // variables.
    private static final class Frame {
        final FdVar var;
        final long[][] ranges;
        final boolean up;
        final int mark;
        final int scanFrom;
        int ri;
        long next;
        boolean done;

        Frame(FdVar var, IntervalDomain d, boolean up, int mark, int scanFrom) {
            this.var = var;
            this.ranges = d.rangeArray();
            this.up = up;
            this.mark = mark;
            this.scanFrom = scanFrom;
            if (ranges.length == 0) { done = true; return; }
            ri = up ? 0 : ranges.length - 1;
            next = up ? ranges[0][0] : ranges[ranges.length - 1][1];
        }

        long take() {
            long v = next;
            if (up) {
                if (next < ranges[ri][1]) next++;
                else if (++ri < ranges.length) next = ranges[ri][0];
                else done = true;
            } else {
                if (next > ranges[ri][0]) next--;
                else if (--ri >= 0) next = ranges[ri][1];
                else done = true;
            }
            return v;
        }
    }

    /** Index of the next variable to branch on (-1: all fixed, -2: a wiped-out domain). */
    private static int select(ClpStore s, List<FdVar> vars, VarSel varSel, int from) {
        int chosen = -1;
        IntervalDomain cd = null;
        for (int i = (varSel == VarSel.LEFTMOST ? from : 0); i < vars.size(); i++) {
            IntervalDomain d = s.dom(vars.get(i));
            if (d.isEmpty()) return -2;
            if (d.isSingleton()) continue;
            if (chosen < 0) {
                chosen = i; cd = d;
                if (varSel == VarSel.LEFTMOST) break;
                continue;
            }
            boolean better;
            switch (varSel) {
                case FF:  better = d.size() < cd.size(); break;
                case FFC: {
                    long a = d.size(), b = cd.size();
                    better = a < b || (a == b && s.degree(vars.get(i)) > s.degree(vars.get(chosen)));
                    break;
                }
                case MIN: better = d.min() < cd.min(); break;
                case MAX: better = d.max() > cd.max(); break;
                default:  better = false; break;
            }
            if (better) { chosen = i; cd = d; }
        }
        return chosen;
    }

    /**
     * The optimum of {@code obj} (minimised or maximised) over the labelings of {@code vars}, or
     * null when there is none. The store is restored before returning.
     *
     * @throws IllegalStateException when a complete labeling leaves {@code obj} unfixed (the
     *         caller maps it to an instantiation error)
     */
    public static Long optimum(ClpStore s, List<FdVar> vars, VarSel varSel, ValOrder valOrder,
                               FdVar obj, boolean minimize) {
        final int rootMark = s.mark();
        final boolean up = valOrder == ValOrder.UP;
        Long best = null;
        ArrayList<Frame> stack = new ArrayList<>();
        try {
            int first = select(s, vars, varSel, 0);
            if (first == -2) return null;
            if (first == -1) return fixedObjective(s, obj);
            stack.add(frame(s, vars, first, up));
            long nodes = 0;
            while (!stack.isEmpty()) {
                Frame top = stack.get(stack.size() - 1);
                if (top.done) {
                    s.undo(top.mark);
                    stack.remove(stack.size() - 1);
                    continue;
                }
                long val = top.take();
                s.undo(top.mark);
                if ((++nodes & 0x3FF) == 0) ClpStore.poll();
                boolean ok = s.assign(top.var, val);
                if (ok && best != null) {
                    ok = minimize ? s.removeAbove(obj, best - 1) : s.removeBelow(obj, best + 1);
                }
                if (!ok || !s.propagate()) continue;
                int nx = select(s, vars, varSel, top.scanFrom);
                if (nx == -2) continue;
                if (nx == -1) {                                    // a solution: tighten the bound
                    best = fixedObjective(s, obj);
                    continue;
                }
                stack.add(frame(s, vars, nx, up));
            }
            return best;
        } finally {
            s.undo(rootMark);
        }
    }

    private static Frame frame(ClpStore s, List<FdVar> vars, int i, boolean up) {
        IntervalDomain d = s.dom(vars.get(i));
        if (d.size() > MAX_LABEL_DOMAIN) throw new TooLargeToLabel(d.size());
        return new Frame(vars.get(i), d, up, s.mark(), i);
    }

    private static long fixedObjective(ClpStore s, FdVar obj) {
        IntervalDomain od = s.dom(obj);
        if (!od.isSingleton()) throw new IllegalStateException("objective not fixed by the labeling");
        return od.value();
    }
    // END_CHANGE: ISS-2025-0769
}
