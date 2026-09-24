package it.denzosoft.jprolog.builtin.clpfd.v2;

import it.denzosoft.jprolog.builtin.clpfd.v2.ClpStore.FdVar;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static it.denzosoft.jprolog.builtin.clpfd.v2.IntervalDomain.INF;
import static it.denzosoft.jprolog.builtin.clpfd.v2.IntervalDomain.SUP;

/**
 * START_CHANGE: ISS-2025-0762 / ISS-2025-0763 - 4.6 wave Q5.2: the global constraints that need
 * a propagator of their own ({@code circuit/1}, {@code cumulative/1,2}). The ones SWI itself
 * decomposes ({@code chain/2}, {@code lex_chain/1}, {@code disjoint2/1}, {@code automaton/3,8},
 * {@code zcompare/3}) live in {@code prelude/clpfd.pl}.
 */
public final class Globals {

    private Globals() {}

    // =====================================================================================
    // circuit(Succs): Succs (1-based successors) form ONE Hamiltonian cycle
    // =====================================================================================
    /**
     * The no-subtour part of {@code circuit/1} (the all-different part is a separate
     * {@link Constraint.AllDistinct}, hidden from the residual goals). Propagation, SWI level:
     * no self loop; every chain of fixed successors shorter than N may not close on its own start
     * (the chain's end loses the start's value); and the successor graph (i -> j for j in the
     * domain of S_i) must stay strongly connected, otherwise it can only split into subtours.
     */
    public static final class Circuit extends Constraint {
        private final FdVar[] succ;

        public Circuit(FdVar[] succ) { this.succ = succ.clone(); }

        @Override public List<FdVar> variables() { return Arrays.asList(succ); }

        @Override void render(Residuals r, List<Term> out) {
            out.add(Residuals.op("circuit", Residuals.list(r.terms(Arrays.asList(succ)))));
        }

        @Override public boolean propagate(ClpStore s) {
            int n = succ.length;
            if (n == 0) return true;
            if (n == 1) return s.assign(succ[0], 1);
            for (int i = 0; i < n; i++) {
                if (!s.narrow(succ[i], IntervalDomain.interval(1, n))) return false;
                if (!s.removeValue(succ[i], i + 1)) return false;
            }
            // fixed successor chains: a chain shorter than n may not close on itself
            int[] next = new int[n];
            boolean[] hasPred = new boolean[n];
            for (int i = 0; i < n; i++) {
                IntervalDomain d = s.dom(succ[i]);
                next[i] = d.isSingleton() ? (int) d.value() - 1 : -1;
                if (next[i] >= 0) {
                    if (hasPred[next[i]]) return false;               // two predecessors
                    hasPred[next[i]] = true;
                }
            }
            int covered = 0;
            for (int start = 0; start < n; start++) {
                if (hasPred[start]) continue;                        // not a chain start
                int end = start, len = 1;
                while (next[end] >= 0) {
                    end = next[end];
                    if (++len > n) return false;
                }
                covered += len;
                if (len < n && !s.removeValue(succ[end], start + 1)) return false;
            }
            if (covered < n) {
                // every node not on a chain lies on a cycle of fixed successors: it must be the
                // whole circuit
                for (int i = 0; i < n; i++) {
                    if (!hasPred[i]) continue;
                    int len = 1, j = next[i];
                    while (j >= 0 && j != i && len <= n) { j = next[j]; len++; }
                    if (j == i && len < n) return false;
                }
            }
            return stronglyConnected(s, n);
        }

        /** Forward and backward reachability from node 0 over the domains. */
        private boolean stronglyConnected(ClpStore s, int n) {
            long[][][] adj = new long[n][][];
            for (int i = 0; i < n; i++) adj[i] = s.dom(succ[i]).rangeArray();
            boolean[] seen = new boolean[n];
            int[] stack = new int[n];
            int sp = 0, count = 1;
            seen[0] = true;
            stack[sp++] = 0;
            while (sp > 0) {
                int i = stack[--sp];
                for (long[] rg : adj[i]) {
                    for (long v = Math.max(1, rg[0]); v <= Math.min(n, rg[1]); v++) {
                        int j = (int) v - 1;
                        if (!seen[j]) { seen[j] = true; count++; stack[sp++] = j; }
                    }
                }
            }
            if (count < n) return false;
            // backward: j reaches 0 iff 0 is reachable from j in the reversed graph
            List<List<Integer>> rev = new ArrayList<>(n);
            for (int i = 0; i < n; i++) rev.add(new ArrayList<Integer>(4));
            for (int i = 0; i < n; i++) {
                for (long[] rg : adj[i]) {
                    for (long v = Math.max(1, rg[0]); v <= Math.min(n, rg[1]); v++) rev.get((int) v - 1).add(i);
                }
            }
            Arrays.fill(seen, false);
            sp = 0; count = 1;
            seen[0] = true;
            stack[sp++] = 0;
            while (sp > 0) {
                int j = stack[--sp];
                for (int i : rev.get(j)) {
                    if (!seen[i]) { seen[i] = true; count++; stack[sp++] = i; }
                }
            }
            return count == n;
        }
    }
    // END_CHANGE: ISS-2025-0762

    // START_CHANGE: ISS-2025-0763
    // =====================================================================================
    // cumulative(Tasks, [limit(L)]): at every time the running tasks use at most L
    // =====================================================================================
    /**
     * Time-table propagation over the tasks' compulsory parts ({@code [lst, est + dmin)}, where a
     * task must be running whatever its start): the profile of those parts may not exceed the
     * limit, and a task may not start where its minimal duration would overlap a segment the
     * profile (without the task's own part) leaves too little room in. The durations, resource
     * amounts and the limit may be variables; their bounds are used (sound). {@code S + D #= E}
     * is posted separately, as SWI does.
     */
    public static final class Cumulative extends Constraint {
        private final FdVar[] start, dur, res;
        private final FdVar limit;
        private final Term[] taskTerms;                      // task(S,D,E,C,T), for the residual

        public Cumulative(FdVar[] start, FdVar[] dur, FdVar[] res, FdVar limit, Term[] taskTerms) {
            this.start = start.clone();
            this.dur = dur.clone();
            this.res = res.clone();
            this.limit = limit;
            this.taskTerms = taskTerms.clone();
        }

        @Override public List<FdVar> variables() {
            List<FdVar> vs = new ArrayList<>(start.length * 3 + 1);
            vs.addAll(Arrays.asList(start));
            vs.addAll(Arrays.asList(dur));
            vs.addAll(Arrays.asList(res));
            vs.add(limit);
            return vs;
        }

        @Override void render(Residuals r, List<Term> out) {
            List<Term> ts = new ArrayList<>(taskTerms.length);
            for (Term t : taskTerms) ts.add(t);
            Term opts = Residuals.list(Arrays.asList(
                (Term) new CompoundTerm(new Atom("limit"), Arrays.asList(r.term(limit)))));
            out.add(Residuals.op("cumulative", Residuals.list(ts), opts));
        }

        @Override public boolean propagate(ClpStore s) {
            int n = start.length;
            IntervalDomain ld = s.dom(limit);
            if (ld.isEmpty()) return false;
            long lmax = ld.max();
            for (int i = 0; i < n; i++) {
                if (!s.removeBelow(dur[i], 0) || !s.removeBelow(res[i], 0)) return false;
            }
            // compulsory parts
            long[] ca = new long[n], cb = new long[n], ch = new long[n];
            int parts = 0;
            for (int i = 0; i < n; i++) {
                IntervalDomain sd = s.dom(start[i]);
                long est = sd.min(), lst = sd.max(), dmin = s.dom(dur[i]).min(), cmin = s.dom(res[i]).min();
                long ect = add(est, dmin);
                if (isInf(est) || isInf(lst) || dmin <= 0 || cmin <= 0 || lst >= ect) { ch[i] = 0; continue; }
                ca[i] = lst; cb[i] = ect; ch[i] = cmin;
                parts++;
            }
            if (parts == 0) return true;
            // the profile: sorted event points, one height per segment between them
            long[] pts = new long[2 * parts];
            int k = 0;
            for (int i = 0; i < n; i++) if (ch[i] > 0) { pts[k++] = ca[i]; pts[k++] = cb[i]; }
            Arrays.sort(pts);
            int m = 0;
            for (int i = 0; i < pts.length; i++) if (m == 0 || pts[i] != pts[m - 1]) pts[m++] = pts[i];
            long[] segA = Arrays.copyOf(pts, m - 1), segB = new long[m - 1], segH = new long[m - 1];
            for (int j = 0; j < m - 1; j++) segB[j] = pts[j + 1];
            for (int i = 0; i < n; i++) {
                if (ch[i] == 0) continue;
                int from = Arrays.binarySearch(pts, 0, m, ca[i]);
                for (int j = from; j < m - 1 && segA[j] < cb[i]; j++) segH[j] = add(segH[j], ch[i]);
            }
            long peak = 0;
            for (int j = 0; j < m - 1; j++) peak = Math.max(peak, segH[j]);
            if (peak > lmax) return false;
            if (!s.removeBelow(limit, peak)) return false;
            // filtering of the start times
            for (int i = 0; i < n; i++) {
                IntervalDomain sd = s.dom(start[i]);
                long est = sd.min(), lst = sd.max();
                long dmin = s.dom(dur[i]).min(), cmin = s.dom(res[i]).min();
                if (dmin <= 0 || cmin <= 0 || isInf(est) || isInf(lst)) continue;
                // earliest start: push past every segment the task cannot share
                long t = est;
                boolean moved = true;
                while (moved && t <= lst) {
                    moved = false;
                    long tEnd = add(t, dmin);
                    for (int j = 0; j < m - 1; j++) {
                        if (segB[j] <= t) continue;
                        if (segA[j] >= tEnd) break;
                        long h = segH[j] - (ch[i] > 0 && segA[j] >= ca[i] && segB[j] <= cb[i] ? ch[i] : 0);
                        if (add(h, cmin) > lmax) { t = segB[j]; moved = true; break; }
                    }
                }
                if (t > lst) return false;
                // latest start: pull before every such segment
                long u = lst;
                moved = true;
                while (moved && u >= t) {
                    moved = false;
                    long uEnd = add(u, dmin);
                    for (int j = m - 2; j >= 0; j--) {
                        if (segA[j] >= uEnd) continue;
                        if (segB[j] <= u) break;
                        long h = segH[j] - (ch[i] > 0 && segA[j] >= ca[i] && segB[j] <= cb[i] ? ch[i] : 0);
                        if (add(h, cmin) > lmax) { u = sub(segA[j], dmin); moved = true; break; }
                    }
                }
                if (u < t) return false;
                if (t > est && !s.removeBelow(start[i], t)) return false;
                if (u < lst && !s.removeAbove(start[i], u)) return false;
            }
            return true;
        }
    }
    // END_CHANGE: ISS-2025-0763
}
