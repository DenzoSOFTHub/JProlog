package it.denzosoft.jprolog.builtin.clpfd.v2;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.List;

/**
 * Clean-room CLP(FD) constraint store.
 *
 * <p>Design goals (each fixes an audit finding the legacy store has):
 * <ul>
 *   <li><b>Per-instance, identity-keyed</b> — variables are {@link FdVar} objects, not raw
 *       Prolog names, and the store is a normal object (not a JVM-wide singleton), so domains
 *       never leak across queries or engines.
 *   <li><b>Interval-set domains</b> ({@link IntervalDomain}) — no per-value enumeration / OOM.
 *   <li><b>Propagation queue</b> — constraints are (re-)awoken whenever a watched variable's
 *       domain narrows, so e.g. {@code #\=} fires as soon as either side becomes a singleton.
 *   <li><b>Trail-based backtracking</b> — {@link #mark()} / {@link #undo(int)} restore domains
 *       in O(changes), used by the labeler instead of full snapshot/restore.
 * </ul>
 *
 * <p>START_CHANGE: ISS-2025-0642 - 4.5 wave P5 (lazy labeling makes this the per-node hot path):
 * a variable carries its own domain and watcher list, the queue membership is a flag on the
 * constraint, and the trail is two parallel arrays, so a narrowing is a field write instead of
 * three hash-map operations. The public API is unchanged. END_CHANGE: ISS-2025-0642
 */
public final class ClpStore {

    /** A finite-domain variable (identity-based). */
    public static final class FdVar {
        public final String name;
        IntervalDomain dom;
        final List<Constraint> watchers = new ArrayList<>(4);
        /** Constraints that only care when this variable becomes FIXED (ISS-2025-0642). */
        final List<Constraint> fixWatchers = new ArrayList<>(4);
        /** ISS-2025-0781: registration position of the engine cell it stands for (-1: none),
         *  and the scan stamp that de-duplicates it inside one determinedCells() pass. */
        int regIdx = -1;
        int seenStamp;
        FdVar(String name) { this.name = name; }
        @Override public String toString() { return name; }
    }

    private final List<FdVar> vars = new ArrayList<>();
    private final List<Constraint> constraints = new ArrayList<>();

    private final Deque<Constraint> queue = new ArrayDeque<>();

    /** Trail of (var, previousDomain) pairs for backtracking. */
    private FdVar[] trailVar = new FdVar[256];
    private IntervalDomain[] trailDom = new IntervalDomain[256];
    private int top = 0;

    // ---------------------------------------------------------------- variables

    public FdVar newVar(String name, long lo, long hi) {
        return newVar(name, IntervalDomain.interval(lo, hi));
    }

    public FdVar newVar(String name, IntervalDomain dom) {
        FdVar v = new FdVar(name);
        v.dom = dom;
        vars.add(v);
        if (unbounded(dom)) unboundedCount++;                  // ISS-2025-0781
        return v;
    }

    // START_CHANGE: ISS-2025-0781 - 4.6 wave Q6 (extra 1): CHANGE TRACKING. The bridge binds the
    // FD cells whose domain became a single value after every post and every labeling step; it
    // used to find them by scanning EVERY registered variable (O(n) per step, quadratic for a
    // 20 000-variable model). The domain trail already records every variable whose domain
    // changed, in order, so "what changed since the last scan" is the trail suffix from a low-water
    // mark: undo lowers the mark, consumeChanges() raises it to the top. The same for the
    // constraint list (a new constraint may make an unbounded variable exactly determined), and a
    // count of variables with an unbounded domain tells the bridge whether that case can arise.
    private int dirtyFrom = 0;
    private int consumedConstraints = 0;
    private int unboundedCount = 0;

    private static boolean unbounded(IntervalDomain d) { return !d.isEmpty() && !d.isFinite(); }

    /** First trail position not yet consumed by {@link #consumeChanges()}. */
    public int changedFrom() { return Math.min(dirtyFrom, top); }

    /** The current trail height. */
    public int trailTop() { return top; }

    /** The variable whose domain the trail entry {@code i} records a change of. */
    public FdVar trailVar(int i) { return trailVar[i]; }

    /** First constraint not yet consumed. */
    public int newConstraintsFrom() { return Math.min(consumedConstraints, constraints.size()); }

    /** How many variables (may) have an unbounded, non-empty domain (an over-approximation). */
    public int unboundedCount() { return unboundedCount; }

    /** Everything recorded so far has been looked at. */
    public void consumeChanges() {
        dirtyFrom = top;
        consumedConstraints = constraints.size();
    }
    // END_CHANGE: ISS-2025-0781

    public IntervalDomain dom(FdVar v) { return v.dom; }

    public Iterable<FdVar> variables() { return Collections.unmodifiableList(vars); }

    // ---------------------------------------------------------------- narrowing

    /**
     * Intersect {@code v}'s domain with {@code restriction}. Returns false on wipeout
     * (the caller should treat that as inconsistency). Records the trail and enqueues
     * watchers when the domain actually changes.
     */
    public boolean narrow(FdVar v, IntervalDomain restriction) {
        IntervalDomain cur = v.dom;
        IntervalDomain next = cur.intersect(restriction);
        return install(v, cur, next);
    }

    private boolean install(FdVar v, IntervalDomain cur, IntervalDomain next) {
        if (next == cur || next.equals(cur)) return !next.isEmpty();
        push(v, cur);
        v.dom = next;
        if (unbounded(cur) != unbounded(next)) unboundedCount += unbounded(next) ? 1 : -1;   // ISS-2025-0781
        if (next.isEmpty()) return false;
        enqueueWatchers(v.watchers);
        if (next.isSingleton()) enqueueWatchers(v.fixWatchers);
        return true;
    }

    private void push(FdVar v, IntervalDomain d) {
        if (top == trailVar.length) {
            trailVar = java.util.Arrays.copyOf(trailVar, top << 1);
            trailDom = java.util.Arrays.copyOf(trailDom, top << 1);
        }
        trailVar[top] = v;
        trailDom[top] = d;
        top++;
    }

    public boolean removeBelow(FdVar v, long bound) {
        IntervalDomain cur = v.dom;
        if (cur.isEmpty()) return false;
        if (cur.min() >= bound) return true;                      // nothing to remove
        return install(v, cur, cur.removeBelow(bound));
    }

    public boolean removeAbove(FdVar v, long bound) {
        IntervalDomain cur = v.dom;
        if (cur.isEmpty()) return false;
        if (cur.max() <= bound) return true;
        return install(v, cur, cur.removeAbove(bound));
    }

    public boolean removeValue(FdVar v, long value) {
        IntervalDomain cur = v.dom;
        if (!cur.contains(value)) return !cur.isEmpty();
        return install(v, cur, cur.removeValue(value));
    }

    // START_CHANGE: ISS-2025-0644
    private int exactDepth;

    /**
     * The exact value of {@code v}: its fixed value, the out-of-range value its domain records, or
     * -- for an unbounded or degenerate domain -- the value a functional constraint whose other
     * variables are all fixed solves it to (followed through at most 8 degenerate auxiliary variables). Null
     * when not determined.
     */
    public java.math.BigInteger exactValue(FdVar v) {
        IntervalDomain d = v.dom;
        if (d.isSingleton()) return java.math.BigInteger.valueOf(d.value());
        if (d.exactBig() != null) return d.exactBig();
        if (d.isEmpty() || d.isFinite() || exactDepth >= 8) return null;
        // below the top level only a DEGENERATE domain (an out-of-range value computed by some
        // propagator: X #= Y*Z's auxiliary product) is followed, so the search stays linear
        if (exactDepth > 0 && d.min() != d.max()) return null;
        exactDepth++;
        try {
            for (int i = 0; i < v.watchers.size(); i++) {
                java.math.BigInteger r = v.watchers.get(i).solveFor(v, this);
                if (r != null) return r;
            }
            for (int i = 0; i < v.fixWatchers.size(); i++) {
                java.math.BigInteger r = v.fixWatchers.get(i).solveFor(v, this);
                if (r != null) return r;
            }
            return null;
        } finally {
            exactDepth--;
        }
    }

    /** Fix {@code v} to an integer beyond the 64-bit range (its domain must reach that infinity). */
    public boolean assignBig(FdVar v, java.math.BigInteger value) {
        IntervalDomain cur = v.dom;
        IntervalDomain big = IntervalDomain.bigValue(value);
        if (cur.exactBig() != null) return cur.exactBig().equals(value);
        if (!cur.contains(big.min())) {
            // contains() tests a value; the infinity point is covered when the domain reaches it
            if (value.signum() > 0 ? cur.max() != IntervalDomain.SUP : cur.min() != IntervalDomain.INF) return false;
        }
        return install(v, cur, big);
    }
    // END_CHANGE: ISS-2025-0644

    public boolean assign(FdVar v, long value) { return narrow(v, IntervalDomain.singleton(value)); }

    private void enqueueWatchers(List<Constraint> ws) {
        for (int i = 0, n = ws.size(); i < n; i++) {
            Constraint c = ws.get(i);
            if (!c.queued) { c.queued = true; queue.add(c); }
        }
    }

    // ---------------------------------------------------------------- constraints

    /** Add a constraint, register it as a watcher of its variables, and propagate once. */
    public boolean addConstraint(Constraint c) {
        constraints.add(c);
        boolean fixOnly = c.wakesOnFixOnly();
        for (FdVar v : c.variables()) {
            (fixOnly ? v.fixWatchers : v.watchers).add(c);
        }
        if (!c.queued) { c.queued = true; queue.add(c); }
        return propagate();
    }

    public List<Constraint> constraints() { return constraints; }

    // START_CHANGE: ISS-2025-0642 - ffc labeling breaks domain-size ties by constraint count
    /** Number of constraints currently watching {@code v}. */
    public int degree(FdVar v) {
        return v.watchers.size() + v.fixWatchers.size();
    }
    // END_CHANGE: ISS-2025-0642

    // START_CHANGE: ISS-2025-0645 - a long propagation must stay interruptible and budgeted: the
    // store calls this hook every POLL_EVERY constraint runs; the v4 engine points it at the running
    // machine's ResourceGuard, so the inference budget and an IDE/embedder Stop reach a propagation
    // loop too (the hook may throw the engine's control exceptions, which pass straight through).
    private static volatile Runnable pollHook;
    private static final int POLL_EVERY = 1024;
    private int sincePoll = 0;

    /** Install the hook the store polls during long propagation (null = none). */
    public static void setPollHook(Runnable hook) { pollHook = hook; }

    /** Poll the hook now (a long search between propagations, ISS-2025-0769). */
    static void poll() {
        Runnable h = pollHook;
        if (h != null) h.run();
    }
    // END_CHANGE: ISS-2025-0645

    /** Run the propagation queue to a fixpoint. Returns false on any wipeout. */
    public boolean propagate() {
        while (!queue.isEmpty()) {
            // START_CHANGE: ISS-2025-0645
            if (++sincePoll >= POLL_EVERY) {
                sincePoll = 0;
                Runnable h = pollHook;
                if (h != null) {
                    try {
                        h.run();
                    } catch (RuntimeException e) {
                        clearQueue();
                        throw e;
                    }
                }
            }
            // END_CHANGE: ISS-2025-0645
            Constraint c = queue.poll();
            c.queued = false;
            if (!c.propagate(this)) {
                clearQueue();
                return false;
            }
        }
        return true;
    }

    private void clearQueue() {
        while (!queue.isEmpty()) queue.poll().queued = false;
    }

    // ---------------------------------------------------------------- backtracking

    /** Current trail position; pass to {@link #undo(int)} to restore. */
    public int mark() { return top; }

    /** Undo all domain changes recorded after {@code mark}. */
    public void undo(int mark) {
        if (mark < dirtyFrom) dirtyFrom = mark;                                  // ISS-2025-0781
        for (int i = top - 1; i >= mark; i--) {
            IntervalDomain now = trailVar[i].dom;                                // ISS-2025-0781
            if (unbounded(now) != unbounded(trailDom[i])) unboundedCount += unbounded(trailDom[i]) ? 1 : -1;
            trailVar[i].dom = trailDom[i];
            trailVar[i] = null;
            trailDom[i] = null;
        }
        if (mark < top) top = mark;
        if (!queue.isEmpty()) clearQueue();
    }

    // START_CHANGE: ISS-2025-0356 - full store rollback (domains AND constraints) for engine backtracking
    /** Current constraint count; pass to {@link #rollbackTo(int, int)} to restore. */
    public int constraintMark() { return constraints.size(); }

    /**
     * Restore the store to a snapshot taken before a constraint post: undo all domain changes after
     * {@code domainMark} and remove every constraint added after {@code constraintMark}, including
     * its watcher registrations — so a constraint posted in a failed/abandoned branch can never
     * re-propagate. Used by the engine's backtracking (via {@code core.engine.v4.Undo}).
     */
    public void rollbackTo(int domainMark, int constraintMark) {
        undo(domainMark);
        if (constraintMark < consumedConstraints) consumedConstraints = constraintMark;   // ISS-2025-0781
        for (int i = constraints.size() - 1; i >= constraintMark; i--) {
            Constraint c = constraints.remove(i);
            boolean fixOnly = c.wakesOnFixOnly();
            for (FdVar v : c.variables()) {
                List<Constraint> ws = fixOnly ? v.fixWatchers : v.watchers;
                for (int j = ws.size() - 1; j >= 0; j--) {   // identity-based removal of ONE registration
                    if (ws.get(j) == c) { ws.remove(j); break; }
                }
            }
        }
    }
    // END_CHANGE: ISS-2025-0356

    /** True if every variable's domain is a singleton (a complete consistent assignment). */
    public boolean allAssigned() {
        for (FdVar v : vars) {
            if (!v.dom.isSingleton()) return false;
        }
        return true;
    }
}
