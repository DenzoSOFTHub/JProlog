package it.denzosoft.jprolog.builtin.clpfd.v2;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

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
 */
public final class ClpStore {

    /** A finite-domain variable (identity-based). */
    public static final class FdVar {
        public final String name;
        FdVar(String name) { this.name = name; }
        @Override public String toString() { return name; }
    }

    private final Map<FdVar, IntervalDomain> domains = new IdentityHashMap<>();
    private final Map<FdVar, List<Constraint>> watchers = new IdentityHashMap<>();
    private final List<Constraint> constraints = new ArrayList<>();

    private final Deque<Constraint> queue = new ArrayDeque<>();
    private final Set<Constraint> queued = new HashSet<>();

    /** Trail of (var, previousDomain) pairs for backtracking. */
    private final List<Object[]> trail = new ArrayList<>();

    // ---------------------------------------------------------------- variables

    public FdVar newVar(String name, long lo, long hi) {
        FdVar v = new FdVar(name);
        domains.put(v, IntervalDomain.interval(lo, hi));
        watchers.put(v, new ArrayList<>());
        return v;
    }

    public FdVar newVar(String name, IntervalDomain dom) {
        FdVar v = new FdVar(name);
        domains.put(v, dom);
        watchers.put(v, new ArrayList<>());
        return v;
    }

    public IntervalDomain dom(FdVar v) { return domains.get(v); }

    public Iterable<FdVar> variables() { return domains.keySet(); }

    // ---------------------------------------------------------------- narrowing

    /**
     * Intersect {@code v}'s domain with {@code restriction}. Returns false on wipeout
     * (the caller should treat that as inconsistency). Records the trail and enqueues
     * watchers when the domain actually changes.
     */
    public boolean narrow(FdVar v, IntervalDomain restriction) {
        IntervalDomain cur = domains.get(v);
        IntervalDomain next = cur.intersect(restriction);
        if (next.equals(cur)) return !next.isEmpty();
        trail.add(new Object[]{v, cur});
        domains.put(v, next);
        if (next.isEmpty()) return false;
        enqueueWatchers(v);
        return true;
    }

    public boolean removeBelow(FdVar v, long bound) { return narrow(v, domains.get(v).removeBelow(bound)); }
    public boolean removeAbove(FdVar v, long bound) { return narrow(v, domains.get(v).removeAbove(bound)); }
    public boolean removeValue(FdVar v, long value) { return narrow(v, domains.get(v).removeValue(value)); }
    public boolean assign(FdVar v, long value) { return narrow(v, IntervalDomain.singleton(value)); }

    private void enqueueWatchers(FdVar v) {
        for (Constraint c : watchers.get(v)) {
            if (queued.add(c)) queue.add(c);
        }
    }

    // ---------------------------------------------------------------- constraints

    /** Add a constraint, register it as a watcher of its variables, and propagate once. */
    public boolean addConstraint(Constraint c) {
        constraints.add(c);
        for (FdVar v : c.variables()) {
            watchers.get(v).add(c);
        }
        if (queued.add(c)) queue.add(c);
        return propagate();
    }

    public List<Constraint> constraints() { return constraints; }

    /** Run the propagation queue to a fixpoint. Returns false on any wipeout. */
    public boolean propagate() {
        while (!queue.isEmpty()) {
            Constraint c = queue.poll();
            queued.remove(c);
            if (!c.propagate(this)) {
                queue.clear();
                queued.clear();
                return false;
            }
        }
        return true;
    }

    // ---------------------------------------------------------------- backtracking

    /** Current trail position; pass to {@link #undo(int)} to restore. */
    public int mark() { return trail.size(); }

    /** Undo all domain changes recorded after {@code mark}. */
    @SuppressWarnings("unchecked")
    public void undo(int mark) {
        for (int i = trail.size() - 1; i >= mark; i--) {
            Object[] e = trail.get(i);
            domains.put((FdVar) e[0], (IntervalDomain) e[1]);
        }
        while (trail.size() > mark) trail.remove(trail.size() - 1);
        queue.clear();
        queued.clear();
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
        for (int i = constraints.size() - 1; i >= constraintMark; i--) {
            Constraint c = constraints.remove(i);
            for (FdVar v : c.variables()) {
                List<Constraint> ws = watchers.get(v);
                if (ws == null) continue;
                for (int j = ws.size() - 1; j >= 0; j--) {   // identity-based removal of ONE registration
                    if (ws.get(j) == c) { ws.remove(j); break; }
                }
            }
        }
    }
    // END_CHANGE: ISS-2025-0356

    /** True if every variable's domain is a singleton (a complete consistent assignment). */
    public boolean allAssigned() {
        for (IntervalDomain d : domains.values()) {
            if (!d.isSingleton()) return false;
        }
        return true;
    }
}
