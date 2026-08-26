package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.ResourceGuard;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

// START_CHANGE: ISS-2025-0440 - engine v4, design B.3 (bindings, trail, conditional trailing).
/**
 * The v4 binding store: there isn't one.
 *
 * <p>A v4 binding lives IN the variable cell ({@link Variable#ref}); this class only owns the
 * <b>trail</b> — the list of cells to reset (and undo actions to run) when execution backtracks to
 * a mark. That is the whole point of the cell model: because a cell is reachable only from live
 * terms, the JVM reclaims every binding of a finished deterministic call, so {@code loop(N)} runs
 * in O(1) heap for any N (design limit L-01 / LIM-033).
 *
 * <h3>Conditional trailing</h3>
 * A binding needs trailing only when some future {@code undo} can reach it. Two cases:
 * <ul>
 *   <li>a choice point exists and the cell is <b>older</b> than it ({@code serial <= barrierSerial});
 *       a cell created after the newest choice point is unreachable once we backtrack past it, so
 *       resetting it would be pure waste;</li>
 *   <li>an explicit mark/undo extent is open ({@code forceTrail > 0}) — {@code findall/3},
 *       {@code \=/2}, the catcher unification, {@code \+/1}: those brackets take their own mark and
 *       must be able to undo everything inside, choice point or not.</li>
 * </ul>
 * When neither holds the trail is not merely skipped but {@link #clearIfUnreachable cleared}: with
 * no live mark the accumulated entries are garbage.
 */
final class Bindings {

    private Object[] trail = new Object[512];      // Variable (reset ref) or Runnable (undo action)
    private int top = 0;

    /** Depth of open explicit mark/undo extents; while > 0 every binding is trailed. */
    int forceTrail = 0;

    /** Serial watermark of the newest choice point: cells with {@code serial <= barrierSerial} are
     *  older than it and must be trailed. 0 = no choice point. */
    long barrierSerial = 0;

    /** Budget/cancellation guard, polled by the walkers in {@link Unify}. */
    final ResourceGuard guard;

    /** Attributed-variable hook (freeze/when/dif/CLP(FD)); null when nothing is attributed. */
    Unify.AttrHandler attrHandler;

    Bindings(ResourceGuard guard) { this.guard = guard; }

    int mark() { return top; }

    /** Bind {@code v} to {@code value}, trailing the cell when some undo can reach it. */
    void bind(Variable v, Term value) {
        v.ref = value;
        if (forceTrail > 0 || v.serial <= barrierSerial) push(v);
    }

    /** Bind without any trailing decision — used for cells the caller knows are brand new. */
    void bindUntrailed(Variable v, Term value) { v.ref = value; }

    /** Record an undo action (attribute change, catch-frame disarm, setarg). */
    void pushUndo(Runnable undo) { push(undo); }

    private void push(Object e) {
        if (top == trail.length) {
            Object[] bigger = new Object[trail.length << 1];
            System.arraycopy(trail, 0, bigger, 0, top);
            trail = bigger;
        }
        trail[top++] = e;
    }

    /** Undo every trail entry above {@code m}. */
    void undo(int m) {
        // START_CHANGE: ISS-2025-0448 - a mark can be ABOVE the current top when the trail was
        // cleared under an extent that had already closed (the bug this issue fixes). Setting
        // top = m there would resurrect null slots and NPE on the next undo; a stale mark is a
        // no-op instead.
        if (m >= top) return;
        // END_CHANGE: ISS-2025-0448
        for (int i = top - 1; i >= m; i--) {
            Object e = trail[i];
            trail[i] = null;
            if (e instanceof Variable) ((Variable) e).ref = null;
            else ((Runnable) e).run();
        }
        top = m;
    }

    /**
     * Drop the whole trail when nothing can undo it (no choice point, no open extent). Called after
     * a trust-me pop and after a cut: this is what keeps a deterministic million-step recursion at
     * a bounded trail.
     *
     * <p><b>Contract for callers of {@link #forceTrail}</b> (ISS-2025-0448): an extent must run its
     * {@link #undo} BEFORE decrementing {@code forceTrail}. Cut and trust-me both end here, so a
     * mark taken by an extent that has already closed can be wiped before it is used — which is
     * exactly how {@code findall/3} stopped being opaque.
     */
    void clearIfUnreachable(boolean noChoicePoints) {
        if (forceTrail == 0 && noChoicePoints && top > 0) {
            java.util.Arrays.fill(trail, 0, top, null);
            top = 0;
            barrierSerial = 0;
        }
    }

    // START_CHANGE: ISS-2025-0458 - unifiable/3 (and therefore dif/2 on partially instantiated
    // terms) needs to know WHICH cells a probe unification bound. Valid only inside the
    // forceTrail extent that took {@code m}, where every binding is trailed unconditionally.
    /** The cells bound since {@code m}, oldest first. Undo actions in the range are skipped. */
    java.util.List<Variable> boundSince(int m) {
        java.util.ArrayList<Variable> out = new java.util.ArrayList<Variable>();
        if (m < 0) m = 0;
        for (int i = m; i < top; i++) {
            Object e = trail[i];
            if (e instanceof Variable) out.add((Variable) e);
        }
        return out;
    }
    // END_CHANGE: ISS-2025-0458

    /** Test hook: number of live trail entries. */
    int size() { return top; }
}
// END_CHANGE: ISS-2025-0440
