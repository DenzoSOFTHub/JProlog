// START_CHANGE: R1 - Trail engine for backtrackable mutation
package it.denzosoft.jprolog.core.engine;

import java.util.ArrayDeque;
import java.util.Deque;

/**
 * Trail engine for backtrackable side-effects.
 *
 * Mutations that need to be undone on backtracking (b_setval, op/3 reassign,
 * setarg/3, attribute changes) push a {@link Runnable} undo action onto the
 * current trail. The solver marks the trail at choicepoints and rolls back
 * to that mark on failure.
 *
 * Thread-local design — one trail per solver thread.
 */
public final class Trail {
    private static final ThreadLocal<Deque<Runnable>> ACTIONS =
        ThreadLocal.withInitial(ArrayDeque::new);

    private Trail() {}

    /** Record an undo action. Called by code that performs backtrackable mutation. */
    public static void record(Runnable undo) {
        ACTIONS.get().push(undo);
    }

    /** Snapshot the current trail depth. Returned to {@link #rollbackTo}. */
    public static int mark() {
        return ACTIONS.get().size();
    }

    /** Run all undo actions back to the given mark (last-in-first-out). */
    public static void rollbackTo(int mark) {
        Deque<Runnable> a = ACTIONS.get();
        while (a.size() > mark) {
            Runnable r = a.pop();
            try { r.run(); } catch (RuntimeException ignored) {}
        }
    }

    /** Clear all actions from current trail. For top-level solve completion. */
    public static void clear() {
        // START_CHANGE: Round5 minor - ThreadLocal.remove to release reference fully
        ACTIONS.remove();
        // END_CHANGE: Round5 minor
    }

    /** Current depth, for debugging. */
    public static int size() {
        return ACTIONS.get().size();
    }
}
// END_CHANGE: R1
