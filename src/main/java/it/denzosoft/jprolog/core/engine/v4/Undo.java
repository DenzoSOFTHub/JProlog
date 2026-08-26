package it.denzosoft.jprolog.core.engine.v4;

// START_CHANGE: ISS-2025-0492 - 4.1 wave A: the engine-owned undo trail for BRIDGED built-ins.
/**
 * Where a legacy built-in records a backtrackable side effect.
 *
 * <p>A {@code BuiltIn} sees a goal, a binding map and a solution list — it has no machine, no
 * choice point and no trail. Until 4.1 the five built-ins that mutate engine state outside the
 * binding cells ({@code b_setval/2}, {@code op/3}, {@code setarg/3}, the CLP(FD) store and the
 * attribute registrations it makes) pushed their undo action onto {@code core.engine.Trail}, a
 * process-wide {@code ThreadLocal} stack that every choice point had to mark and roll back in
 * parallel with the real trail ({@code CP.legacyMark}). Two trails, two marks, one ordering rule
 * to get wrong.
 *
 * <p>There is one trail now: {@link Bindings}, which has always accepted undo actions next to its
 * cell resets ({@link Bindings#pushUndo}). This class is only the <em>doorway</em> — it hands a
 * built-in the machine that is running on its thread, and the machine pushes the action onto its
 * own trail, where {@code B.undo(cp.trailMark)} runs it at exactly the point the cells are reset.
 *
 * <p>Consequences worth knowing:
 * <ul>
 *   <li>an action recorded with <b>no machine running</b> on the thread (a directly-instantiated
 *       built-in, a {@code :- op(...)} directive at consult time, a unit test) is <b>not</b>
 *       recorded: nothing can backtrack over it, so the mutation is permanent — exactly what the
 *       old trail did when no choice point had ever marked it;</li>
 *   <li>the action is subject to the trail's own economy: {@link Bindings#clearIfUnreachable}
 *       drops it once no choice point and no open mark/undo extent can reach it, which is the same
 *       condition under which a binding is not trailed at all;</li>
 *   <li>a nested machine (a sub-solve started from {@code EngineContext}) is current for its own
 *       duration, so its actions land on its own trail and die with it.</li>
 * </ul>
 */
final class Undo {

    private static final ThreadLocal<Machine> CURRENT = new ThreadLocal<Machine>();

    private Undo() {}

    /**
     * Record {@code undo} on the trail of the machine running on this thread; a no-op when there
     * is none (see the class comment).
     *
     * <p>START_CHANGE: ISS-2025-0500 - package-private since 4.3.0. The five built-ins that used to
     * call it from outside are native now ({@code op/3}, {@code char_conversion/2},
     * {@code b_setval/2}, {@code setarg/3}) or reach the trail through a sink the engine installs
     * ({@code builtin.clpfd.v2.ClpfdV2Bridge.setUndoSink}), so the doorway is an implementation
     * detail of the machine again. END_CHANGE: ISS-2025-0500
     */
    static void record(Runnable undo) {
        if (undo == null) return;
        Machine m = CURRENT.get();
        if (m != null) m.pushUndo(undo);
    }

    /** Install {@code m} as this thread's machine; returns the previous one for the finally block. */
    static Machine enter(Machine m) {
        Machine prev = CURRENT.get();
        if (m == null) CURRENT.remove(); else CURRENT.set(m);
        return prev;
    }

    /** Restore the machine {@link #enter} returned. */
    static void exit(Machine prev) {
        if (prev == null) CURRENT.remove(); else CURRENT.set(prev);
    }

    /** Test hook: the machine currently taking undo actions on this thread, or null. */
    static Machine current() { return CURRENT.get(); }
}
// END_CHANGE: ISS-2025-0492
