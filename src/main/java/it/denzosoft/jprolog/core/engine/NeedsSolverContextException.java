package it.denzosoft.jprolog.core.engine;

// START_CHANGE: ISS-2025-0426 - ENG-05: explicit "this built-in cannot be bridged here" signal.
/**
 * Raised by a built-in that cannot run in the caller's context (typically a
 * {@link BuiltInWithContext} implementation reached without a {@link SolverContext}). The v2 engine's
 * {@code MachineSolver.bridgeBuiltin} treats it — and only it — as "not bridgeable", falling back to
 * user clauses; every other {@link RuntimeException} escaping a built-in is now surfaced as a
 * catchable {@code system_error} instead of being silently swallowed.
 *
 * <p>Like the other engine-control exceptions this is a plain {@link RuntimeException}, never a
 * {@code PrologException}, so it cannot be trapped by {@code catch/3}.
 */
public class NeedsSolverContextException extends RuntimeException {

    public NeedsSolverContextException(String message) {
        super(message, null, false, false);
    }

    public NeedsSolverContextException(String message, Throwable cause) {
        super(message, cause, false, false);
    }
}
// END_CHANGE: ISS-2025-0426
