package it.denzosoft.jprolog.core.engine;

// START_CHANGE: ISS-2025-0431 - ENG-04/ENG-05: keep the engine-control exceptions out of the
// library's broad catch clauses.
/**
 * Guard for the three engine-control exceptions.
 *
 * <p>{@link InferenceLimitException} (budget), {@link QueryCancelledException} (IDE/embedder Stop)
 * and {@link DebugController.DebugStopException} (debugger Stop) are deliberately plain
 * {@link RuntimeException}s rather than {@code PrologException}s, so untrusted {@code catch/3}
 * cannot trap them. That guarantee was leaking: the built-in library has ~90 broad
 * {@code catch (Exception e)} / {@code catch (RuntimeException e)} clauses, several of which
 * re-wrapped whatever they caught into a {@code PrologEvaluationException} — a
 * {@code PrologException}, and therefore catchable. With a budget set,
 * {@code catch(aggregate_all(count, Loop, _), _, true)} could swallow the abort and keep running.
 *
 * <p>Every such clause now begins with {@link #rethrowIfControl}, which lets the three control
 * exceptions through untouched and returns for everything else.
 */
public final class ControlFlow {

    private ControlFlow() {}

    /** Rethrow {@code t} unchanged if it is an engine-control exception; otherwise return. */
    public static void rethrowIfControl(Throwable t) {
        if (t instanceof InferenceLimitException) throw (InferenceLimitException) t;
        if (t instanceof QueryCancelledException) throw (QueryCancelledException) t;
        if (t instanceof DebugController.DebugStopException) throw (DebugController.DebugStopException) t;
    }

    /** True when {@code t} is an engine-control exception (for callers that need to test, not throw). */
    public static boolean isControl(Throwable t) {
        return t instanceof InferenceLimitException
            || t instanceof QueryCancelledException
            || t instanceof DebugController.DebugStopException;
    }
}
// END_CHANGE: ISS-2025-0431
