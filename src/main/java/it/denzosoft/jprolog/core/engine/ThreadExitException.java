package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.core.terms.Term;

// START_CHANGE: ISS-2025-0632 - wave P6.5: thread_exit/1.
/**
 * Raised by {@code thread_exit(Term)} inside a worker thread: it unwinds the worker's machine
 * (running the cleanup of every open {@code setup_call_cleanup/3} frame on the way) and the worker's
 * top level turns it into the join status {@code exited(Term)}.
 *
 * <p>Like the other engine-control exceptions it is NOT a {@code PrologException}: a
 * {@code catch/3} inside the thread cannot intercept it, and {@link ControlFlow#rethrowIfControl}
 * lets it through every broad catch clause of the bridged built-ins.
 */
public class ThreadExitException extends RuntimeException {
    private final transient Term term;

    public ThreadExitException(Term term) {
        super("thread_exit", null, false, false);
        this.term = term;
    }

    /** The term the join status reports as {@code exited(Term)} (already detached from any cell). */
    public Term getTerm() { return term; }
}
// END_CHANGE: ISS-2025-0632
