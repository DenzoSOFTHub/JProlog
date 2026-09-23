package it.denzosoft.jprolog.test.support;

import it.denzosoft.jprolog.builtin.io.StreamManager;
import it.denzosoft.jprolog.core.engine.Prolog;

import java.io.OutputStream;
import java.io.PrintStream;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * START_CHANGE: ISS-2025-0664 - latch synchronisation for the interrupt tests.
 *
 * <p>The Stop/interrupt tests used to start a solver thread, {@code sleep(N)} and interrupt it,
 * hoping the query was running by then. Under load the thread may not even have been scheduled
 * (or may still be consulting), so what was tested depended on the machine. Here the query
 * itself announces that it is running: it starts with {@code write(go), flush_output}, and the
 * solver thread's output is redirected (thread-locally, as the IDE does) into a stream that
 * opens the latch on the first byte. The test waits on the latch and only then interrupts.
 * END_CHANGE: ISS-2025-0664
 */
public final class QueryStartLatch {

    private final CountDownLatch started = new CountDownLatch(1);

    /** Prefix a query body with this so it announces that it is running. */
    public static final String ANNOUNCE = "write(go), flush_output, ";

    /** Run {@code query} (which must start with {@link #ANNOUNCE}) on the calling thread. */
    public java.util.List<java.util.Map<String, it.denzosoft.jprolog.core.terms.Term>> solve(
            Prolog p, String query) {
        PrintStream ps = new PrintStream(new OutputStream() {
            @Override public void write(int b) { started.countDown(); }
        }, true);
        StreamManager.setThreadLocalOutput(ps);
        try {
            return p.solve(query);
        } finally {
            StreamManager.setThreadLocalOutput(null);
        }
    }

    /** Block until the query has started (fails the test after {@code seconds}). */
    public void await(int seconds) throws InterruptedException {
        if (!started.await(seconds, TimeUnit.SECONDS)) {
            throw new AssertionError("the query did not start within " + seconds + " s");
        }
    }
}
