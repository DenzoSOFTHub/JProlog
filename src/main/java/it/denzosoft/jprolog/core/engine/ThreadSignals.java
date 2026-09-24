package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.core.terms.Term;

import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;

// START_CHANGE: ISS-2025-0749 - 4.6 wave Q4.1: thread_signal/2.
/**
 * The per-Prolog-thread signal queues behind {@code thread_signal/2}.
 *
 * <p>A signal is never run asynchronously: the sender only queues a (copied) goal on the target's
 * {@link Box} and wakes it if it is blocked in a thread built-in. The target runs the goal
 * <b>on its own goal stack</b>: the v4 drive loop polls {@link #poll()} where it polls the
 * resource guard (one volatile read per step while no signal is pending anywhere) and pushes the
 * goal in front of its continuation, so bindings and the trail stay the target's own; a thread
 * blocked in {@code thread_get_message/1,2,3}, {@code thread_join/1,2}, {@code thread_sleep/1},
 * {@code mutex_lock/1} or {@code with_mutex/2} runs it from inside that built-in. An exception the
 * signal goal raises unwinds the target as if it had been raised at that point (SWI).
 *
 * <p>Every non-worker thread is the Prolog thread {@code main} (ISS-2025-0620), so they share the
 * one {@link #MAIN} box: a signal to {@code main} is run by whichever such thread polls first —
 * the CLI's, an embedder's or a JUnit body — including one blocked in {@code thread_join/2}. A
 * worker machine (thread_create/2,3, the concurrent_* pool) binds its own box, so a pool thread
 * never takes {@code main}'s signals.
 */
public final class ThreadSignals {

    private ThreadSignals() {}

    /** Signals queued and not yet taken, JVM-wide: the drive loop's fast-path test. */
    public static final AtomicInteger PENDING = new AtomicInteger();

    /** One Prolog thread's signal queue. */
    public static final class Box {
        final ConcurrentLinkedQueue<Term> queue = new ConcurrentLinkedQueue<Term>();
        /** The monitor the owner is blocked on (a message queue, a sleep lock), or null. */
        public volatile Object monitor;
        /** The resource guard of the machine running on the owner thread (thread_statistics/3). */
        public volatile ResourceGuard guard;
        /** The JVM thread that last ran a query as this Prolog thread (thread_statistics/3). */
        public volatile Thread thread;
        /** False once the owner thread has ended: a signal to it is refused. */
        public volatile boolean live = true;
    }

    /** The box of {@code main}: every thread that is not a worker. */
    public static final Box MAIN = new Box();

    private static final ThreadLocal<Box> CURRENT = new ThreadLocal<Box>();

    /** The calling thread's box. */
    public static Box current() {
        Box b = CURRENT.get();
        return (b != null) ? b : MAIN;
    }

    /** Is a box bound to the calling thread (i.e. is it a worker)? */
    public static boolean bound() { return CURRENT.get() != null; }

    /** Bind {@code b} to the calling thread; returns the previous binding (may be null). */
    public static Box bind(Box b) {
        Box prev = CURRENT.get();
        if (b == null) CURRENT.remove(); else CURRENT.set(b);
        return prev;
    }

    /** Queue {@code goal} (already copied) for the owner of {@code b} and wake it. */
    public static void send(Box b, Term goal) {
        b.queue.add(goal);
        PENDING.incrementAndGet();
        Object mon = b.monitor;
        if (mon != null) {
            synchronized (mon) { mon.notifyAll(); }
        }
    }

    /** The next signal for the calling thread, or null. */
    public static Term poll() {
        if (PENDING.get() == 0) return null;
        Term g = current().queue.poll();
        if (g != null) PENDING.decrementAndGet();
        return g;
    }

    /** Has the calling thread a signal waiting? */
    public static boolean hasPending() {
        return PENDING.get() != 0 && !current().queue.isEmpty();
    }

    /** Signal goals being run from inside a blocking built-in on this thread (nesting depth). */
    private static final ThreadLocal<int[]> HANDLING = new ThreadLocal<int[]>();

    /** A blocking built-in starts running this thread's signals: no machine may take another. */
    public static void enterHandler() {
        int[] d = HANDLING.get();
        if (d == null) HANDLING.set(new int[] {1}); else d[0]++;
    }

    public static void exitHandler() {
        int[] d = HANDLING.get();
        if (d != null && --d[0] <= 0) HANDLING.remove();
    }

    /** Are signal goals running on this thread right now (from a blocking built-in)? */
    public static boolean handling() { return HANDLING.get() != null; }

    /** The owner of {@code b} has ended: drop what it never ran. */
    public static void discard(Box b) {
        b.live = false;
        while (b.queue.poll() != null) PENDING.decrementAndGet();
    }
}
// END_CHANGE: ISS-2025-0749
