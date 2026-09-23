package it.denzosoft.jprolog.core.engine;

import java.util.concurrent.ConcurrentHashMap;

// START_CHANGE: ISS-2025-0639 - wave P6 (extra): who is blocked joining whom.
/**
 * A registry of the Prolog threads that are blocked in {@code thread_join/1,2}, and on which
 * thread. The per-engine load lock ({@link LoadLock}) reads it to recognise the one way a load can
 * deadlock on itself: a directive of file A starts a thread that loads file B and then JOINS it.
 * The joining thread holds the load lock and cannot resume until the joined thread ends, so the
 * joined thread may safely use the loader in its place.
 */
public final class ThreadWaits {

    private ThreadWaits() {}

    private static final ConcurrentHashMap<Thread, Thread> JOINING = new ConcurrentHashMap<Thread, Thread>();

    /** The calling thread is about to block until {@code target} ends. */
    public static void enterJoin(Thread target) {
        if (target != null) JOINING.put(Thread.currentThread(), target);
    }

    /** The calling thread is no longer blocked in a join. */
    public static void exitJoin() {
        JOINING.remove(Thread.currentThread());
    }

    /** Is {@code waiter} blocked — directly or through a chain of joins — until {@code target} ends? */
    static boolean blockedOn(Thread waiter, Thread target) {
        Thread t = waiter;
        for (int i = 0; i < 64 && t != null; i++) {
            Thread next = JOINING.get(t);
            if (next == target) return true;
            t = next;
        }
        return false;
    }
}
// END_CHANGE: ISS-2025-0639
