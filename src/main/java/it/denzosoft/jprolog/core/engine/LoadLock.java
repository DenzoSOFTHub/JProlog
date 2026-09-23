package it.denzosoft.jprolog.core.engine;

import java.util.HashMap;
import java.util.Map;

// START_CHANGE: ISS-2025-0639 - wave P6 (extra): the per-engine load lock, deadlock-aware.
/**
 * Serialises the loads of one engine (the load-context stack, the loaded-file table and the
 * current module are engine-wide), like the {@code synchronized} block it replaces — reentrant for
 * the owner, blocking (interruptibly) for everybody else — with ONE exception that removes a
 * self-deadlock: a thread may enter as a <em>guest</em> while the owner is blocked in
 * {@code thread_join} waiting for that very thread (directly or through a chain of joins,
 * {@link ThreadWaits}). The owner cannot resume before the guest ends, so the two never touch the
 * loader state at the same time. The typical case is a directive of a file being loaded that
 * starts a thread which consults another file and joins it: it used to hang forever.
 *
 * <p>Not covered (documented as LIM-045): an owner blocked in some OTHER wait on the thread that
 * wants to load — {@code thread_get_message/1,2} for a message the loader thread would send only
 * after its load, or a {@code concurrent_*} predicate whose worker loads. Those still wait for the
 * load lock until the owner's load finishes, i.e. forever when the owner waits for them.
 */
final class LoadLock {

    private Thread owner;
    private int depth;
    private final Map<Thread, int[]> guests = new HashMap<Thread, int[]>();

    synchronized void lock() {
        Thread me = Thread.currentThread();
        for (;;) {
            if (owner == null) { owner = me; depth = 1; return; }
            if (owner == me) { depth++; return; }
            int[] g = guests.get(me);
            if (g != null) { g[0]++; return; }
            if (ThreadWaits.blockedOn(owner, me)) { guests.put(me, new int[] {1}); return; }
            try {
                wait(20);            // re-check: the owner may start joining us at any moment
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                throw new QueryCancelledException();
            }
        }
    }

    synchronized void unlock() {
        Thread me = Thread.currentThread();
        int[] g = guests.get(me);
        if (g != null) {
            if (--g[0] == 0) guests.remove(me);
            notifyAll();
            return;
        }
        if (owner == me && --depth == 0) {
            owner = null;
            notifyAll();
        }
    }
}
// END_CHANGE: ISS-2025-0639
