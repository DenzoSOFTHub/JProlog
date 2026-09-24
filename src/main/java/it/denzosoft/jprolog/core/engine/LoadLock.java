package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.core.terms.Atom;

import java.util.ArrayDeque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

// START_CHANGE: ISS-2025-0739 - 4.6 wave Q3.8: the load lock is per FILE (it was per engine,
// ISS-2025-0639). Two threads loading different files proceed in parallel; the same file loaded
// concurrently waits for the first load (re-entrant for its owner). The loader state that used to
// need the engine-wide lock is per thread now (the load-context stack, the module being loaded)
// or guarded on its own (the loaded-file table).
//
// A wait that can never end is refused instead of hanging: before waiting for file F owned by
// thread U, the lock walks the waits-for graph from U — U waiting for a file lock, blocked in
// thread_join/1,2 or awaiting concurrent_* workers ({@link ThreadWaits}) — and when the walk comes
// back to the calling thread it raises permission_error(load, source_sink, F).
/** The per-engine table of per-file load locks. */
final class LoadLock {

    private static final class Owner {
        final Thread thread;
        int depth;
        Owner(Thread t) { thread = t; depth = 1; }
    }

    private final Map<String, Owner> owners = new HashMap<String, Owner>();
    /** thread -> the file it is waiting to load. */
    private final Map<Thread, String> waiting = new HashMap<Thread, String>();

    /** Acquire {@code key} (a canonical path, or a pseudo-key for text loads). */
    synchronized void lock(String key) {
        Thread me = Thread.currentThread();
        for (;;) {
            Owner o = owners.get(key);
            if (o == null) { owners.put(key, new Owner(me)); return; }
            if (o.thread == me) { o.depth++; return; }
            if (!o.thread.isAlive()) { owners.put(key, new Owner(me)); return; }   // a dead owner
            if (waitsFor(o.thread, me)) {
                throw it.denzosoft.jprolog.core.engine.v4.Errors.permission("load", "source_sink",
                    new Atom(key), "load_files", 2,
                    "file is being loaded by a thread that waits for this one (load deadlock)");
            }
            waiting.put(me, key);
            try {
                wait(20);            // re-check: the owner may start waiting for us at any moment
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                throw new QueryCancelledException();
            } finally {
                waiting.remove(me);
            }
        }
    }

    synchronized void unlock(String key) {
        Owner o = owners.get(key);
        if (o != null && o.thread == Thread.currentThread() && --o.depth == 0) {
            owners.remove(key);
            notifyAll();
        }
    }

    // START_CHANGE: ISS-2025-0746 - 4.6 wave Q4 (extra): the walk understands a MESSAGE wait.
    // A file-lock, join, await or mutex wait is waiting for one definite thread (for an await,
    // any blocked worker blocks it), so reaching the caller along such edges proves the deadlock.
    // A thread blocked in thread_get_message/1,2,3 waits for ANY sender, so it is blocked for good
    // only when every thread that could send it a message is itself blocked — the caller (about to
    // wait for the file) included. A cycle of waits with no way out counts as blocked. The walk
    // is bounded (a budget of visits); running out of it answers "no deadlock", i.e. keep waiting.
    /** Does {@code from} wait — through file locks, joins, worker awaits, mutexes or messages — for {@code target}? */
    private boolean waitsFor(Thread from, Thread target) {
        int[] budget = {2000};
        return blocked(from, target, new HashSet<Thread>(), budget);
    }

    /** Is {@code t} blocked for good, assuming {@code me} is about to block? */
    private boolean blocked(Thread t, Thread me, Set<Thread> path, int[] budget) {
        if (t == me) return true;
        if (--budget[0] < 0) return false;
        if (!path.add(t)) return true;                  // a cycle of waits: nobody on it moves
        try {
            String f = waiting.get(t);
            if (f != null) {
                Owner o = owners.get(f);
                if (o != null && blocked(o.thread, me, path, budget)) return true;
            }
            for (Thread n : ThreadWaits.waitsOn(t)) {
                if (blocked(n, me, path, budget)) return true;
            }
            java.util.Collection<Thread> any = ThreadWaits.anyOf(t);
            if (any != null) {                               // a pool wait: any member ending frees it
                for (Thread s : any) {
                    if (!blocked(s, me, path, budget)) return false;
                }
                return true;
            }
            if (ThreadWaits.inMessageWait(t)) {
                java.util.List<Thread> extra = new java.util.ArrayList<Thread>(waiting.keySet());
                for (Owner o : owners.values()) extra.add(o.thread);
                extra.add(me);
                for (Thread s : ThreadWaits.possibleSenders(t, extra)) {
                    if (!blocked(s, me, path, budget)) return false;
                }
                return true;
            }
            return false;
        } finally {
            path.remove(t);
        }
    }
    // END_CHANGE: ISS-2025-0746
}
// END_CHANGE: ISS-2025-0739
