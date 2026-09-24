package it.denzosoft.jprolog.core.engine;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

// START_CHANGE: ISS-2025-0639 - wave P6 (extra): who is blocked joining whom.
// START_CHANGE: ISS-2025-0739 - 4.6 wave Q3.8: also who is blocked awaiting its concurrent_*
// workers; the per-file load lock walks these edges to refuse a load that can never proceed.
/**
 * A registry of the waits the load lock ({@link LoadLock}) must know about: a Prolog thread
 * blocked in {@code thread_join/1,2} (on one thread), and a thread blocked awaiting the workers
 * of a {@code concurrent_*} call (on the pool threads running its goals).
 */
public final class ThreadWaits {

    private ThreadWaits() {}

    private static final ConcurrentHashMap<Thread, Thread> JOINING = new ConcurrentHashMap<Thread, Thread>();
    /** worker thread -> the thread whose concurrent_* goal it is running. */
    private static final ConcurrentHashMap<Thread, Thread> WORKER_OF = new ConcurrentHashMap<Thread, Thread>();
    /** threads blocked awaiting their workers. */
    private static final Map<Thread, Boolean> AWAITING = new ConcurrentHashMap<Thread, Boolean>();

    /** The calling thread is about to block until {@code target} ends. */
    public static void enterJoin(Thread target) {
        if (target != null) JOINING.put(Thread.currentThread(), target);
    }

    /** The calling thread is no longer blocked in a join. */
    public static void exitJoin() {
        JOINING.remove(Thread.currentThread());
    }

    /** The calling (pool) thread starts running a goal on behalf of {@code parent}. */
    public static void enterWorker(Thread parent) {
        if (parent != null) WORKER_OF.put(Thread.currentThread(), parent);
    }

    /** The calling (pool) thread finished that goal. */
    public static void exitWorker() {
        WORKER_OF.remove(Thread.currentThread());
    }

    /** The calling thread blocks until its workers answer. */
    public static void enterAwait() { AWAITING.put(Thread.currentThread(), Boolean.TRUE); }

    public static void exitAwait() { AWAITING.remove(Thread.currentThread()); }

    // START_CHANGE: ISS-2025-0746 - 4.6 wave Q4 (extra): message-queue and mutex waits, and the
    // live Prolog threads, so the load-cycle check (LoadLock) sees a load that waits for a message
    // only the blocked loader could send.
    /** Threads blocked in thread_get_message/1,2,3 (or a full queue's thread_send_message). */
    private static final Map<Thread, Boolean> MESSAGE_WAIT = new ConcurrentHashMap<Thread, Boolean>();
    /** Threads blocked on a mutex -> who holds it right now. */
    private static final Map<Thread, java.util.function.Supplier<Thread>> LOCK_WAIT =
        new ConcurrentHashMap<Thread, java.util.function.Supplier<Thread>>();
    /** The live thread_create/2,3 threads. */
    private static final Map<Thread, Boolean> PROLOG_THREADS = new ConcurrentHashMap<Thread, Boolean>();

    public static void enterMessageWait() { MESSAGE_WAIT.put(Thread.currentThread(), Boolean.TRUE); }

    public static void exitMessageWait() { MESSAGE_WAIT.remove(Thread.currentThread()); }

    /** Threads blocked until ANY of a known set of threads moves (a full thread pool's members). */
    private static final Map<Thread, java.util.function.Supplier<java.util.Collection<Thread>>> ANY_WAIT =
        new ConcurrentHashMap<Thread, java.util.function.Supplier<java.util.Collection<Thread>>>();

    public static void enterAnyWait(java.util.function.Supplier<java.util.Collection<Thread>> who) {
        ANY_WAIT.put(Thread.currentThread(), who);
    }

    public static void exitAnyWait() { ANY_WAIT.remove(Thread.currentThread()); }

    /** The threads any one of which could release {@code t} from its pool wait, or null. */
    static java.util.Collection<Thread> anyOf(Thread t) {
        java.util.function.Supplier<java.util.Collection<Thread>> s = ANY_WAIT.get(t);
        return (s == null) ? null : s.get();
    }

    /** The calling thread blocks on a mutex whose current holder {@code owner} reports. */
    public static void enterLockWait(java.util.function.Supplier<Thread> owner) {
        if (owner != null) LOCK_WAIT.put(Thread.currentThread(), owner);
    }

    public static void exitLockWait() { LOCK_WAIT.remove(Thread.currentThread()); }

    /** Threads running a top-level query right now (nesting depth): a `main` thread can send too. */
    private static final Map<Thread, int[]> IN_QUERY = new ConcurrentHashMap<Thread, int[]>();

    public static void enterQuery() {
        int[] d = IN_QUERY.get(Thread.currentThread());
        if (d == null) IN_QUERY.put(Thread.currentThread(), new int[] {1}); else d[0]++;
    }

    public static void exitQuery() {
        int[] d = IN_QUERY.get(Thread.currentThread());
        if (d != null && --d[0] <= 0) IN_QUERY.remove(Thread.currentThread());
    }

    public static void enterThread() { PROLOG_THREADS.put(Thread.currentThread(), Boolean.TRUE); }

    public static void exitThread() { PROLOG_THREADS.remove(Thread.currentThread()); }

    /** Is {@code t} blocked waiting for a message (from anybody)? */
    static boolean inMessageWait(Thread t) { return MESSAGE_WAIT.containsKey(t); }

    /**
     * Every thread that could send {@code t} a message, as far as the engine can see: the live
     * Prolog threads, every thread in a registered wait and {@code extra} — minus {@code t}.
     */
    static java.util.Set<Thread> possibleSenders(Thread t, java.util.Collection<Thread> extra) {
        java.util.Set<Thread> out = new java.util.LinkedHashSet<Thread>();
        for (Thread x : PROLOG_THREADS.keySet()) if (x.isAlive()) out.add(x);
        for (Thread x : IN_QUERY.keySet()) if (x.isAlive()) out.add(x);
        out.addAll(JOINING.keySet());
        out.addAll(AWAITING.keySet());
        out.addAll(WORKER_OF.keySet());
        out.addAll(MESSAGE_WAIT.keySet());
        out.addAll(LOCK_WAIT.keySet());
        out.addAll(ANY_WAIT.keySet());
        out.addAll(extra);
        out.remove(t);
        return out;
    }
    // END_CHANGE: ISS-2025-0746

    /** The threads {@code t} is directly blocked on (its join target, its running workers). */
    static List<Thread> waitsOn(Thread t) {
        Thread j = JOINING.get(t);
        boolean awaiting = AWAITING.containsKey(t);
        java.util.function.Supplier<Thread> lk = LOCK_WAIT.get(t);            // ISS-2025-0746
        Thread holder = (lk != null) ? lk.get() : null;
        if (j == null && !awaiting && holder == null) return Collections.emptyList();
        List<Thread> out = new ArrayList<Thread>();
        if (j != null) out.add(j);
        if (holder != null && holder != t) out.add(holder);                  // ISS-2025-0746
        if (awaiting) {
            for (Map.Entry<Thread, Thread> e : WORKER_OF.entrySet()) {
                if (e.getValue() == t) out.add(e.getKey());
            }
        }
        return out;
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
// END_CHANGE: ISS-2025-0739
// END_CHANGE: ISS-2025-0639
