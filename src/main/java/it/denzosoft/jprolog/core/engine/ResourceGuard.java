package it.denzosoft.jprolog.core.engine;

// START_CHANGE: ISS-2025-0431 - ENG-04: shared budget + cancellation counter.
/**
 * The per-query resource budget, shared between the {@link it.denzosoft.jprolog.core.engine.v4.Machine}
 * running the query and every sub-solve it delegates to.
 *
 * <p>Before ENG-04 the inference budget and the IDE/embedder Stop interrupt were polled only by the
 * v2 drive loop, so any goal that ran through a {@link BuiltInWithContext} built-in — {@code once/1},
 * {@code ignore/1}, {@code forall/2}, {@code aggregate_all/3}, {@code setup_call_cleanup/3},
 * {@code maplist/N}, … — escaped both: {@code once(Loop)} was all untrusted code needed to run
 * forever with a budget set. The machine now installs a guard on the shared {@code EngineContext} for
 * the duration of a query, and every nested sub-solve charges every resolution step to
 * it, so the SAME counter and the SAME interrupt check cover every goal.
 *
 * <p>Both exceptions it raises are plain {@link RuntimeException}s, never {@code PrologException}s:
 * untrusted {@code catch/3} must not be able to trap them.
 */
public final class ResourceGuard {

    /** Poll the thread interrupt flag once every this many steps (the check itself is cheap, but
     *  it is on the hottest path in the engine). */
    private static final int INTERRUPT_POLL_MASK = 0x3FF;   // every 1024 steps

    private final long budget;      // 0 = unlimited
    private long steps;

    // START_CHANGE: ISS-2025-0624 - wave P6.3: ONE budget per query, shared by every machine of it.
    // A worker machine (thread_create/2,3, the concurrent_* family) used to get a fresh guard with
    // the parent's FULL limit, so N workers multiplied the budget by N+1. The limit now lives in a
    // shared pool (an AtomicLong holding the steps still available); each guard draws CREDIT from it
    // in chunks of at most CHUNK steps so the hot path stays a plain decrement of a field. A single
    // guard is exact (it can never draw more than the pool holds); with several machines the
    // overshoot is bounded by the credit the others still hold, and a worker hands its unused credit
    // back when it finishes ({@link #release()}).
    private static final long CHUNK = 1024L;
    /** Steps still available to the whole query; null when unlimited. */
    private final java.util.concurrent.atomic.AtomicLong pool;
    /** Steps this guard may take before drawing from the pool again. */
    private long credit;

    public ResourceGuard(long budget) {
        this.budget = budget;
        this.pool = (budget > 0) ? new java.util.concurrent.atomic.AtomicLong(budget) : null;
    }

    private ResourceGuard(ResourceGuard parent) {
        this.budget = parent.budget;
        this.pool = parent.pool;
    }

    /**
     * A guard for another machine of the SAME query (a worker thread): it shares this guard's
     * budget pool, but keeps its own step counter and polls its own thread's interrupt flag.
     */
    public ResourceGuard child() {
        return new ResourceGuard(this);
    }

    /** Hand the credit this guard drew but did not use back to the shared pool. */
    public void release() {
        if (pool != null && credit > 0) {
            pool.addAndGet(credit);
            credit = 0;
        }
    }

    /** The steps still available to the query (Long.MAX_VALUE when unlimited). */
    public long remaining() {
        return (pool == null) ? Long.MAX_VALUE : Math.max(0L, pool.get()) + Math.max(0L, credit);
    }

    /** Draw the next chunk of credit, or raise the limit when the pool is empty. */
    private void refill() {
        for (;;) {
            long avail = pool.get();
            if (avail <= 0) throw exhausted();
            long take = Math.min(CHUNK, avail);
            if (pool.compareAndSet(avail, avail - take)) {
                credit += take;     // credit was -1: this step is paid from the new chunk
                return;
            }
        }
    }

    /**
     * Charge {@code n} steps at once — for a native that did O(n) work in one call (a long
     * {@code length/2}, {@code copy_term/2}, {@code msort/2}, ...). Polls the interrupt too.
     */
    public void charge(long n) {
        if (n <= 0) return;
        steps += n;
        if (pool != null) {
            credit -= n;
            while (credit < 0) refill();
        }
        if (Thread.currentThread().isInterrupted()) throw new QueryCancelledException();
    }
    // END_CHANGE: ISS-2025-0624

    /** Charge one resolution step. */
    public void step() {
        long n = ++steps;
        if (pool != null && --credit < 0) refill();                   // ISS-2025-0624
        if ((n & INTERRUPT_POLL_MASK) == 0 && Thread.currentThread().isInterrupted()) {
            throw new QueryCancelledException();
        }
    }

    // START_CHANGE: ISS-2025-0523 - wave P1.10: once the budget is exhausted the query is being
    // torn down — InferenceLimitException is not a PrologException, so no catch/3 can resume it —
    // but the teardown still has to run the cleanup of every setup_call_cleanup/3 frame it
    // abandons (closing a stream, releasing a mutex). With the counter left past the budget each
    // cleanup would be aborted at its very first step. So every time the limit is hit, the counter
    // is moved back to leave a small fresh allowance for that teardown work; a cleanup that
    // overruns it is itself aborted, and the next cleanup gets the next allowance (each frame's
    // cleanup runs at most once, so this is bounded by the number of frames).
    /** Steps a cleanup may take after the budget has been exhausted. */
    static final long TEARDOWN_ALLOWANCE = 100000L;

    private InferenceLimitException exhausted() {
        long allowance = Math.min(budget, TEARDOWN_ALLOWANCE);
        credit = allowance;          // ISS-2025-0624: a local allowance, never drawn from the pool
        return new InferenceLimitException(budget);
    }
    // END_CHANGE: ISS-2025-0523

    /** Charge one step and always poll the interrupt (used by the v2 drive loop, which must react
     *  to Stop promptly even when no budget is set). */
    public void stepChecked() {
        if (Thread.currentThread().isInterrupted()) throw new QueryCancelledException();
        ++steps;
        if (pool != null && --credit < 0) refill();                   // ISS-2025-0624
    }

    public long getSteps() { return steps; }
    public long getBudget() { return budget; }
    public boolean isUnlimited() { return budget <= 0; }
}
// END_CHANGE: ISS-2025-0431
