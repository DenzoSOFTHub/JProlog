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

    public ResourceGuard(long budget) {
        this.budget = budget;
    }

    /** Charge one resolution step. */
    public void step() {
        long n = ++steps;
        if (budget > 0 && n > budget) throw new InferenceLimitException(budget);
        if ((n & INTERRUPT_POLL_MASK) == 0 && Thread.currentThread().isInterrupted()) {
            throw new QueryCancelledException();
        }
    }

    /** Charge one step and always poll the interrupt (used by the v2 drive loop, which must react
     *  to Stop promptly even when no budget is set). */
    public void stepChecked() {
        if (Thread.currentThread().isInterrupted()) throw new QueryCancelledException();
        long n = ++steps;
        if (budget > 0 && n > budget) throw new InferenceLimitException(budget);
    }

    public long getSteps() { return steps; }
    public long getBudget() { return budget; }
    public boolean isUnlimited() { return budget <= 0; }
}
// END_CHANGE: ISS-2025-0431
