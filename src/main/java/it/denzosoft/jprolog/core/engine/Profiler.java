// START_CHANGE: CR-2025-0009 - basic predicate profiling
package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.core.engine.v4.EngineState;

import java.util.Map;

/**
 * Lightweight per-predicate call counter.
 * Enabled via {@link #enable()} (zero overhead when disabled — solvers
 * check {@link #isEnabled()} before recording).
 *
 * <p>START_CHANGE: ISS-2025-0477 - engine v4 wave W7 (the tail of LIM-034): the counters are
 * PER ENGINE, not per JVM. {@code profile(on)} in one {@code Prolog} used to start counting for
 * every engine in the process and {@code profile(reset)} in one wiped everyone's numbers. This is
 * now a static facade over {@code EngineState.current().profile()}, exactly like the flag store.
 */
public final class Profiler {

    private Profiler() {}

    private static EngineState.Profile p() { return EngineState.current().profile(); }

    public static void enable() { p().enable(); }
    public static void disable() { p().disable(); }
    public static boolean isEnabled() { return p().isEnabled(); }

    public static void reset() { p().reset(); }

    /** Record a call to predicate Name/Arity. No-op when disabled. */
    public static void recordCall(String name, int arity) { p().record(name, arity); }

    /** Snapshot of counters: key -> count, highest first. */
    public static Map<String, Long> snapshot() { return p().snapshot(); }
}
// END_CHANGE: CR-2025-0009
// END_CHANGE: ISS-2025-0477
