// START_CHANGE: CR-2025-0009 - basic predicate profiling
package it.denzosoft.jprolog.core.engine;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Lightweight per-predicate call counter.
 * Enabled via {@link #enable()} (zero overhead when disabled — solvers
 * check {@link #isEnabled()} before recording).
 */
public final class Profiler {

    private static volatile boolean enabled = false;
    private static final Map<String, AtomicLong> COUNTERS = new ConcurrentHashMap<>();

    private Profiler() {}

    public static void enable() { enabled = true; }
    public static void disable() { enabled = false; }
    public static boolean isEnabled() { return enabled; }

    public static void reset() {
        COUNTERS.clear();
    }

    /** Record a call to predicate Name/Arity. No-op when disabled. */
    public static void recordCall(String name, int arity) {
        if (!enabled || name == null) return;
        String key = name + "/" + arity;
        COUNTERS.computeIfAbsent(key, k -> new AtomicLong()).incrementAndGet();
    }

    /** Snapshot of counters: key → count. */
    public static Map<String, Long> snapshot() {
        Map<String, Long> out = new java.util.LinkedHashMap<>();
        // Sort by count desc
        COUNTERS.entrySet().stream()
            .sorted((a, b) -> Long.compare(b.getValue().get(), a.getValue().get()))
            .forEach(e -> out.put(e.getKey(), e.getValue().get()));
        return out;
    }
}
// END_CHANGE: CR-2025-0009
