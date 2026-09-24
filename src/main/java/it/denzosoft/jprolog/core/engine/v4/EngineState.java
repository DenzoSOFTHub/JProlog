package it.denzosoft.jprolog.core.engine.v4;

// START_CHANGE: ISS-2025-0472 - engine v4 wave W7 (design B.11/B.12): the per-engine state that
// used to live in process-global statics — the stream table, the operator store, the spy points and
// the profiler counters (limits L-06, L-07, LIM-025, LIM-034).
/**
 * The state one {@code Prolog} instance owns that the ~400 legacy built-ins reach through static
 * facades ({@code StreamManager}, {@code OperatorDefinition}, {@code Spy}, {@code Profiler}).
 *
 * <p>Exactly the shape {@link it.denzosoft.jprolog.core.system.PrologFlags} has had since
 * ISS-2025-0437: each {@code Prolog} owns one instance and installs it as the <em>thread-current</em>
 * state around every solve/consult entry point, so the unchanged static API routes to the right
 * engine and the previous state is restored afterwards (an engine invoked from inside another
 * engine's built-in cannot leave its state behind). Code with no engine in scope — a
 * directly-instantiated built-in, a unit test — sees a process-wide default, exactly as before.
 *
 * <p>It lives outside the machine on purpose: the ~310 bridged built-ins reach it through their
 * static facades, with no engine object in hand. (Until 4.1 it was also shared with the v2 engine,
 * which design decision 1 of B.17 kept selectable for one release.)
 */
public final class EngineState {

    private final Streams streams = new Streams();
    private final Ops ops = new Ops();
    private final Spies spies = new Spies();
    private final Profile profile = new Profile();

    /** The state used when no engine is current on this thread (standalone built-in use, tests). */
    private static final EngineState DEFAULT = new EngineState();
    private static final ThreadLocal<EngineState> CURRENT = new ThreadLocal<EngineState>();

    /** The engine state in effect on this thread. */
    public static EngineState current() {
        EngineState s = CURRENT.get();
        return (s != null) ? s : DEFAULT;
    }

    /**
     * Install {@code s} as this thread's state (null restores the process-wide default).
     *
     * @return the previous state, so a caller can restore it in a finally block
     */
    public static EngineState setCurrent(EngineState s) {
        EngineState prev = CURRENT.get();
        if (s == null) CURRENT.remove(); else CURRENT.set(s);
        return prev;
    }

    // START_CHANGE: ISS-2025-0745 - 4.6 wave Q4 (extra): the working directory is PER ENGINE.
    // working_directory/2 used to set the JVM-wide `user.dir` property — a hazard with several
    // engines or embedders in one JVM, and ineffective besides (java.io.File caches user.dir at
    // startup, so relative paths never followed it). Every built-in that takes a file name now
    // resolves a relative one against the working directory of the engine current on the thread.
    /** The JVM's working directory at startup (java.io.File's own cached value). */
    private static final String JVM_CWD = new java.io.File("").getAbsolutePath();

    private volatile String workingDirectory = JVM_CWD;

    /** This engine's working directory: an absolute path, without a trailing separator. */
    public String workingDirectory() { return workingDirectory; }

    /** Change this engine's working directory ({@code dir} must be an absolute directory path). */
    public void setWorkingDirectory(String dir) { this.workingDirectory = dir; }

    /** {@code path} as a file, a relative one resolved against the current engine's directory. */
    public static java.io.File file(String path) {
        java.io.File f = new java.io.File(path);
        if (f.isAbsolute()) return f;
        String base = current().workingDirectory;
        return path.isEmpty() ? new java.io.File(base) : new java.io.File(base, path);
    }

    /** {@link #file(String)} as a path string. */
    public static String path(String path) { return file(path).getPath(); }
    // END_CHANGE: ISS-2025-0745

    /** This engine's stream table (design B.11). */
    public Streams streams() { return streams; }

    /** This engine's operator store (design B.12). */
    public Ops ops() { return ops; }

    /** This engine's spy points. */
    public Spies spies() { return spies; }

    /** This engine's profiler counters. */
    public Profile profile() { return profile; }

    // ------------------------------------------------------------------
    // Spy points and profiler counters: two one-field owners that used to be statics on
    // builtin.debug.Spy and core.engine.Profiler (the tail of LIM-034).
    // ------------------------------------------------------------------

    /** The spy points of one engine. */
    public static final class Spies {
        private final java.util.Set<String> points =
            java.util.Collections.newSetFromMap(new java.util.concurrent.ConcurrentHashMap<String, Boolean>());

        public boolean has(String name, int arity) { return points.contains(name + "/" + arity); }
        public void add(String pi) { points.add(pi); }
        public void remove(String pi) { points.remove(pi); }
        public void clear() { points.clear(); }
        public java.util.Set<String> snapshot() { return new java.util.HashSet<String>(points); }
    }

    /** The per-predicate call counters of one engine. */
    public static final class Profile {
        private volatile boolean enabled = false;
        private final java.util.Map<String, java.util.concurrent.atomic.AtomicLong> counters =
            new java.util.concurrent.ConcurrentHashMap<String, java.util.concurrent.atomic.AtomicLong>();

        public void enable() { enabled = true; }
        public void disable() { enabled = false; }
        public boolean isEnabled() { return enabled; }
        public void reset() { counters.clear(); }

        public void record(String name, int arity) {
            if (!enabled || name == null) return;
            String key = name + "/" + arity;
            java.util.concurrent.atomic.AtomicLong c = counters.get(key);
            if (c == null) {
                counters.putIfAbsent(key, new java.util.concurrent.atomic.AtomicLong());
                c = counters.get(key);
            }
            c.incrementAndGet();
        }

        public java.util.Map<String, Long> snapshot() {
            java.util.List<java.util.Map.Entry<String, java.util.concurrent.atomic.AtomicLong>> es =
                new java.util.ArrayList<java.util.Map.Entry<String, java.util.concurrent.atomic.AtomicLong>>(counters.entrySet());
            java.util.Collections.sort(es,
                new java.util.Comparator<java.util.Map.Entry<String, java.util.concurrent.atomic.AtomicLong>>() {
                    public int compare(java.util.Map.Entry<String, java.util.concurrent.atomic.AtomicLong> a,
                                       java.util.Map.Entry<String, java.util.concurrent.atomic.AtomicLong> b) {
                        return Long.compare(b.getValue().get(), a.getValue().get());
                    }
                });
            java.util.Map<String, Long> out = new java.util.LinkedHashMap<String, Long>();
            for (java.util.Map.Entry<String, java.util.concurrent.atomic.AtomicLong> e : es) {
                out.put(e.getKey(), e.getValue().get());
            }
            return out;
        }
    }
}
// END_CHANGE: ISS-2025-0472
