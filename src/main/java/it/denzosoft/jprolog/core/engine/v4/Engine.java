package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.BuiltInRegistry;
import it.denzosoft.jprolog.core.engine.KnowledgeBase;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.EngineContext;
import it.denzosoft.jprolog.core.engine.TableStore;
import it.denzosoft.jprolog.core.module.ModuleManager;

// START_CHANGE: ISS-2025-0444 - engine v4, design B.13 (the per-engine context).
/**
 * Everything one {@code Prolog} instance owns that the v4 machine needs: the clause store, the
 * knowledge base behind it, the built-in tables (native v4 and legacy), the module manager, the
 * table store and the {@link EngineContext} that is the durable home of the IDE debug
 * controller and the query ResourceGuard.
 *
 * <p>One {@code Engine} per {@link Prolog}; one {@link Machine} per query over it. The machine
 * therefore holds no cross-query state, exactly as the v2 engine did before it.
 *
 * <p>Design B.13 also gives the Engine the stream table, the operator store and the flags; those
 * arrive in wave W7 — until then v4 uses the same per-engine {@code PrologFlags} and the same
 * (still process-global) {@code StreamManager} as the v2 engine, so the two behave identically.
 */
public final class Engine {

    private final Prolog prolog;
    private final KnowledgeBase kb;
    private final BuiltInRegistry registry;
    private final EngineContext context;
    private final ModuleManager modules;
    private final TableStore tables;
    private final ClauseStore store;
    private final BuiltinTable natives = new BuiltinTable();
    // START_CHANGE: ISS-2025-0463 - wave W5: the v4 tabling store (design B.8). TableStore stays
    // the home of the `:- table p/n` DECLARATIONS (written by the consult-time directive and by
    // table/1); the ANSWERS live here, as variant-normalised terms on the cell model rather than
    // name-keyed solution maps (ISS-2025-0491: and now they live ONLY here).
    private final Tabling tabling = new Tabling();
    // END_CHANGE: ISS-2025-0463
    // START_CHANGE: ISS-2025-0608 - P4.15: statistics(inferences, N) — the steps of every
    // finished top-level query of this engine (the running query adds its guard's count).
    private final java.util.concurrent.atomic.AtomicLong inferences = new java.util.concurrent.atomic.AtomicLong();
    void addInferences(long n) { if (n > 0) inferences.addAndGet(n); }
    long inferences() { return inferences.get(); }
    // END_CHANGE: ISS-2025-0608
    // START_CHANGE: ISS-2025-0466 - wave W6: the v4 module owner (design B.10). It REPLACES
    // ModuleManager as the resolver on the v4 path; the manager stays the consult-time recorder
    // shared with the legacy and v2 engines, and `modules4` mirrors the user-defined modules from
    // it. `user` is the flat clause store, so there is no `modules.size() > 1` special case left.
    private final Modules modules4;
    // END_CHANGE: ISS-2025-0466

    public Engine(Prolog prolog, KnowledgeBase kb, BuiltInRegistry registry,
                  EngineContext context, ModuleManager modules, TableStore tables) {
        this.prolog = prolog;
        this.kb = kb;
        this.registry = registry;
        this.context = context;
        this.modules = modules;
        this.tables = tables;
        this.store = new ClauseStore(kb);
        NativeBuiltins.register(natives);
        // ISS-2025-0466: the prelude is no longer loaded here. Library modules are autoloaded by
        // predicate indicator on first reference (design B.10), so engine creation costs one
        // HashMap of module headers — read once per JVM — instead of parsing every prelude file.
        this.modules4 = new Modules(this, modules);
    }

    public Prolog prolog() { return prolog; }
    public KnowledgeBase kb() { return kb; }
    public BuiltInRegistry registry() { return registry; }
    /** ISS-2025-0484 - wave W9: the durable per-engine context (debug controller, guard). */
    public EngineContext context() { return context; }
    public ModuleManager modules() { return modules; }
    /** The v4 module owner (design B.10, wave W6). */
    public Modules modules4() { return modules4; }
    public TableStore tables() { return tables; }
    public ClauseStore store() { return store; }
    public BuiltinTable natives() { return natives; }
    Tabling tabling() { return tabling; }

    // START_CHANGE: ISS-2025-0540 - wave P2.1: the dispatch stamp. A body goal's call-site cache
    // (Machine.CallSite) records that "name/arity is a plain user predicate" — not a native, not a
    // registry built-in, not tabled — and that answer depends only on these three tables. Each only
    // ever counts up, so their sum changes whenever any of them does.
    /** Changes whenever the native table, the built-in registry or the table declarations do. */
    long dispatchStamp() {
        return (long) natives.modCount()
            + (registry == null ? 0 : registry.modCount())
            + (tables == null ? 0 : tables.modCount())
            + modules4.stamp();
    }
    // END_CHANGE: ISS-2025-0540
}
// END_CHANGE: ISS-2025-0444
