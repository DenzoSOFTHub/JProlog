package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.module.Module;
import it.denzosoft.jprolog.core.module.ModuleManager;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

// START_CHANGE: ISS-2025-0466 - engine v4 wave W6, design B.10: the module owner.
/**
 * The v4 engine's module system. One instance per {@link Engine}; it replaces
 * {@code core.module.ModuleManager} <b>as the resolver</b> on the v4 path (the manager stays the
 * consult-time recorder shared with the legacy and v2 engines, and this class mirrors it).
 *
 * <h3>The three kinds of module</h3>
 * <ul>
 *   <li><b>{@code user}</b> — the default, and it <i>is</i> the flat clause store: a predicate of
 *       module {@code user} lives in {@link ClauseStore}/{@code KnowledgeBase} exactly as it did
 *       before there was a module system. The {@code modules.size() &gt; 1} special case the v4
 *       machine inherited from v2 is therefore gone.</li>
 *   <li><b>library modules</b> — {@code lists}, {@code apply}, {@code pairs},
 *       {@code coroutining}, … written in Prolog and loaded from {@code prelude/*.pl} classpath
 *       resources. They are <b>autoloaded by predicate indicator</b>: the index of
 *       {@code indicator -> module} is read from the resources' {@code :- module/2} headers once
 *       per JVM, and a module's clauses are parsed and compiled the first time one of its
 *       predicates is actually referenced.</li>
 *   <li><b>{@code system}</b> — the built-ins (v4 natives and legacy-registry ones). It holds no
 *       clauses; {@code system:G} is a built-in dispatch, and {@code Prolog.enableSafeMode()} still
 *       strips the host-touching built-ins from it because it strips them from the registry.</li>
 * </ul>
 *
 * <h3>Resolution order</h3>
 * For an unqualified {@code f/n} called from module {@code M}:
 * {@code M} -> {@code M}'s imports -> {@code user} -> autoload libraries -> {@code system}.
 * ({@code system} is applied by {@link Machine#stepN}, which consults the built-in tables before
 * the clause layers for compatibility — see the deviation note in the progress report.)
 *
 * <h3>Overriding</h3>
 * A library predicate is the LAST resort, so a definition of the same indicator in the calling
 * context wins: in {@code user} it is the flat store, in a module {@code M} it is {@code M}'s own
 * clauses — and a definition inside {@code M} overrides only for {@code M} and the modules that
 * import it, never for everyone.
 */
public final class Modules {

    public static final String USER = "user";
    public static final String SYSTEM = "system";

    /** A meta-argument that is not module-sensitive ({@code + - ? *} and plain terms). */
    static final int META_PLAIN = -1;
    /** {@code :} — a module-sensitive term (qualified, but not called). */
    static final int META_COLON = 100;

    /**
     * One module.
     *
     * <p>START_CHANGE: ISS-2025-0747 - 4.6 wave Q4 (extra): a module is read by every thread of
     * the engine while a library is parsed on first use or the mirror is rebuilt on another one.
     * Its collections are therefore never mutated once published: a load or a re-sync builds new
     * ones and swaps them in through these volatile fields, and {@code loaded} is written LAST
     * (it used to be set first, so a second thread autoloading at the same moment saw a "loaded"
     * module with no clauses and raised existence_error). END_CHANGE: ISS-2025-0747
     */
    static final class Mod {
        final String name;
        /** Classpath resource for a library module; null for a user-defined one. */
        final String resource;
        volatile boolean loaded;
        boolean explicitExports;
        volatile Set<String> exports = new LinkedHashSet<String>();
        volatile List<String> imports = new ArrayList<String>();
        volatile Map<String, Pred> clauses = new HashMap<String, Pred>();
        volatile Map<String, int[]> meta = new HashMap<String, int[]>();
        /** Import restrictions per imported module (ISS-2025-0735); absent = everything. */
        volatile Map<String, Module.ImportSpec> specs = new HashMap<String, Module.ImportSpec>();
        // START_CHANGE: ISS-2025-0784 - what the mirror of a user module was built from
        Module src;
        long srcVersion = -1;
        int rulesSeen;
        // END_CHANGE: ISS-2025-0784

        Mod(String name, String resource) { this.name = name; this.resource = resource; }

        boolean isLibrary() { return resource != null; }
    }

    /** A successful lookup: the clauses plus the module whose context their bodies run in. */
    static final class Hit {
        final Clause[] clauses;
        final String module;
        Hit(Clause[] clauses, String module) { this.clauses = clauses; this.module = module; }
    }

    // START_CHANGE: ISS-2025-0468 - a module predicate gets the SAME first-argument indexing the
    // flat store has. Without it `append([H|T], L, [H|R]) :- append(T, L, R).` pushed a 2-clause
    // choice point per element and left a million live frames behind on a 1 000 000-element list
    // (measured: 2.7 us per inference against 0.35 us with the index).
    //
    // The index is built ONCE, when the module's clauses are installed, and never changes — a
    // library is immutable and a mirrored user module is rebuilt wholesale. It is also bounded by
    // construction: only keys that occur in a clause head get an entry, so a predicate called with
    // a different integer every time adds nothing (the v3.8.0 unbounded-cache hazard).
    /** One indicator's clauses inside a module, with a first-argument index. */
    static final class Pred {
        final Clause[] all;
        // START_CHANGE: ISS-2025-0784 - built on the first indexed selection, not with the
        // predicate: a module that grows clause by clause (the incremental mirror) gets a new Pred
        // per change, and most of them are never selected by key. The merge of a bucket with the
        // variable-headed clauses is by position, O(|bucket| + |var-headed|) — it rescanned the
        // whole predicate per bucket (O(buckets * clauses)).
        private volatile Index index;

        private static final class Index {
            final Map<Object, Clause[]> byKey;
            final Clause[] varHeaded;
            Index(Map<Object, Clause[]> byKey, Clause[] varHeaded) { this.byKey = byKey; this.varHeaded = varHeaded; }
        }

        Pred(Clause[] all) {
            this.all = all;
        }

        private Index index() {
            Index ix = index;
            if (ix != null) return ix;
            Map<Object, int[]> groups = new java.util.LinkedHashMap<Object, int[]>();   // key -> {count, pos...}
            int[] vpos = new int[4];
            int nv = 0;
            for (int i = 0; i < all.length; i++) {
                Object k = all[i].firstArgKey;
                if (k == null) {
                    if (nv == vpos.length) vpos = Arrays.copyOf(vpos, nv * 2);
                    vpos[nv++] = i;
                } else {
                    int[] g = groups.get(k);
                    if (g == null) { g = new int[4]; groups.put(k, g); }
                    if (g[0] + 1 == g.length) { g = Arrays.copyOf(g, g.length * 2); groups.put(k, g); }
                    g[++g[0]] = i;
                }
            }
            Clause[] varHeaded = new Clause[nv];
            for (int i = 0; i < nv; i++) varHeaded[i] = all[vpos[i]];
            Map<Object, Clause[]> m;
            if (groups.isEmpty()) {
                m = java.util.Collections.emptyMap();
            } else {
                m = new HashMap<Object, Clause[]>();
                for (Map.Entry<Object, int[]> e : groups.entrySet()) {
                    int[] g = e.getValue();
                    int nb = g[0];
                    Clause[] merged = new Clause[nb + nv];
                    int a = 1, b = 0, k = 0;
                    while (a <= nb || b < nv) {                  // both position lists ascend
                        if (b >= nv || (a <= nb && g[a] < vpos[b])) merged[k++] = all[g[a++]];
                        else merged[k++] = all[vpos[b++]];
                    }
                    m.put(e.getKey(), merged);
                }
            }
            ix = new Index(m, varHeaded);
            index = ix;
            return ix;
        }

        /** The clauses whose head could match a goal with first-argument key {@code k}. */
        Clause[] select(Object k) {
            if (k == null) return all;
            Index ix = index();
            if (ix.byKey.isEmpty()) return all;
            Clause[] s = ix.byKey.get(k);
            return (s != null) ? s : ix.varHeaded;      // no head has that key: only the variable ones
        }
        // END_CHANGE: ISS-2025-0784
    }
    // END_CHANGE: ISS-2025-0468

    private final Engine engine;
    private final ModuleManager legacy;
    private final Map<String, Mod> mods = new java.util.concurrent.ConcurrentHashMap<String, Mod>();   // ISS-2025-0747
    private volatile long mirrorStamp = -1;

    Modules(Engine engine, ModuleManager legacy) {
        this.engine = engine;
        this.legacy = legacy;
        mods.put(USER, new Mod(USER, null));
        mods.put(SYSTEM, new Mod(SYSTEM, null));
        for (Prelude.Lib lib : Prelude.libraries()) {
            Mod m = new Mod(lib.module, lib.resource);
            m.explicitExports = true;
            m.exports.addAll(lib.exports);
            mods.put(lib.module, m);
        }
    }

    // ------------------------------------------------------------------ mirroring

    /**
     * Bring the user-defined modules in line with the {@link ModuleManager} the consult path
     * writes to. Only non-{@code user} modules are mirrored: {@code user} is the flat store.
     */
    private void sync() {
        if (legacy == null) return;
        long s = legacy.getStamp();
        if (s == mirrorStamp) return;
        // ISS-2025-0739: loads may run on several threads now — one rebuild at a time, and the
        // stamp is published only once the mirror is complete
        synchronized (this) {
            s = legacy.getStamp();
            if (s == mirrorStamp) return;
            syncLocked();
            mirrorStamp = s;
        }
    }

    private void syncLocked() {
        for (Map.Entry<String, Module> e : legacy.getAllModules().entrySet()) {
            String name = e.getKey();
            if (USER.equals(name) || SYSTEM.equals(name)) {
                // `user` is the flat store; we still take its import list and meta declarations.
                Mod um = mods.get(USER);
                // ISS-2025-0747: new collections, swapped in (never mutated in place)
                um.imports = new ArrayList<String>(new TreeSet<String>(e.getValue().getImportedModules().keySet()));
                um.specs = new HashMap<String, Module.ImportSpec>(e.getValue().getImportSpecs());   // ISS-2025-0735
                um.meta = copyMeta(e.getValue());
                continue;
            }
            Mod m = mods.get(name);
            if (m == null || m.isLibrary()) {
                if (m == null) { m = new Mod(name, null); mods.put(name, m); }
                else continue;                      // a user module may not shadow a library one
            }
            Module lm = e.getValue();
            // START_CHANGE: ISS-2025-0784 - 4.6 wave Q6 (extra 4): unchanged -> nothing to do;
            // only rules appended -> compile just those and replace just their predicates.
            long ver = lm.getStructVersion();
            if (m.src == lm && m.srcVersion == ver) {
                int count = lm.getLocalRuleCount();
                if (count == m.rulesSeen) continue;
                if (count > m.rulesSeen) {
                    appendRules(m, lm, lm.getLocalRulesFrom(m.rulesSeen));
                    continue;
                }
            }
            // END_CHANGE: ISS-2025-0784
            // ISS-2025-0747: build everything aside, then swap it in
            Set<String> exps = new LinkedHashSet<String>();
            for (it.denzosoft.jprolog.core.module.PredicateSignature sig : lm.getExportedPredicates()) {
                exps.add(sig.getFunctor() + "/" + sig.getArity());
            }
            m.exports = exps;
            m.imports = new ArrayList<String>(new TreeSet<String>(lm.getImportedModules().keySet()));
            m.specs = new HashMap<String, Module.ImportSpec>(lm.getImportSpecs());      // ISS-2025-0735
            // ISS-2025-0784: a concurrent map, so the incremental path can replace one predicate
            // at a time (each Pred is immutable; a reader sees the old one or the new one)
            Map<String, Pred> cls = new java.util.concurrent.ConcurrentHashMap<String, Pred>();
            Map<String, List<Clause>> staging = new java.util.LinkedHashMap<String, List<Clause>>();
            List<Rule> rules = lm.getLocalRules();                                     // ISS-2025-0784
            for (Rule r : rules) {
                Term h = r.getHead();
                String f;
                int ar;
                if (h instanceof Atom) { f = ((Atom) h).getName(); ar = 0; }
                else if (h instanceof CompoundTerm) {
                    f = ((CompoundTerm) h).getName();
                    ar = ((CompoundTerm) h).getArguments().size();
                } else continue;
                String key = f + "/" + ar;
                List<Clause> acc = staging.get(key);
                if (acc == null) { acc = new ArrayList<Clause>(); staging.put(key, acc); }
                Clause c = ClauseStore.compiled(r);
                c.birth = 0;
                c.death = Long.MAX_VALUE;
                acc.add(c);
                mirrorCompiles++;                                                       // ISS-2025-0784
            }
            for (Map.Entry<String, List<Clause>> se : staging.entrySet()) {
                cls.put(se.getKey(), new Pred(se.getValue().toArray(new Clause[se.getValue().size()])));
            }
            m.clauses = cls;
            m.meta = copyMeta(lm);
            m.explicitExports = lm.hasExplicitExportList();                             // ISS-2025-0784
            m.src = lm;                                                                  // ISS-2025-0784
            m.srcVersion = ver;
            m.rulesSeen = rules.size();
            m.loaded = true;
        }
    }

    // START_CHANGE: ISS-2025-0784
    /** Mirror rules appended to a module since the last sync: only their predicates change. */
    /** Clauses the mirror has compiled (test hook). */
    long mirrorCompiles;

    private void appendRules(Mod m, Module lm, List<Rule> added) {
        mirrorCompiles += added.size();
        Map<String, List<Clause>> staging = new java.util.LinkedHashMap<String, List<Clause>>();
        Set<String> newExports = null;
        for (Rule r : added) {
            Term h = r.getHead();
            String f;
            int ar;
            if (h instanceof Atom) { f = ((Atom) h).getName(); ar = 0; }
            else if (h instanceof CompoundTerm) {
                f = ((CompoundTerm) h).getName();
                ar = ((CompoundTerm) h).getArguments().size();
            } else continue;
            String key = f + "/" + ar;
            List<Clause> acc = staging.get(key);
            if (acc == null) { acc = new ArrayList<Clause>(); staging.put(key, acc); }
            Clause c = ClauseStore.compiled(r);
            c.birth = 0;
            c.death = Long.MAX_VALUE;
            acc.add(c);
            // a module without an export list exports what it defines (Module.addRule)
            if (!m.explicitExports && !m.exports.contains(key)) {
                if (newExports == null) newExports = new LinkedHashSet<String>(m.exports);
                newExports.add(key);
            }
        }
        Map<String, Pred> cls = m.clauses;
        if (!(cls instanceof java.util.concurrent.ConcurrentHashMap)) {
            cls = new java.util.concurrent.ConcurrentHashMap<String, Pred>(cls);
        }
        for (Map.Entry<String, List<Clause>> se : staging.entrySet()) {
            Pred old = cls.get(se.getKey());
            List<Clause> add = se.getValue();
            Clause[] all;
            if (old == null) {
                all = add.toArray(new Clause[add.size()]);
            } else {
                all = Arrays.copyOf(old.all, old.all.length + add.size());
                for (int i = 0; i < add.size(); i++) all[old.all.length + i] = add.get(i);
            }
            cls.put(se.getKey(), new Pred(all));
        }
        m.clauses = cls;
        if (newExports != null) m.exports = newExports;
        m.rulesSeen += added.size();
    }
    // END_CHANGE: ISS-2025-0784

    private static Map<String, int[]> copyMeta(Module lm) {
        Map<String, int[]> out = new HashMap<String, int[]>();
        for (Map.Entry<it.denzosoft.jprolog.core.module.PredicateSignature, List<String>> me
                : lm.getMetaPredicateDeclarations().entrySet()) {
            int[] spec = new int[me.getKey().getArity()];
            List<String> raw = me.getValue();
            for (int i = 0; i < spec.length; i++) {
                spec[i] = (i < raw.size()) ? parseMetaSpecAtom(raw.get(i)) : META_PLAIN;
            }
            out.put(me.getKey().getFunctor() + "/" + me.getKey().getArity(), spec);
        }
        return out;
    }

    /** One {@code meta_predicate} argument specifier, as the textual form the manager records. */
    static int parseMetaSpecAtom(String s) {
        if (s == null) return META_PLAIN;
        String t = s.trim();
        if (t.length() == 1 && t.charAt(0) >= '0' && t.charAt(0) <= '9') return t.charAt(0) - '0';
        if (":".equals(t) || "^".equals(t) || "//".equals(t)) return META_COLON;
        return META_PLAIN;
    }

    // ------------------------------------------------------------------ queries

    /** The module a top-level query runs in (the consult path's "current module"). */
    public String currentModule() {
        if (legacy == null || legacy.getCurrentModule() == null) return USER;
        String n = legacy.getCurrentModule().getName();
        return (n == null) ? USER : n;
    }

    /** Every known module name (user-defined, {@code user}, {@code system} and the libraries). */
    public Set<String> names() {
        sync();
        return new LinkedHashSet<String>(mods.keySet());
    }

    /** Is {@code name} a module this engine knows? */
    public boolean isModule(String name) {
        sync();
        return mods.containsKey(name);
    }

    Mod mod(String name) {
        sync();
        return (name == null) ? null : mods.get(name);                      // ISS-2025-0747
    }

    /** Does module {@code m} export {@code f/n}? A module with no explicit export list (the plain
     *  {@code :- module(M)} / mirrored form) exports everything it defines. */
    public boolean exports(String m, String f, int n) {
        Mod mm = mod(m);
        if (mm == null) return false;
        return mm.exports.contains(f + "/" + n);
    }

    /**
     * Clauses {@code f/n} is defined by INSIDE module {@code m}, loading a library module on
     * demand. {@code null} when the module does not define it. {@code user} always answers null:
     * its clauses are the flat store.
     */
    // START_CHANGE: ISS-2025-0540 - wave P2.1: what a module call site caches. Only for a module
    // that is already loaded (a site never triggers a load) and whose own clauses exist.
    /** {@code m}'s own, loaded, non-empty clauses for {@code f/n}; null otherwise. */
    Pred localPred(String m, String f, int n) {
        Mod mm = mod(m);
        if (mm == null || USER.equals(m) || SYSTEM.equals(m)) return null;
        if (mm.isLibrary() && !mm.loaded) return null;
        Pred p = mm.clauses.get(f + "/" + n);
        return (p == null || p.all.length == 0) ? null : p;
    }

    // START_CHANGE: ISS-2025-0732 - the indicators ("name/arity") module m defines itself; a
    // library module is loaded only when {@code loadLibrary} (it was named explicitly).
    public java.util.List<String> localKeys(String m, boolean loadLibrary) {
        Mod mm = mod(m);
        java.util.List<String> out = new java.util.ArrayList<String>();
        if (mm == null || USER.equals(m) || SYSTEM.equals(m)) return out;
        if (mm.isLibrary() && !mm.loaded) {
            if (!loadLibrary) return out;
            load(mm);
        }
        for (Map.Entry<String, Pred> e : mm.clauses.entrySet()) {
            if (e.getValue().all.length > 0) out.add(e.getKey());
        }
        java.util.Collections.sort(out);
        return out;
    }
    // END_CHANGE: ISS-2025-0732

    /** Does module {@code m} (loaded, not user/system) define {@code f/n} itself? */
    public boolean definesLocally(String m, String f, int n) { return localPred(m, f, n) != null; }   // ISS-2025-0731

    /** Changes whenever the mirrored module structure may have (the ModuleManager's stamp). */
    long stamp() { return (legacy == null) ? 0 : legacy.getStamp(); }
    // END_CHANGE: ISS-2025-0540

    Clause[] localClauses(String m, String f, int n) { return localClauses(m, f, n, null); }

    Clause[] localClauses(String m, String f, int n, Object argKey) {
        Mod mm = mod(m);
        if (mm == null || USER.equals(m) || SYSTEM.equals(m)) return null;
        String key = f + "/" + n;
        if (mm.isLibrary() && !mm.loaded) {
            // Cheap negative: never parse a library because some unrelated name was called. Once
            // the module IS loaded the export list stops mattering here — a module sees its own
            // private '$'-prefixed helpers, which is exactly what encapsulation means.
            if (!mm.exports.contains(key)) return null;
            load(mm);
        }
        Pred p = mm.clauses.get(key);
        // START_CHANGE: ISS-2025-0770 - null means "the module does not define it"; a defined
        // predicate whose first-argument index selects no clause answers an EMPTY array (the call
        // fails), never null (which made m:p([]) against p([_|_]) an existence error).
        return (p == null || p.all.length == 0) ? null : p.select(argKey);
        // END_CHANGE: ISS-2025-0770
    }

    /** Resolve {@code f/n} through {@code m}'s imports, in import order, honouring exports. */
    Hit fromImports(String m, String f, int n) { return fromImports(m, f, n, null); }

    Hit fromImports(String m, String f, int n, Object argKey) {
        Mod mm = mod(m);
        if (mm == null || mm.imports.isEmpty()) return null;
        String key = f + "/" + n;
        // START_CHANGE: ISS-2025-0735 - import lists: aliases first, then only/except filters
        if (!mm.specs.isEmpty()) {
            for (Map.Entry<String, Module.ImportSpec> se : mm.specs.entrySet()) {
                String orig = se.getValue().aliases.get(key);
                if (orig == null) continue;
                Mod sm = mods.get(se.getKey());
                if (sm == null) continue;
                if (sm.isLibrary()) load(sm);
                Pred p = sm.clauses.get(orig + "/" + n);
                if (p != null && p.all.length > 0) return new Hit(p.select(argKey), se.getKey());
            }
        }
        // END_CHANGE: ISS-2025-0735
        for (int i = 0; i < mm.imports.size(); i++) {
            String src = mm.imports.get(i);
            Mod sm = mods.get(src);
            if (sm == null || !sm.exports.contains(key)) continue;
            // START_CHANGE: ISS-2025-0735
            Module.ImportSpec spec = mm.specs.isEmpty() ? null : mm.specs.get(src);
            if (spec != null && ((spec.only != null && !spec.only.contains(key)) || spec.except.contains(key))) continue;
            // END_CHANGE: ISS-2025-0735
            if (sm.isLibrary()) load(sm);
            Pred p = sm.clauses.get(key);
            if (p != null && p.all.length > 0) return new Hit(p.select(argKey), src);
        }
        return null;
    }

    /**
     * The autoload step: the library module that exports {@code f/n}, loaded on first reference.
     * Returns null when no library owns the indicator, or when it owns it but implements it as a
     * built-in rather than as prelude clauses (e.g. {@code lists:length/2}).
     */
    Hit autoload(String f, int n) { return autoload(f, n, null); }

    Hit autoload(String f, int n, Object argKey) {
        String owner = Prelude.owner(f, n);
        if (owner == null) return null;
        Mod mm = mod(owner);
        if (mm == null) return null;
        load(mm);
        Pred p = mm.clauses.get(f + "/" + n);
        return (p == null || p.all.length == 0) ? null : new Hit(p.select(argKey), owner);
    }

    /** Cheap, allocation-free test used on the hot goal path: could {@code f/n} be a library
     *  predicate? Exact for "definitely not", and it never triggers a load. */
    public boolean isLibraryIndicator(String f, int n) {
        return Prelude.owner(f, n) != null;
    }

    // START_CHANGE: ISS-2025-0501 - the same test with a key the caller already built
    /** Is {@code key} ({@code "name/arity"}) exported by an autoloadable library? */
    boolean isLibraryIndicatorKey(String key) {
        return Prelude.ownerKey(key) != null;
    }
    // END_CHANGE: ISS-2025-0501

    /** Does an autoloadable library actually define {@code f/n} in Prolog? (An export with no
     *  clauses — {@code lists:length/2} — is implemented as a built-in and answers false.) */
    public boolean hasLibraryClauses(String f, int n) {
        return autoload(f, n) != null;
    }

    /**
     * Must a call to {@code f/n} from context {@code ctx} go to CLAUSES rather than to the legacy
     * built-in registry? True when the context module (or a module it imports) defines it, and
     * when an autoloadable library does.
     *
     * <p>This is the one module test on the hot goal path: EVERY goal that is not inline, not a
     * v4 native and not a control construct pays it, {@code app/3} in nrev included. The answer
     * itself is two string concatenations and up to four map probes ({@code Prelude.owner}, then
     * {@code autoload}'s {@code mod}/{@code load}/{@code clauses.get}), which measured at ~13-16 %
     * of nrev and ~5 % of a fact-table lookup loop — so it is MEMOISED here (ISS-2025-0493).
     *
     * <p>The memo is a small direct-mapped cache of immutable entries stamped with the
     * {@link ModuleManager} modification stamp — the same stamp {@link #sync()} mirrors on. A
     * stamp bump (a module defined, an import added, a clause consulted into a module) makes every
     * stale entry miss; nothing is cleared and nothing is locked, so a worker thread racing on the
     * same {@code Modules} can at worst recompute an entry. A library {@code load()} cannot flip
     * an entry either: {@code autoload} loads the module and only then reads its clauses.
     *
     * @param ctx the context module, {@code null} for {@code user}
     */
    public boolean overridesBuiltin(String ctx, String f, int n) {
        // START_CHANGE: ISS-2025-0493 - memoise the hot-path module test
        long stamp = (legacy == null) ? 0L : legacy.getStamp();
        int i = slot(ctx, f, n);
        Dispatch d = dispatch[i];
        if (d != null && d.stamp == stamp && d.arity == n && d.functor.equals(f) && sameCtx(d.ctx, ctx)) {
            return d.overrides;
        }
        boolean v = computeOverridesBuiltin(ctx, f, n);
        dispatch[i] = new Dispatch(ctx, f, n, v, stamp);
        return v;
        // END_CHANGE: ISS-2025-0493
    }

    private boolean computeOverridesBuiltin(String ctx, String f, int n) {
        if (ctx != null && !USER.equals(ctx)) {
            if (localClauses(ctx, f, n) != null) return true;
            if (fromImports(ctx, f, n) != null) return true;
        }
        return hasLibraryClauses(f, n);
    }

    // START_CHANGE: ISS-2025-0493 - the dispatch memo (see overridesBuiltin)
    /** One memoised decision. Immutable, so publishing the reference publishes the fields. */
    private static final class Dispatch {
        final String ctx;                 // null == user
        final String functor;
        final int arity;
        final boolean overrides;
        final long stamp;
        Dispatch(String ctx, String functor, int arity, boolean overrides, long stamp) {
            this.ctx = ctx; this.functor = functor; this.arity = arity;
            this.overrides = overrides; this.stamp = stamp;
        }
    }

    private static final int DISPATCH_SLOTS = 512;            // power of two
    private final Dispatch[] dispatch = new Dispatch[DISPATCH_SLOTS];

    private static int slot(String ctx, String f, int n) {
        int h = f.hashCode() * 31 + n;                        // String caches its hash
        if (ctx != null) h = h * 31 + ctx.hashCode();
        return h & (DISPATCH_SLOTS - 1);
    }

    private static boolean sameCtx(String a, String b) { return (a == null) ? b == null : a.equals(b); }
    // END_CHANGE: ISS-2025-0493

    /** The {@code meta_predicate} specification of {@code f/n} as declared in module {@code m},
     *  or null. */
    int[] metaSpec(String m, String f, int n) {
        Mod mm = mod(m);
        if (mm == null) return null;
        if (mm.isLibrary()) load(mm);
        return mm.meta.get(f + "/" + n);
    }

    /** Which module a predicate visible from {@code ctx} is defined in, for
     *  {@code predicate_property/2}. Null when it is not a clause predicate. */
    public String definingModule(String ctx, String f, int n) {
        sync();
        String key = f + "/" + n;
        if (!USER.equals(ctx)) {
            if (localClauses(ctx, f, n) != null) return ctx;
            Hit h = fromImports(ctx, f, n);
            if (h != null) return h.module;
        }
        if (engine != null && engine.store().lookup(f, n).size() > 0) return USER;
        if (USER.equals(ctx)) {
            Hit h = fromImports(USER, f, n);
            if (h != null) return h.module;
        }
        String owner = Prelude.owner(f, n);
        if (owner != null) {
            Mod mm = mods.get(owner);
            if (mm != null) { load(mm); if (mm.clauses.containsKey(key)) return owner; }
            return owner;
        }
        return null;
    }

    // ------------------------------------------------------------------ loading

    /** Install a library module's clauses (idempotent, and never fatal). */
    private void load(Mod m) {
        if (m.loaded) return;
        // START_CHANGE: ISS-2025-0747 - one parse per module, published whole: the clauses and
        // meta declarations first, `loaded` last (a failure is still "tried once").
        synchronized (m) {
            if (m.loaded) return;
            try {
                Prelude.Parsed p = Prelude.parse(m.resource);
                if (p == null) return;
                Map<String, Pred> cls = new HashMap<String, Pred>(m.clauses);
                for (Map.Entry<String, List<Rule>> e : p.byIndicator.entrySet()) {
                    List<Rule> rs = e.getValue();
                    Clause[] cs = new Clause[rs.size()];
                    for (int i = 0; i < cs.length; i++) {
                        Clause c = Clause.compile(rs.get(i));
                        c.birth = 0;
                        c.death = Long.MAX_VALUE;
                        cs[i] = c;
                    }
                    if ("apply".equals(m.name)) NativeApply.tag(e.getKey(), cs);   // ISS-2025-0780
                    cls.put(e.getKey(), new Pred(cs));
                }
                Map<String, int[]> mt = new HashMap<String, int[]>(m.meta);
                mt.putAll(p.meta);
                m.clauses = cls;
                m.meta = mt;
            } finally {
                m.loaded = true;
            }
        }
        // END_CHANGE: ISS-2025-0747
    }

    /** Test hook: how many library modules have actually been parsed. */
    public int loadedLibraryCount() {
        sync();
        int n = 0;
        for (Mod m : mods.values()) if (m.isLibrary() && m.loaded) n++;
        return n;
    }

    /** Test hook: is this library module loaded? */
    public boolean isLoaded(String name) {
        Mod m = mod(name);
        return m != null && m.loaded;
    }

    /** Force a library module in (used by {@code use_module(library(X))} and by the tests). */
    public void ensureLoaded(String name) {
        Mod m = mod(name);
        if (m != null && m.isLibrary()) load(m);
    }

    // ------------------------------------------------------------------ meta-argument qualification

    /**
     * The wrapper a meta-argument travels in: {@code '$mctx'(Module, Goal)} — "run Goal with
     * Module as the CONTEXT module".
     *
     * <p>It is deliberately <b>not</b> {@code Module:Goal}. JProlog enforces export visibility on
     * an explicit {@code M:G} call (ISS-2025-0314, and two tests pin it), but a meta-argument
     * travelling back into its own caller must see that module from the INSIDE — {@code maplist}
     * called from {@code m} with a private helper {@code mk/2} has to reach {@code m:mk/2}. The
     * two notions are genuinely different, so they get two functors; {@code '$mctx'/2} is engine
     * internal and a user-written {@code M:G} meta-argument is left exactly as written.
     */
    static final String MCTX = "$mctx";

    /** Is {@code t} a module qualification of either kind? */
    static boolean isQualified(Term t) {
        if (!(t instanceof CompoundTerm)) return false;
        CompoundTerm c = (CompoundTerm) t;
        return c.getArguments().size() == 2 && (":".equals(c.getName()) || MCTX.equals(c.getName()));
    }

    /**
     * SWI's rule: when a predicate declared {@code meta_predicate p(0, +, …)} in module M is
     * called from module {@code caller}, its module-sensitive arguments are qualified with
     * {@code caller} before the head is unified, so the callee's {@code call/N} runs them in the
     * CALLER's context. Returns {@code goal} unchanged when nothing needs qualifying.
     */
    static Term qualifyMetaArgs(Term goal, int[] spec, String caller) {
        if (!(goal instanceof CompoundTerm)) return goal;
        CompoundTerm c = (CompoundTerm) goal;
        List<Term> args = c.getArguments();
        int n = Math.min(args.size(), spec.length);
        List<Term> out = null;
        Atom mod = null;
        for (int i = 0; i < n; i++) {
            if (spec[i] == META_PLAIN) continue;
            Term a = Unify.deref(args.get(i));
            if (isQualified(a)) continue;                     // already qualified
            if (out == null) { out = new ArrayList<Term>(args); mod = new Atom(caller); }
            out.set(i, new CompoundTerm(new Atom(MCTX), Arrays.asList((Term) mod, a)));
        }
        return (out == null) ? goal : new CompoundTerm(c.getFunctor(), out);
    }

    // ------------------------------------------------------------------ directives at consult time

    /** {@code current_module/1} enumeration order: user first, then the rest, sorted. */
    public List<Term> currentModuleTerms() {
        sync();
        List<Term> out = new ArrayList<Term>();
        out.add(new Atom(USER));
        TreeSet<String> rest = new TreeSet<String>(mods.keySet());
        rest.remove(USER);
        for (String s : rest) out.add(new Atom(s));
        return out;
    }
}
// END_CHANGE: ISS-2025-0466
