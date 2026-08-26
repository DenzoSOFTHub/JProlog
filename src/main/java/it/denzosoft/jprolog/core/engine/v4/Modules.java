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

    /** One module. */
    static final class Mod {
        final String name;
        /** Classpath resource for a library module; null for a user-defined one. */
        final String resource;
        boolean loaded;
        boolean explicitExports;
        final Set<String> exports = new LinkedHashSet<String>();
        final List<String> imports = new ArrayList<String>();
        final Map<String, Pred> clauses = new HashMap<String, Pred>();
        final Map<String, int[]> meta = new HashMap<String, int[]>();

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
        private final Map<Object, Clause[]> byKey;
        private final Clause[] varHeaded;

        Pred(Clause[] all) {
            this.all = all;
            Map<Object, List<Clause>> groups = new java.util.LinkedHashMap<Object, List<Clause>>();
            List<Clause> vars = new ArrayList<Clause>();
            for (int i = 0; i < all.length; i++) {
                Object k = all[i].firstArgKey;
                if (k == null) vars.add(all[i]);
                else {
                    List<Clause> g = groups.get(k);
                    if (g == null) { g = new ArrayList<Clause>(); groups.put(k, g); }
                    g.add(all[i]);
                }
            }
            this.varHeaded = vars.toArray(new Clause[vars.size()]);
            if (groups.isEmpty()) {
                this.byKey = java.util.Collections.emptyMap();
            } else {
                Map<Object, Clause[]> m = new HashMap<Object, Clause[]>();
                for (Map.Entry<Object, List<Clause>> e : groups.entrySet()) {
                    // merge the bucket with the variable-headed clauses, in source order
                    java.util.IdentityHashMap<Clause, Boolean> ok = new java.util.IdentityHashMap<Clause, Boolean>();
                    for (int i = 0; i < e.getValue().size(); i++) ok.put(e.getValue().get(i), Boolean.TRUE);
                    for (int i = 0; i < varHeaded.length; i++) ok.put(varHeaded[i], Boolean.TRUE);
                    List<Clause> merged = new ArrayList<Clause>(ok.size());
                    for (int i = 0; i < all.length; i++) if (ok.containsKey(all[i])) merged.add(all[i]);
                    m.put(e.getKey(), merged.toArray(new Clause[merged.size()]));
                }
                this.byKey = m;
            }
        }

        /** The clauses whose head could match a goal with first-argument key {@code k}. */
        Clause[] select(Object k) {
            if (k == null || byKey.isEmpty()) return all;
            Clause[] s = byKey.get(k);
            return (s != null) ? s : varHeaded;      // no head has that key: only the variable ones
        }
    }
    // END_CHANGE: ISS-2025-0468

    private final Engine engine;
    private final ModuleManager legacy;
    private final Map<String, Mod> mods = new HashMap<String, Mod>();
    private long mirrorStamp = -1;

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
        mirrorStamp = s;
        for (Map.Entry<String, Module> e : legacy.getAllModules().entrySet()) {
            String name = e.getKey();
            if (USER.equals(name) || SYSTEM.equals(name)) {
                // `user` is the flat store; we still take its import list and meta declarations.
                Mod um = mods.get(USER);
                um.imports.clear();
                um.imports.addAll(new TreeSet<String>(e.getValue().getImportedModules().keySet()));
                um.meta.clear();
                copyMeta(e.getValue(), um);
                continue;
            }
            Mod m = mods.get(name);
            if (m == null || m.isLibrary()) {
                if (m == null) { m = new Mod(name, null); mods.put(name, m); }
                else continue;                      // a user module may not shadow a library one
            }
            Module lm = e.getValue();
            m.exports.clear();
            for (it.denzosoft.jprolog.core.module.PredicateSignature sig : lm.getExportedPredicates()) {
                m.exports.add(sig.getFunctor() + "/" + sig.getArity());
            }
            m.imports.clear();
            m.imports.addAll(new TreeSet<String>(lm.getImportedModules().keySet()));
            m.clauses.clear();
            Map<String, List<Clause>> staging = new java.util.LinkedHashMap<String, List<Clause>>();
            for (Rule r : lm.getLocalRules()) {
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
            }
            for (Map.Entry<String, List<Clause>> se : staging.entrySet()) {
                m.clauses.put(se.getKey(), new Pred(se.getValue().toArray(new Clause[se.getValue().size()])));
            }
            m.meta.clear();
            copyMeta(lm, m);
            m.loaded = true;
        }
    }

    private static void copyMeta(Module lm, Mod m) {
        for (Map.Entry<it.denzosoft.jprolog.core.module.PredicateSignature, List<String>> me
                : lm.getMetaPredicateDeclarations().entrySet()) {
            int[] spec = new int[me.getKey().getArity()];
            List<String> raw = me.getValue();
            for (int i = 0; i < spec.length; i++) {
                spec[i] = (i < raw.size()) ? parseMetaSpecAtom(raw.get(i)) : META_PLAIN;
            }
            m.meta.put(me.getKey().getFunctor() + "/" + me.getKey().getArity(), spec);
        }
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
        return mods.get(name);
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
        return (p == null) ? null : p.select(argKey);
    }

    /** Resolve {@code f/n} through {@code m}'s imports, in import order, honouring exports. */
    Hit fromImports(String m, String f, int n) { return fromImports(m, f, n, null); }

    Hit fromImports(String m, String f, int n, Object argKey) {
        Mod mm = mod(m);
        if (mm == null || mm.imports.isEmpty()) return null;
        String key = f + "/" + n;
        for (int i = 0; i < mm.imports.size(); i++) {
            String src = mm.imports.get(i);
            Mod sm = mods.get(src);
            if (sm == null || !sm.exports.contains(key)) continue;
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

    /** Does an autoloadable library actually define {@code f/n} in Prolog? (An export with no
     *  clauses — {@code lists:length/2} — is implemented as a built-in and answers false.) */
    public boolean hasLibraryClauses(String f, int n) {
        return autoload(f, n) != null;
    }

    /**
     * Must a call to {@code f/n} from context {@code ctx} go to CLAUSES rather than to the legacy
     * built-in registry? True when the context module (or a module it imports) defines it, and
     * when an autoloadable library does. This is the one module test on the hot goal path, and for
     * the {@code user} context it degenerates to a single {@code HashMap} probe of the prelude
     * index — no load, no allocation.
     *
     * @param ctx the context module, {@code null} for {@code user}
     */
    public boolean overridesBuiltin(String ctx, String f, int n) {
        if (ctx != null && !USER.equals(ctx)) {
            if (localClauses(ctx, f, n) != null) return true;
            if (fromImports(ctx, f, n) != null) return true;
        }
        return hasLibraryClauses(f, n);
    }

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
        m.loaded = true;                                  // even a failure is "tried once"
        Prelude.Parsed p = Prelude.parse(m.resource);
        if (p == null) return;
        for (Map.Entry<String, List<Rule>> e : p.byIndicator.entrySet()) {
            List<Rule> rs = e.getValue();
            Clause[] cs = new Clause[rs.size()];
            for (int i = 0; i < cs.length; i++) {
                Clause c = Clause.compile(rs.get(i));
                c.birth = 0;
                c.death = Long.MAX_VALUE;
                cs[i] = c;
            }
            m.clauses.put(e.getKey(), new Pred(cs));
        }
        m.meta.putAll(p.meta);
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
