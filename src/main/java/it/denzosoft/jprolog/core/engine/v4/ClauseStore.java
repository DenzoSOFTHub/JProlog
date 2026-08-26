package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.KnowledgeBase;
import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

// START_CHANGE: ISS-2025-0445 - engine v4 wave W2, design B.7 (clause store with generations).
/**
 * The v4 clause store: one {@link Predicate} per {@code name/arity}, holding the compiled
 * {@link Clause} skeletons, their birth/death generations and an incremental first-argument index.
 *
 * <h3>Logical update view without snapshots</h3>
 * The v3.8.0 engine gave every call an immutable {@code List<Rule>} snapshot rebuilt once per
 * database version — correct, but O(#clauses) per write on a large dynamic predicate. Here every
 * clause carries {@code birth} and {@code death} generation numbers and a call captures
 * {@code (array, from, to, generation)}: {@code assertz} appends (amortised O(1)), {@code asserta}
 * prepends, {@code retract} sets {@code death} (O(1)). Nothing is rebuilt, and a running call keeps
 * seeing exactly the clauses that existed when it started. Dead clauses are compacted away as soon
 * as they outnumber the live ones (ISS-2025-0449), plus a sweep at each query boundary: compaction
 * installs a new array and never touches birth/death, so a call that already captured
 * {@code (array, size, generation)} is unaffected.
 *
 * <h3>Relationship with {@link KnowledgeBase}</h3>
 * {@code KnowledgeBase} remains the database of record: {@code consult}, {@code listing}, the IDE
 * and the ~400 legacy built-ins all read and write it, and {@code Rule.sourceLine} (hence
 * {@code Prolog.getPredicateIndicatorAtLine} and the IDE's line breakpoints) lives there. This
 * store is the v4 engine's compiled view of it: writes made through v4 update both, in step; a
 * write made by any other route bumps the KnowledgeBase's per-predicate version and this store
 * re-syncs that one predicate on its next lookup. Compiled skeletons are cached on the {@link Rule}
 * itself, so a re-sync is a pointer copy, not a recompilation.
 */
public final class ClauseStore {

    private final KnowledgeBase kb;
    private final ConcurrentHashMap<String, Predicate> preds = new ConcurrentHashMap<String, Predicate>();
    private volatile long generation = 1;
    private final ArrayList<Predicate> compactionQueue = new ArrayList<Predicate>();

    public ClauseStore(KnowledgeBase kb) { this.kb = kb; }

    public KnowledgeBase knowledgeBase() { return kb; }

    /** The current database generation; a call captures this and never sees a later write. */
    public long generation() { return generation; }

    private long nextGeneration() { return ++generation; }

    static String key(String name, int arity) { return name + "/" + arity; }

    /** The predicate entry for {@code name/arity}, synchronised with the KnowledgeBase. */
    public Predicate lookup(String name, int arity) {
        String k = key(name, arity);
        Predicate p = preds.get(k);
        if (p == null) {
            p = new Predicate(name, arity, k);
            Predicate prev = preds.putIfAbsent(k, p);
            if (prev != null) p = prev;
        }
        p.sync(kb);
        return p;
    }

    /** Compile (or reuse the compilation of) one {@link Rule}. */
    public static Clause compiled(Rule r) {
        Object c = r.getCompiled();
        if (c instanceof Clause) return (Clause) c;
        Clause cl = Clause.compile(r);
        r.setCompiled(cl);
        return cl;
    }

    /** Compile a rule list coming from the module manager (no predicate entry involved). */
    public static Clause[] compiledAll(List<Rule> rules) {
        Clause[] out = new Clause[rules.size()];
        for (int i = 0; i < out.length; i++) {
            Clause c = compiled(rules.get(i));
            c.birth = 0;
            c.death = Long.MAX_VALUE;
            out[i] = c;
        }
        return out;
    }

    // ------------------------------------------------------------------ writes

    /** Add {@code rule} to the KnowledgeBase and to this store, in one generation step. */
    public Clause assertRule(Rule rule, boolean front) {
        Term h = rule.getHead();
        String name;
        int arity;
        if (h instanceof Atom) { name = ((Atom) h).getName(); arity = 0; }
        else { name = ((CompoundTerm) h).getName(); arity = ((CompoundTerm) h).getArguments().size(); }
        Predicate p = lookup(name, arity);
        kb.markDynamic(name, arity);
        if (front) kb.asserta(rule); else kb.addRule(rule);
        Clause c = compiled(rule);
        c.birth = nextGeneration();
        c.death = Long.MAX_VALUE;
        synchronized (p) {
            p.insert(c, front);
            p.kbVersion = kb.getPredicateVersion(name, arity);        // stay in sync: no rebuild
        }
        return c;
    }

    // START_CHANGE: ISS-2025-0449 - dead clauses must be reclaimed DURING a query, not only at the
    // query boundary. `cnt(N) :- retract(counter(C)), ..., assertz(counter(C1)), ...` grows the
    // predicate's array by one dead clause per iteration, and both the retract candidate scan
    // (Predicate.all()) and the clause-iterator scan are O(#clauses) — so the loop was quadratic:
    // cnt(100000) took 60 s on v4 against 5.2 s on v2.
    //
    // Waiting for the query boundary turned out to be unnecessarily conservative. Compaction
    // installs a NEW array and never touches any clause's birth/death, so a call that already
    // captured (array, size, generation) keeps a perfectly valid snapshot: it still walks the old
    // array, and `isAlive(gen)` still answers from the clause objects themselves. Nothing a running
    // call can observe changes. The logical update view is therefore preserved whenever we compact.
    /** How dead a predicate has to get before a retract compacts it in place. */
    private static final int COMPACT_MIN_DEAD = 32;

    /** Retract one clause: remove it from the KnowledgeBase and give it a death generation. */
    public boolean retractClause(Predicate p, Clause c) {
        if (c.death != Long.MAX_VALUE) return false;                  // already retracted
        if (!kb.retract(c.rule)) return false;
        boolean compactNow;
        synchronized (p) {
            c.death = nextGeneration();
            p.deadCount++;
            p.kbVersion = kb.getPredicateVersion(p.name, p.arity);
            p.writeStamp++;
            // Amortised O(1): compact once the dead clauses outnumber the live ones (and there are
            // enough of them to be worth an array rebuild), so a retract/assert loop over a
            // one-clause predicate rebuilds a ~33-entry array every 32 iterations.
            compactNow = p.deadCount >= COMPACT_MIN_DEAD && p.deadCount * 2 >= p.size();
            if (compactNow) p.compact();
        }
        if (!compactNow) enqueueCompaction(p);
        return true;
    }
    // END_CHANGE: ISS-2025-0449

    private void enqueueCompaction(Predicate p) {
        synchronized (compactionQueue) {
            if (!p.compactionQueued) { p.compactionQueued = true; compactionQueue.add(p); }
        }
    }

    /**
     * Physically drop the dead clauses of every predicate that still has some. Called at a query
     * boundary as a sweep for predicates that never crossed the in-query threshold of
     * {@link #retractClause}; safe at any time (see ISS-2025-0449 above).
     */
    public void compact() {
        ArrayList<Predicate> todo;
        synchronized (compactionQueue) {
            if (compactionQueue.isEmpty()) return;
            todo = new ArrayList<Predicate>(compactionQueue);
            compactionQueue.clear();
        }
        for (int i = 0; i < todo.size(); i++) {
            Predicate p = todo.get(i);
            synchronized (p) { p.compactionQueued = false; p.compact(); }
        }
    }

    /** Forget everything (an {@code abolish} or a KB reset): the next lookup re-syncs. */
    public void invalidate(String name, int arity) {
        Predicate p = preds.get(key(name, arity));
        if (p != null) synchronized (p) { p.kbVersion = -1; }
    }

    /** Forget every predicate (used when the whole KnowledgeBase is cleared). */
    public void invalidateAll() {
        for (Predicate p : preds.values()) synchronized (p) { p.kbVersion = -1; }
    }

    // START_CHANGE: ISS-2025-0466 - the flat library layer added by ISS-2025-0454 has MOVED to
    // {@link Modules}: with wave W6 a prelude predicate belongs to a library module
    // (`lists`, `apply`, `pairs`, `coroutining`) that is autoloaded by indicator, so a single
    // engine-wide `indicator -> clauses` map no longer expresses what the resolver needs. This
    // store is now exactly what design B.10 says it is: the clauses of module `user`.
    // END_CHANGE: ISS-2025-0466

    // ==================================================================
    /** One predicate: its clauses in source order, plus the first-argument index. */
    public static final class Predicate {

        final String name;
        final int arity;
        final String key;

        /** Clauses in source order (dead ones included until compaction). */
        private Clause[] array = EMPTY;
        private int count = 0;
        /** Version of the KnowledgeBase predicate this view was built from. */
        long kbVersion = -1;
        int deadCount = 0;
        boolean compactionQueued = false;
        /** Bumped on every write; invalidates the cached selection arrays. */
        long writeStamp = 0;

        // first-argument index (design B.7): incremental, no bucket cap.
        private final Map<Object, Bucket> byKey = new HashMap<Object, Bucket>();
        private final ArrayList<Clause> varHeaded = new ArrayList<Clause>();
        private Clause[] varSel;
        private long varStamp = -1;
        private Clause[] allSel;
        private long allStamp = -1;

        private static final Clause[] EMPTY = new Clause[0];

        private static final class Bucket {
            final ArrayList<Clause> list = new ArrayList<Clause>();
            Clause[] sel;                 // bucket merged with the variable-headed clauses
            long stamp = -1;
        }

        Predicate(String name, int arity, String key) {
            this.name = name;
            this.arity = arity;
            this.key = key;
        }

        public String getName() { return name; }
        public int getArity() { return arity; }

        /** Rebuild from the KnowledgeBase when something wrote to it outside the v4 engine. */
        void sync(KnowledgeBase kb) {
            long v = kb.getPredicateVersion(name, arity);
            if (v == kbVersion) return;
            synchronized (this) {
                if (v == kbVersion) return;
                List<Rule> snap = kb.getClauseSnapshot(name, arity);
                Clause[] arr = new Clause[snap.size()];
                for (int i = 0; i < arr.length; i++) {
                    Clause c = compiled(snap.get(i));
                    c.birth = 0;
                    c.death = Long.MAX_VALUE;
                    arr[i] = c;
                }
                array = arr;
                count = arr.length;
                deadCount = 0;
                kbVersion = v;
                rebuildIndex();
            }
        }

        private void rebuildIndex() {
            byKey.clear();
            varHeaded.clear();
            for (int i = 0; i < count; i++) addToIndex(array[i], false);
            writeStamp++;
        }

        private void addToIndex(Clause c, boolean front) {
            Object k = c.firstArgKey;
            if (k == null) {
                if (front) varHeaded.add(0, c); else varHeaded.add(c);
            } else {
                Bucket b = byKey.get(k);
                if (b == null) { b = new Bucket(); byKey.put(k, b); }
                if (front) b.list.add(0, c); else b.list.add(c);
            }
        }

        /**
         * Insert one clause. {@code assertz} appends IN PLACE (into spare capacity, or into a
         * freshly grown array) — a running call that captured {@code (array, limit)} cannot see the
         * new slot, because it stops at its own limit, so no snapshot has to be rebuilt.
         * {@code asserta} always allocates a new array: shifting in place would move clauses under
         * the feet of exactly those running calls.
         */
        void insert(Clause c, boolean front) {
            if (front) {
                Clause[] shifted = new Clause[count + 1];
                System.arraycopy(array, 0, shifted, 1, count);
                shifted[0] = c;
                array = shifted;
            } else {
                if (count == array.length) {
                    Clause[] bigger = new Clause[Math.max(8, array.length << 1)];
                    System.arraycopy(array, 0, bigger, 0, count);
                    array = bigger;
                }
                array[count] = c;
            }
            count++;
            addToIndex(c, front);
            writeStamp++;
        }

        void compact() {
            if (deadCount == 0) return;
            Clause[] live = new Clause[count - deadCount < 0 ? count : count - deadCount];
            int n = 0;
            for (int i = 0; i < count; i++) {
                Clause c = array[i];
                if (c.death == Long.MAX_VALUE) {
                    if (n == live.length) {                            // defensive: recount
                        Clause[] bigger = new Clause[n + 8];
                        System.arraycopy(live, 0, bigger, 0, n);
                        live = bigger;
                    }
                    live[n++] = c;
                }
            }
            array = live;
            count = n;
            deadCount = 0;
            rebuildIndex();
        }

        /** True when the predicate has at least one clause alive at {@code gen}. */
        public boolean hasClauses(long gen) {
            for (int i = 0; i < count; i++) if (array[i].isAlive(gen)) return true;
            return false;
        }

        /**
         * The backing clause array, valid up to {@link #size()}. Callers MUST capture both together
         * and never read past the captured size: everything beyond it may be overwritten by a later
         * {@code assertz}. Every operation that would disturb the first {@code size} entries
         * ({@code asserta}, compaction, a re-sync) installs a new array instead, so a captured
         * (array, size) pair stays a valid snapshot for the whole life of a call — a logical update
         * view with no copying at all.
         */
        public synchronized Clause[] rawArray() { return array; }

        /** Every clause in source order, as an exact-length copy (for callers that cannot carry a
         *  separate limit, e.g. {@code retract/1}'s candidate list). */
        public synchronized Clause[] all() {
            if (allStamp != writeStamp) {
                Clause[] a = new Clause[count];
                System.arraycopy(array, 0, a, 0, count);
                allSel = a;
                allStamp = writeStamp;
            }
            return allSel;
        }

        /**
         * The clauses whose head could match a goal with first-argument key {@code k}: the matching
         * bucket merged, in source order, with the variable-headed clauses. {@code null} key (an
         * unbound or unindexable argument) returns the full list — an index miss must never drop a
         * clause (the ISS-2025-0340 hazard).
         *
         * <p>The merged array is cached per key. The cache is bounded by construction: it is
         * populated only for keys that actually occur in a clause head, so a recursive predicate
         * called with a different integer every time ({@code loop(1000000)}, {@code loop(999999)},
         * ...) adds nothing to it — that unbounded growth is exactly what forced the 512-entry cap
         * on the v3.8.0 KnowledgeBase bucket cache.
         */
        public synchronized Clause[] select(Object k) {
            if (k == null) return all();
            Bucket b = byKey.get(k);
            if (varHeaded.isEmpty()) {
                if (b == null) return EMPTY;
                if (b.stamp != writeStamp) {
                    b.sel = b.list.toArray(new Clause[b.list.size()]);
                    b.stamp = writeStamp;
                }
                return b.sel;
            }
            if (b == null) {
                if (varStamp != writeStamp) {
                    varSel = varHeaded.toArray(new Clause[varHeaded.size()]);
                    varStamp = writeStamp;
                }
                return varSel;
            }
            if (b.stamp != writeStamp) {
                // merge bucket + variable-headed clauses preserving source order
                java.util.IdentityHashMap<Clause, Boolean> eligible =
                    new java.util.IdentityHashMap<Clause, Boolean>();
                for (int i = 0; i < b.list.size(); i++) eligible.put(b.list.get(i), Boolean.TRUE);
                for (int i = 0; i < varHeaded.size(); i++) eligible.put(varHeaded.get(i), Boolean.TRUE);
                Clause[] merged = new Clause[eligible.size()];
                int n = 0;
                for (int i = 0; i < count; i++) {
                    if (eligible.containsKey(array[i])) merged[n++] = array[i];
                }
                if (n != merged.length) {
                    Clause[] exact = new Clause[n];
                    System.arraycopy(merged, 0, exact, 0, n);
                    merged = exact;
                }
                b.sel = merged;
                b.stamp = writeStamp;
            }
            return b.sel;
        }

        /** Number of stored clauses, dead ones included (the limit for {@link #rawArray()}). */
        public synchronized int size() { return count; }

        /** Test hook: number of cached first-argument buckets. */
        public synchronized int bucketCount() { return byKey.size(); }
    }
}
// END_CHANGE: ISS-2025-0445
