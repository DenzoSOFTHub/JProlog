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
    // START_CHANGE: ISS-2025-0541 - wave P2.1/P2.2: predicates are found by NAME, then by arity in
    // a small copy-on-write array, so a lookup builds no "name/arity" string; and a Predicate,
    // once created, is never removed or replaced — which is what lets a call site keep a direct
    // reference to it (Machine.CallSite).
    private final ConcurrentHashMap<String, Predicate[]> preds = new ConcurrentHashMap<String, Predicate[]>();
    // END_CHANGE: ISS-2025-0541
    // START_CHANGE: ISS-2025-0525 - an atomic counter: `++generation` on a volatile field is a
    // read-modify-write, and two threads asserting into different predicates could draw the same
    // birth generation.
    private final java.util.concurrent.atomic.AtomicLong generation = new java.util.concurrent.atomic.AtomicLong(1);
    private final ArrayList<Predicate> compactionQueue = new ArrayList<Predicate>();

    public ClauseStore(KnowledgeBase kb) { this.kb = kb; }

    public KnowledgeBase knowledgeBase() { return kb; }

    /** The current database generation; a call captures this and never sees a later write. */
    public long generation() { return generation.get(); }

    private long nextGeneration() { return generation.incrementAndGet(); }
    // END_CHANGE: ISS-2025-0525

    static String key(String name, int arity) { return name + "/" + arity; }

    /** The predicate entry for {@code name/arity}, synchronised with the KnowledgeBase. */
    public Predicate lookup(String name, int arity) {
        Predicate p = find(name, arity);
        if (p == null) p = create(name, arity);
        p.sync(kb);
        return p;
    }

    // START_CHANGE: ISS-2025-0541
    private Predicate find(String name, int arity) {
        Predicate[] a = preds.get(name);
        return (a != null && arity < a.length) ? a[arity] : null;
    }

    private synchronized Predicate create(String name, int arity) {
        Predicate p = find(name, arity);
        if (p != null) return p;
        p = new Predicate(name, arity, key(name, arity), kb.entry(name, arity));
        Predicate[] a = preds.get(name);
        Predicate[] b = new Predicate[Math.max(arity + 1, a == null ? 0 : a.length)];
        if (a != null) System.arraycopy(a, 0, b, 0, a.length);
        b[arity] = p;
        preds.put(name, b);
        return p;
    }
    // END_CHANGE: ISS-2025-0541

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
            c.ord = i;
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
        else { name = ((CompoundTerm) h).getName(); arity = ((CompoundTerm) h).arity(); }
        Predicate p = lookup(name, arity);
        KnowledgeBase.PredEntry e = p.kbEntry;                      // ISS-2025-0541: no lookup
        kb.markDynamic(e);
        Clause c = compiled(rule);
        // START_CHANGE: ISS-2025-0525 - wave P1.12: the KnowledgeBase write and the store insert
        // are ONE step under the predicate lock. They used to be two: the KB add happened outside
        // the lock, so a concurrent lookup() on another thread could see the KB version move,
        // re-sync the predicate from the KB snapshot (which already held the new clause) — and then
        // this thread inserted the clause AGAIN. 4 threads x 300 asserts left 1201-1202 clauses.
        // sync() takes the same lock, so it now either runs before the KB write or sees the insert.
        // The version bookkeeping is exact: the store adopts the KB's new version only when the KB
        // moved by exactly this one write since the store was last in step; a write that came in by
        // another route (consult, a legacy built-in) in between makes the next lookup re-sync.
        synchronized (p) {
            long before = e.version();
            kb.addRule(e, rule, front);                              // ISS-2025-0544: O(1)
            long after = e.version();
            c.birth = nextGeneration();
            c.death = Long.MAX_VALUE;
            p.insert(c, front);
            p.kbVersion = (before == p.kbVersion && after == before + 1) ? after : -1;
        }
        // END_CHANGE: ISS-2025-0525
        return c;
    }

    // START_CHANGE: ISS-2025-0449 - dead clauses must be reclaimed DURING a query, not only at the
    // query boundary. Compaction installs a NEW array and never touches any clause's birth/death,
    // so a call that already captured (array, from, to, generation) keeps a perfectly valid
    // snapshot: it still walks the old array, and `isAlive(gen)` still answers from the clause
    // objects themselves. The logical update view is therefore preserved whenever we compact.
    /** How dead a predicate has to get before a retract compacts it in place. */
    private static final int COMPACT_MIN_DEAD = 32;

    /** Retract one clause: remove it from the KnowledgeBase and give it a death generation. */
    public boolean retractClause(Predicate p, Clause c) {
        boolean compactNow;
        // START_CHANGE: ISS-2025-0525 - wave P1.12: test-and-retract under the predicate lock.
        synchronized (p) {
            if (c.death != Long.MAX_VALUE) return false;              // already retracted
            KnowledgeBase.PredEntry e = p.kbEntry;
            long before = e.version();
            if (!kb.retract(e, c.rule)) return false;                 // ISS-2025-0544: O(1)
            long after = e.version();
            c.death = nextGeneration();
            p.kbVersion = (before == p.kbVersion && after == before + 1) ? after : -1;
            // END_CHANGE: ISS-2025-0525
            p.noteDeath(c);                                           // ISS-2025-0546
            // Amortised O(1): compact once the dead clauses outnumber the live ones (and there are
            // enough of them to be worth an array rebuild).
            compactNow = p.deadCount >= COMPACT_MIN_DEAD && p.deadCount * 2 >= p.all.size();
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
        Predicate p = find(name, arity);
        if (p != null) synchronized (p) { p.kbVersion = -1; }
    }

    /** Forget every predicate (used when the whole KnowledgeBase is cleared). */
    public void invalidateAll() {
        for (Predicate[] a : preds.values()) {
            for (Predicate p : a) if (p != null) synchronized (p) { p.kbVersion = -1; }
        }
    }

    // START_CHANGE: ISS-2025-0466 - the flat library layer added by ISS-2025-0454 has MOVED to
    // {@link Modules}: this store is exactly what design B.10 says it is: the clauses of `user`.
    // END_CHANGE: ISS-2025-0466

    // ==================================================================
    // START_CHANGE: ISS-2025-0546 - wave P2.6/P2.7/P2.9: the clause lists are gap buffers.
    /**
     * A captured clause window: {@code a[from..to)}. Valid for the whole life of a call: nothing
     * ever writes into a slot inside a window once it has been handed out (see {@link Seq}).
     * One instance is reused by its owner (a Machine), which copies the three fields out at once.
     */
    public static final class View {
        public Clause[] a;
        public int from;
        public int to;
        void set(Clause[] a, int from, int to) { this.a = a; this.from = from; this.to = to; }
        public int size() { return to - from; }
    }

    /**
     * An ordered clause list with room at both ends. The logical update view needs no copying:
     * assertz writes into the slot just PAST {@code end} and asserta into the slot just BEFORE
     * {@code start}, and a window handed out is always inside {@code [start, end)} at the time it
     * is taken — so no write can ever land inside a captured window. When there is no room, a NEW
     * array is installed; the old one stays valid for whoever captured it.
     *
     * <p>{@code from} ({@code start <= from <= end}) skips a prefix of clauses that are dead for
     * every call that can still start: a retract of the first live clause advances it, which is
     * what keeps `retract(p(_))` in a loop linear (it used to rescan — or, through the old
     * {@code all()}, re-copy — every dead clause in front on every call).
     */
    static final class Seq {
        private static final Clause[] NONE = new Clause[0];
        Clause[] a = NONE;
        int start, from, end;
        /** Inserts so far (a merged bucket view is valid while this has not moved). */
        long writes;
        // merged-with-variable-headed cache (buckets only)
        Clause[] merged;
        long mergedWrites = -1, mergedVarWrites = -1;
        Seq mergedVarSeq;

        int size() { return end - from; }

        void addLast(Clause c) {
            if (end == a.length) regrow();
            a[end++] = c;
            writes++;
        }

        void addFirst(Clause c) {
            if (start == 0) regrow();
            a[--start] = c;
            from = start;
            writes++;
        }

        /** New array, room at both ends; the dead prefix before {@code from} is dropped. */
        private void regrow() {
            int n = end - from;
            int cap = Math.max(8, n * 2 + 8);
            Clause[] b = new Clause[cap];
            int ns = (cap - n) / 2;
            System.arraycopy(a, from, b, ns, n);
            a = b;
            start = from = ns;
            end = ns + n;
        }

        /** Advance {@code from} past clauses that are already dead; returns how many it skipped. */
        int skipDeadHead() {
            int k = 0;
            while (from < end && a[from].death != Long.MAX_VALUE) { from++; k++; }
            return k;
        }
    }
    // END_CHANGE: ISS-2025-0546

    /** One predicate: its clauses in source order, plus the first-argument index. */
    public static final class Predicate {

        final String name;
        final int arity;
        final String key;
        /** ISS-2025-0541 (P2.2): the KnowledgeBase's own entry for this predicate — a stable handle,
         *  so sync() reads a volatile field instead of building "name/arity" and probing a map. */
        final KnowledgeBase.PredEntry kbEntry;

        /** Clauses in source order (dead ones included until compaction). */
        final Seq all = new Seq();
        /** Version of the KnowledgeBase predicate this view was built from. Volatile: sync()
         *  reads it without the lock on every lookup (ISS-2025-0525). */
        volatile long kbVersion = -1;
        /** Dead clauses inside {@code all[from, end)}. */
        int deadCount = 0;
        boolean compactionQueued = false;
        /** Bumped on every write; invalidates the cached {@link #all()} copy. */
        long writeStamp = 0;
        /** ISS-2025-0547: the next ordinals for assertz / asserta. */
        private long hiOrd = -1, loOrd = 0;

        // first-argument index (design B.7): incremental, no bucket cap.
        private final Map<Object, Seq> byKey = new HashMap<Object, Seq>();
        private Seq varHeaded = new Seq();
        private Clause[] allSel;
        private long allStamp = -1;

        private static final Clause[] EMPTY = new Clause[0];

        Predicate(String name, int arity, String key, KnowledgeBase.PredEntry kbEntry) {
            this.name = name;
            this.arity = arity;
            this.key = key;
            this.kbEntry = kbEntry;
        }

        public String getName() { return name; }
        public int getArity() { return arity; }

        /** Rebuild from the KnowledgeBase when something wrote to it outside the v4 engine. */
        void sync(KnowledgeBase kb) {
            long v = kbEntry.version();                                 // ISS-2025-0541
            if (v == kbVersion) return;
            synchronized (this) {
                v = kbEntry.version();
                if (v == kbVersion) return;
                List<Rule> snap = kb.getClauseSnapshot(kbEntry);
                Seq s = new Seq();
                for (int i = 0; i < snap.size(); i++) {
                    Clause c = compiled(snap.get(i));
                    c.birth = 0;
                    c.death = Long.MAX_VALUE;
                    c.ord = i;
                    s.addLast(c);
                }
                installAll(s);
                hiOrd = snap.size() - 1;
                loOrd = 0;
                kbVersion = v;
            }
        }

        /** Install {@code s} as the clause list and rebuild the index from it. */
        private void installAll(Seq s) {
            // copy the fields into the final holder (the Seq object itself stays the same one)
            all.a = s.a; all.start = s.start; all.from = s.from; all.end = s.end;
            all.writes++;
            deadCount = 0;
            for (int i = all.from; i < all.end; i++) if (all.a[i].death != Long.MAX_VALUE) deadCount++;
            byKey.clear();
            varHeaded = new Seq();
            for (int i = all.from; i < all.end; i++) addToIndex(all.a[i], false);
            writeStamp++;
        }

        private void addToIndex(Clause c, boolean front) {
            Object k = c.firstArgKey;
            Seq s;
            if (k == null) {
                s = varHeaded;
            } else {
                s = byKey.get(k);
                if (s == null) { s = new Seq(); byKey.put(k, s); }
            }
            if (front) s.addFirst(c); else s.addLast(c);
        }

        /**
         * Insert one clause. Both ends are amortised O(1) (ISS-2025-0546): {@code asserta} used to
         * allocate and copy the whole array on every call (100 000 asserta: 5-16 s).
         */
        void insert(Clause c, boolean front) {
            if (front) { c.ord = --loOrd; all.addFirst(c); }
            else { c.ord = ++hiOrd; all.addLast(c); }
            addToIndex(c, front);
            writeStamp++;
        }

        /** Bookkeeping after {@code c} got its death generation (caller holds the lock). */
        void noteDeath(Clause c) {
            deadCount++;
            deadCount -= all.skipDeadHead();
            Object k = c.firstArgKey;
            if (k == null) {
                varHeaded.skipDeadHead();
            } else {
                Seq b = byKey.get(k);
                if (b != null) {
                    b.skipDeadHead();
                    if (b.size() == 0) byKey.remove(k);                 // bounded: counter loops
                }
            }
            writeStamp++;
        }

        void compact() {
            if (deadCount == 0 && all.from == all.start) return;
            Seq s = new Seq();
            for (int i = all.from; i < all.end; i++) {
                Clause c = all.a[i];
                if (c.death == Long.MAX_VALUE) s.addLast(c);             // ordinals are kept
            }
            installAll(s);
        }

        /** True when the predicate has at least one clause alive at {@code gen}. */
        public synchronized boolean hasClauses(long gen) {
            for (int i = all.from; i < all.end; i++) if (all.a[i].isAlive(gen)) return true;
            return false;
        }

        /** Every clause in source order, as an exact-length copy (dead ones included until
         *  compaction). Cached per write; the engine's own paths use {@link #view} instead. */
        public synchronized Clause[] all() {
            if (allStamp != writeStamp) {
                Clause[] a = new Clause[all.end - all.from];
                System.arraycopy(all.a, all.from, a, 0, a.length);
                allSel = a;
                allStamp = writeStamp;
            }
            return allSel;
        }

        /**
         * The clauses whose head could match a goal with first-argument key {@code k}, as an
         * exact-length array: see {@link #view}. {@code null} returns the full list.
         */
        public Clause[] select(Object k) {
            View v = new View();
            view(k, v);
            if (v.from == 0 && v.to == v.a.length) return v.a;
            Clause[] out = new Clause[v.to - v.from];
            System.arraycopy(v.a, v.from, out, 0, out.length);
            return out;
        }

        /**
         * Capture the candidate window for a goal with first-argument key {@code k} into
         * {@code out}: the matching bucket merged, in source order, with the variable-headed
         * clauses. {@code null} key (an unbound or unindexable argument) captures the full list —
         * an index miss must never drop a clause (the ISS-2025-0340 hazard).
         *
         * <p>START_CHANGE: ISS-2025-0546/0547 - no copy is made: a bucket and the full list are
         * handed out as windows over their gap buffers. Only a bucket that must be merged with
         * variable-headed clauses builds an array, by ORDINAL (O(|bucket| + |var-headed|)), and
         * the merge is cached until one of the two lists gets a new clause — a write elsewhere in
         * the predicate no longer invalidates it. The old merge rescanned the whole predicate after
         * every write, so asserting into a predicate with a variable-headed clause while calling
         * it was quadratic (3e4: 5.5 s). END_CHANGE: ISS-2025-0546/0547
         */
        public synchronized void view(Object k, View out) {
            if (k == null) { out.set(all.a, all.from, all.end); return; }
            Seq b = byKey.get(k);
            Seq v = varHeaded;
            if (v.size() == 0) {
                if (b == null) out.set(EMPTY, 0, 0); else out.set(b.a, b.from, b.end);
                return;
            }
            if (b == null || b.size() == 0) { out.set(v.a, v.from, v.end); return; }
            if (b.merged == null || b.mergedWrites != b.writes || b.mergedVarSeq != v
                    || b.mergedVarWrites != v.writes) {
                Clause[] m = new Clause[b.size() + v.size()];
                int i = b.from, j = v.from, n = 0;
                while (i < b.end && j < v.end) {
                    m[n++] = (b.a[i].ord < v.a[j].ord) ? b.a[i++] : v.a[j++];
                }
                while (i < b.end) m[n++] = b.a[i++];
                while (j < v.end) m[n++] = v.a[j++];
                mergedSlots += m.length;                               // test hook (ISS-2025-0547)
                b.merged = m;
                b.mergedWrites = b.writes;
                b.mergedVarSeq = v;
                b.mergedVarWrites = v.writes;
            }
            out.set(b.merged, 0, b.merged.length);
        }

        /** Number of stored clauses, dead ones included until they are compacted — including the
         *  dead prefix a window skips, so "the predicate has clauses" means what it always meant. */
        public synchronized int size() { return all.end - all.start; }

        /** Test hook: total length of every merged bucket view built so far (ISS-2025-0547). */
        long mergedSlots;

        /** Test hook: number of cached first-argument buckets. */
        public synchronized int bucketCount() { return byKey.size(); }
    }
}
// END_CHANGE: ISS-2025-0445
