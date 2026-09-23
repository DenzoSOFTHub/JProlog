package it.denzosoft.jprolog.core.engine;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.logging.Logger;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

// START_CHANGE: ISS-2025-0544 - wave P2.5/P2.7/P2.8: the storage of the database of record was
// rebuilt around ONE structure per predicate.
//
// It used to keep every clause three times: a global `rules` ArrayList (in assert order across
// all predicates), a per-predicate `ruleIndex` ArrayList and a per-predicate, per-first-argument
// `firstArgIndex` ArrayList. Every write paid for all three, and two of them were quadratic:
// `asserta` did `rules.add(0, ...)` on the GLOBAL list (O(N) per call: 100 000 asserta took
// 5-16 s), `retract(Rule)` scanned the global list by identity and then `remove(i)`d from it
// (~48 us per retract in a 200 000-clause base), and `retractall/1` removed clauses one at a time
// from the front of each list (25 000 / 50 000 / 100 000 clauses: 0.17 / 0.61 / 3.5 s — quadratic).
//
// Now each predicate owns a {@link PredEntry}: its write counter, its dynamic flag and a
// {@link RuleSeq} — a gap buffer with tombstones in which assertz and asserta are amortised O(1),
// a retract of the stored Rule object is O(1) (the Rule carries a slot hint) and a bulk removal is
// one pass plus one compaction. The global order, which only getRules() needs (listing, the CLI's
// :save, persistence), is reconstructed from a per-rule sequence number: assertz counts up,
// asserta counts down, which is exactly the order the old global list had. The first-argument
// index of this class is gone: the v4 engine selects through its own index (ClauseStore) on every
// path, including retractall/1 (ISS-2025-0545); the two public index accessors that remain
// (getRulesWithFirstArgIndex, getClauseSnapshot with a first argument) filter the predicate's
// clause list with the same over-approximating key rule, so they can never drop a clause
// (the ISS-2025-0340 hazard).
//
// The PredEntry of a predicate is created once and never removed or replaced, so the v4
// ClauseStore holds a direct reference to it and reads its version as a field (P2.2, ISS-2025-0541)
// instead of building "name/arity" and probing a map on every call.
// END_CHANGE: ISS-2025-0544
public class KnowledgeBase {
    private static final Logger LOGGER = Logger.getLogger(KnowledgeBase.class.getName());

    // START_CHANGE: ISS-2025-0544
    /** Sequence numbers reproducing the old global order: assertz counts up, asserta down. */
    private long hiSeq = 0;
    private long loSeq = 0;

    /**
     * One predicate's clauses and bookkeeping. Created on first reference and never removed, so a
     * reference to it is a stable handle ({@link #entry}). The clause list is guarded by the
     * KnowledgeBase lock; {@link #version()} may be read without it.
     */
    public static final class PredEntry {
        final String key;                                             // "name/arity"
        volatile long version = 0;                                    // bumped on every write
        volatile boolean dynamic = false;                             // ISS-2025-0347
        final RuleSeq rules = new RuleSeq();
        volatile long fullVersion = -1;
        volatile List<Rule> full = Collections.emptyList();

        PredEntry(String key) { this.key = key; }

        /** Write counter of this predicate; bumped by every assert/retract/abolish. */
        public long version() { return version; }
    }

    /**
     * A predicate's clause list: a gap buffer over {@code a[lo..hi)} in which removed clauses are
     * tombstones ({@code null}) until the next compaction. addLast/addFirst are amortised O(1);
     * removing the stored Rule object is O(1) through {@link Rule#kbSlot}, with an identity scan
     * and then an {@code equals} scan as fallbacks (a freshly parsed Rule has no slot).
     */
    static final class RuleSeq {
        private static final Rule[] EMPTY = new Rule[0];
        private Rule[] a = EMPTY;
        private int lo = 0, hi = 0;
        private int live = 0;

        int size() { return live; }

        void addLast(Rule r) {
            if (hi == a.length) regrow();
            a[hi] = r;
            r.kbSlot = hi;
            hi++;
            live++;
        }

        void addFirst(Rule r) {
            if (lo == 0) regrow();
            lo--;
            a[lo] = r;
            r.kbSlot = lo;
            live++;
        }

        /** Rebuild into a new array with room at both ends; drops the tombstones. */
        private void regrow() {
            int cap = Math.max(8, live * 2 + 8);
            Rule[] b = new Rule[cap];
            int at = (cap - live) / 2;
            int nlo = at;
            for (int i = lo; i < hi; i++) {
                Rule r = a[i];
                if (r != null) { b[at] = r; r.kbSlot = at; at++; }
            }
            a = b;
            lo = nlo;
            hi = at;
        }

        /** Remove one occurrence of {@code r} (identity first, then equals); the removed Rule or null. */
        Rule remove(Rule r) {
            int s = r.kbSlot;
            int idx = -1;
            if (s >= lo && s < hi && a[s] == r) {
                idx = s;
            } else {
                for (int i = lo; i < hi; i++) if (a[i] == r) { idx = i; break; }
                if (idx < 0) {
                    for (int i = lo; i < hi; i++) if (a[i] != null && a[i].equals(r)) { idx = i; break; }
                }
            }
            if (idx < 0) return null;
            Rule removed = a[idx];
            removeAt(idx);
            return removed;
        }

        private void removeAt(int idx) {
            Rule removed = a[idx];
            a[idx] = null;
            if (removed.kbSlot == idx) removed.kbSlot = -1;
            live--;
            while (lo < hi && a[lo] == null) lo++;
            while (hi > lo && a[hi - 1] == null) hi--;
            if (live == 0) { lo = hi = a.length / 2; }
            else if (hi - lo > 32 && live * 2 < hi - lo) regrow();
        }

        /** Remove every clause {@code test} accepts in one pass; returns the removed clauses. */
        List<Rule> removeIf(java.util.function.Predicate<Rule> test) {
            List<Rule> out = new ArrayList<Rule>();
            for (int i = lo; i < hi; i++) {
                Rule r = a[i];
                if (r != null && test.test(r)) {
                    a[i] = null;
                    if (r.kbSlot == i) r.kbSlot = -1;
                    live--;
                    out.add(r);
                }
            }
            if (!out.isEmpty()) {
                if (live == 0) { a = EMPTY; lo = hi = 0; } else regrow();
            }
            return out;
        }

        /** The clauses in order, as a fresh list. */
        List<Rule> toList() {
            List<Rule> out = new ArrayList<Rule>(live);
            for (int i = lo; i < hi; i++) if (a[i] != null) out.add(a[i]);
            return out;
        }

        /** The clauses from the LAST to the first (for the scans that historically ran backwards). */
        List<Rule> toReversedList() {
            List<Rule> out = new ArrayList<Rule>(live);
            for (int i = hi - 1; i >= lo; i--) if (a[i] != null) out.add(a[i]);
            return out;
        }

        void clear() {
            for (int i = lo; i < hi; i++) if (a[i] != null && a[i].kbSlot == i) a[i].kbSlot = -1;
            a = EMPTY;
            lo = hi = 0;
            live = 0;
        }
    }

    private final java.util.concurrent.ConcurrentHashMap<String, PredEntry> predEntries =
        new java.util.concurrent.ConcurrentHashMap<>();

    /**
     * The stable handle of {@code functor/arity}, created on first reference. A caller may keep it
     * for the life of this KnowledgeBase and read {@link PredEntry#version()} without a lookup.
     */
    // START_CHANGE: ISS-2025-0571 - a cheap "does this predicate have clauses" by key
    public boolean hasRules(String key) {
        PredEntry e = predEntries.get(key);
        return e != null && e.rules.size() > 0;
    }
    // END_CHANGE: ISS-2025-0571

    public PredEntry entry(String functor, int arity) {
        return entryForKey(functor + "/" + arity);
    }

    private PredEntry entryForKey(String key) {
        PredEntry e = predEntries.get(key);
        if (e == null) e = predEntries.computeIfAbsent(key, PredEntry::new);
        return e;
    }

    /** The entry of a clause head, or null for a non-callable head. */
    private PredEntry entryOfHead(Term head) {
        if (head instanceof Atom) return entryForKey(((Atom) head).getName() + "/0");
        if (head instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) head;
            return entryForKey(c.getName() + "/" + c.arity());
        }
        return null;
    }
    // END_CHANGE: ISS-2025-0544

    // START_CHANGE: ISS-2025-0445 - engine v4 (design B.7): the v4 ClauseStore keeps a COMPILED
    // view of each predicate and must know when something wrote to this KnowledgeBase by any other
    // route (consult, a legacy built-in's assert, the IDE). The per-predicate version counter that
    // already invalidates the snapshot caches is exactly that signal; expose it read-only.
    /** Write counter of {@code functor/arity}; bumped by every assert/retract/abolish. */
    public long getPredicateVersion(String functor, int arity) {
        PredEntry e = predEntries.get(functor + "/" + arity);
        return (e == null) ? 0 : e.version;
    }
    // END_CHANGE: ISS-2025-0445

    // START_CHANGE: ISS-2025-0433 - ENG-13: versioned immutable clause snapshots. A snapshot is
    // built once per version and reused; a published snapshot is never mutated, so it is safe to
    // hold across backtracking.
    /**
     * The immutable clause list of {@code functor/arity} for the current database version — built
     * at most once per version, never copied per call.
     */
    public List<Rule> getClauseSnapshot(String functor, int arity) {
        PredEntry e = predEntries.get(functor + "/" + arity);
        if (e == null) return Collections.emptyList();
        return fullSnapshot(e);
    }

    /** The snapshot of the predicate {@code e} is the handle of (see {@link #entry}). */
    public List<Rule> getClauseSnapshot(PredEntry e) {
        return fullSnapshot(e);
    }

    private List<Rule> fullSnapshot(PredEntry e) {
        long v = e.version;
        if (e.fullVersion == v) return e.full;
        synchronized (this) {
            v = e.version;
            List<Rule> snap = (e.rules.size() == 0)
                ? Collections.<Rule>emptyList()
                : Collections.unmodifiableList(e.rules.toList());
            e.full = snap;
            e.fullVersion = v;
            return snap;
        }
    }

    /**
     * The immutable clause list of {@code functor/arity} restricted, when possible, to the clauses
     * whose head could unify with a goal whose first argument is {@code firstArg}, in source order.
     * An unbound or unindexable {@code firstArg} degrades to the full list — never to a short one
     * (the ISS-2025-0340 hazard).
     */
    public List<Rule> getClauseSnapshot(String functor, int arity, Term firstArg) {
        PredEntry e = predEntries.get(functor + "/" + arity);
        if (e == null) return Collections.emptyList();
        List<Rule> full = fullSnapshot(e);
        if (full.size() < 2) return full;
        return Collections.unmodifiableList(filterByFirstArg(full, firstArg));
    }
    // END_CHANGE: ISS-2025-0433

    // START_CHANGE: ISS-2025-0347 - track dynamic procedures: declared via ':- dynamic' or implied
    // by assert/retractall (ISO 8.9.1: asserting an unknown procedure makes it dynamic). The mark
    // survives retracting every clause, so a retracted-to-empty dynamic predicate FAILS instead of
    // raising existence_error under the 'unknown' flag.
    // ISS-2025-0544: the mark is a field of the predicate's entry (it was a synchronized HashSet of
    // "name/arity" strings consulted on every assert).
    /** Mark {@code functor/arity} as a dynamic procedure. */
    public void markDynamic(String functor, int arity) {
        entry(functor, arity).dynamic = true;
    }

    /** Mark the predicate {@code e} is the handle of as dynamic (no lookup). */
    public void markDynamic(PredEntry e) {
        e.dynamic = true;
    }

    /** True when {@code functor/arity} was declared dynamic or created by assert/retractall. */
    public boolean isDynamic(String functor, int arity) {
        PredEntry e = predEntries.get(functor + "/" + arity);
        return e != null && e.dynamic;
    }
    // END_CHANGE: ISS-2025-0347

    /**
     * Add a rule to the knowledge base.
     *
     * @param rule The rule to add
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public void addRule(Rule rule) {
        Objects.requireNonNull(rule, "Rule cannot be null");
        PredEntry e = entryOfHead(rule.getHead());
        synchronized (this) {
            appendRule(e, rule);
            if (LOGGER.isLoggable(java.util.logging.Level.FINE)) LOGGER.fine("Rule added: " + rule);   // ISS-2025-0524: lazy, no rendering of every clause
        }
    }
    // END_CHANGE: ISS-2025-0164

    // START_CHANGE: ISS-2025-0544
    /** Append {@code rule} to {@code e} (caller holds the lock). A non-callable head is kept in a
     *  pseudo-predicate so getRules() still returns it, as the old global list did. */
    private void appendRule(PredEntry e, Rule rule) {
        if (e == null) e = entryForKey("unknown/0");
        rule.kbSeq = ++hiSeq;
        e.rules.addLast(rule);
        e.version++;
    }

    private void prependRule(PredEntry e, Rule rule) {
        if (e == null) e = entryForKey("unknown/0");
        rule.kbSeq = --loSeq;
        e.rules.addFirst(rule);
        e.version++;
    }

    /**
     * assertz/asserta through a handle the caller already holds (the v4 ClauseStore): no lookup,
     * no string building. Bumps the entry's version by exactly one.
     */
    public void addRule(PredEntry e, Rule rule, boolean front) {
        Objects.requireNonNull(rule, "Rule cannot be null");
        synchronized (this) {
            if (front) prependRule(e, rule); else appendRule(e, rule);
        }
    }

    /** Retract the stored {@code rule} from the predicate {@code e}; O(1) for a stored Rule. */
    public boolean retract(PredEntry e, Rule rule) {
        synchronized (this) {
            Rule removed = e.rules.remove(rule);
            if (removed == null) return false;
            e.version++;
            return true;
        }
    }
    // END_CHANGE: ISS-2025-0544

    /**
     * Add multiple rules to the knowledge base.
     *
     * @param rulesToAdd The rules to add
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public void addRules(List<Rule> rulesToAdd) {
        synchronized (this) {
            if (rulesToAdd != null) {
                for (Rule rule : rulesToAdd) {
                    appendRule(entryOfHead(Objects.requireNonNull(rule, "Rule cannot be null").getHead()), rule);
                }
                if (LOGGER.isLoggable(java.util.logging.Level.FINE)) LOGGER.fine(rulesToAdd.size() + " rules added.");   // ISS-2025-0524: lazy, no rendering of every clause
            }
        }
    }
    // END_CHANGE: ISS-2025-0164

    /**
     * Get all rules in the knowledge base, in the order they were added (asserta'd clauses first).
     *
     * @return An immutable copy of the rules list
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public List<Rule> getRules() {
        synchronized (this) {
            // ISS-2025-0544: the global order is the per-rule sequence number (see the class note).
            List<Rule> all = new ArrayList<Rule>();
            for (PredEntry e : predEntries.values()) {
                if (e.rules.size() > 0) all.addAll(e.rules.toList());
            }
            all.sort((x, y) -> Long.compare(x.kbSeq, y.kbSeq));
            return Collections.unmodifiableList(all);
        }
    }
    // END_CHANGE: ISS-2025-0164

    /**
     * Get rules matching a specific predicate functor and arity.
     *
     * @param functor The predicate functor name
     * @param arity The predicate arity
     * @return An unmodifiable list of matching rules (empty if none found)
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public List<Rule> getRulesForPredicate(String functor, int arity) {
        PredEntry e = predEntries.get(functor + "/" + arity);
        if (e == null) return Collections.emptyList();
        return fullSnapshot(e);                                   // ISS-2025-0544: immutable, shared
    }
    // END_CHANGE: ISS-2025-0164

    // START_CHANGE: ISS-2025-0093 - First-argument indexing for faster clause selection
    /**
     * Get rules matching a specific predicate, filtered by first argument: the clauses whose
     * head's first argument could match {@code firstArg} (same key, or a variable), in source order.
     * A null, unbound or unindexable {@code firstArg} returns every clause of the predicate.
     *
     * <p>ISS-2025-0544: this is a filter over the predicate's clause list now (the separate
     * first-argument index of this class is gone — the v4 engine selects through its own index on
     * every path). Same over-approximating key rule as before, so no clause can ever be dropped.
     */
    public List<Rule> getRulesWithFirstArgIndex(String functor, int arity, Term firstArg) {
        return filterByFirstArg(getRulesForPredicate(functor, arity), firstArg);
    }
    // END_CHANGE: ISS-2025-0093

    // START_CHANGE: ISS-2025-0544
    private static List<Rule> filterByFirstArg(List<Rule> all, Term firstArg) {
        if (firstArg == null || firstArg instanceof Variable || all.isEmpty()) return all;
        String key = getFirstArgKey(firstArg);
        if (VAR_KEY.equals(key)) return all;
        List<Rule> out = new ArrayList<Rule>();
        for (Rule r : all) {
            Term h = getHeadFirstArg(r);
            if (h == null) { out.add(r); continue; }
            String k = getFirstArgKey(h);
            if (VAR_KEY.equals(k) || key.equals(k)) out.add(r);
        }
        return out;
    }
    // END_CHANGE: ISS-2025-0544

    private static final String VAR_KEY = "_VAR";

    /**
     * Get the indexing key for the first argument of a term.
     * Atoms use their name, Numbers use their string value,
     * CompoundTerms use functor/arity, Variables use VAR_KEY.
     */
    private static String getFirstArgKey(Term arg) {
        if (arg instanceof Variable) {
            return VAR_KEY;
        } else if (arg instanceof Atom) {
            return "a:" + ((Atom) arg).getName();
        } else if (arg instanceof it.denzosoft.jprolog.core.terms.Number) {
            // START_CHANGE: ISS-2025-0433 - ENG-13: a TYPE-FAITHFUL numeric key. getValue() returns
            // a double, so 1 and 1.0 shared a bucket (they do not unify — ISS-2025-0261) and every
            // integer beyond 2^53 collided with its neighbours. Integers key on their exact
            // BigInteger value, floats on their double.
            it.denzosoft.jprolog.core.terms.Number num = (it.denzosoft.jprolog.core.terms.Number) arg;
            if (!num.isInteger()) return "f:" + num.doubleValue();
            return num.fitsInLong() ? ("i:" + num.longValue()) : ("i:" + num.bigIntegerValue());
            // END_CHANGE: ISS-2025-0433
        } else if (arg instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) arg;
            return "c:" + ct.getFunctor().getName() + "/" + ct.getArguments().size();
        }
        return VAR_KEY; // unknown term type, treat as variable
    }

    /**
     * Get the first argument of a rule's head, or null if arity 0.
     */
    private static Term getHeadFirstArg(Rule rule) {
        Term head = rule.getHead();
        if (head instanceof CompoundTerm) {
            List<Term> args = ((CompoundTerm) head).getArguments();
            if (args != null && !args.isEmpty()) {
                return args.get(0);
            }
        }
        return null;
    }

    /**
     * Add a rule at the beginning of the knowledge base.
     *
     * @param rule The rule to add
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public void asserta(Rule rule) {
        Objects.requireNonNull(rule, "Rule cannot be null");
        PredEntry e = entryOfHead(rule.getHead());
        synchronized (this) {
            prependRule(e, rule);                                  // ISS-2025-0544: O(1) amortised
            if (LOGGER.isLoggable(java.util.logging.Level.FINE)) LOGGER.fine("Rule asserted at the beginning: " + rule);   // ISS-2025-0524: lazy, no rendering of every clause
        }
    }
    // END_CHANGE: ISS-2025-0164

    /**
     * Remove a rule from the knowledge base.
     *
     * @param rule The rule to remove
     * @return true when a clause was removed, false when {@code rule} is not (or no longer) present
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    // START_CHANGE: ISS-2025-0396 - report whether a clause was actually removed, so the engine's
    // re-executable retract/1 can skip a snapshot clause that was already retracted on a redo.
    public boolean retract(Rule rule) {
    // END_CHANGE: ISS-2025-0396
        // START_CHANGE: ISS-2025-0344 - remove exactly ONE clause (ISO 8.9.3), preferring an
        // identity match (the engine passes the stored Rule object) and falling back to the first
        // equals match (Prolog.retract(String) passes a freshly parsed Rule).
        // ISS-2025-0544: within the rule's own predicate, O(1) for the stored object.
        PredEntry e = entryOfHead(rule.getHead());
        if (e == null) return false;
        boolean removed = retract(e, rule);
        if (LOGGER.isLoggable(java.util.logging.Level.FINE)) {
            LOGGER.fine((removed ? "Rule retracted: " : "Attempted to retract rule but it was not found: ") + rule);
        }
        return removed;
        // END_CHANGE: ISS-2025-0344
    }
    // END_CHANGE: ISS-2025-0164

    /**
     * Add a clause to the beginning of the database.
     *
     * @param clause The clause to add
     */
    // START_CHANGE: ISS-2025-0180 - Core engine bug fixes
    public void addClauseFirst(Clause clause) {
        List<Term> bodyList = clause.getBody() != null ?
            java.util.Arrays.asList(clause.getBody()) :
            Collections.emptyList();
        Rule rule = new Rule(clause.getHead(), bodyList);
        PredEntry e = entryOfHead(rule.getHead());
        synchronized (this) {
            prependRule(e, rule);
            // START_CHANGE: ISS-2025-0347 - asserta implies the procedure is dynamic (ISO 8.9.1)
            if (e != null) e.dynamic = true;
            // END_CHANGE: ISS-2025-0347
            if (LOGGER.isLoggable(java.util.logging.Level.FINE)) LOGGER.fine("Clause added at beginning: " + clause);   // ISS-2025-0524: lazy, no rendering of every clause
        }
    }
    // END_CHANGE: ISS-2025-0180

    /**
     * Add a clause to the end of the database.
     *
     * @param clause The clause to add
     */
    // START_CHANGE: ISS-2025-0180 - Core engine bug fixes
    public void addClauseLast(Clause clause) {
        List<Term> bodyList = clause.getBody() != null ?
            java.util.Arrays.asList(clause.getBody()) :
            Collections.emptyList();
        Rule rule = new Rule(clause.getHead(), bodyList);
        PredEntry e = entryOfHead(rule.getHead());
        synchronized (this) {
            appendRule(e, rule);
            // START_CHANGE: ISS-2025-0347 - assertz implies the procedure is dynamic (ISO 8.9.1)
            if (e != null) e.dynamic = true;
            // END_CHANGE: ISS-2025-0347
            if (LOGGER.isLoggable(java.util.logging.Level.FINE)) LOGGER.fine("Clause added at end: " + clause);   // ISS-2025-0524: lazy, no rendering of every clause
        }
    }
    // END_CHANGE: ISS-2025-0180

    // START_CHANGE: ISS-2025-0544 - the candidates of a legacy retract scan: the pattern's own
    // predicate when the pattern is callable (only its clauses can unify), else every clause in
    // the global order, exactly as the old full scan saw them.
    private List<Rule> scanCandidates(Term headPattern) {
        if (headPattern instanceof Atom || headPattern instanceof CompoundTerm) {
            PredEntry e = entryOfHead(headPattern);
            return e.rules.toList();
        }
        return new ArrayList<Rule>(getRules());
    }

    private void removeStored(Rule rule) {
        PredEntry e = entryOfHead(rule.getHead());
        if (e == null) e = entryForKey("unknown/0");
        if (e.rules.remove(rule) != null) e.version++;
    }
    // END_CHANGE: ISS-2025-0544

    /**
     * Remove clauses that match the given term.
     *
     * @param term The term to match for retraction
     * @return true if any clauses were removed
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public boolean retractClauses(Term term) {
        synchronized (this) {
            // historical behaviour: the scan runs from the LAST clause and removes one match
            List<Rule> cands = scanCandidates(term);
            for (int i = cands.size() - 1; i >= 0; i--) {
                Rule rule = cands.get(i);
                if (unifiable(rule.getHead(), term)) {
                    removeStored(rule);
                    if (LOGGER.isLoggable(java.util.logging.Level.FINE)) LOGGER.fine("Retracted clause: " + rule);   // ISS-2025-0524: lazy, no rendering of every clause
                    return true;
                }
            }
            return false;
        }
    }
    // END_CHANGE: ISS-2025-0164

    // START_CHANGE: ISS-2025-0122 - Retract with unification bindings
    /**
     * Retract first matching clause and return unification bindings.
     * This is needed so retract(counter(N)) properly binds N to the value.
     *
     * @param term The term to match
     * @param bindings Current bindings to extend with unification result
     * @return New bindings map if a clause was retracted, null otherwise
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public java.util.Map<String, it.denzosoft.jprolog.core.terms.Term> retractClauseWithBindings(
            Term term, java.util.Map<String, it.denzosoft.jprolog.core.terms.Term> bindings) {
        synchronized (this) {
            // START_CHANGE: ISS-2025-0251 - Support retract((Head :- Body)) (see notes above).
            Term[] pat = splitClausePattern(term.resolveBindings(bindings));
            Term headPattern = pat[0];
            Term bodyPattern = pat[1];
            for (Rule rule : scanCandidates(headPattern)) {
                Term freshClause = makeClauseTerm(rule).copy();
                Term freshHead = ((CompoundTerm) freshClause).getArguments().get(0);
                Term freshBody = ((CompoundTerm) freshClause).getArguments().get(1);
                java.util.Map<String, it.denzosoft.jprolog.core.terms.Term> newBindings =
                    new java.util.HashMap<>(bindings);
                if (headPattern.unify(freshHead, newBindings)
                        && (bodyPattern == null || bodyPattern.unify(freshBody, newBindings))) {
                    removeStored(rule);
                    if (LOGGER.isLoggable(java.util.logging.Level.FINE)) LOGGER.fine("Retracted clause with bindings: " + rule);   // ISS-2025-0524: lazy, no rendering of every clause
                    return newBindings;
                }
            }
            return null;
            // END_CHANGE: ISS-2025-0251
        }
    }
    // END_CHANGE: ISS-2025-0164
    // END_CHANGE: ISS-2025-0122

    // START_CHANGE: ISS-2025-0164 - Non-deterministic retract/1
    /**
     * Retract ALL matching clauses and return unification bindings for each.
     * This supports non-deterministic retract/1 which should return multiple
     * solutions on backtracking per ISO Prolog.
     *
     * @param term The term to match
     * @param bindings Current bindings to extend with unification result
     * @return List of binding maps, one per retracted clause; empty if none matched
     */
    public java.util.List<java.util.Map<String, it.denzosoft.jprolog.core.terms.Term>> retractAllClausesWithBindings(
            Term term, java.util.Map<String, it.denzosoft.jprolog.core.terms.Term> bindings) {
        synchronized (this) {
            java.util.List<java.util.Map<String, it.denzosoft.jprolog.core.terms.Term>> results =
                new java.util.ArrayList<>();
            // START_CHANGE: ISS-2025-0251 - Support retract((Head :- Body)): the query is split
            // into a head pattern and an optional body pattern, unified against a single fresh
            // copy of the clause (head+body share renamed variables).
            Term[] pat = splitClausePattern(term.resolveBindings(bindings));
            Term headPattern = pat[0];
            Term bodyPattern = pat[1];
            for (Rule rule : scanCandidates(headPattern)) {
                Term freshClause = makeClauseTerm(rule).copy();
                Term freshHead = ((CompoundTerm) freshClause).getArguments().get(0);
                Term freshBody = ((CompoundTerm) freshClause).getArguments().get(1);
                java.util.Map<String, it.denzosoft.jprolog.core.terms.Term> newBindings =
                    new java.util.HashMap<>(bindings);
                boolean matched = headPattern.unify(freshHead, newBindings)
                    && (bodyPattern == null || bodyPattern.unify(freshBody, newBindings));
                if (matched) {
                    removeStored(rule);
                    if (LOGGER.isLoggable(java.util.logging.Level.FINE)) LOGGER.fine("Retracted clause with bindings: " + rule);   // ISS-2025-0524: lazy, no rendering of every clause
                    results.add(newBindings);
                }
            }
            return results;
            // END_CHANGE: ISS-2025-0251
        }
    }
    // END_CHANGE: ISS-2025-0164

    // START_CHANGE: ISS-2025-0251 - Helpers for clause-form retract.
    /**
     * Build a single clause term (Head :- BodyGoal) from a Rule, where BodyGoal is
     * 'true' for a fact, the single goal for a one-goal body, or a right-nested
     * conjunction (G1, (G2, ...)) otherwise. Used so the head and body share renamed
     * variables when the clause is copied.
     */
    private Term makeClauseTerm(Rule rule) {
        java.util.List<Term> body = rule.getBody();
        Term bodyGoal;
        if (body.isEmpty()) {
            bodyGoal = new Atom("true");
        } else {
            bodyGoal = body.get(body.size() - 1);
            for (int j = body.size() - 2; j >= 0; j--) {
                bodyGoal = new CompoundTerm(new Atom(","),
                    java.util.Arrays.asList(body.get(j), bodyGoal));
            }
        }
        return new CompoundTerm(new Atom(":-"), java.util.Arrays.asList(rule.getHead(), bodyGoal));
    }

    /**
     * Split a retract/clause query term into [headPattern, bodyPattern]. For a
     * clause term (Head :- Body) returns {Head, Body}; for a bare head returns
     * {term, null}, where a null body pattern means "match the head only" (legacy
     * behaviour, retracts facts and rules by head).
     */
    private Term[] splitClausePattern(Term term) {
        if (term instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) term;
            if (c.getName().equals(":-") && c.getArguments().size() == 2) {
                return new Term[] { c.getArguments().get(0), c.getArguments().get(1) };
            }
        }
        return new Term[] { term, null };
    }
    // END_CHANGE: ISS-2025-0251

    /**
     * Remove all clauses that match the given term.
     *
     * @param term The term to match for retraction
     * @return Number of clauses removed
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public int retractAllClauses(Term term) {
        // START_CHANGE: ISS-2025-0544 - wave P2.5: ONE pass over the predicate's own clauses and
        // one compaction, instead of removing each match from the front of three lists (quadratic:
        // 100 000 clauses took 3.5 s). A head whose arguments are distinct unbound variables
        // matches every clause, so the per-clause unification (a HashMap each) is skipped for it.
        // (The v4 engine's retractall/1 goes through the ClauseStore's first-argument index
        // instead — ISS-2025-0545 — so this is the Java-API / legacy path.)
        final PredEntry e = entryOfHead(term);
        if (e == null) return 0;
        synchronized (this) {
            // START_CHANGE: ISS-2025-0347 - retractall creates the procedure as dynamic when it
            // does not exist (SWI semantics), so a later call fails instead of raising
            // existence_error under unknown=error.
            e.dynamic = true;
            // END_CHANGE: ISS-2025-0347
            final boolean all = isMostGeneral(term);
            List<Rule> gone = e.rules.removeIf(r -> all || unifiable(r.getHead(), term));
            if (!gone.isEmpty()) e.version++;
            return gone.size();
        }
        // END_CHANGE: ISS-2025-0544
    }
    // END_CHANGE: ISS-2025-0164

    // START_CHANGE: ISS-2025-0544
    /** An atom, or a compound whose arguments are pairwise distinct unbound variables. */
    private static boolean isMostGeneral(Term t) {
        if (t instanceof Atom) return true;
        if (!(t instanceof CompoundTerm)) return false;
        CompoundTerm c = (CompoundTerm) t;
        Set<String> seen = null;                   // by NAME: the unifier used below is name-keyed
        for (int i = 0; i < c.arity(); i++) {
            Term a = c.arg(i);
            if (!(a instanceof Variable) || ((Variable) a).getRef() != null) return false;
            if (c.arity() > 1) {
                if (seen == null) seen = new HashSet<String>();
                if (!seen.add(((Variable) a).getName())) return false;
            }
        }
        return true;
    }
    // END_CHANGE: ISS-2025-0544

    /**
     * Remove all clauses for the given predicate.
     *
     * @param functor The predicate functor
     * @param arity The predicate arity
     * @return Number of clauses removed
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public int abolishPredicate(String functor, int arity) {
        PredEntry e = predEntries.get(functor + "/" + arity);
        if (e == null) return 0;
        synchronized (this) {
            int count = e.rules.size();
            // START_CHANGE: ISS-2025-0610 - P4.16: abolish/1 removes the PREDICATE, not only its
            // clauses: the dynamic declaration goes too, so a later call raises
            // existence_error(procedure, F/A) (ISO 8.9.4, SWI) instead of failing silently.
            boolean wasDynamic = e.dynamic;
            e.dynamic = false;
            if (count > 0 || wasDynamic) {
                e.rules.clear();                                   // ISS-2025-0544
                e.version++;   // ISS-2025-0433 - ENG-13: invalidate cached snapshots
                if (LOGGER.isLoggable(java.util.logging.Level.FINE)) LOGGER.fine("Abolished " + count + " clauses of " + e.key);
            }
            // END_CHANGE: ISS-2025-0610
            return count;
        }
    }
    // END_CHANGE: ISS-2025-0164

    /**
     * Get all predicate indicators in the knowledge base.
     *
     * @return Set of predicate indicators (functor/arity)
     */
    // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
    // START_CHANGE: ISS-2025-0610 - P4.16: the DEFINED predicates for current_predicate/1: the
    // ones with clauses plus the declared dynamic ones without any (SWI).
    public Set<String> getDefinedPredicates() {
        synchronized (this) {
            Set<String> out = new HashSet<>();
            for (PredEntry e : predEntries.values()) {
                if (e.rules.size() > 0 || e.dynamic) out.add(e.key);
            }
            return out;
        }
    }
    // END_CHANGE: ISS-2025-0610

    public Set<String> getCurrentPredicates() {
        // START_CHANGE: ISS-2025-0280 - synchronize like the sibling mutators
        synchronized (this) {
            Set<String> out = new HashSet<>();
            for (PredEntry e : predEntries.values()) {
                if (e.rules.size() > 0) out.add(e.key);            // ISS-2025-0544: non-empty only
            }
            return out;
        }
        // END_CHANGE: ISS-2025-0280
    }
    // END_CHANGE: ISS-2025-0075

    private boolean unifiable(Term term1, Term term2) {
        // Simple unification check - could be more sophisticated
        try {
            java.util.Map<String, Term> bindings = new java.util.HashMap<>();
            return term1.unify(term2, bindings);
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            return false;
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("KnowledgeBase:\n");
        for (Rule rule : getRules()) {
            sb.append("  ").append(rule).append("\n");
        }
        return sb.toString();
    }
}
