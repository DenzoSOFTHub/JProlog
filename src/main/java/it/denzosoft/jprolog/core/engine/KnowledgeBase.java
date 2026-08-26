package it.denzosoft.jprolog.core.engine;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
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

public class KnowledgeBase {
    private static final Logger LOGGER = Logger.getLogger(KnowledgeBase.class.getName());
    private final List<Rule> rules = new ArrayList<>();
    // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
    private final Map<String, List<Rule>> ruleIndex = new HashMap<>();
    // END_CHANGE: ISS-2025-0075
    // START_CHANGE: ISS-2025-0093 - First-argument indexing for faster clause selection
    /** Two-level index: predicate indicator -> first-arg key -> rules */
    private final Map<String, Map<String, List<Rule>>> firstArgIndex = new HashMap<>();
    private static final String VAR_KEY = "_VAR";
    // END_CHANGE: ISS-2025-0093
    // START_CHANGE: LIM-014 - Multi-argument indexing (second argument)
    /** Three-level index: predicate indicator -> arg1 key -> arg2 key -> rules */
    // START_CHANGE: ISS-2025-0436 - ENG-17: the multi-argument index (LIM-014) had no caller outside
    // this class — the legacy solver and the v2 engine both use the first-argument index — while
    // still being built and maintained on EVERY assert/retract, costing a second nested map per
    // clause. Removed together with its unused accessor getRulesWithMultiArgIndex.
    // END_CHANGE: ISS-2025-0436
    // END_CHANGE: LIM-014

    // START_CHANGE: ISS-2025-0433 - ENG-13: versioned immutable clause snapshots + first-arg buckets.
    // getRulesForPredicate used to copy the whole clause list under `synchronized` on EVERY call, so
    // a 20 000-fact table cost a 20 000-element copy per lookup (measured 2.5 ms/call, the same
    // whether the first or the last clause matched — the cost was pure per-call setup). Each
    // predicate now owns a PredEntry holding a version counter (bumped on assert/retract), the
    // immutable full-clause snapshot, and the immutable first-argument bucket snapshots. A snapshot
    // is built once per version and reused; reads take no lock and no copy, and the logical update
    // view comes for free because a published snapshot is never mutated.
    private static final class PredEntry {
        volatile long version = 0;                                    // bumped on every write
        volatile long fullVersion = -1;
        volatile List<Rule> full = Collections.emptyList();
        volatile long bucketVersion = -1;
        final java.util.concurrent.ConcurrentHashMap<String, List<Rule>> buckets =
            new java.util.concurrent.ConcurrentHashMap<>();
    }

    private final java.util.concurrent.ConcurrentHashMap<String, PredEntry> predEntries =
        new java.util.concurrent.ConcurrentHashMap<>();

    /** Invalidate every cached snapshot of {@code predKey} (called from the index writers). */
    private void bumpVersion(String predKey) {
        predEntries.computeIfAbsent(predKey, k -> new PredEntry()).version++;
    }

    /**
     * The immutable clause list of {@code functor/arity} for the current database version — built
     * at most once per version, never copied per call. Safe to hold across backtracking: a later
     * assert/retract publishes a NEW snapshot and leaves this one untouched.
     */
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

    public List<Rule> getClauseSnapshot(String functor, int arity) {
        PredEntry e = predEntries.get(functor + "/" + arity);
        if (e == null) return Collections.emptyList();
        return fullSnapshot(e, functor + "/" + arity);
    }

    private List<Rule> fullSnapshot(PredEntry e, String predKey) {
        long v = e.version;
        if (e.fullVersion == v) return e.full;
        synchronized (this) {
            List<Rule> live = ruleIndex.get(predKey);
            List<Rule> snap = (live == null || live.isEmpty())
                ? Collections.<Rule>emptyList()
                : Collections.unmodifiableList(new ArrayList<>(live));
            e.full = snap;
            e.fullVersion = v;
            return snap;
        }
    }

    /**
     * The immutable clause list of {@code functor/arity} restricted, when possible, to the clauses
     * whose head could unify with a goal whose first argument is {@code firstArg}: those in the
     * matching first-argument bucket plus those whose head has a VARIABLE first argument, in source
     * order. An unbound or unindexable {@code firstArg}, a single-clause predicate, or a predicate
     * with no first-argument index degrades to the full list — never to a short one (the
     * ISS-2025-0340 hazard).
     */
    public List<Rule> getClauseSnapshot(String functor, int arity, Term firstArg) {
        String predKey = functor + "/" + arity;
        PredEntry e = predEntries.get(predKey);
        if (e == null) return Collections.emptyList();
        List<Rule> full = fullSnapshot(e, predKey);
        if (full.size() < 2) return full;                     // nothing to filter
        if (firstArg == null || firstArg instanceof Variable) return full;
        if (!(firstArg instanceof Atom) && !(firstArg instanceof it.denzosoft.jprolog.core.terms.Number)
                && !(firstArg instanceof CompoundTerm)) {
            return full;                                      // e.g. a string: not an indexable key
        }
        long v = e.version;
        if (e.bucketVersion != v) {
            synchronized (this) {
                if (e.bucketVersion != v) { e.buckets.clear(); e.bucketVersion = v; }
            }
        }
        String argKey = getFirstArgKey(firstArg);
        List<Rule> bucket = e.buckets.get(argKey);
        if (bucket != null) return bucket;
        synchronized (this) {
            bucket = buildIndexedSnapshot(predKey, argKey, full);
            // START_CHANGE: ISS-2025-0433 - the bucket cache MUST be bounded. A recursive predicate
            // called with a different integer every time (loop(1000000), loop(999999), ...) produces
            // a distinct key per call, so an unbounded cache grew to one entry per call and turned a
            // deterministic recursion into a memory leak (loop(3000000) went from 5.8 s in 1 GB to
            // an OutOfMemoryError). Past the cap the snapshot is still computed and returned, just
            // not remembered — and the computation is cheap precisely in that case, because a
            // predicate with thousands of distinct first arguments has tiny buckets.
            if (e.buckets.size() < MAX_CACHED_BUCKETS) e.buckets.put(argKey, bucket);
            // END_CHANGE: ISS-2025-0433
            return bucket;
        }
    }

    /** Upper bound on cached first-argument buckets per predicate (see getClauseSnapshot). */
    private static final int MAX_CACHED_BUCKETS = 512;

    private List<Rule> buildIndexedSnapshot(String predKey, String argKey, List<Rule> full) {
        Map<String, List<Rule>> argIndex = firstArgIndex.get(predKey);
        if (argIndex == null) return full;                    // no index -> never drop clauses
        List<Rule> matching = argIndex.get(argKey);
        List<Rule> vars = argIndex.get(VAR_KEY);
        boolean noMatch = (matching == null || matching.isEmpty());
        boolean noVars = (vars == null || vars.isEmpty());
        if (noMatch && noVars) return Collections.emptyList();
        if (noVars) return Collections.unmodifiableList(new ArrayList<>(matching));
        if (noMatch) return Collections.unmodifiableList(new ArrayList<>(vars));
        // both buckets non-empty: merge preserving source order
        Set<Rule> eligible = Collections.newSetFromMap(new java.util.IdentityHashMap<Rule, Boolean>());
        eligible.addAll(matching);
        eligible.addAll(vars);
        List<Rule> result = new ArrayList<>(eligible.size());
        for (Rule r : full) if (eligible.contains(r)) result.add(r);
        return Collections.unmodifiableList(result);
    }
    // END_CHANGE: ISS-2025-0433
    // START_CHANGE: ISS-2025-0347 - track dynamic procedures: declared via ':- dynamic' or implied
    // by assert/retractall (ISO 8.9.1: asserting an unknown procedure makes it dynamic). The mark
    // survives retracting every clause, so a retracted-to-empty dynamic predicate FAILS instead of
    // raising existence_error under the 'unknown' flag.
    private final Set<String> dynamicPredicates = new HashSet<>();

    /** Mark {@code functor/arity} as a dynamic procedure. */
    public void markDynamic(String functor, int arity) {
        synchronized (this) {
            dynamicPredicates.add(functor + "/" + arity);
        }
    }

    /** True when {@code functor/arity} was declared dynamic or created by assert/retractall. */
    public boolean isDynamic(String functor, int arity) {
        synchronized (this) {
            return dynamicPredicates.contains(functor + "/" + arity);
        }
    }
    // END_CHANGE: ISS-2025-0347

    /**
     * Add a rule to the knowledge base.
     *
     * @param rule The rule to add
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public void addRule(Rule rule) {
        synchronized (this) {
            rules.add(Objects.requireNonNull(rule, "Rule cannot be null"));
            // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
            addToIndex(rule);
            // END_CHANGE: ISS-2025-0075
            LOGGER.fine("Rule added: " + rule);
        }
    }
    // END_CHANGE: ISS-2025-0164

    /**
     * Add multiple rules to the knowledge base.
     *
     * @param rulesToAdd The rules to add
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public void addRules(List<Rule> rulesToAdd) {
        synchronized (this) {
            if (rulesToAdd != null) {
                rules.addAll(rulesToAdd);
                // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
                for (Rule rule : rulesToAdd) {
                    addToIndex(rule);
                }
                // END_CHANGE: ISS-2025-0075
                LOGGER.fine(rulesToAdd.size() + " rules added.");
            }
        }
    }
    // END_CHANGE: ISS-2025-0164

    /**
     * Get all rules in the knowledge base.
     *
     * @return An immutable copy of the rules list
     */
    // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public List<Rule> getRules() {
        synchronized (this) {
            return Collections.unmodifiableList(new ArrayList<>(rules));
        }
    }
    // END_CHANGE: ISS-2025-0164

    /**
     * Get rules matching a specific predicate functor and arity.
     * Uses the functor/arity index for O(1) lookup instead of scanning all rules.
     *
     * @param functor The predicate functor name
     * @param arity The predicate arity
     * @return An unmodifiable list of matching rules (empty if none found)
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public List<Rule> getRulesForPredicate(String functor, int arity) {
        synchronized (this) {
            String key = functor + "/" + arity;
            List<Rule> indexed = ruleIndex.get(key);
            if (indexed == null) {
                return Collections.emptyList();
            }
            return Collections.unmodifiableList(new ArrayList<>(indexed));
        }
    }
    // END_CHANGE: ISS-2025-0164
    // END_CHANGE: ISS-2025-0075

    // START_CHANGE: ISS-2025-0093 - First-argument indexing for faster clause selection
    /**
     * Get rules matching a specific predicate, filtered by first argument.
     * If the first argument of the query is ground (atom/number/ground compound),
     * returns only rules whose head's first argument matches or is a variable.
     * This dramatically reduces unification attempts for large predicate tables.
     *
     * @param functor The predicate functor name
     * @param arity The predicate arity
     * @param firstArg The resolved first argument of the query (null to skip filtering)
     * @return List of matching rules
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public List<Rule> getRulesWithFirstArgIndex(String functor, int arity, Term firstArg) {
        synchronized (this) {
            String predKey = functor + "/" + arity;
            Map<String, List<Rule>> argIndex = firstArgIndex.get(predKey);
            if (argIndex == null) {
                // START_CHANGE: ISS-2025-0344 - an index miss must degrade to the full predicate
                // list, never silently drop clauses that exist in ruleIndex (ISS-2025-0340 hazard)
                return getRulesForPredicate(functor, arity);
                // END_CHANGE: ISS-2025-0344
            }

            // If first arg is null, variable, or non-indexable, return all rules for this predicate
            if (firstArg == null || firstArg instanceof Variable) {
                List<Rule> all = ruleIndex.get(predKey);
                return all != null ? Collections.unmodifiableList(new ArrayList<>(all)) : Collections.emptyList();
            }

            String argKey = getFirstArgKey(firstArg);
            List<Rule> matchingRules = argIndex.get(argKey);
            List<Rule> varRules = argIndex.get(VAR_KEY);

            if (matchingRules == null && varRules == null) {
                return Collections.emptyList();
            }
            if (matchingRules == null) {
                return Collections.unmodifiableList(new ArrayList<>(varRules));
            }
            if (varRules == null) {
                return Collections.unmodifiableList(new ArrayList<>(matchingRules));
            }

            // Merge matching + variable rules, preserving original order
            // We need to return them in the order they appear in ruleIndex
            List<Rule> allForPred = ruleIndex.get(predKey);
            if (allForPred == null) return Collections.emptyList();

            // Build a set of eligible rules for fast lookup
            Set<Rule> eligible = new HashSet<>(matchingRules.size() + varRules.size());
            eligible.addAll(matchingRules);
            eligible.addAll(varRules);

            List<Rule> result = new ArrayList<>(eligible.size());
            for (Rule r : allForPred) {
                if (eligible.contains(r)) {
                    result.add(r);
                }
            }
            return result;
        }
    }
    // END_CHANGE: ISS-2025-0164

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

    private void addToFirstArgIndex(Rule rule) {
        String predKey = getPredicateIndicator(rule.getHead());
        Map<String, List<Rule>> argIndex = firstArgIndex.computeIfAbsent(predKey, k -> new HashMap<>());
        Term firstArg = getHeadFirstArg(rule);
        String argKey = (firstArg != null) ? getFirstArgKey(firstArg) : VAR_KEY;
        argIndex.computeIfAbsent(argKey, k -> new ArrayList<>()).add(rule);
    }

    private void addToFirstArgIndexFirst(Rule rule) {
        String predKey = getPredicateIndicator(rule.getHead());
        Map<String, List<Rule>> argIndex = firstArgIndex.computeIfAbsent(predKey, k -> new HashMap<>());
        Term firstArg = getHeadFirstArg(rule);
        String argKey = (firstArg != null) ? getFirstArgKey(firstArg) : VAR_KEY;
        argIndex.computeIfAbsent(argKey, k -> new ArrayList<>()).add(0, rule);
    }

    // ISS-2025-0436 - ENG-17: multi-argument index helpers removed (no callers).

    private void removeFromFirstArgIndex(Rule rule) {
        String predKey = getPredicateIndicator(rule.getHead());
        Map<String, List<Rule>> argIndex = firstArgIndex.get(predKey);
        if (argIndex != null) {
            Term firstArg = getHeadFirstArg(rule);
            String argKey = (firstArg != null) ? getFirstArgKey(firstArg) : VAR_KEY;
            List<Rule> list = argIndex.get(argKey);
            if (list != null) {
                // START_CHANGE: ISS-2025-0344 - identity-preferring removal
                removeOneOccurrence(list, rule);
                // END_CHANGE: ISS-2025-0344
                if (list.isEmpty()) {
                    argIndex.remove(argKey);
                }
            }
            if (argIndex.isEmpty()) {
                firstArgIndex.remove(predKey);
            }
        }
        // ISS-2025-0436 - ENG-17: multi-argument index removal dropped with the index itself
    }
    // END_CHANGE: ISS-2025-0093

    /**
     * Add a rule at the beginning of the knowledge base.
     *
     * @param rule The rule to add
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public void asserta(Rule rule) {
        synchronized (this) {
            rules.add(0, Objects.requireNonNull(rule, "Rule cannot be null"));
            // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
            addToIndexFirst(rule);
            // END_CHANGE: ISS-2025-0075
            LOGGER.fine("Rule asserted at the beginning: " + rule);
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
        synchronized (this) {
            // START_CHANGE: ISS-2025-0344 - remove exactly ONE clause (ISO 8.9.3) and keep the
            // rules list and ruleIndex/firstArgIndex in sync. The old removeIf(equals) dropped
            // EVERY duplicate clause from `rules` while removeFromIndex removed only one index
            // entry, leaving immortal phantom clauses visible to the engine but not to listing.
            // Prefer an identity match (the engine passes the stored Rule object); fall back to
            // the first equals match (Prolog.retract(String) passes a freshly parsed Rule).
            int idx = -1;
            for (int i = 0; i < rules.size(); i++) {
                if (rules.get(i) == rule) { idx = i; break; }
            }
            if (idx < 0) {
                for (int i = 0; i < rules.size(); i++) {
                    if (rules.get(i).equals(rule)) { idx = i; break; }
                }
            }
            if (idx >= 0) {
                Rule removed = rules.remove(idx);
                removeFromIndex(removed);
                LOGGER.fine("Rule retracted: " + removed);
                // START_CHANGE: ISS-2025-0396 - signal removal to the caller
                return true;
                // END_CHANGE: ISS-2025-0396
            } else {
                LOGGER.fine("Attempted to retract rule but it was not found: " + rule);
                // START_CHANGE: ISS-2025-0396 - signal the clause was not found
                return false;
                // END_CHANGE: ISS-2025-0396
            }
            // END_CHANGE: ISS-2025-0344
        }
    }
    // END_CHANGE: ISS-2025-0164

    /**
     * Add a clause to the beginning of the database.
     *
     * @param clause The clause to add
     */
    // START_CHANGE: ISS-2025-0180 - Core engine bug fixes
    public void addClauseFirst(Clause clause) {
        synchronized (this) {
            List<Term> bodyList = clause.getBody() != null ?
                java.util.Arrays.asList(clause.getBody()) :
                Collections.emptyList();
            Rule rule = new Rule(clause.getHead(), bodyList);
            rules.add(0, rule);
            // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
            addToIndexFirst(rule);
            // END_CHANGE: ISS-2025-0075
            // START_CHANGE: ISS-2025-0347 - asserta implies the procedure is dynamic (ISO 8.9.1)
            dynamicPredicates.add(getPredicateIndicator(rule.getHead()));
            // END_CHANGE: ISS-2025-0347
            LOGGER.fine("Clause added at beginning: " + clause);
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
        synchronized (this) {
            List<Term> bodyList = clause.getBody() != null ?
                java.util.Arrays.asList(clause.getBody()) :
                Collections.emptyList();
            Rule rule = new Rule(clause.getHead(), bodyList);
            rules.add(rule);
            // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
            addToIndex(rule);
            // END_CHANGE: ISS-2025-0075
            // START_CHANGE: ISS-2025-0347 - assertz implies the procedure is dynamic (ISO 8.9.1)
            dynamicPredicates.add(getPredicateIndicator(rule.getHead()));
            // END_CHANGE: ISS-2025-0347
            LOGGER.fine("Clause added at end: " + clause);
        }
    }
    // END_CHANGE: ISS-2025-0180

    /**
     * Remove clauses that match the given term.
     *
     * @param term The term to match for retraction
     * @return true if any clauses were removed
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public boolean retractClauses(Term term) {
        synchronized (this) {
            boolean removed = false;
            for (int i = rules.size() - 1; i >= 0; i--) {
                Rule rule = rules.get(i);
                if (unifiable(rule.getHead(), term)) {
                    rules.remove(i);
                    // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
                    removeFromIndex(rule);
                    // END_CHANGE: ISS-2025-0075
                    removed = true;
                    LOGGER.fine("Retracted clause: " + rule);
                    break; // Only remove first match
                }
            }
            return removed;
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
            for (int i = 0; i < rules.size(); i++) {
                Rule rule = rules.get(i);
                Term freshClause = makeClauseTerm(rule).copy();
                Term freshHead = ((CompoundTerm) freshClause).getArguments().get(0);
                Term freshBody = ((CompoundTerm) freshClause).getArguments().get(1);
                java.util.Map<String, it.denzosoft.jprolog.core.terms.Term> newBindings =
                    new java.util.HashMap<>(bindings);
                if (headPattern.unify(freshHead, newBindings)
                        && (bodyPattern == null || bodyPattern.unify(freshBody, newBindings))) {
                    rules.remove(i);
                    removeFromIndex(rule);
                    LOGGER.fine("Retracted clause with bindings: " + rule);
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
            // START_CHANGE: ISS-2025-0251 - Support retract((Head :- Body)). Previously only the
            // rule HEAD was unified against the whole query term, so the clause form (H:-B) never
            // matched a stored rule. Now we split the query into a head pattern and an optional
            // body pattern and unify both against a single fresh copy of the clause (head+body
            // share renamed variables).
            Term[] pat = splitClausePattern(term.resolveBindings(bindings));
            Term headPattern = pat[0];
            Term bodyPattern = pat[1];
            for (int i = 0; i < rules.size(); ) {
                Rule rule = rules.get(i);
                Term freshClause = makeClauseTerm(rule).copy();
                Term freshHead = ((CompoundTerm) freshClause).getArguments().get(0);
                Term freshBody = ((CompoundTerm) freshClause).getArguments().get(1);
                java.util.Map<String, it.denzosoft.jprolog.core.terms.Term> newBindings =
                    new java.util.HashMap<>(bindings);
                boolean matched = headPattern.unify(freshHead, newBindings)
                    && (bodyPattern == null || bodyPattern.unify(freshBody, newBindings));
                if (matched) {
                    rules.remove(i);
                    removeFromIndex(rule);
                    LOGGER.fine("Retracted clause with bindings: " + rule);
                    results.add(newBindings);
                } else {
                    i++;
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
        synchronized (this) {
            // START_CHANGE: ISS-2025-0347 - retractall creates the procedure as dynamic when it
            // does not exist (SWI semantics), so a later call fails instead of raising
            // existence_error under unknown=error.
            dynamicPredicates.add(getPredicateIndicator(term));
            // END_CHANGE: ISS-2025-0347
            int count = 0;
            for (int i = rules.size() - 1; i >= 0; i--) {
                Rule rule = rules.get(i);
                if (unifiable(rule.getHead(), term)) {
                    rules.remove(i);
                    // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
                    removeFromIndex(rule);
                    // END_CHANGE: ISS-2025-0075
                    count++;
                    LOGGER.fine("Retracted clause: " + rule);
                }
            }
            return count;
        }
    }
    // END_CHANGE: ISS-2025-0164

    /**
     * Remove all clauses for the given predicate.
     *
     * @param functor The predicate functor
     * @param arity The predicate arity
     * @return Number of clauses removed
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public int abolishPredicate(String functor, int arity) {
        synchronized (this) {
            int count = 0;
            for (int i = rules.size() - 1; i >= 0; i--) {
                Rule rule = rules.get(i);
                Term head = rule.getHead();

                if (matchesPredicate(head, functor, arity)) {
                    rules.remove(i);
                    count++;
                    LOGGER.fine("Abolished clause: " + rule);
                }
            }
            // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
            if (count > 0) {
                String key = functor + "/" + arity;
                ruleIndex.remove(key);
                // START_CHANGE: ISS-2025-0093 - Clear first-argument index on abolish
                firstArgIndex.remove(key);
                // END_CHANGE: ISS-2025-0093
                bumpVersion(key);   // ISS-2025-0433 - ENG-13: invalidate cached snapshots
            }
            // END_CHANGE: ISS-2025-0075
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
    public Set<String> getCurrentPredicates() {
        // START_CHANGE: ISS-2025-0280 - synchronize like the sibling mutators; iterating the
        // ruleIndex keySet while another thread asserts/retracts can corrupt or throw.
        synchronized (this) {
            return new HashSet<>(ruleIndex.keySet());
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

    private boolean matchesPredicate(Term term, String functor, int arity) {
        if (term instanceof Atom) {
            return ((Atom) term).getName().equals(functor) && arity == 0;
        } else if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            return compound.getFunctor().getName().equals(functor) &&
                   compound.getArguments().size() == arity;
        }
        return false;
    }

    private String getPredicateIndicator(Term term) {
        if (term instanceof Atom) {
            return ((Atom) term).getName() + "/0";
        } else if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            return compound.getFunctor().getName() + "/" + compound.getArguments().size();
        }
        return "unknown/0";
    }

    // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
    /**
     * Add a rule to the end of the index list for its predicate indicator.
     */
    private void addToIndex(Rule rule) {
        String key = getPredicateIndicator(rule.getHead());
        bumpVersion(key);                     // ISS-2025-0433 - ENG-13
        ruleIndex.computeIfAbsent(key, k -> new ArrayList<>()).add(rule);
        // START_CHANGE: ISS-2025-0093 - Maintain first-argument index
        addToFirstArgIndex(rule);
        // END_CHANGE: ISS-2025-0093
    }

    /**
     * Add a rule to the beginning of the index list for its predicate indicator.
     */
    private void addToIndexFirst(Rule rule) {
        String key = getPredicateIndicator(rule.getHead());
        bumpVersion(key);                     // ISS-2025-0433 - ENG-13
        ruleIndex.computeIfAbsent(key, k -> new ArrayList<>()).add(0, rule);
        // START_CHANGE: ISS-2025-0093 - Maintain first-argument index
        addToFirstArgIndexFirst(rule);
        // END_CHANGE: ISS-2025-0093
    }

    /**
     * Remove a rule from the index list for its predicate indicator.
     */
    private void removeFromIndex(Rule rule) {
        String key = getPredicateIndicator(rule.getHead());
        bumpVersion(key);                     // ISS-2025-0433 - ENG-13
        List<Rule> indexed = ruleIndex.get(key);
        if (indexed != null) {
            // START_CHANGE: ISS-2025-0344 - identity-preferring removal (see removeOneOccurrence)
            removeOneOccurrence(indexed, rule);
            // END_CHANGE: ISS-2025-0344
            if (indexed.isEmpty()) {
                ruleIndex.remove(key);
            }
        }
        // START_CHANGE: ISS-2025-0093 - Maintain first-argument index
        removeFromFirstArgIndex(rule);
        // END_CHANGE: ISS-2025-0093
    }
    // END_CHANGE: ISS-2025-0075

    // START_CHANGE: ISS-2025-0344 - remove exactly one occurrence, preferring object identity
    /**
     * Remove exactly one occurrence of {@code rule} from {@code list}. The add paths store the
     * same Rule object in {@code rules} and every index, so an identity match removes precisely
     * the retracted clause even when duplicate clauses compare equal; the equals fallback keeps
     * externally constructed (parsed) rules working.
     */
    private static boolean removeOneOccurrence(List<Rule> list, Rule rule) {
        for (int i = 0; i < list.size(); i++) {
            if (list.get(i) == rule) {
                list.remove(i);
                return true;
            }
        }
        return list.remove(rule);
    }
    // END_CHANGE: ISS-2025-0344

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("KnowledgeBase:\n");
        for (Rule rule : rules) {
            sb.append("  ").append(rule).append("\n");
        }
        return sb.toString();
    }
}
