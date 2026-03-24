package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.core.terms.*;

import java.util.*;
import java.util.logging.Logger;

// START_CHANGE: ISS-2025-0092 - Tabling (memoization) support
/**
 * Stores tabled predicate declarations and cached solutions.
 * Variant tabling with canonical variable normalization.
 */
public class TableStore {

    // START_CHANGE: ISS-2025-0173 - Add cache size limit and eviction to prevent unbounded growth
    private static final Logger LOGGER = Logger.getLogger(TableStore.class.getName());
    private static final int MAX_CACHE_SIZE = 10000;
    // END_CHANGE: ISS-2025-0173

    /** Set of tabled predicate indicators: "fib/2", "path/2", etc. */
    private final Set<String> tabledPredicates = new HashSet<>();

    /** Cache: normalized goal string -> list of canonical answer maps */
    private final Map<String, List<Map<String, Term>>> cache = new LinkedHashMap<>(16, 0.75f, true);

    /** Goals currently being computed (loop detection) */
    private final Set<String> inProgress = new HashSet<>();

    public void declareTable(String functor, int arity) {
        tabledPredicates.add(functor + "/" + arity);
    }

    public boolean isTabled(String functor, int arity) {
        return tabledPredicates.contains(functor + "/" + arity);
    }

    public List<Map<String, Term>> getCachedSolutions(String cacheKey) {
        return cache.get(cacheKey);
    }

    public void cacheSolutions(String cacheKey, List<Map<String, Term>> solutions) {
        // START_CHANGE: ISS-2025-0173 - Evict oldest half of cache when size limit exceeded
        if (cache.size() >= MAX_CACHE_SIZE) {
            LOGGER.warning("TableStore cache exceeded " + MAX_CACHE_SIZE + " entries (" + cache.size()
                + "). Evicting oldest half.");
            int toRemove = cache.size() / 2;
            Iterator<String> it = cache.keySet().iterator();
            for (int i = 0; i < toRemove && it.hasNext(); i++) {
                it.next();
                it.remove();
            }
        }
        // END_CHANGE: ISS-2025-0173
        List<Map<String, Term>> copied = new ArrayList<>(solutions.size());
        for (Map<String, Term> sol : solutions) {
            copied.add(new HashMap<>(sol));
        }
        cache.put(cacheKey, copied);
    }

    // START_CHANGE: ISS-2025-0173 - Add getCacheSize() for monitoring
    /**
     * Returns the current number of entries in the solutions cache.
     */
    public int getCacheSize() {
        return cache.size();
    }
    // END_CHANGE: ISS-2025-0173

    public boolean isInProgress(String cacheKey) {
        return inProgress.contains(cacheKey);
    }

    public void markInProgress(String cacheKey) {
        inProgress.add(cacheKey);
    }

    public void unmarkInProgress(String cacheKey) {
        inProgress.remove(cacheKey);
    }

    public void abolishAllTables() {
        cache.clear();
        inProgress.clear();
    }

    // START_CHANGE: ISS-2025-0124 - abolish_table/1 support
    /**
     * Remove cached solutions for a specific tabled predicate.
     * Also removes the predicate from the tabled set.
     */
    public void abolishTable(String functor, int arity) {
        String key = functor + "/" + arity;
        tabledPredicates.remove(key);
        // START_CHANGE: ISS-2025-0191 - Use exact functor/arity matching to prevent prefix collisions
        // Match "functor(" for arity>0, or exact "functor" for arity==0
        // Ensure no prefix collision: "path(" must NOT match "path_query("
        cache.entrySet().removeIf(entry -> matchesPredicate(entry.getKey(), functor, arity));
        inProgress.removeIf(k -> matchesPredicate(k, functor, arity));
        // END_CHANGE: ISS-2025-0191
    }
    // END_CHANGE: ISS-2025-0124

    // START_CHANGE: ISS-2025-0191 - Exact predicate matching helper
    private static boolean matchesPredicate(String cacheKey, String functor, int arity) {
        if (arity == 0) {
            return cacheKey.equals(functor);
        }
        // For arity>0, must match "functor(" exactly (not "functor_ext(")
        return cacheKey.startsWith(functor + "(");
    }
    // END_CHANGE: ISS-2025-0191

    public Set<String> getTabledPredicates() {
        return Collections.unmodifiableSet(tabledPredicates);
    }

    /**
     * Normalize a resolved goal for cache lookup.
     * Replaces unbound variables with positional canonical variables (_TV0, _TV1, ...)
     * so that structurally identical calls share the same cache key.
     * Returns the normalized result containing cache key, normalized pattern,
     * and the mapping from canonical variable names to original variable names.
     */
    public NormalizedGoal normalize(Term resolvedGoal) {
        Map<String, String> origToCanonical = new HashMap<>();
        Map<String, String> canonicalToOrig = new HashMap<>();
        int[] counter = {0};
        Term pattern = normalizeVars(resolvedGoal, origToCanonical, canonicalToOrig, counter);
        return new NormalizedGoal(pattern, pattern.toString(), canonicalToOrig);
    }

    private Term normalizeVars(Term term, Map<String, String> origToCanonical,
                               Map<String, String> canonicalToOrig, int[] counter) {
        if (term instanceof Variable) {
            String origName = ((Variable) term).getName();
            String canonical = origToCanonical.get(origName);
            if (canonical == null) {
                canonical = "_TV" + (counter[0]++);
                origToCanonical.put(origName, canonical);
                canonicalToOrig.put(canonical, origName);
            }
            return new Variable(canonical);
        } else if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            List<Term> newArgs = new ArrayList<>(compound.getArguments().size());
            boolean changed = false;
            for (Term arg : compound.getArguments()) {
                Term newArg = normalizeVars(arg, origToCanonical, canonicalToOrig, counter);
                newArgs.add(newArg);
                if (newArg != arg) changed = true;
            }
            if (!changed) return term; // All ground — reuse
            return new CompoundTerm(compound.getFunctor(), newArgs);
        }
        return term; // Atom, Number — already ground
    }

    /**
     * Result of goal normalization for tabling.
     */
    public static class NormalizedGoal {
        public final Term pattern;
        public final String cacheKey;
        /** Maps canonical variable name (_TV0) -> original variable name */
        public final Map<String, String> canonicalToOrig;

        public NormalizedGoal(Term pattern, String cacheKey, Map<String, String> canonicalToOrig) {
            this.pattern = pattern;
            this.cacheKey = cacheKey;
            this.canonicalToOrig = canonicalToOrig;
        }
    }
}
// END_CHANGE: ISS-2025-0092
