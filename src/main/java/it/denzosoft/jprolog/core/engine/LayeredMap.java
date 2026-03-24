package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.core.terms.Term;

import java.util.*;

// START_CHANGE: ISS-2025-0095 - Structural sharing for binding maps
/**
 * A layered map that shares structure with its parent.
 * Only new/modified bindings are stored in the local layer.
 * Reads check local first, then delegate to parent.
 * This reduces HashMap allocation overhead during backtracking.
 *
 * Implements Map<String, Term> for drop-in compatibility with existing code.
 */
public class LayeredMap implements Map<String, Term> {

    private final Map<String, Term> parent;
    private final Map<String, Term> local;
    /** Keys removed from parent (for overwrite semantics) */
    private Set<String> removed;

    /**
     * Create a new layered map on top of a parent.
     */
    public LayeredMap(Map<String, Term> parent) {
        this.parent = parent;
        this.local = new HashMap<>();
        this.removed = null;
    }

    @Override
    public Term get(Object key) {
        Term val = local.get(key);
        if (val != null) return val;
        if (removed != null && removed.contains(key)) return null;
        return parent.get(key);
    }

    @Override
    public boolean containsKey(Object key) {
        if (local.containsKey(key)) return true;
        if (removed != null && removed.contains(key)) return false;
        return parent.containsKey(key);
    }

    @Override
    public int size() {
        // Approximate: may double-count if local overrides parent
        return flatten().size();
    }

    // START_CHANGE: ISS-2025-0189 - Account for removed set in isEmpty()
    @Override
    public boolean isEmpty() {
        if (!local.isEmpty()) return false;
        if (parent.isEmpty()) return true;
        if (removed == null || removed.isEmpty()) return false;
        // Check if all parent keys were removed
        for (String key : parent.keySet()) {
            if (!removed.contains(key)) return false;
        }
        return true;
    }
    // END_CHANGE: ISS-2025-0189

    @Override
    public boolean containsValue(Object value) {
        return flatten().containsValue(value);
    }

    @Override
    public Term remove(Object key) {
        Term localVal = local.remove(key);
        if (parent.containsKey(key)) {
            if (removed == null) removed = new HashSet<>();
            removed.add((String) key);
            return localVal != null ? localVal : parent.get(key);
        }
        return localVal;
    }

    @Override
    public void putAll(Map<? extends String, ? extends Term> m) {
        for (Entry<? extends String, ? extends Term> e : m.entrySet()) {
            put(e.getKey(), e.getValue());
        }
    }

    @Override
    public void clear() {
        local.clear();
        if (removed == null) removed = new HashSet<>();
        removed.addAll(parent.keySet());
    }

    @Override
    public Set<String> keySet() {
        return flatten().keySet();
    }

    @Override
    public Collection<Term> values() {
        return flatten().values();
    }

    @Override
    public Set<Entry<String, Term>> entrySet() {
        return flatten().entrySet();
    }

    /**
     * Flatten to a regular HashMap (used when we need full iteration).
     * This is the "materialization" cost — only paid when truly needed.
     */
    public HashMap<String, Term> flatten() {
        HashMap<String, Term> flat = new HashMap<>(parent);
        if (removed != null) {
            for (String key : removed) {
                flat.remove(key);
            }
        }
        flat.putAll(local);
        return flat;
    }

    /**
     * Create a snapshot as a regular HashMap.
     * Use this when the solution needs to be stored permanently.
     */
    public HashMap<String, Term> snapshot() {
        return flatten();
    }

    // START_CHANGE: ISS-2025-0163 - Improved mark/rollback: track both additions and overwrites
    /**
     * Journal of changes since mark, stored as (key, previousValue) pairs.
     * previousValue is null for new keys (key didn't exist in local before).
     * For overwrites, previousValue is the old local value to restore.
     */
    private List<Object[]> changeJournal = null;

    /**
     * Mark the current state for potential rollback.
     * Returns the current journal size.
     */
    public int mark() {
        if (changeJournal == null) {
            changeJournal = new ArrayList<>();
        }
        return changeJournal.size();
    }

    /**
     * Rollback local changes to a previous mark point.
     * Undoes all put() calls after the mark in reverse order.
     */
    public void rollbackToMark(int markPoint) {
        if (changeJournal != null && changeJournal.size() > markPoint) {
            for (int i = changeJournal.size() - 1; i >= markPoint; i--) {
                Object[] entry = changeJournal.get(i);
                String key = (String) entry[0];
                Term previousValue = (Term) entry[1];
                if (previousValue == null) {
                    // Key was newly added — remove it
                    local.remove(key);
                } else {
                    // Key was overwritten — restore old value
                    local.put(key, previousValue);
                }
                changeJournal.remove(i);
            }
        }
    }

    // Override put to track changes when mark/rollback is active
    @Override
    public Term put(String key, Term value) {
        if (removed != null) removed.remove(key);
        if (changeJournal != null) {
            Term oldValue = local.get(key);
            // oldValue == null means this is a new key in local layer
            changeJournal.add(new Object[]{key, oldValue});
        }
        return local.put(key, value);
    }
    // END_CHANGE: ISS-2025-0163
}
// END_CHANGE: ISS-2025-0095
