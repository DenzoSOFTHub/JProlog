package it.denzosoft.jprolog.core.terms;

// START_CHANGE: LIM-016 - Atom garbage collection via intern table with weak references
import java.lang.ref.WeakReference;
import java.util.concurrent.ConcurrentHashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Global atom intern table with garbage collection support.
 * Uses weak references so that atoms not referenced elsewhere can be reclaimed.
 * Common atoms (true, false, [], etc.) are kept as strong references.
 */
public class AtomTable {
    private static final Logger LOGGER = Logger.getLogger(AtomTable.class.getName());

    /** Weak reference entries for reclaimable atoms */
    private static final ConcurrentHashMap<String, WeakReference<Atom>> internTable = new ConcurrentHashMap<>();

    /** Strong references for permanent atoms that should never be GC'd */
    private static final ConcurrentHashMap<String, Atom> permanentAtoms = new ConcurrentHashMap<>();

    // Pre-intern common atoms as permanent
    static {
        String[] commonAtoms = {
            "true", "false", "fail", "[]", ".", ",", ";", ":-", "?-",
            "=", "\\=", "==", "\\==", "is", "not", "\\+",
            "assert", "asserta", "assertz", "retract", "retractall",
            "write", "writeln", "read", "nl", "tab",
            "atom", "number", "integer", "float", "compound", "var", "nonvar",
            "functor", "arg", "=..", "copy_term",
            "call", "once", "findall", "bagof", "setof",
            "append", "member", "length", "sort", "msort",
            "+", "-", "*", "/", "//", "mod", "rem",
            "=:=", "=\\=", "<", ">", "=<", ">=",
            "@<", "@>", "@=<", "@>=",
            "error", "type_error", "instantiation_error", "existence_error",
            "permission_error", "evaluation_error", "resource_error",
            "end_of_file", "user_input", "user_output", "user_error"
        };
        for (String name : commonAtoms) {
            Atom atom = new Atom(name);
            permanentAtoms.put(name, atom);
        }
    }

    /**
     * Intern an atom name, returning a shared Atom instance.
     * If the atom was previously interned and is still alive, returns the existing instance.
     * Common atoms are always returned from the permanent table.
     */
    // START_CHANGE: ISS-2025-0181 - Term system bug fixes
    public static Atom intern(String name) {
        Atom permanent = permanentAtoms.get(name);
        if (permanent != null) return permanent;

        // START_CHANGE: Round5 - hold strong reference inside compute so GC can't null it
        // between compute() and the subsequent get(). Returns the strong reference directly.
        Atom[] strongHolder = new Atom[1];
        internTable.compute(name, (k, existingRef) -> {
            if (existingRef != null) {
                Atom existing = existingRef.get();
                if (existing != null) {
                    strongHolder[0] = existing;
                    return existingRef;
                }
            }
            Atom fresh = new Atom(k);
            strongHolder[0] = fresh;
            return new WeakReference<>(fresh);
        });
        return strongHolder[0];
        // END_CHANGE: Round5
    }
    // END_CHANGE: ISS-2025-0181

    /**
     * Run garbage collection on the atom table.
     * Removes entries whose weak references have been cleared.
     * @return number of atoms reclaimed
     */
    // START_CHANGE: ISS-2025-0181 - Term system bug fixes
    public static int gc() {
        // START_CHANGE: Round5 minor - atomic remove-if-dead via compute (no race with concurrent intern)
        int[] reclaimed = {0};
        // Snapshot keys; for each, atomic compute to remove only if WeakRef still dead.
        for (String key : new java.util.ArrayList<>(internTable.keySet())) {
            internTable.compute(key, (k, ref) -> {
                if (ref == null) return null;
                Atom alive = ref.get();
                if (alive == null) {
                    reclaimed[0]++;
                    return null; // remove entry
                }
                return ref; // keep
            });
        }
        if (reclaimed[0] > 0) {
            if (LOGGER.isLoggable(java.util.logging.Level.FINE)) LOGGER.fine("Atom GC: reclaimed " + reclaimed[0] + " atoms");   // ISS-2025-0550: lazy
        }
        return reclaimed[0];
        // END_CHANGE: Round5 minor
    }
    // END_CHANGE: ISS-2025-0181

    /**
     * Get the current size of the intern table (including potentially dead references).
     */
    public static int size() {
        return permanentAtoms.size() + internTable.size();
    }

    /**
     * Get the number of permanent (non-reclaimable) atoms.
     */
    public static int permanentSize() {
        return permanentAtoms.size();
    }

    /**
     * Clear the intern table (but keep permanent atoms).
     */
    public static void clear() {
        internTable.clear();
    }
}
// END_CHANGE: LIM-016
