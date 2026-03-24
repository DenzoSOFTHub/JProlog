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
    public static Atom intern(String name) {
        // Check permanent atoms first (fast path for common atoms)
        Atom permanent = permanentAtoms.get(name);
        if (permanent != null) return permanent;

        // Check/update intern table
        WeakReference<Atom> ref = internTable.get(name);
        if (ref != null) {
            Atom existing = ref.get();
            if (existing != null) return existing;
        }

        // Create new atom and intern it
        Atom newAtom = new Atom(name);
        internTable.put(name, new WeakReference<>(newAtom));
        return newAtom;
    }

    /**
     * Run garbage collection on the atom table.
     * Removes entries whose weak references have been cleared.
     * @return number of atoms reclaimed
     */
    public static int gc() {
        int reclaimed = 0;
        for (Map.Entry<String, WeakReference<Atom>> entry : internTable.entrySet()) {
            if (entry.getValue().get() == null) {
                internTable.remove(entry.getKey(), entry.getValue());
                reclaimed++;
            }
        }
        if (reclaimed > 0) {
            LOGGER.fine("Atom GC: reclaimed " + reclaimed + " atoms");
        }
        return reclaimed;
    }

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
