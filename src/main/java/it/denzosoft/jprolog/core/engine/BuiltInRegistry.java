package it.denzosoft.jprolog.core.engine;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

public class BuiltInRegistry {
    private static final Logger LOGGER = Logger.getLogger(BuiltInRegistry.class.getName());
    private final Map<String, BuiltIn> builtIns = new HashMap<>();

    // START_CHANGE: ISS-2025-0091 - Replace 140-line switch with HashMap for O(1) arity lookup
    /** Map from functor name to set of valid arities. */
    private static final Map<String, Set<Integer>> BUILTIN_ARITIES = new HashMap<>();
    /** Predicates that accept a range of arities (e.g., call/1..8). */
    private static final Map<String, int[]> BUILTIN_ARITY_RANGES = new HashMap<>();

    static {
        // Type checking (arity 1)
        for (String p : new String[]{"simple", "number", "atom", "var", "nonvar", "compound",
                "integer", "float", "atomic", "callable", "ground", "is_list", "partial_list"}) {
            putArity(p, 1);
        }
        // Binary operators and predicates (arity 2)
        for (String p : new String[]{"is", "=", "\\=", "==", "\\==", ">", "<", ">=", "=<",
                "=:=", "=\\=", "=..", "@<", "@=<", "@>", "@>=",
                "atom_codes", "number_codes", "string_codes", "to_codes",
                "atom_length", "atom_chars", "member", "length", "copy_term",
                "reverse", "msort", "sort", "number_chars", "atom_number",
                "atom_string", "number_string", "keysort", "succ",
                "term_variables", "subsumes_term", "clause", "forall",
                "->", ";"}) {
            putArity(p, 2);
        }
        // Ternary predicates (arity 3)
        for (String p : new String[]{"functor", "arg", "atom_concat", "append", "findall",
                "bagof", "setof", "between", "select", "nth0", "nth1", "op",
                "current_op", "catch", "plus", "compare"}) {
            putArity(p, 3);
        }
        // Unary predicates
        for (String p : new String[]{"assert", "asserta", "assertz", "retract", "retractall",
                "current_predicate", "once", "ignore", "\\+", "writeln",
                "put_char", "put_code", "write",
                "throw"}) {
            putArity(p, 1);
        }
        // START_CHANGE: R3 - get_char/1,2  get_code/1,2 with stream argument
        putArity("get_char", 1, 2);
        putArity("get_code", 1, 2);
        // END_CHANGE: R3
        // START_CHANGE: ISS-2025-0253 - close/1,2
        putArity("close", 1, 2);
        // END_CHANGE: ISS-2025-0253
        // START_CHANGE: ISS-2025-0203 - read/1 and read/2
        putArity("read", 1, 2);
        // END_CHANGE: ISS-2025-0203
        // Zero-arity predicates
        putArity("!", 0);
        putArity("repeat", 0);
        putArity("nl", 0);
        // Multi-arity predicates
        putArity("abolish", 1, 2);
        putArity("listing", 0, 1);
        putArity("halt", 0, 1);
        putArity("phrase", 2, 3);
        putArity("open", 3, 4);
        putArity("sub_atom", 5);
        // call/1..8
        BUILTIN_ARITY_RANGES.put("call", new int[]{1, 8});
        // START_CHANGE: ISS-2025-0222 - maplist/2..5, sort/4
        BUILTIN_ARITY_RANGES.put("maplist", new int[]{2, 5});
        putArity("sort", 2, 4);
        // partition/4 not registered: would shadow user-defined partition (e.g. quicksort)
        // END_CHANGE: ISS-2025-0222
        // START_CHANGE: ISS-2025-0237 - atomic_list_concat/2,3
        putArity("atomic_list_concat", 2, 3);
        // END_CHANGE: ISS-2025-0237
        // START_CHANGE: ISS-2025-0238 - atom_to_term/3
        putArity("atom_to_term", 3);
        // END_CHANGE: ISS-2025-0238
        // START_CHANGE: R1 - setarg/3
        putArity("setarg", 3);
        // END_CHANGE: R1
        // START_CHANGE: CR-2025-0005 - seek/4
        putArity("seek", 4);
        // END_CHANGE: CR-2025-0005
        // START_CHANGE: CR-2025-0009 - debug + profile predicates
        putArity("debugging", 0);
        putArity("spying", 1);
        putArity("profile", 0);
        putArity("noprofile", 0);
        putArity("profile_data", 1);
        putArity("reset_profile", 0);
        // END_CHANGE: CR-2025-0009
        // START_CHANGE: v2.9.7 - pairs_* + must_be SWI utilities
        putArity("pairs_keys", 2);
        putArity("pairs_values", 2);
        putArity("pairs_keys_values", 3);
        putArity("must_be", 2);
        // END_CHANGE: v2.9.7
        // START_CHANGE: ISS-2025-0092 - Tabling predicates
        putArity("table", 1);
        putArity("abolish_all_tables", 0);
        // END_CHANGE: ISS-2025-0092
        // START_CHANGE: LIM-005 - predicate_property/2
        putArity("predicate_property", 2);
        // END_CHANGE: LIM-005
        // START_CHANGE: LIM-006 - code_type/2
        putArity("code_type", 2);
        // END_CHANGE: LIM-006

        // START_CHANGE: LIM-002 - Attributed variable predicates
        putArity("put_attr", 3);
        putArity("get_attr", 3);
        putArity("del_attr", 2);
        putArity("attvar", 1);
        // END_CHANGE: LIM-002

        // START_CHANGE: LIM-001 - Coroutining predicates
        putArity("freeze", 2);
        putArity("when", 2);
        putArity("dif", 2);
        // END_CHANGE: LIM-001
    }

    private static void putArity(String name, int... arities) {
        Set<Integer> set = BUILTIN_ARITIES.computeIfAbsent(name, k -> new HashSet<>());
        for (int a : arities) {
            set.add(a);
        }
    }
    // END_CHANGE: ISS-2025-0091

    /**
     * Register a built-in predicate.
     */
    public void registerBuiltIn(String name, BuiltIn builtIn) {
        if (builtIns.containsKey(name)) {
            LOGGER.warning("Overriding existing built-in predicate: " + name);
        }
        builtIns.put(name, builtIn);
    }

    /**
     * Get a built-in predicate by name.
     */
    public BuiltIn getBuiltIn(String name) {
        return builtIns.get(name);
    }

    /**
     * Check if a built-in predicate exists.
     */
    public boolean hasBuiltIn(String name) {
        return builtIns.containsKey(name);
    }

    /**
     * Check if a built-in predicate exists with specific arity.
     */
    public boolean isBuiltIn(String functor, int arity) {
        if (!builtIns.containsKey(functor)) {
            return false;
        }

        // Check exact arity match
        Set<Integer> arities = BUILTIN_ARITIES.get(functor);
        if (arities != null) {
            return arities.contains(arity);
        }

        // Check arity range (e.g., call/1..8)
        int[] range = BUILTIN_ARITY_RANGES.get(functor);
        if (range != null) {
            return arity >= range[0] && arity <= range[1];
        }

        // Registered but no arity constraint - accept any arity
        return true;
    }
}
