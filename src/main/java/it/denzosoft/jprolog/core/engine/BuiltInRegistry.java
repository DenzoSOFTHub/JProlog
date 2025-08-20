package it.denzosoft.jprolog.core.engine;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

public class BuiltInRegistry {
    private static final Logger LOGGER = Logger.getLogger(BuiltInRegistry.class.getName());
    private final Map<String, BuiltIn> builtIns = new HashMap<>();

    /**
     * Register a built-in predicate.
     * 
     * @param name The predicate name
     * @param builtIn The built-in implementation
     */
    public void registerBuiltIn(String name, BuiltIn builtIn) {
        if (builtIns.containsKey(name)) {
            LOGGER.warning("Overriding existing built-in predicate: " + name);
        }
        builtIns.put(name, builtIn);
    }

    /**
     * Get a built-in predicate by name.
     * 
     * @param name The predicate name
     * @return The built-in implementation or null if not found
     */
    public BuiltIn getBuiltIn(String name) {
        return builtIns.get(name);
    }

    /**
     * Check if a built-in predicate exists.
     * 
     * @param name The predicate name
     * @return true if the built-in exists
     */
    public boolean hasBuiltIn(String name) {
        return builtIns.containsKey(name);
    }
    
    /**
     * Check if a built-in predicate exists with specific arity.
     * Most built-ins are registered by name only, so we need to guess their typical arity.
     * We only block redefinition if the arity matches the expected built-in arity.
     * 
     * @param functor The predicate functor
     * @param arity The predicate arity
     * @return true if the built-in exists with this arity
     */
    public boolean isBuiltIn(String functor, int arity) {
        if (!builtIns.containsKey(functor)) {
            return false; // Not a built-in at all
        }
        
        // Map of known built-in predicates and their typical arities
        // Only block if the arity matches the expected built-in arity
        switch (functor) {
            case "simple": return arity == 1;  // simple/1
            case "number": return arity == 1;  // number/1 
            case "atom": return arity == 1;    // atom/1
            case "var": return arity == 1;     // var/1
            case "nonvar": return arity == 1;  // nonvar/1
            case "compound": return arity == 1; // compound/1
            case "is": return arity == 2;      // is/2
            case "=": return arity == 2;       // =/2
            case "\\=": return arity == 2;     // \=/2
            case "==": return arity == 2;      // ==/2
            case "\\==": return arity == 2;    // \==/2
            case ">": return arity == 2;       // >/2
            case "<": return arity == 2;       // </2
            case ">=": return arity == 2;      // >=/2
            case "=<": return arity == 2;      // =</2
            // START_CHANGE: ISS-2025-0017 - Add missing arithmetic comparison operators
            case "=:=": return arity == 2;     // =:=/2
            case "=\\=": return arity == 2;    // =\=/2
            // END_CHANGE: ISS-2025-0017
            // START_CHANGE: ISS-2025-0018 - Add missing term manipulation predicates
            case "functor": return arity == 3; // functor/3
            case "arg": return arity == 3;     // arg/3
            case "=..": return arity == 2;     // =../2 (univ)
            // END_CHANGE: ISS-2025-0018
            // START_CHANGE: ISS-2025-0020 - Add missing control structure operators
            case "->": return arity == 2;      // ->/2 (if-then)
            case ";": return arity == 2;       // ;/2 (disjunction/if-then-else)
            case "!": return arity == 0;       // !/0 (cut)
            // END_CHANGE: ISS-2025-0020
            case "phrase": return arity == 2 || arity == 3; // phrase/2, phrase/3
            case "atom_codes": return arity == 2; // atom_codes/2
            case "number_codes": return arity == 2; // number_codes/2
            case "string_codes": return arity == 2; // string_codes/2
            case "to_codes": return arity == 2; // to_codes/2
            // START_CHANGE: ISS-2025-0021 - Add missing atom operation predicates
            case "atom_length": return arity == 2; // atom_length/2
            case "atom_concat": return arity == 3; // atom_concat/3
            case "sub_atom": return arity == 5; // sub_atom/5
            case "atom_chars": return arity == 2; // atom_chars/2
            // END_CHANGE: ISS-2025-0021
            case "append": return arity == 3;  // append/3
            case "member": return arity == 2;  // member/2
            case "length": return arity == 2;  // length/2
            case "findall": return arity == 3; // findall/3
            // START_CHANGE: ISS-2025-0022 - Add missing meta-predicates bagof/3 and setof/3
            case "bagof": return arity == 3;   // bagof/3
            case "setof": return arity == 3;   // setof/3
            // END_CHANGE: ISS-2025-0022
            // START_CHANGE: ISS-2025-0023 - Add missing database predicates
            case "assert": return arity == 1;  // assert/1
            case "asserta": return arity == 1; // asserta/1
            case "assertz": return arity == 1; // assertz/1
            case "retract": return arity == 1; // retract/1
            case "retractall": return arity == 1; // retractall/1
            case "abolish": return arity == 1 || arity == 2; // abolish/1, abolish/2
            case "current_predicate": return arity == 1; // current_predicate/1
            case "listing": return arity == 0 || arity == 1; // listing/0, listing/1
            // END_CHANGE: ISS-2025-0023
            // START_CHANGE: ISS-2025-0025 - Add missing copy_term/2 predicate
            case "copy_term": return arity == 2; // copy_term/2
            // END_CHANGE: ISS-2025-0025
            // START_CHANGE: ISS-2025-0013 - Add missing between/3 predicate for DCG support
            case "between": return arity == 3; // between/3
            // END_CHANGE: ISS-2025-0013
            // START_CHANGE: ISS-2025-0026 - Add missing type checking predicates
            case "integer": return arity == 1; // integer/1
            case "float": return arity == 1;   // float/1
            case "atomic": return arity == 1;  // atomic/1
            case "callable": return arity == 1; // callable/1
            case "ground": return arity == 1;  // ground/1
            case "is_list": return arity == 1; // is_list/1
            case "partial_list": return arity == 1; // partial_list/1
            // END_CHANGE: ISS-2025-0026
            // START_CHANGE: ISS-2025-0027 - Add missing list predicates
            case "reverse": return arity == 2; // reverse/2
            case "msort": return arity == 2;   // msort/2
            case "sort": return arity == 2;    // sort/2
            case "select": return arity == 3;  // select/3
            case "nth0": return arity == 3;    // nth0/3
            case "nth1": return arity == 3;    // nth1/3
            // END_CHANGE: ISS-2025-0027
            // START_CHANGE: ISS-2025-0028 - Add missing control predicates
            case "once": return arity == 1;    // once/1
            case "forall": return arity == 2;  // forall/2
            case "call": return arity == 1;    // call/1 (basic form)
            case "ignore": return arity == 1;  // ignore/1
            case "repeat": return arity == 0;  // repeat/0
            case "\\+": return arity == 1;     // \+/1 (negation as failure)
            // END_CHANGE: ISS-2025-0028
            // START_CHANGE: ISS-2025-0029 - Add missing I/O predicates
            case "read": return arity == 1;    // read/1
            case "writeln": return arity == 1; // writeln/1
            case "get_char": return arity == 1; // get_char/1
            case "put_char": return arity == 1; // put_char/1
            case "get_code": return arity == 1; // get_code/1
            case "put_code": return arity == 1; // put_code/1
            case "open": return arity == 3 || arity == 4; // open/3, open/4
            case "close": return arity == 1;   // close/1
            // END_CHANGE: ISS-2025-0029
            // START_CHANGE: ISS-2025-0030 - Add missing conversion predicates
            case "number_chars": return arity == 2; // number_chars/2
            case "atom_number": return arity == 2;  // atom_number/2
            case "atom_string": return arity == 2;  // atom_string/2
            case "number_string": return arity == 2; // number_string/2
            // END_CHANGE: ISS-2025-0030
            // START_CHANGE: ISS-2025-0031 - Add missing term comparison predicates
            case "@<": return arity == 2;      // @</2
            case "@=<": return arity == 2;     // @=</2
            case "@>": return arity == 2;      // @>/2
            case "@>=": return arity == 2;     // @>=/2
            // END_CHANGE: ISS-2025-0031
            case "write": return arity == 1;   // write/1
            case "nl": return arity == 0;      // nl/0
            case "halt": return arity == 0 || arity == 1; // halt/0, halt/1
            // START_CHANGE: ISS-2025-0049 - Add operator definition predicates  
            case "op": return arity == 3;         // op/3
            case "current_op": return arity == 3;  // current_op/3
            // END_CHANGE: ISS-2025-0049
            // START_CHANGE: ISS-2025-0044 - Add exception handling predicates
            case "throw": return arity == 1;       // throw/1
            case "catch": return arity == 3;       // catch/3
            // END_CHANGE: ISS-2025-0044
            default:
                // For unknown predicates, assume arity 1 is the most common
                return arity == 1;
        }
    }
}
