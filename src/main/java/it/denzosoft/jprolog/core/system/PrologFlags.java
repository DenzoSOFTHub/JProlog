package it.denzosoft.jprolog.core.system;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Manages ISO Prolog system flags.
 * Prolog flags control various aspects of the system behavior.
 */
public class PrologFlags {
    private static final Map<String, Term> FLAGS = new HashMap<>();
    
    static {
        // Initialize standard ISO Prolog flags
        initializeStandardFlags();
    }
    
    private static void initializeStandardFlags() {
        // bounded/1 - Whether integers are bounded
        FLAGS.put("bounded", new Atom("true"));
        
        // max_integer/1 - Maximum integer value (if bounded)
        FLAGS.put("max_integer", new Number(Long.MAX_VALUE));
        
        // min_integer/1 - Minimum integer value (if bounded)
        FLAGS.put("min_integer", new Number(Long.MIN_VALUE));
        
        // integer_rounding_function/1 - How integer division rounds
        FLAGS.put("integer_rounding_function", new Atom("toward_zero"));
        
        // char_conversion/1 - Whether character conversion is performed
        FLAGS.put("char_conversion", new Atom("off"));
        
        // debug/1 - Debug mode
        FLAGS.put("debug", new Atom("off"));
        
        // max_arity/1 - Maximum arity of compound terms
        FLAGS.put("max_arity", new Atom("unbounded"));
        
        // unknown/1 - What to do with undefined predicates
        FLAGS.put("unknown", new Atom("error"));
        
        // double_quotes/1 - How to interpret double-quoted strings
        FLAGS.put("double_quotes", new Atom("codes"));
        
        // prolog_version/1 - Version information
        FLAGS.put("prolog_version", new Atom("jprolog-2.0.15"));
        
        // dialect/1 - Prolog dialect
        FLAGS.put("dialect", new Atom("iso"));
        
        // version/1 - Implementation version
        FLAGS.put("version", new Atom("2.0.15"));
        
        // version_data/1 - Structured version data
        FLAGS.put("version_data", new Atom("jprolog(2,0,15)"));
        
        // occurs_check/1 - Whether unification performs occurs check
        FLAGS.put("occurs_check", new Atom("false"));
        
        // syntax_errors/1 - How to handle syntax errors
        FLAGS.put("syntax_errors", new Atom("error"));
        
        // Additional ISO 13211-1 required flags
        
        // character_escapes/1 - Whether escape sequences are processed in quoted atoms
        FLAGS.put("character_escapes", new Atom("true"));
        
        // initialization/1 - Initialization goal behavior
        FLAGS.put("initialization", new Atom("true"));
        
        // strict_iso/1 - Whether to enforce strict ISO compliance
        FLAGS.put("strict_iso", new Atom("false"));
        
        // encoding/1 - Default text encoding
        FLAGS.put("encoding", new Atom("utf8"));
        
        // argv/1 - Command line arguments (empty list for now)
        FLAGS.put("argv", new Atom("[]"));
        
        // gc/1 - Garbage collection control
        FLAGS.put("gc", new Atom("on"));
        
        // stack_limit/1 - Stack size limit
        FLAGS.put("stack_limit", new Number(1000000));
        
        // trace/1 - Tracing mode
        FLAGS.put("trace", new Atom("off"));
        
        // optimize/1 - Optimization level
        FLAGS.put("optimize", new Atom("false"));
        
        // toplevel_print_options/1 - Options for toplevel printing
        FLAGS.put("toplevel_print_options", new Atom("[]"));
        
        // write_strings/1 - How to write string objects
        FLAGS.put("write_strings", new Atom("true"));
        
        // traditional/1 - Traditional (non-ISO) mode
        FLAGS.put("traditional", new Atom("false"));
    }
    
    /**
     * Get the value of a flag.
     * @param flagName The name of the flag
     * @return The flag value, or null if the flag doesn't exist
     */
    public static Term getFlag(String flagName) {
        return FLAGS.get(flagName);
    }
    
    /**
     * Set the value of a flag.
     * @param flagName The name of the flag
     * @param value The new value
     * @return true if the flag was set successfully, false if read-only or invalid
     */
    public static boolean setFlag(String flagName, Term value) {
        // Some flags are read-only
        if (isReadOnlyFlag(flagName)) {
            return false;
        }
        
        // Validate the value for specific flags
        if (!isValidFlagValue(flagName, value)) {
            return false;
        }
        
        FLAGS.put(flagName, value);
        return true;
    }
    
    /**
     * Check if a flag exists.
     * @param flagName The name of the flag
     * @return true if the flag exists
     */
    public static boolean hasFlag(String flagName) {
        return FLAGS.containsKey(flagName);
    }
    
    /**
     * Get all flag names.
     * @return Set of all flag names
     */
    public static Set<String> getAllFlagNames() {
        return FLAGS.keySet();
    }
    
    /**
     * Check if a flag is read-only.
     */
    private static boolean isReadOnlyFlag(String flagName) {
        switch (flagName) {
            // Core system flags (read-only)
            case "bounded":
            case "max_integer":
            case "min_integer":
            case "integer_rounding_function":
            case "max_arity":
            case "prolog_version":
            case "dialect":
            case "version":
            case "version_data":
            case "encoding":  // System encoding is fixed
            case "argv":      // Command line arguments are set at startup
                return true;
            default:
                return false;
        }
    }
    
    /**
     * Validate flag values for specific flags.
     */
    private static boolean isValidFlagValue(String flagName, Term value) {
        // Handle numeric flags
        if (flagName.equals("stack_limit")) {
            return value instanceof Number && ((Number) value).getValue() > 0;
        }
        
        // Most other flags expect atom values
        if (!(value instanceof Atom)) {
            return false;
        }
        
        String atomValue = ((Atom) value).getName();
        
        switch (flagName) {
            // Boolean-style flags (on/off)
            case "debug":
            case "char_conversion":
            case "gc":
            case "trace":
                return "on".equals(atomValue) || "off".equals(atomValue);
                
            // Boolean-style flags (true/false)
            case "occurs_check":
            case "character_escapes":
            case "initialization":
            case "strict_iso":
            case "optimize":
            case "write_strings":
            case "traditional":
                return "true".equals(atomValue) || "false".equals(atomValue);
                
            // Multi-value flags
            case "unknown":
            case "syntax_errors":
                return "error".equals(atomValue) || "fail".equals(atomValue) || "warning".equals(atomValue);
                
            case "double_quotes":
                return "codes".equals(atomValue) || "chars".equals(atomValue) || "atom".equals(atomValue);
                
            default:
                return true; // Allow any value for user-defined flags
        }
    }
}