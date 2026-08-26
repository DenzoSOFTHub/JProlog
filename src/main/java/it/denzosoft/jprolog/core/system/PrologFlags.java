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

    // START_CHANGE: ISS-2025-0437 - ENG-06: the flag store is PER ENGINE, not per JVM.
    // FLAGS used to be a static HashMap, so set_prolog_flag(unknown, fail),
    // set_prolog_flag(double_quotes, codes) or set_prolog_flag(occurs_check, true) in ONE Prolog
    // instance silently changed the behaviour of every other instance in the JVM — which
    // undermined the v3.4.0 sandbox guidance ("use a fresh Prolog per security domain"): sandboxed
    // code could flip `unknown` or `occurs_check` for the host's other engines. Concurrent access
    // to the plain HashMap could also corrupt it.
    //
    // Each {@link it.denzosoft.jprolog.core.engine.Prolog} now owns a PrologFlags instance and
    // installs it as the thread-current store around solve/consult, so the (unchanged) static API
    // used by built-ins and the parsers routes to the right engine. Code with no engine in scope —
    // a directly-instantiated parser, a unit test — sees a process-wide default store, exactly as
    // before.
    private final Map<String, Term> flags = new HashMap<>();

    /** The store used when no engine is current on this thread (standalone parser use, tests). */
    private static final PrologFlags DEFAULT = new PrologFlags();
    private static final ThreadLocal<PrologFlags> CURRENT = new ThreadLocal<>();

    public PrologFlags() {
        initializeStandardFlags(this.flags);
    }

    /** The flag store in effect on this thread. */
    public static PrologFlags current() {
        PrologFlags f = CURRENT.get();
        return (f != null) ? f : DEFAULT;
    }

    /** Install {@code f} as this thread's store (null restores the process-wide default).
     *  Returns the previous store so a caller can restore it in a finally block. */
    public static PrologFlags setCurrent(PrologFlags f) {
        PrologFlags prev = CURRENT.get();
        if (f == null) CURRENT.remove(); else CURRENT.set(f);
        return prev;
    }

    // Fast path for the unification hot path: occurs_check is off in essentially every program, so
    // a single volatile read answers isOccursCheck() without touching the ThreadLocal. The flag is
    // only ever turned ON here, so a false reading is always authoritative.
    private static volatile boolean anyOccursCheck = false;
    private boolean occursCheck = false;

    /** True when THIS engine has occurs_check enabled. */
    public boolean isOccursCheck() { return occursCheck; }

    /** True when the engine current on this thread has occurs_check enabled. */
    public static boolean isOccursCheckEnabled() {
        return anyOccursCheck && current().occursCheck;
    }

    /** Set occurs_check on the thread-current store (used by Variable.setOccursCheckEnabled). */
    public static void setOccursCheckEnabled(boolean enabled) {
        current().setOccursCheck(enabled);
    }

    // START_CHANGE: ISS-2025-0437 - ENG-06: trace/0 state is per engine too. Trace.tracingEnabled
    // was a process-global static, so `trace.` in one engine turned four-port tracing on for every
    // engine in the JVM (and notrace/0 in one turned it off for all). Same volatile fast path as
    // occurs_check: tracing is off in essentially every run, and the machine consults it on the
    // hot path (debugTraceActive()).
    private static volatile boolean anyTracing = false;
    private boolean tracing = false;

    public boolean isTracing() { return tracing; }

    public void setTracing(boolean enabled) {
        this.tracing = enabled;
        if (enabled) anyTracing = true;
        flags.put("trace", new Atom(enabled ? "on" : "off"));
    }

    /** True when the engine current on this thread has four-port tracing enabled. */
    public static boolean isTracingEnabled() {
        return anyTracing && current().tracing;
    }

    /** Enable/disable tracing on the engine current on this thread. */
    public static void setTracingEnabled(boolean enabled) {
        current().setTracing(enabled);
    }
    // END_CHANGE: ISS-2025-0437

    void setOccursCheck(boolean enabled) {
        this.occursCheck = enabled;
        if (enabled) anyOccursCheck = true;
        flags.put("occurs_check", new Atom(enabled ? "true" : "false"));
    }
    // END_CHANGE: ISS-2025-0437

    private static void initializeStandardFlags(Map<String, Term> FLAGS) {
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
        // START_CHANGE: ISS-2025-0200 - default "string" preserves JProlog legacy behaviour; set to "codes" for strict ISO
        FLAGS.put("double_quotes", new Atom("string"));
        // END_CHANGE: ISS-2025-0200
        
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
        return current().get(flagName);              // ISS-2025-0437 - ENG-06
    }

    /** Instance accessor: the value of {@code flagName} in THIS engine's store. */
    public Term get(String flagName) {
        return flags.get(flagName);
    }
    
    /**
     * Set the value of a flag.
     * @param flagName The name of the flag
     * @param value The new value
     * @return true if the flag was set successfully, false if read-only or invalid
     */
    public static boolean setFlag(String flagName, Term value) {
        return current().set(flagName, value);       // ISS-2025-0437 - ENG-06
    }

    /** Instance mutator: set {@code flagName} in THIS engine's store. */
    public boolean set(String flagName, Term value) {
        // Some flags are read-only
        if (isReadOnlyFlag(flagName)) {
            return false;
        }
        
        // Validate the value for specific flags
        if (!isValidFlagValue(flagName, value)) {
            return false;
        }
        
        flags.put(flagName, value);
        // START_CHANGE: ISS-2025-0246 - Wire occurs_check flag into actual unification.
        // START_CHANGE: ISS-2025-0437 - ENG-06: per-engine, not a process-wide static.
        if ("occurs_check".equals(flagName) && value instanceof Atom) {
            // ISS-2025-0441: `error` also turns the check ON; the v4 unifier then raises instead of
            // failing (it re-reads the flag VALUE to tell the two apart). Set the boolean here
            // rather than through setOccursCheck(), which would overwrite the stored atom with
            // true/false and lose the third mode.
            this.occursCheck = !"false".equals(((Atom) value).getName());
            if (this.occursCheck) anyOccursCheck = true;
        }
        // END_CHANGE: ISS-2025-0437
        // END_CHANGE: ISS-2025-0246
        return true;
    }

    /**
     * Check if a flag exists.
     * @param flagName The name of the flag
     * @return true if the flag exists
     */
    public static boolean hasFlag(String flagName) {
        return current().flags.containsKey(flagName);   // ISS-2025-0437 - ENG-06
    }
    
    /**
     * Get all flag names.
     * @return Set of all flag names
     */
    public static Set<String> getAllFlagNames() {
        return current().flags.keySet();                // ISS-2025-0437 - ENG-06
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
                
            // START_CHANGE: ISS-2025-0441 - ISO 7.11.2.4 gives occurs_check three values, not two:
            // true (check and fail), false (no check) and ERROR (check and raise
            // representation_error(cyclic_term)). The third one was rejected, which left no way to
            // ask for the ISO behaviour now that the v4 engine supports rational trees by default
            // (design decision 2). On the v2/legacy engines `error` behaves like `true` (it fails);
            // only the v4 unifier raises.
            case "occurs_check":
                return "true".equals(atomValue) || "false".equals(atomValue) || "error".equals(atomValue);
            // END_CHANGE: ISS-2025-0441
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
                // START_CHANGE: ISS-2025-0200 - accept "string" as additional value
                return "codes".equals(atomValue) || "chars".equals(atomValue) || "atom".equals(atomValue) || "string".equals(atomValue);
                // END_CHANGE: ISS-2025-0200
                
            default:
                return true; // Allow any value for user-defined flags
        }
    }
}