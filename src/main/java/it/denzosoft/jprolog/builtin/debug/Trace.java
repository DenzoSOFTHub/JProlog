package it.denzosoft.jprolog.builtin.debug;

import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

/**
 * Implementation of trace/0 predicate.
 * 
 * trace
 * 
 * Enables tracing mode. In tracing mode, the execution of goals is traced
 * and debugging information is displayed. This is a simplified implementation
 * that sets a global flag.
 * 
 * Examples:
 * ?- trace.
 * true.
 */
public class Trace implements BuiltIn {
    
    // START_CHANGE: ISS-2025-0437 - ENG-06: tracing state moved into the engine's PrologFlags store
    // (see PrologFlags.isTracingEnabled). It used to be a process-global static here, so `trace.`
    // in one Prolog instance enabled four-port tracing for every instance in the JVM. The static
    // accessors below are kept and simply route to the engine current on this thread; callers that
    // toggle tracing from OUTSIDE a query (the IDE Run panel, the CLI ':trace' command) must use
    // Prolog.setTracing(boolean) on their own engine instead.
    // END_CHANGE: ISS-2025-0437
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() != null && !query.getArguments().isEmpty()) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0697
        }
        
        it.denzosoft.jprolog.core.system.PrologFlags.setTracingEnabled(true);   // ISS-2025-0437
        it.denzosoft.jprolog.builtin.io.StreamManager.out().println("% Tracing enabled");
        
        solutions.add(bindings);
        return true;
    }
    
    /**
     * Check if tracing is currently enabled.
     */
    public static boolean isTracingEnabled() {
        return it.denzosoft.jprolog.core.system.PrologFlags.isTracingEnabled();   // ISS-2025-0437
    }
    
    /**
     * Set the tracing state (used by notrace/0).
     */
    public static void setTracingEnabled(boolean enabled) {
        it.denzosoft.jprolog.core.system.PrologFlags.setTracingEnabled(enabled);   // ISS-2025-0437
    }
}