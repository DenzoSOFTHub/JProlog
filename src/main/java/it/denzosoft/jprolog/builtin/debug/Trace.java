package it.denzosoft.jprolog.builtin.debug;

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
    
    // Global tracing state (simplified implementation)
    private static boolean tracingEnabled = false;
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() != null && !query.getArguments().isEmpty()) {
            throw new PrologEvaluationException("trace/0 takes no arguments");
        }
        
        tracingEnabled = true;
        System.out.println("% Tracing enabled");
        
        solutions.add(bindings);
        return true;
    }
    
    /**
     * Check if tracing is currently enabled.
     */
    public static boolean isTracingEnabled() {
        return tracingEnabled;
    }
    
    /**
     * Set the tracing state (used by notrace/0).
     */
    public static void setTracingEnabled(boolean enabled) {
        tracingEnabled = enabled;
    }
}