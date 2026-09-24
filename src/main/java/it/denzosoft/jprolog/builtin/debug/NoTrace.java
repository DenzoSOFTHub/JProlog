package it.denzosoft.jprolog.builtin.debug;

import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

/**
 * Implementation of notrace/0 predicate.
 * 
 * notrace
 * 
 * Disables tracing mode. Turns off the debugging information display
 * that was enabled by trace/0.
 * 
 * Examples:
 * ?- notrace.
 * true.
 */
public class NoTrace implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() != null && !query.getArguments().isEmpty()) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0697
        }
        
        Trace.setTracingEnabled(false);
        it.denzosoft.jprolog.builtin.io.StreamManager.out().println("% Tracing disabled");
        
        solutions.add(bindings);
        return true;
    }
}