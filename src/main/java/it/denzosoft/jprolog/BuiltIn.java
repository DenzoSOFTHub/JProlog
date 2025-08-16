package it.denzosoft.jprolog;

import it.denzosoft.jprolog.terms.Term;

import java.util.List;
import java.util.Map;

public interface BuiltIn {
    /**
     * Execute a built-in predicate.
     * 
     * @param query The query term to execute
     * @param bindings Current variable bindings
     * @param solutions List to add successful solutions to
     * @return true if execution was successful
     */
    boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions);
}
