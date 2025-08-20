package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.Map;

public interface Extension {
    /**
     * Execute an extension.
     * 
     * @param term The compound term to execute
     * @param substitution Variable bindings
     * @return true if execution was successful
     */
    boolean execute(CompoundTerm term, Map<String, Term> substitution);
}
