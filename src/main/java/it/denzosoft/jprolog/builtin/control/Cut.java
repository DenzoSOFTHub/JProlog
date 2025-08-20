package it.denzosoft.jprolog.builtin.control;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.CutStatus;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Cut operator (!).
 * The cut prevents backtracking to choice points created before it.
 * This is a simplified implementation that works with the current architecture.
 */
public class Cut implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() != null && query.getArguments().size() != 0) {
            throw new PrologEvaluationException("cut/0 takes no arguments.");
        }
        
        // Add current bindings to solutions - cut always succeeds
        solutions.add(new HashMap<>(bindings));
        
        // The cut behavior (preventing backtracking) is implemented by:
        // 1. Only returning one solution (first success)  
        // 2. The QuerySolver handles cut detection for rule choice points
        
        return true;
    }
    
}
