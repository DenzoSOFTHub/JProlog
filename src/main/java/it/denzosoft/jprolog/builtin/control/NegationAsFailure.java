package it.denzosoft.jprolog.builtin.control;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.engine.CutStatus;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of negation as failure (\\+) predicate.
 * 
 * The \\+ operator succeeds if its argument goal fails, and fails if its argument goal succeeds.
 * This is called "negation as failure" because it doesn't represent logical negation,
 * but rather the inability to prove the goal.
 * 
 * Syntax: \\+ Goal
 * 
 * Examples:
 * ?- \\+ fail.    % succeeds
 * ?- \\+ true.    % fails
 * ?- \\+ (1 = 2). % succeeds
 * ?- \\+ (1 = 1). % fails
 */
public class NegationAsFailure implements BuiltInWithContext {
    
    private QuerySolver solver;
    
    public NegationAsFailure(QuerySolver solver) {
        this.solver = solver;
    }
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, Map<String, Term> bindings, 
                                     List<Map<String, Term>> solutions) {
        
        // The goal should have exactly one argument
        if (query.getArguments() == null || query.getArguments().size() != 1) {
            throw new IllegalArgumentException("\\+ requires exactly one argument");
        }
        
        Term goal = query.getArguments().get(0).resolveBindings(bindings);
        
        // Try to solve the goal
        List<Map<String, Term>> goalSolutions = new java.util.ArrayList<>();
        boolean goalSucceeds = solver.solve(goal, new HashMap<>(bindings), goalSolutions, CutStatus.notOccurred());
        
        // Negation as failure: succeed if the goal fails, fail if the goal succeeds
        if (!goalSucceeds || goalSolutions.isEmpty()) {
            // Goal failed, so \\+ succeeds
            solutions.add(new HashMap<>(bindings));
            return true;
        } else {
            // Goal succeeded, so \\+ fails
            return false;
        }
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("\\+ requires context");
    }
}