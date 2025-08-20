package it.denzosoft.jprolog.builtin.meta;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.CutStatus;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of once/1 predicate.
 * 
 * once(+Goal)
 * 
 * Execute Goal, but succeed at most once. If Goal has multiple solutions,
 * only the first one is returned.
 */
public class Once implements BuiltInWithContext {
    
    private final QuerySolver querySolver;
    
    public Once(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, 
                                    Map<String, Term> bindings, 
                                    List<Map<String, Term>> solutions) {
        
        if (query.getArguments().size() != 1) {
            throw new PrologException(createTypeError("callable", query, "once/1 requires exactly one argument"));
        }
        
        Term goal = query.getArguments().get(0).resolveBindings(bindings);
        
        // Check if goal is instantiated
        if (goal instanceof Variable) {
            throw new PrologException(createInstantiationError("once/1: goal must be instantiated"));
        }
        
        // Execute the goal and get only the first solution
        List<Map<String, Term>> goalSolutions = new ArrayList<>();
        boolean success = solver.solve(goal, new HashMap<>(bindings), goalSolutions, CutStatus.notOccurred());
        
        if (success && !goalSolutions.isEmpty()) {
            // Return only the first solution
            solutions.add(goalSolutions.get(0));
            return true;
        }
        
        return false;
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("once/1 requires context");
    }
    
    private Term createInstantiationError(String context) {
        try {
            return new CompoundTerm(
                new Atom("error"),
                java.util.Arrays.asList(
                    new Atom("instantiation_error"),
                    new Atom(context)
                )
            );
        } catch (Exception e) {
            return new Atom("instantiation_error");
        }
    }
    
    private Term createTypeError(String expectedType, Term culprit, String context) {
        try {
            return new CompoundTerm(
                new Atom("error"),
                java.util.Arrays.asList(
                    new CompoundTerm(
                        new Atom("type_error"),
                        java.util.Arrays.asList(
                            new Atom(expectedType),
                            culprit
                        )
                    ),
                    new Atom(context)
                )
            );
        } catch (Exception e) {
            return new Atom("type_error");
        }
    }
}