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
 * Implementation of forall/2 predicate.
 * 
 * forall(+Condition, +Action)
 * 
 * For all solutions of Condition, Action must succeed.
 * The predicate succeeds if Action succeeds for every solution of Condition.
 * If Condition has no solutions, forall/2 succeeds trivially.
 */
public class ForAll implements BuiltInWithContext {
    
    private final QuerySolver querySolver;
    
    public ForAll(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, 
                                    Map<String, Term> bindings, 
                                    List<Map<String, Term>> solutions) {
        
        if (query.getArguments().size() != 2) {
            throw new PrologException(createTypeError("callable", query, "forall/2 requires exactly two arguments"));
        }
        
        Term condition = query.getArguments().get(0).resolveBindings(bindings);
        Term action = query.getArguments().get(1).resolveBindings(bindings);
        
        // Check if both arguments are instantiated
        if (condition instanceof Variable) {
            throw new PrologException(createInstantiationError("forall/2: condition must be instantiated"));
        }
        if (action instanceof Variable) {
            throw new PrologException(createInstantiationError("forall/2: action must be instantiated"));
        }
        
        // Find all solutions to the condition
        List<Map<String, Term>> conditionSolutions = new ArrayList<>();
        boolean conditionHasSolutions = solver.solve(condition, new HashMap<>(bindings), 
                                                   conditionSolutions, CutStatus.notOccurred());
        
        if (!conditionHasSolutions || conditionSolutions.isEmpty()) {
            // No solutions to condition - forall succeeds trivially
            solutions.add(new HashMap<>(bindings));
            return true;
        }
        
        // Check that action succeeds for each condition solution
        for (Map<String, Term> conditionBinding : conditionSolutions) {
            List<Map<String, Term>> actionSolutions = new ArrayList<>();
            boolean actionSucceeds = solver.solve(action, new HashMap<>(conditionBinding), 
                                                actionSolutions, CutStatus.notOccurred());
            
            if (!actionSucceeds || actionSolutions.isEmpty()) {
                // Action failed for this condition - forall fails
                return false;
            }
        }
        
        // All actions succeeded - forall succeeds
        solutions.add(new HashMap<>(bindings));
        return true;
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("forall/2 requires context");
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