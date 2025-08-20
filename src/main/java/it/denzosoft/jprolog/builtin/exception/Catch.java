package it.denzosoft.jprolog.builtin.exception;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.CutStatus;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of catch/3 predicate.
 * 
 * catch(+Goal, +Catcher, +Recovery)
 * 
 * Execute Goal. If an exception is thrown that unifies with Catcher,
 * execute Recovery instead.
 */
public class Catch implements BuiltInWithContext {
    
    private final QuerySolver querySolver;
    
    public Catch(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, 
                                    Map<String, Term> bindings, 
                                    List<Map<String, Term>> solutions) {
        
        if (query.getArguments().size() != 3) {
            return false;
        }
        
        Term goal = query.getArguments().get(0);
        Term catcher = query.getArguments().get(1);
        Term recovery = query.getArguments().get(2);
        
        try {
            // Try to execute the goal
            List<Map<String, Term>> goalSolutions = new ArrayList<>();
            boolean success = solver.solve(goal, new HashMap<>(bindings), goalSolutions, CutStatus.notOccurred());
            
            if (success) {
                solutions.addAll(goalSolutions);
                return true;
            } else {
                return false;
            }
            
        } catch (PrologException prologEx) {
            // A Prolog exception was thrown
            if (prologEx.isHalt()) {
                // Re-throw halt exceptions
                throw prologEx;
            }
            
            Term thrownTerm = prologEx.getErrorTerm();
            if (thrownTerm != null) {
                // Try to unify the thrown term with the catcher
                Map<String, Term> catcherBindings = new HashMap<>(bindings);
                if (catcher.unify(thrownTerm, catcherBindings)) {
                    // Exception matches - execute recovery
                    List<Map<String, Term>> recoverySolutions = new ArrayList<>();
                    boolean recoverySuccess = solver.solve(recovery, catcherBindings, recoverySolutions, CutStatus.notOccurred());
                    
                    if (recoverySuccess) {
                        solutions.addAll(recoverySolutions);
                        return true;
                    }
                }
            }
            
            // Exception doesn't match or recovery failed - re-throw
            throw prologEx;
            
        } catch (Exception javaEx) {
            // Convert Java exceptions to Prolog exceptions
            // This maintains compatibility with existing code
            throw new PrologException(createSystemErrorTerm(javaEx.getMessage()));
        }
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("catch/3 requires context");
    }
    
    private Term createSystemErrorTerm(String message) {
        // Create a system_error term
        try {
            return new it.denzosoft.jprolog.core.terms.Atom("system_error('" + message + "')");
        } catch (Exception e) {
            return new it.denzosoft.jprolog.core.terms.Atom("system_error");
        }
    }
}