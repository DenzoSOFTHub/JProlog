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
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Implementation of catch/3 predicate.
 *
 * catch(+Goal, +Catcher, +Recovery)
 *
 * Execute Goal. If an exception is thrown that unifies with Catcher,
 * execute Recovery instead.
 *
 * <p><b>Design decision - Stream cleanup on exception:</b>
 * Per ISO Prolog standard behavior, streams opened inside Goal are NOT
 * automatically closed when an exception is thrown. Stream lifecycle
 * management is the responsibility of the Prolog programmer (e.g., using
 * setup_call_cleanup/3). This is consistent with SICStus, SWI-Prolog,
 * and other conforming implementations.</p>
 *
 * <p><b>Nested exception handling:</b>
 * If Recovery itself throws an exception, that exception propagates upward
 * and is NOT caught by this catch/3 invocation. This is correct ISO behavior.</p>
 */
// START_CHANGE: ISS-2025-0171 - Improved catch/3 exception handling and diagnostics
public class Catch implements BuiltInWithContext {

    private static final Logger LOGGER = Logger.getLogger(Catch.class.getName());

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
                    // Exception matches - execute recovery goal.
                    // NOTE: If Recovery itself throws an exception, it propagates
                    // upward uncaught by this catch/3 — this is correct ISO behavior.
                    List<Map<String, Term>> recoverySolutions = new ArrayList<>();
                    boolean recoverySuccess = solver.solve(recovery, catcherBindings, recoverySolutions, CutStatus.notOccurred());

                    if (recoverySuccess) {
                        solutions.addAll(recoverySolutions);
                        return true;
                    }
                    return false;
                }
            }

            // Exception doesn't match - re-throw
            throw prologEx;

        } catch (Exception javaEx) {
            // Log the full stack trace so programming errors are not silently masked.
            // The original exception details (class name, message) are included in the
            // system_error term for diagnostic purposes.
            LOGGER.log(Level.SEVERE, "Java exception caught in catch/3: "
                    + javaEx.getClass().getName() + ": " + javaEx.getMessage(), javaEx);
            throw new PrologException(createSystemErrorTerm(javaEx));
        }
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("catch/3 requires context");
    }

    private Term createSystemErrorTerm(Exception javaEx) {
        // Include the Java exception class name and message for better diagnostics
        String exClassName = javaEx.getClass().getName();
        String exMessage = javaEx.getMessage() != null ? javaEx.getMessage() : "null";
        String description = exClassName + ": " + exMessage;
        return ISOErrorTerms.systemError(description, "catch/3");
    }
}
// END_CHANGE: ISS-2025-0171