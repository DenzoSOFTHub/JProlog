package it.denzosoft.jprolog.builtin.control;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.CutStatus;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of the conjunction operator (,).
 * 
 * In Prolog, the comma operator represents logical AND:
 * - Both left and right goals must succeed
 * - Variable bindings from the left goal are passed to the right goal
 * - All combinations of solutions are generated
 */
public class Conjunction implements BuiltInWithContext {
    
    private final QuerySolver querySolver;
    
    public Conjunction(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        return executeConjunction(query, bindings, solutions, solver);
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("Context-dependent built-in ',' must be invoked with context");
    }
    
    private boolean executeConjunction(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions, QuerySolver solver) {
        if (!(query instanceof CompoundTerm)) {
            throw new PrologEvaluationException("Conjunction operator requires compound term structure");
        }
        
        CompoundTerm conjunctionTerm = (CompoundTerm) query;
        if (conjunctionTerm.getArguments().size() != 2) {
            throw new PrologEvaluationException("Conjunction operator requires exactly 2 arguments");
        }
        
        Term leftGoal = conjunctionTerm.getArguments().get(0);
        Term rightGoal = conjunctionTerm.getArguments().get(1);
        
        // Solve the left goal first
        List<Map<String, Term>> leftSolutions = new ArrayList<>();
        boolean leftSuccess = solver.solve(leftGoal, new HashMap<>(bindings), leftSolutions, CutStatus.notOccurred());
        
        if (!leftSuccess || leftSolutions.isEmpty()) {
            // Left goal failed, so the entire conjunction fails
            return false;
        }
        
        // For each solution of the left goal, try the right goal
        boolean anySuccess = false;
        for (Map<String, Term> leftBinding : leftSolutions) {
            List<Map<String, Term>> rightSolutions = new ArrayList<>();
            boolean rightSuccess = solver.solve(rightGoal, new HashMap<>(leftBinding), rightSolutions, CutStatus.notOccurred());
            
            if (rightSuccess && !rightSolutions.isEmpty()) {
                // Add all solutions from the right goal that succeeded
                solutions.addAll(rightSolutions);
                anySuccess = true;
            }
        }
        
        return anySuccess;
    }
}