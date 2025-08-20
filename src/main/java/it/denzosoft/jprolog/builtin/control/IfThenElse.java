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
 * Implementation of the if-then-else construct (Condition -> Then ; Else).
 * 
 * In Prolog:
 * - If Condition succeeds, execute Then
 * - If Condition fails, execute Else
 * - The construct is deterministic: it commits to the first choice
 */
public class IfThenElse implements BuiltInWithContext {
    
    private final QuerySolver querySolver;
    
    public IfThenElse(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        return executeSemicolon(query, bindings, solutions, solver);
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("Context-dependent built-in ';' must be invoked with context");
    }
    
    private boolean executeSemicolon(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions, QuerySolver solver) {
        // This handles the semicolon (;) operator
        // The structure should be: ; (-> (Condition, Then), Else)
        
        if (!(query instanceof CompoundTerm)) {
            throw new PrologEvaluationException("Semicolon operator requires compound term structure");
        }
        
        CompoundTerm semicolonTerm = (CompoundTerm) query;
        if (semicolonTerm.getArguments().size() != 2) {
            throw new PrologEvaluationException("Semicolon operator requires exactly 2 arguments");
        }
        
        Term leftTerm = semicolonTerm.getArguments().get(0);
        Term elseTerm = semicolonTerm.getArguments().get(1);
        
        // Check if left term is an if-then construct (->)
        if (leftTerm instanceof CompoundTerm) {
            CompoundTerm leftCompound = (CompoundTerm) leftTerm;
            if (leftCompound.getFunctor().getName().equals("->") && leftCompound.getArguments().size() == 2) {
                // This is a proper if-then-else: (Condition -> Then ; Else)
                Term condition = leftCompound.getArguments().get(0);
                Term thenTerm = leftCompound.getArguments().get(1);
                
                return executeIfThenElse(condition, thenTerm, elseTerm, bindings, solutions, solver);
            }
        }
        
        // If not if-then-else, treat as simple disjunction (A ; B)
        return executeDisjunction(leftTerm, elseTerm, bindings, solutions, solver);
    }
    
    private boolean executeIfThenElse(Term condition, Term thenTerm, Term elseTerm, 
                                    Map<String, Term> bindings, List<Map<String, Term>> solutions, QuerySolver solver) {
        // Try to solve the condition
        List<Map<String, Term>> conditionSolutions = new ArrayList<>();
        boolean conditionSuccess = solver.solve(condition, new HashMap<>(bindings), conditionSolutions, CutStatus.notOccurred());
        
        if (conditionSuccess && !conditionSolutions.isEmpty()) {
            // Condition succeeded - execute Then part for each solution
            boolean success = false;
            for (Map<String, Term> conditionBinding : conditionSolutions) {
                List<Map<String, Term>> thenSolutions = new ArrayList<>();
                boolean thenSuccess = solver.solve(thenTerm, new HashMap<>(conditionBinding), thenSolutions, CutStatus.notOccurred());
                if (thenSuccess) {
                    solutions.addAll(thenSolutions);
                    success = true;
                }
            }
            return success;
        } else {
            // Condition failed - execute Else part
            List<Map<String, Term>> elseSolutions = new ArrayList<>();
            boolean elseSuccess = solver.solve(elseTerm, new HashMap<>(bindings), elseSolutions, CutStatus.notOccurred());
            if (elseSuccess) {
                solutions.addAll(elseSolutions);
            }
            return elseSuccess;
        }
    }
    
    private boolean executeDisjunction(Term leftTerm, Term rightTerm, 
                                     Map<String, Term> bindings, List<Map<String, Term>> solutions, QuerySolver solver) {
        boolean success = false;
        
        // Try left term first
        List<Map<String, Term>> leftSolutions = new ArrayList<>();
        boolean leftSuccess = solver.solve(leftTerm, new HashMap<>(bindings), leftSolutions, CutStatus.notOccurred());
        if (leftSuccess) {
            solutions.addAll(leftSolutions);
            success = true;
        }
        
        // Try right term
        List<Map<String, Term>> rightSolutions = new ArrayList<>();
        boolean rightSuccess = solver.solve(rightTerm, new HashMap<>(bindings), rightSolutions, CutStatus.notOccurred());
        if (rightSuccess) {
            solutions.addAll(rightSolutions);
            success = true;
        }
        
        return success;
    }
}