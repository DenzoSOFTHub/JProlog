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
 * Implementation of the if-then construct (Condition -> Then).
 * 
 * In Prolog:
 * - If Condition succeeds, execute Then
 * - If Condition fails, the whole construct fails
 * - This is typically used within if-then-else (Condition -> Then ; Else)
 */
public class IfThen implements BuiltInWithContext {
    
    private final QuerySolver querySolver;
    
    public IfThen(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        return executeIfThen(query, bindings, solutions, solver);
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("Context-dependent built-in '->' must be invoked with context");
    }
    
    // START_CHANGE: ISS-2025-0190 - ISO: commit to first condition solution, add cut propagation
    private boolean executeIfThen(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions, QuerySolver solver) {
        if (!(query instanceof CompoundTerm)) {
            throw new PrologEvaluationException("If-then operator requires compound term structure");
        }

        CompoundTerm ifThenTerm = (CompoundTerm) query;
        if (ifThenTerm.getArguments().size() != 2) {
            throw new PrologEvaluationException("If-then operator requires exactly 2 arguments");
        }

        Term condition = ifThenTerm.getArguments().get(0);
        Term thenTerm = ifThenTerm.getArguments().get(1);

        // Get parent cut status from solver for propagation
        CutStatus parentCutStatus = solver.getCurrentCutStatus();

        // Try to solve the condition (cut inside condition is scoped per ISO)
        List<Map<String, Term>> conditionSolutions = new ArrayList<>();
        boolean conditionSuccess = solver.solve(condition, new HashMap<>(bindings), conditionSolutions, CutStatus.notOccurred());

        if (conditionSuccess && !conditionSolutions.isEmpty()) {
            // ISO Prolog: standalone (->) commits to the FIRST condition solution
            Map<String, Term> firstConditionBinding = conditionSolutions.get(0);
            List<Map<String, Term>> thenSolutions = new ArrayList<>();
            CutStatus thenCutStatus = CutStatus.notOccurred();
            boolean thenSuccess = solver.solve(thenTerm, new HashMap<>(firstConditionBinding), thenSolutions, thenCutStatus);
            if (thenSuccess) {
                solutions.addAll(thenSolutions);
            }
            // Propagate cut from then-branch to enclosing clause
            if (thenCutStatus.isCutOccurred() && parentCutStatus != null) {
                parentCutStatus.setCutOccurred();
            }
            return thenSuccess;
        } else {
            // Condition failed - whole construct fails
            return false;
        }
    }
    // END_CHANGE: ISS-2025-0190
}