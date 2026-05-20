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
 * Also handles simple disjunction (A ; B).
 *
 * In Prolog:
 * - If Condition succeeds, execute Then
 * - If Condition fails, execute Else
 * - The construct is deterministic: it commits to the first choice
 * - Cut (!) inside Then or Else branches propagates to enclosing clause (ISO 7.8.8)
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
                Term condition = leftCompound.getArguments().get(0);
                Term thenTerm = leftCompound.getArguments().get(1);

                return executeIfThenElse(condition, thenTerm, elseTerm, bindings, solutions, solver);
            }
            // START_CHANGE: ISS-2025-0201 - soft-cut (Cond *-> Then ; Else)
            if (leftCompound.getFunctor().getName().equals("*->") && leftCompound.getArguments().size() == 2) {
                Term condition = leftCompound.getArguments().get(0);
                Term thenTerm = leftCompound.getArguments().get(1);
                return executeSoftCut(condition, thenTerm, elseTerm, bindings, solutions, solver);
            }
            // END_CHANGE: ISS-2025-0201
        }

        // If not if-then-else, treat as simple disjunction (A ; B)
        return executeDisjunction(leftTerm, elseTerm, bindings, solutions, solver);
    }

    // START_CHANGE: ISS-2025-0163 - Propagate cut from Then/Else/disjunction branches to parent clause
    private boolean executeIfThenElse(Term condition, Term thenTerm, Term elseTerm,
                                    Map<String, Term> bindings, List<Map<String, Term>> solutions, QuerySolver solver) {
        // Get parent cut status from solver (set by handleBuiltIn before calling us)
        CutStatus parentCutStatus = solver.getCurrentCutStatus();

        // Try to solve the condition (cut inside condition is scoped to condition per ISO 7.8.8)
        List<Map<String, Term>> conditionSolutions = new ArrayList<>();
        boolean conditionSuccess = solver.solve(condition, new HashMap<>(bindings), conditionSolutions, CutStatus.notOccurred());

        if (conditionSuccess && !conditionSolutions.isEmpty()) {
            // ISO Prolog: (Cond -> Then ; Else) commits to the FIRST solution of Cond
            Map<String, Term> firstConditionBinding = conditionSolutions.get(0);
            List<Map<String, Term>> thenSolutions = new ArrayList<>();
            // Cut in Then branch must propagate to enclosing clause
            CutStatus thenCutStatus = CutStatus.notOccurred();
            boolean thenSuccess = solver.solve(thenTerm, new HashMap<>(firstConditionBinding), thenSolutions, thenCutStatus);
            if (thenSuccess) {
                solutions.addAll(thenSolutions);
            }
            if (thenCutStatus.isCutOccurred() && parentCutStatus != null) {
                parentCutStatus.setCutOccurred();
            }
            return thenSuccess;
        } else {
            // Condition failed - execute Else part
            List<Map<String, Term>> elseSolutions = new ArrayList<>();
            CutStatus elseCutStatus = CutStatus.notOccurred();
            boolean elseSuccess = solver.solve(elseTerm, new HashMap<>(bindings), elseSolutions, elseCutStatus);
            if (elseSuccess) {
                solutions.addAll(elseSolutions);
            }
            if (elseCutStatus.isCutOccurred() && parentCutStatus != null) {
                parentCutStatus.setCutOccurred();
            }
            return elseSuccess;
        }
    }

    // START_CHANGE: ISS-2025-0201 - soft cut: enumerate all condition solutions
    private boolean executeSoftCut(Term condition, Term thenTerm, Term elseTerm,
                                    Map<String, Term> bindings, List<Map<String, Term>> solutions, QuerySolver solver) {
        CutStatus parentCutStatus = solver.getCurrentCutStatus();
        List<Map<String, Term>> conditionSolutions = new ArrayList<>();
        solver.solve(condition, new HashMap<>(bindings), conditionSolutions, CutStatus.notOccurred());

        if (!conditionSolutions.isEmpty()) {
            boolean anyThenSuccess = false;
            for (Map<String, Term> cb : conditionSolutions) {
                List<Map<String, Term>> thenSolutions = new ArrayList<>();
                CutStatus thenCutStatus = CutStatus.notOccurred();
                boolean ok = solver.solve(thenTerm, new HashMap<>(cb), thenSolutions, thenCutStatus);
                if (ok) {
                    solutions.addAll(thenSolutions);
                    anyThenSuccess = true;
                }
                if (thenCutStatus.isCutOccurred()) {
                    if (parentCutStatus != null) parentCutStatus.setCutOccurred();
                    break;
                }
            }
            return anyThenSuccess;
        }
        List<Map<String, Term>> elseSolutions = new ArrayList<>();
        CutStatus elseCutStatus = CutStatus.notOccurred();
        boolean ok = solver.solve(elseTerm, new HashMap<>(bindings), elseSolutions, elseCutStatus);
        if (ok) solutions.addAll(elseSolutions);
        if (elseCutStatus.isCutOccurred() && parentCutStatus != null) {
            parentCutStatus.setCutOccurred();
        }
        return ok;
    }
    // END_CHANGE: ISS-2025-0201

    private boolean executeDisjunction(Term leftTerm, Term rightTerm,
                                     Map<String, Term> bindings, List<Map<String, Term>> solutions, QuerySolver solver) {
        CutStatus parentCutStatus = solver.getCurrentCutStatus();
        boolean success = false;

        // START_CHANGE: R1 - mark Trail before each branch; rollback on left failure
        int trailMark = it.denzosoft.jprolog.core.engine.Trail.mark();
        // END_CHANGE: R1

        List<Map<String, Term>> leftSolutions = new ArrayList<>();
        CutStatus leftCutStatus = CutStatus.notOccurred();
        boolean leftSuccess = solver.solve(leftTerm, new HashMap<>(bindings), leftSolutions, leftCutStatus);
        if (leftSuccess) {
            solutions.addAll(leftSolutions);
            success = true;
        } else {
            // START_CHANGE: R1 - undo any backtrackable mutations from failed left branch
            it.denzosoft.jprolog.core.engine.Trail.rollbackTo(trailMark);
            // END_CHANGE: R1
        }
        if (leftCutStatus.isCutOccurred()) {
            if (parentCutStatus != null) {
                parentCutStatus.setCutOccurred();
            }
            return success;
        }

        List<Map<String, Term>> rightSolutions = new ArrayList<>();
        CutStatus rightCutStatus = CutStatus.notOccurred();
        boolean rightSuccess = solver.solve(rightTerm, new HashMap<>(bindings), rightSolutions, rightCutStatus);
        if (rightSuccess) {
            solutions.addAll(rightSolutions);
            success = true;
        }
        if (rightCutStatus.isCutOccurred() && parentCutStatus != null) {
            parentCutStatus.setCutOccurred();
        }

        return success;
    }
    // END_CHANGE: ISS-2025-0163
}
