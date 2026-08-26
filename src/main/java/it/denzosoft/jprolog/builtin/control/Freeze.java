// START_CHANGE: LIM-001 - Coroutining: freeze/2
package it.denzosoft.jprolog.builtin.control;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.Arrays;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implements freeze/2: freeze(Var, Goal).
 * If Var is bound, execute Goal immediately.
 * If Var is unbound, store Goal as an attribute on Var under module "freeze".
 * When Var gets unified with a non-variable, the attribute unification hook
 * triggers and executes the stored Goal.
 */
public class Freeze implements BuiltInWithContext {

    /** The module key used to store freeze goals as attributes. */
    public static final String FREEZE_MODULE = "freeze";

    private final SolverContext solver;

    public Freeze(SolverContext solver) {
        this.solver = solver;
    }

    @Override
    public boolean executeWithContext(SolverContext solver, Term query, Map<String, Term> bindings,
                                      List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args == null || args.size() != 2) return false;

        Term varTerm = args.get(0);
        Term goal = args.get(1);

        // Dereference the variable through bindings
        Term derefVar = resolveToEnd(varTerm, bindings);

        if (derefVar instanceof Variable) {
            Variable var = (Variable) derefVar;
            // START_CHANGE: ISS-2025-0246 - aggregate multiple frozen goals as conjunction
            Term resolvedGoal = goal.resolveBindings(bindings);
            Term existing = var.getAttribute(FREEZE_MODULE);
            Term combined = existing == null
                ? resolvedGoal
                : new CompoundTerm(new Atom(","), Arrays.asList(existing, resolvedGoal));
            var.putAttribute(FREEZE_MODULE, combined);
            // END_CHANGE: ISS-2025-0246
            solutions.add(new HashMap<>(bindings));
            return true;
        } else {
            // Variable is already bound — execute the goal immediately
            Term resolvedGoal = goal.resolveBindings(bindings);
            List<Map<String, Term>> goalSolutions = new ArrayList<>();
            if (solver.solveMeta(resolvedGoal, bindings, goalSolutions)) {   // ISS-2025-0485
                solutions.addAll(goalSolutions);
                return true;
            }
            return false;
        }
    }

    /**
     * Execute a frozen goal. Called by the attribute unification hook.
     * @param solver the query solver
     * @param goal the frozen goal to execute
     * @param bindings current bindings
     * @return true if the goal succeeded
     */
    public static boolean executeFrozenGoal(SolverContext solver, Term goal, Map<String, Term> bindings) {
        List<Map<String, Term>> solutions = new ArrayList<>();
        return solver.solveMeta(goal, bindings, solutions);   // ISS-2025-0485
    }

    /**
     * Dereference a term through bindings to its final value.
     */
    private Term resolveToEnd(Term term, Map<String, Term> bindings) {
        Term current = term;
        while (current instanceof Variable) {
            String varName = ((Variable) current).getName();
            Term bound = bindings.get(varName);
            if (bound == null) {
                return current;
            }
            current = bound;
        }
        return current;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException(
            "Context-dependent built-in 'freeze' must be invoked with context");
    }
}
// END_CHANGE: LIM-001
