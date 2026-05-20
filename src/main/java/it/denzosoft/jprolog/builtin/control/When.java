// START_CHANGE: LIM-001 - Coroutining: when/2
package it.denzosoft.jprolog.builtin.control;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.CutStatus;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Implements when/2: when(Condition, Goal).
 * Delays execution of Goal until Condition becomes true.
 *
 * Supported conditions:
 * - nonvar(X): true when X becomes bound
 * - ground(X): true when X becomes ground
 * - (Cond1, Cond2): both conditions must be met
 * - (Cond1 ; Cond2): either condition must be met
 */
public class When implements BuiltInWithContext {

    public static final String WHEN_MODULE = "when";

    private final QuerySolver solver;

    public When(QuerySolver solver) {
        this.solver = solver;
    }

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, Map<String, Term> bindings,
                                      List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args == null || args.size() != 2) return false;

        Term condition = args.get(0).resolveBindings(bindings);
        Term goal = args.get(1);

        // Check if condition is already satisfied
        if (isConditionSatisfied(condition, bindings)) {
            // Execute goal immediately
            Term resolvedGoal = goal.resolveBindings(bindings);
            List<Map<String, Term>> goalSolutions = new ArrayList<>();
            CutStatus cutStatus = CutStatus.notOccurred();
            if (solver.solve(resolvedGoal, bindings, goalSolutions, cutStatus)) {
                solutions.addAll(goalSolutions);
                return true;
            }
            return false;
        }

        // Condition not yet satisfied — find unbound variables in condition and attach goals
        Set<Variable> unboundVars = new HashSet<>();
        collectUnboundVariables(condition, bindings, unboundVars);

        if (unboundVars.isEmpty()) {
            // No unbound variables but condition not satisfied — fail
            return false;
        }

        // Store the when goal as an attribute on all relevant unbound variables
        Term resolvedGoal = goal.resolveBindings(bindings);
        // Store the full when(Condition, Goal) as attribute so we can re-check condition
        Term whenTerm = new CompoundTerm(new Atom("when"), java.util.Arrays.asList(condition, resolvedGoal));
        for (Variable var : unboundVars) {
            var.putAttribute(WHEN_MODULE, whenTerm);
        }

        solutions.add(new HashMap<>(bindings));
        return true;
    }

    /**
     * Check if a when-condition is currently satisfied.
     */
    public static boolean isConditionSatisfied(Term condition, Map<String, Term> bindings) {
        if (condition instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) condition;
            String name = ct.getName();
            List<Term> args = ct.getArguments();

            if ("nonvar".equals(name) && args != null && args.size() == 1) {
                Term resolved = resolveToEnd(args.get(0), bindings);
                return !(resolved instanceof Variable);
            }

            if ("ground".equals(name) && args != null && args.size() == 1) {
                Term resolved = args.get(0).resolveBindings(bindings);
                return isGround(resolved);
            }

            if (",".equals(name) && args != null && args.size() == 2) {
                return isConditionSatisfied(args.get(0), bindings)
                    && isConditionSatisfied(args.get(1), bindings);
            }

            if (";".equals(name) && args != null && args.size() == 2) {
                return isConditionSatisfied(args.get(0), bindings)
                    || isConditionSatisfied(args.get(1), bindings);
            }
        }
        return false;
    }

    /**
     * Check if a when-condition is satisfied given the attribute's stored condition and current bindings.
     */
    public static boolean executeWhenGoal(QuerySolver solver, Term whenTerm, Map<String, Term> bindings) {
        if (whenTerm instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) whenTerm;
            if ("when".equals(ct.getName()) && ct.getArguments() != null && ct.getArguments().size() == 2) {
                Term condition = ct.getArguments().get(0);
                Term goal = ct.getArguments().get(1);

                if (isConditionSatisfied(condition, bindings)) {
                    Term resolvedGoal = goal.resolveBindings(bindings);
                    List<Map<String, Term>> solutions = new ArrayList<>();
                    CutStatus cutStatus = CutStatus.notOccurred();
                    return solver.solve(resolvedGoal, bindings, solutions, cutStatus);
                }
                // START_CHANGE: v2.9.4 - re-suspend with RESOLVED condition + goal so cross-solve hooks still see prior bindings
                Term resolvedCondition = condition.resolveBindings(bindings);
                Term resolvedGoal = goal.resolveBindings(bindings);
                Term resuspendedWhen = new CompoundTerm(new Atom("when"),
                    java.util.Arrays.asList(resolvedCondition, resolvedGoal));
                Set<Variable> remaining = new HashSet<>();
                collectRemainingUnbound(resolvedCondition, bindings, remaining);
                for (Variable v : remaining) {
                    v.putAttribute(WHEN_MODULE, resuspendedWhen);
                }
                return true;
                // END_CHANGE: v2.9.4
            }
        }
        return true;
    }

    // START_CHANGE: ISS-2025-0247 - static helper for re-suspension
    private static void collectRemainingUnbound(Term condition, Map<String, Term> bindings, Set<Variable> out) {
        if (condition instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) condition;
            String name = ct.getName();
            List<Term> args = ct.getArguments();
            if (("nonvar".equals(name) || "ground".equals(name)) && args != null && args.size() == 1) {
                collectVarsStatic(args.get(0), bindings, out);
            } else if ((",".equals(name) || ";".equals(name)) && args != null && args.size() == 2) {
                collectRemainingUnbound(args.get(0), bindings, out);
                collectRemainingUnbound(args.get(1), bindings, out);
            }
        }
    }
    private static void collectVarsStatic(Term term, Map<String, Term> bindings, Set<Variable> out) {
        Term r = resolveToEnd(term, bindings);
        if (r instanceof Variable) out.add((Variable) r);
        else if (r instanceof CompoundTerm) {
            for (Term a : ((CompoundTerm) r).getArguments()) collectVarsStatic(a, bindings, out);
        }
    }
    // END_CHANGE: ISS-2025-0247

    /**
     * Collect all unbound variables referenced in a condition.
     */
    private void collectUnboundVariables(Term condition, Map<String, Term> bindings, Set<Variable> vars) {
        if (condition instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) condition;
            String name = ct.getName();
            List<Term> args = ct.getArguments();

            if (("nonvar".equals(name) || "ground".equals(name)) && args != null && args.size() == 1) {
                collectVarsFromTerm(args.get(0), bindings, vars);
            } else if ((",".equals(name) || ";".equals(name)) && args != null && args.size() == 2) {
                collectUnboundVariables(args.get(0), bindings, vars);
                collectUnboundVariables(args.get(1), bindings, vars);
            }
        }
    }

    /**
     * Collect unbound variables from a term.
     */
    private void collectVarsFromTerm(Term term, Map<String, Term> bindings, Set<Variable> vars) {
        Term resolved = resolveToEnd(term, bindings);
        if (resolved instanceof Variable) {
            vars.add((Variable) resolved);
        } else if (resolved instanceof CompoundTerm) {
            for (Term arg : ((CompoundTerm) resolved).getArguments()) {
                collectVarsFromTerm(arg, bindings, vars);
            }
        }
    }

    private static Term resolveToEnd(Term term, Map<String, Term> bindings) {
        Term current = term;
        while (current instanceof Variable) {
            String varName = ((Variable) current).getName();
            Term bound = bindings.get(varName);
            if (bound == null) return current;
            current = bound;
        }
        return current;
    }

    private static boolean isGround(Term term) {
        if (term instanceof Variable) return false;
        if (term instanceof CompoundTerm) {
            for (Term arg : ((CompoundTerm) term).getArguments()) {
                if (!isGround(arg)) return false;
            }
        }
        return true;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException(
            "Context-dependent built-in 'when' must be invoked with context");
    }
}
// END_CHANGE: LIM-001
