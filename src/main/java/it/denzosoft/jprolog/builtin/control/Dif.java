// START_CHANGE: LIM-001 - Coroutining: dif/2
package it.denzosoft.jprolog.builtin.control;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.SolverContext;
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
 * Implements dif/2: dif(X, Y).
 * Constrains X and Y to be different.
 * - If both are ground and equal, fails.
 * - If both are ground and different, succeeds.
 * - Otherwise, delays using attributed variables until both become ground.
 */
public class Dif implements BuiltInWithContext {

    public static final String DIF_MODULE = "dif";

    private final SolverContext solver;

    public Dif(SolverContext solver) {
        this.solver = solver;
    }

    @Override
    public boolean executeWithContext(SolverContext solver, Term query, Map<String, Term> bindings,
                                      List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args == null || args.size() != 2) return false;

        Term x = args.get(0).resolveBindings(bindings);
        Term y = args.get(1).resolveBindings(bindings);

        // Try to unify X and Y in a temporary substitution
        Map<String, Term> tempBindings = new HashMap<>(bindings);
        if (!x.unify(y, tempBindings)) {
            // Cannot unify — they are definitely different, succeed
            solutions.add(new HashMap<>(bindings));
            return true;
        }

        // They could unify. Check if they are already ground.
        if (isGround(x) && isGround(y)) {
            // Both ground and unifiable — they are equal, fail
            return false;
        }

        // Not yet determinable — delay using attributed variables
        // Collect unbound variables from both X and Y
        Set<Variable> unboundVars = new HashSet<>();
        collectUnboundVars(args.get(0), bindings, unboundVars);
        collectUnboundVars(args.get(1), bindings, unboundVars);

        if (unboundVars.isEmpty()) {
            // No unbound vars but we got here — the terms are equal, fail
            return false;
        }

        // Store dif constraint as an attribute on all relevant variables
        // Store the original (unreduced) terms so we can re-check after binding
        Term difTerm = new CompoundTerm(new Atom("dif"), java.util.Arrays.asList(args.get(0), args.get(1)));
        for (Variable var : unboundVars) {
            var.putAttribute(DIF_MODULE, difTerm);
        }

        solutions.add(new HashMap<>(bindings));
        return true;
    }

    /**
     * Re-check a dif constraint when one of its variables gets bound.
     * Called by the attribute unification hook.
     * @return true if the constraint still holds or is still delayed, false if violated
     */
    public static boolean checkDifConstraint(SolverContext solver, Term difTerm, Map<String, Term> bindings) {
        if (difTerm instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) difTerm;
            if ("dif".equals(ct.getName()) && ct.getArguments() != null && ct.getArguments().size() == 2) {
                Term x = ct.getArguments().get(0).resolveBindings(bindings);
                Term y = ct.getArguments().get(1).resolveBindings(bindings);

                // Try unification in a temporary map
                Map<String, Term> tempBindings = new HashMap<>(bindings);
                if (!x.unify(y, tempBindings)) {
                    // Definitely different — constraint satisfied, remove attributes
                    return true;
                }

                // Still unifiable. Check if both ground.
                if (isGround(x) && isGround(y)) {
                    // Both ground and unifiable — they are equal, constraint violated
                    return false;
                }

                // Still delayed — re-attach to any remaining unbound variables
                Set<Variable> unboundVars = new HashSet<>();
                collectUnboundVarsStatic(ct.getArguments().get(0), bindings, unboundVars);
                collectUnboundVarsStatic(ct.getArguments().get(1), bindings, unboundVars);
                for (Variable var : unboundVars) {
                    var.putAttribute(DIF_MODULE, difTerm);
                }
                return true;
            }
        }
        return true;
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

    private void collectUnboundVars(Term term, Map<String, Term> bindings, Set<Variable> vars) {
        collectUnboundVarsStatic(term, bindings, vars);
    }

    private static void collectUnboundVarsStatic(Term term, Map<String, Term> bindings, Set<Variable> vars) {
        Term current = term;
        while (current instanceof Variable) {
            String varName = ((Variable) current).getName();
            Term bound = bindings.get(varName);
            if (bound == null) {
                vars.add((Variable) current);
                return;
            }
            current = bound;
        }
        if (current instanceof CompoundTerm) {
            for (Term arg : ((CompoundTerm) current).getArguments()) {
                collectUnboundVarsStatic(arg, bindings, vars);
            }
        }
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException(
            "Context-dependent built-in 'dif' must be invoked with context");
    }
}
// END_CHANGE: LIM-001
