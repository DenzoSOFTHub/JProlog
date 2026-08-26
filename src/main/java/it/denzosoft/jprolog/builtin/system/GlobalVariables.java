// START_CHANGE: LIM-003 - Global Variables (nb_setval/2, nb_getval/2, nb_current/2, nb_delete/1, b_setval/2, b_getval/2)
package it.denzosoft.jprolog.builtin.system;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of global variable predicates:
 * <ul>
 *   <li>nb_setval(+Name, +Value) - Set non-backtrackable global variable</li>
 *   <li>nb_getval(+Name, -Value) - Get non-backtrackable global variable</li>
 *   <li>nb_current(?Name, ?Value) - Enumerate global variables</li>
 *   <li>nb_delete(+Name) - Delete a global variable</li>
 *   <li>b_setval(+Name, +Value) - Set backtrackable global variable (alias for nb_setval)</li>
 *   <li>b_getval(+Name, -Value) - Get backtrackable global variable (alias for nb_getval)</li>
 * </ul>
 *
 * Backtrackability:
 * - nb_setval/nb_getval: non-backtrackable (value persists across backtracks).
 * - b_setval/b_getval: backtrackable via Trail engine (R1, v2.9.0+). On failure of
 *   the choice point containing the b_setval, the previous value (or absence) is
 *   restored via {@link it.denzosoft.jprolog.core.engine.Trail#record}.
 */
public class GlobalVariables implements BuiltInWithContext {

    public enum Mode {
        NB_SETVAL, NB_GETVAL, NB_CURRENT, NB_DELETE,
        B_SETVAL, B_GETVAL
    }

    private final Mode mode;

    public GlobalVariables(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        // Requires context - should not be called directly
        return false;
    }

    @Override
    public boolean executeWithContext(SolverContext solver, Term query, Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        if (solver.getPrologContext() == null) {
            return false;
        }

        List<Term> args = query.getArguments();

        switch (mode) {
            case NB_SETVAL:
            case B_SETVAL: {
                boolean ok = executeSetval(solver, args, bindings);
                // START_CHANGE: R1 - emit bindings as solution so conjunction continues
                if (ok) solutions.add(new HashMap<>(bindings));
                return ok;
                // END_CHANGE: R1
            }

            case NB_GETVAL:
            case B_GETVAL:
                return executeGetval(solver, args, bindings, solutions);

            case NB_CURRENT:
                return executeCurrent(solver, args, bindings, solutions);

            case NB_DELETE: {
                boolean okd = executeDelete(solver, args, bindings);
                if (okd) solutions.add(new HashMap<>(bindings));
                return okd;
            }

            default:
                return false;
        }
    }

    private boolean executeSetval(SolverContext solver, List<Term> args, Map<String, Term> bindings) {
        if (args == null || args.size() != 2) {
            throw new PrologException(ISOErrorTerms.typeError("callable",
                new Atom("nb_setval"), "nb_setval/2 requires exactly 2 arguments"));
        }

        Term nameTerm = args.get(0).resolveBindings(bindings);
        Term valueTerm = args.get(1).resolveBindings(bindings);

        if (nameTerm instanceof Variable) {
            throw new PrologException(ISOErrorTerms.instantiationError(
                mode == Mode.B_SETVAL ? "b_setval/2" : "nb_setval/2"));
        }

        if (!(nameTerm instanceof Atom)) {
            throw new PrologException(ISOErrorTerms.typeError("atom", nameTerm,
                mode == Mode.B_SETVAL ? "b_setval/2" : "nb_setval/2"));
        }

        String name = ((Atom) nameTerm).getName();
        // START_CHANGE: R1 - b_setval records undo on Trail; nb_setval does not
        if (mode == Mode.B_SETVAL) {
            final Term oldValue = solver.getPrologContext().nbGetval(name);
            final it.denzosoft.jprolog.core.engine.Prolog ctx = solver.getPrologContext();
            it.denzosoft.jprolog.core.engine.Trail.record(() -> {
                if (oldValue == null) ctx.nbDelete(name);
                else ctx.nbSetval(name, oldValue);
            });
        }
        solver.getPrologContext().nbSetval(name, valueTerm);
        return true;
        // END_CHANGE: R1
    }

    private boolean executeGetval(SolverContext solver, List<Term> args, Map<String, Term> bindings,
                                  List<Map<String, Term>> solutions) {
        if (args == null || args.size() != 2) {
            throw new PrologException(ISOErrorTerms.typeError("callable",
                new Atom("nb_getval"), "nb_getval/2 requires exactly 2 arguments"));
        }

        Term nameTerm = args.get(0).resolveBindings(bindings);
        Term valueTerm = args.get(1).resolveBindings(bindings);

        if (nameTerm instanceof Variable) {
            throw new PrologException(ISOErrorTerms.instantiationError(
                mode == Mode.B_GETVAL ? "b_getval/2" : "nb_getval/2"));
        }

        if (!(nameTerm instanceof Atom)) {
            throw new PrologException(ISOErrorTerms.typeError("atom", nameTerm,
                mode == Mode.B_GETVAL ? "b_getval/2" : "nb_getval/2"));
        }

        String name = ((Atom) nameTerm).getName();
        Term storedValue = solver.getPrologContext().nbGetval(name);

        if (storedValue == null) {
            throw new PrologException(ISOErrorTerms.existenceError("variable",
                nameTerm, mode == Mode.B_GETVAL ? "b_getval/2" : "nb_getval/2",
                "Global variable '" + name + "' does not exist"));
        }

        // Unify the stored value with the second argument
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (storedValue.unify(valueTerm, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    private boolean executeCurrent(SolverContext solver, List<Term> args, Map<String, Term> bindings,
                                   List<Map<String, Term>> solutions) {
        if (args == null || args.size() != 2) {
            throw new PrologException(ISOErrorTerms.typeError("callable",
                new Atom("nb_current"), "nb_current/2 requires exactly 2 arguments"));
        }

        Term nameTerm = args.get(0).resolveBindings(bindings);
        Term valueTerm = args.get(1).resolveBindings(bindings);

        Map<String, Term> allVars = solver.getPrologContext().nbCurrentAll();

        if (nameTerm instanceof Atom) {
            // Specific name given - check if it exists and unify value
            String name = ((Atom) nameTerm).getName();
            Term storedValue = allVars.get(name);
            if (storedValue != null) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (storedValue.unify(valueTerm, newBindings)) {
                    solutions.add(newBindings);
                    return true;
                }
            }
            return false;
        }

        // Enumerate all global variables (non-deterministic)
        boolean found = false;
        for (Map.Entry<String, Term> entry : allVars.entrySet()) {
            Map<String, Term> newBindings = new HashMap<>(bindings);
            Atom nameAtom = new Atom(entry.getKey());
            if (nameAtom.unify(nameTerm, newBindings) && entry.getValue().unify(valueTerm, newBindings)) {
                solutions.add(newBindings);
                found = true;
            }
        }
        return found;
    }

    private boolean executeDelete(SolverContext solver, List<Term> args, Map<String, Term> bindings) {
        if (args == null || args.size() != 1) {
            throw new PrologException(ISOErrorTerms.typeError("callable",
                new Atom("nb_delete"), "nb_delete/1 requires exactly 1 argument"));
        }

        Term nameTerm = args.get(0).resolveBindings(bindings);

        if (nameTerm instanceof Variable) {
            throw new PrologException(ISOErrorTerms.instantiationError("nb_delete/1"));
        }

        if (!(nameTerm instanceof Atom)) {
            throw new PrologException(ISOErrorTerms.typeError("atom", nameTerm, "nb_delete/1"));
        }

        String name = ((Atom) nameTerm).getName();
        solver.getPrologContext().nbDelete(name);
        return true;
    }
}
// END_CHANGE: LIM-003
