package it.denzosoft.jprolog.builtin.database;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.BuiltInRegistry;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.Arrays;

// START_CHANGE: ISS-2025-0366 - shared ISO validation helpers for the clause-database built-ins
// (asserta/assertz/retract/retractall/abolish/clause). Also serves ISS-2025-0367 (static-procedure
// protection) and ISS-2025-0368 (assert argument validation) on the legacy-engine paths.
/**
 * ISO 13211-1 argument validation shared by the clause-database built-ins (8.9.1.3, 8.9.2.3,
 * 8.9.3.3, 8.9.4.3, 8.8.1.3):
 * <ul>
 *   <li>an unbound Clause (or clause head) raises {@code instantiation_error};</li>
 *   <li>a non-callable head or body goal raises {@code type_error(callable, T)};</li>
 *   <li>modifying or accessing a procedure the {@link BuiltInRegistry} claims as a built-in raises
 *       {@code permission_error(Operation, static_procedure|private_procedure, Name/Arity)}.</li>
 * </ul>
 */
final class DatabaseValidation {

    private DatabaseValidation() {
    }

    /**
     * Validate an assert/retract Clause argument (already resolved against the bindings):
     * unbound -> instantiation_error; for {@code (Head :- Body)} an unbound Head ->
     * instantiation_error; a non-callable head -> type_error(callable, Head); when
     * {@code checkBody} is set, a number/string in body goal position -> type_error(callable, G).
     *
     * @return the clause head (the term itself for a fact)
     */
    static Term checkClauseTerm(Term clauseTerm, String context, boolean checkBody) {
        if (clauseTerm instanceof Variable) {
            throw new PrologException(ISOErrorTerms.instantiationError(context));
        }
        Term head = clauseTerm;
        if (clauseTerm instanceof CompoundTerm && ":-".equals(((CompoundTerm) clauseTerm).getName())
                && ((CompoundTerm) clauseTerm).getArguments().size() == 2) {
            head = ((CompoundTerm) clauseTerm).getArguments().get(0);
            if (head instanceof Variable) {
                throw new PrologException(ISOErrorTerms.instantiationError(context));
            }
            if (checkBody) {
                checkBodyGoals(((CompoundTerm) clauseTerm).getArguments().get(1), context);
            }
        }
        if (!(head instanceof Atom) && !(head instanceof CompoundTerm)) {
            throw new PrologException(ISOErrorTerms.typeError("callable", head, context));
        }
        return head;
    }

    /**
     * Walk a clause body through {@code ','/2}, {@code ';'/2} and {@code '->'/2}: a number or
     * string in goal position raises type_error(callable, G) at assert time (ISO 7.6.2). Unbound
     * goals are legal (converted to call/1 at call time), as is any atom/compound.
     */
    static void checkBodyGoals(Term body, String context) {
        if (body instanceof CompoundTerm && ((CompoundTerm) body).getArguments().size() == 2) {
            String f = ((CompoundTerm) body).getName();
            if (",".equals(f) || ";".equals(f) || "->".equals(f)) {
                checkBodyGoals(((CompoundTerm) body).getArguments().get(0), context);
                checkBodyGoals(((CompoundTerm) body).getArguments().get(1), context);
                return;
            }
        }
        if (body instanceof Number || body instanceof PrologString) {
            throw new PrologException(ISOErrorTerms.typeError("callable", body, context));
        }
    }

    /**
     * Raise {@code permission_error(Operation, PermissionType, Name/Arity)} when the
     * {@link BuiltInRegistry} claims {@code Name/Arity} as a built-in procedure. User predicates
     * that merely share a name with a built-in at a different arity are NOT affected: the check is
     * exactly {@link BuiltInRegistry#isBuiltIn(String, int)}.
     */
    static void checkProcedureAccess(SolverContext solver, Term head, String operation,
                                     String permissionType, String context) {
        String functor;
        int arity;
        if (head instanceof Atom) {
            functor = ((Atom) head).getName();
            arity = 0;
        } else if (head instanceof CompoundTerm) {
            functor = ((CompoundTerm) head).getName();
            arity = ((CompoundTerm) head).getArguments().size();
        } else {
            return;
        }
        checkProcedureAccess(solver, functor, arity, operation, permissionType, context);
    }

    /** Same as {@link #checkProcedureAccess(SolverContext, Term, String, String, String)} by Name/Arity. */
    static void checkProcedureAccess(SolverContext solver, String functor, int arity,
                                     String operation, String permissionType, String context) {
        if (solver == null) {
            return;
        }
        BuiltInRegistry registry = solver.getBuiltInRegistry();
        if (registry != null && registry.isBuiltIn(functor, arity)) {
            Term pi = new CompoundTerm(new Atom("/"),
                    Arrays.asList(new Atom(functor), new Number((long) arity)));
            throw new PrologException(
                    ISOErrorTerms.permissionError(operation, permissionType, pi, context));
        }
    }
}
// END_CHANGE: ISS-2025-0366
