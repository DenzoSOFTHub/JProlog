package it.denzosoft.jprolog.builtin.database;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of retract/1 predicate.
 *
 * retract(+Clause)
 *
 * Non-deterministic: on backtracking, retracts the next matching clause.
 * Each solution removes one clause from the database and returns
 * its unification bindings.
 *
 * START_CHANGE: ISS-2025-0396 - documented legacy-engine gap
 * <p><b>Legacy-engine gap (ISS-2025-0396):</b> this builtin backs retract/1 only under
 * {@code -Djprolog.engine=legacy}. The eager built-in protocol (all solutions materialised in
 * one call, no redo hook) forces it to retract EVERY matching clause up front and return the
 * bindings as alternatives. Enumeration is therefore ISO-correct ({@code retract(p(X)), X == 2}
 * succeeds, findall drains the predicate), but the side effect is not one-clause-per-redo: a
 * query that commits early (e.g. {@code retract(p(X)), !}) has still removed ALL matching
 * clauses. Making this lazy would require redesigning the legacy BuiltIn protocol, so the gap
 * is accepted here; the engine implements retract/1 itself (Machine.retractClause) with the full
 * re-executable ISO 8.9.3 semantics and a real choice point, and never dispatches this class.
 * END_CHANGE: ISS-2025-0396
 */
public class Retract implements BuiltInWithContext {
    
    private final SolverContext querySolver;
    
    public Retract(SolverContext querySolver) {
        this.querySolver = querySolver;
    }
    
    @Override
    public boolean executeWithContext(SolverContext solver, Term query, 
                                    Map<String, Term> bindings, 
                                    List<Map<String, Term>> solutions) {
        
        if (query.getArguments().size() != 1) {
            throw new PrologException(createTypeError("callable", query, "retract/1 requires exactly one argument"));
        }
        
        Term clauseHead = query.getArguments().get(0).resolveBindings(bindings);
        
        // Check if clause head is instantiated
        if (clauseHead instanceof Variable) {
            throw new PrologException(createInstantiationError("retract/1: clause head must be instantiated"));
        }

        // START_CHANGE: ISS-2025-0366 - ISO 8.9.3.3: a non-callable clause/head raises
        // type_error(callable, T); an unbound head inside (Head :- Body) raises instantiation_error.
        Term checkedHead = DatabaseValidation.checkClauseTerm(clauseHead, "retract/1", false);
        // END_CHANGE: ISS-2025-0366
        // START_CHANGE: ISS-2025-0367 - ISO 8.9.3.3: a built-in procedure is static, raise
        // permission_error(modify, static_procedure, Name/Arity) instead of silently failing.
        DatabaseValidation.checkProcedureAccess(solver, checkedHead, "modify", "static_procedure", "retract/1");
        // END_CHANGE: ISS-2025-0367

        try {
            // START_CHANGE: ISS-2025-0164 - Non-deterministic retract/1
            // Retract all matching clauses and return each as a separate solution
            Prolog prolog = solver.getPrologContext();
            if (prolog != null) {
                List<Map<String, Term>> allBindings =
                    prolog.retractAllClausesWithBindings(clauseHead, bindings);

                if (!allBindings.isEmpty()) {
                    solutions.addAll(allBindings);
                    return true;
                } else {
                    return false; // No matching clause found
                }
            } else {
                throw new PrologException(createSystemError("retract/1: cannot access clause database"));
            }
            // END_CHANGE: ISS-2025-0164

        } catch (PrologException e) {
            throw e;
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw new PrologException(createSystemError("retract/1: " + e.getMessage()));
        }
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("retract/1 requires context");
    }
    
    private Term createInstantiationError(String context) {
        try {
            return new CompoundTerm(
                new Atom("error"),
                java.util.Arrays.asList(
                    new Atom("instantiation_error"),
                    new Atom(context)
                )
            );
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            return new Atom("instantiation_error");
        }
    }
    
    private Term createTypeError(String expectedType, Term culprit, String context) {
        try {
            return new CompoundTerm(
                new Atom("error"),
                java.util.Arrays.asList(
                    new CompoundTerm(
                        new Atom("type_error"),
                        java.util.Arrays.asList(
                            new Atom(expectedType),
                            culprit
                        )
                    ),
                    new Atom(context)
                )
            );
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            return new Atom("type_error");
        }
    }
    
    private Term createSystemError(String message) {
        try {
            return new CompoundTerm(
                new Atom("error"),
                java.util.Arrays.asList(
                    new Atom("system_error"),
                    new Atom(message)
                )
            );
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            return new Atom("system_error");
        }
    }
}