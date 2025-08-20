package it.denzosoft.jprolog.builtin.database;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of retractall/1 predicate.
 * 
 * retractall(+ClauseHead)
 * 
 * Remove all clauses from the database that unify with ClauseHead.
 * The predicate always succeeds, even if no clauses are found.
 */
public class Retractall implements BuiltInWithContext {
    
    private final QuerySolver querySolver;
    
    public Retractall(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, 
                                    Map<String, Term> bindings, 
                                    List<Map<String, Term>> solutions) {
        
        if (query.getArguments().size() != 1) {
            throw new PrologException(createTypeError("callable", query, "retractall/1 requires exactly one argument"));
        }
        
        Term clauseHead = query.getArguments().get(0).resolveBindings(bindings);
        
        // Check if clause head is instantiated
        if (clauseHead instanceof Variable) {
            throw new PrologException(createInstantiationError("retractall/1: clause head must be instantiated"));
        }
        
        try {
            // Remove all clauses that match the head
            it.denzosoft.jprolog.core.engine.Prolog prolog = solver.getPrologContext();
            if (prolog != null) {
                int removedCount = prolog.retractAllClauses(clauseHead);
                
                // retractall/1 always succeeds, regardless of how many clauses were removed
                solutions.add(new HashMap<>(bindings));
                return true;
                
            } else {
                throw new PrologException(createSystemError("retractall/1: cannot access clause database"));
            }
            
        } catch (Exception e) {
            throw new PrologException(createSystemError("retractall/1: " + e.getMessage()));
        }
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("retractall/1 requires context");
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
            return new Atom("system_error");
        }
    }
}