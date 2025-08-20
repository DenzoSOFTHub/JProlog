package it.denzosoft.jprolog.builtin.database;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Implementation of current_predicate/1 predicate.
 * 
 * current_predicate(?PredicateIndicator)
 * 
 * Succeeds if PredicateIndicator is a predicate indicator of a predicate
 * currently defined in the database.
 */
public class CurrentPredicate implements BuiltInWithContext {
    
    private final QuerySolver querySolver;
    
    public CurrentPredicate(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, 
                                    Map<String, Term> bindings, 
                                    List<Map<String, Term>> solutions) {
        
        if (query.getArguments().size() != 1) {
            throw new PrologException(createTypeError("callable", query, "current_predicate/1 requires exactly one argument"));
        }
        
        Term predicateIndicator = query.getArguments().get(0);
        
        try {
            // Get all current predicates
            Prolog prolog = solver.getPrologContext();
            if (prolog != null) {
                Set<String> currentPredicates = prolog.getCurrentPredicates();
                
                // Try to unify with each predicate indicator
                for (String predicateStr : currentPredicates) {
                    String[] parts = predicateStr.split("/");
                    if (parts.length == 2) {
                        String functor = parts[0];
                        int arity = Integer.parseInt(parts[1]);
                        
                        // Create predicate indicator term
                        Term indicatorTerm = new CompoundTerm(
                            new Atom("/"),
                            java.util.Arrays.asList(
                                new Atom(functor),
                                new Number(arity)
                            )
                        );
                        
                        // Try to unify
                        Map<String, Term> newBindings = new HashMap<>(bindings);
                        if (predicateIndicator.unify(indicatorTerm, newBindings)) {
                            solutions.add(newBindings);
                        }
                    }
                }
                
                return !solutions.isEmpty();
                
            } else {
                throw new PrologException(createSystemError("current_predicate/1: cannot access clause database"));
            }
            
        } catch (Exception e) {
            throw new PrologException(createSystemError("current_predicate/1: " + e.getMessage()));
        }
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("current_predicate/1 requires context");
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