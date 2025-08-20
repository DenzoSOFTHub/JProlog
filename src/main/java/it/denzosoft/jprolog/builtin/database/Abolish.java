package it.denzosoft.jprolog.builtin.database;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of abolish/1 predicate.
 * 
 * abolish(+PredicateIndicator)
 * 
 * Remove all clauses for the specified predicate.
 * PredicateIndicator should be of the form Functor/Arity.
 */
public class Abolish implements BuiltInWithContext {
    
    private final QuerySolver querySolver;
    
    public Abolish(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, 
                                    Map<String, Term> bindings, 
                                    List<Map<String, Term>> solutions) {
        
        if (query.getArguments().size() != 1) {
            throw new PrologException(createTypeError("callable", query, "abolish/1 requires exactly one argument"));
        }
        
        Term predicateIndicator = query.getArguments().get(0).resolveBindings(bindings);
        
        // Check if predicate indicator is instantiated
        if (predicateIndicator instanceof Variable) {
            throw new PrologException(createInstantiationError("abolish/1: predicate indicator must be instantiated"));
        }
        
        // Parse predicate indicator (Functor/Arity)
        String functor;
        int arity;
        
        if (predicateIndicator instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) predicateIndicator;
            
            if (compound.getFunctor().getName().equals("/") && compound.getArguments().size() == 2) {
                Term functorTerm = compound.getArguments().get(0);
                Term arityTerm = compound.getArguments().get(1);
                
                if (!(functorTerm instanceof Atom)) {
                    throw new PrologException(createTypeError("atom", functorTerm, "abolish/1: functor must be an atom"));
                }
                
                if (!(arityTerm instanceof Number)) {
                    throw new PrologException(createTypeError("integer", arityTerm, "abolish/1: arity must be an integer"));
                }
                
                functor = ((Atom) functorTerm).getName();
                arity = (int) Math.round(((Number) arityTerm).getValue());
                
                if (arity < 0) {
                    throw new PrologException(createDomainError("not_less_than_zero", arityTerm, "abolish/1: arity must be non-negative"));
                }
                
            } else {
                throw new PrologException(createTypeError("predicate_indicator", predicateIndicator, "abolish/1: argument must be Functor/Arity"));
            }
            
        } else {
            throw new PrologException(createTypeError("predicate_indicator", predicateIndicator, "abolish/1: argument must be Functor/Arity"));
        }
        
        try {
            // Remove all clauses for the predicate
            Prolog prolog = solver.getPrologContext();
            if (prolog != null) {
                int removedCount = prolog.abolishPredicate(functor, arity);
                
                // abolish/1 always succeeds
                solutions.add(new HashMap<>(bindings));
                return true;
                
            } else {
                throw new PrologException(createSystemError("abolish/1: cannot access clause database"));
            }
            
        } catch (Exception e) {
            throw new PrologException(createSystemError("abolish/1: " + e.getMessage()));
        }
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("abolish/1 requires context");
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
    
    private Term createDomainError(String domain, Term culprit, String context) {
        try {
            return new CompoundTerm(
                new Atom("error"),
                java.util.Arrays.asList(
                    new CompoundTerm(
                        new Atom("domain_error"),
                        java.util.Arrays.asList(
                            new Atom(domain),
                            culprit
                        )
                    ),
                    new Atom(context)
                )
            );
        } catch (Exception e) {
            return new Atom("domain_error");
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