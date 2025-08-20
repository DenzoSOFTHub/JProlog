package it.denzosoft.jprolog.builtin.exception;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

/**
 * Implementation of throw/1 predicate.
 * 
 * throw(+Ball)
 * 
 * Throw an exception with the given term. This exception can be caught
 * by catch/3 if the Ball unifies with the catcher pattern.
 */
public class Throw implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        
        if (query.getArguments().size() != 1) {
            throw new PrologException(createInstantiationError("throw/1 requires exactly one argument"));
        }
        
        Term ball = query.getArguments().get(0).resolveBindings(bindings);
        
        // Check if the ball is instantiated
        if (ball instanceof it.denzosoft.jprolog.core.terms.Variable) {
            throw new PrologException(createInstantiationError("throw/1: argument must be instantiated"));
        }
        
        // Throw the Prolog exception
        throw new PrologException(ball);
    }
    
    private Term createInstantiationError(String context) {
        // Create an instantiation_error term according to ISO standard
        try {
            return new it.denzosoft.jprolog.core.terms.CompoundTerm(
                new it.denzosoft.jprolog.core.terms.Atom("error"),
                java.util.Arrays.asList(
                    new it.denzosoft.jprolog.core.terms.Atom("instantiation_error"),
                    new it.denzosoft.jprolog.core.terms.Atom(context)
                )
            );
        } catch (Exception e) {
            return new it.denzosoft.jprolog.core.terms.Atom("instantiation_error");
        }
    }
}