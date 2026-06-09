package it.denzosoft.jprolog.builtin.exception;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.util.TermCopier;

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
        
        // START_CHANGE: ISS-2025-0275 - ISO throw/1 throws a COPY of the ball (copy_term), so the
        // thrown term is independent of the variable bindings in the throwing context.
        throw new PrologException(TermCopier.copyWithFreshVariables(ball));
        // END_CHANGE: ISS-2025-0275
    }
    
    private Term createInstantiationError(String context) {
        // Use ISO standard error term factory
        return ISOErrorTerms.instantiationError(context);
    }
}