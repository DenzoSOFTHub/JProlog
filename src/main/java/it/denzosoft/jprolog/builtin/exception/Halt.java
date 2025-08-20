package it.denzosoft.jprolog.builtin.exception;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.List;
import java.util.Map;

/**
 * Implementation of halt/0 and halt/1 predicates.
 * 
 * halt
 * halt(+ExitCode)
 * 
 * Terminate the Prolog system. If ExitCode is provided, it specifies
 * the exit code (default is 0).
 */
public class Halt implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        
        int arity = query.getArguments() != null ? query.getArguments().size() : 0;
        
        if (arity == 0) {
            // halt/0 - exit with code 0
            throw new PrologException(0);
            
        } else if (arity == 1) {
            // halt/1 - exit with specified code
            Term exitCodeTerm = query.getArguments().get(0).resolveBindings(bindings);
            
            if (exitCodeTerm instanceof Variable) {
                throw new PrologException(createInstantiationError("halt/1: exit code must be instantiated"));
            }
            
            if (!(exitCodeTerm instanceof Number)) {
                throw new PrologException(createTypeError("integer", exitCodeTerm, "halt/1: exit code must be an integer"));
            }
            
            int exitCode = (int) Math.round(((Number) exitCodeTerm).getValue());
            throw new PrologException(exitCode);
            
        } else {
            // Wrong arity
            throw new PrologException(createTypeError("callable", query, "halt: wrong number of arguments"));
        }
    }
    
    private Term createInstantiationError(String context) {
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
    
    private Term createTypeError(String expectedType, Term culprit, String context) {
        try {
            return new it.denzosoft.jprolog.core.terms.CompoundTerm(
                new it.denzosoft.jprolog.core.terms.Atom("error"),
                java.util.Arrays.asList(
                    new it.denzosoft.jprolog.core.terms.CompoundTerm(
                        new it.denzosoft.jprolog.core.terms.Atom("type_error"),
                        java.util.Arrays.asList(
                            new it.denzosoft.jprolog.core.terms.Atom(expectedType),
                            culprit
                        )
                    ),
                    new it.denzosoft.jprolog.core.terms.Atom(context)
                )
            );
        } catch (Exception e) {
            return new it.denzosoft.jprolog.core.terms.Atom("type_error");
        }
    }
}