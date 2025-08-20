package it.denzosoft.jprolog.builtin.debug;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.List;
import java.util.Map;

/**
 * Implementation of nospy/1 predicate.
 * 
 * nospy(+PredicateIndicator)
 * 
 * Removes a spy point from the specified predicate. PredicateIndicator
 * should be in the form Name/Arity.
 * 
 * Examples:
 * ?- nospy(member/2).
 * true.
 * 
 * ?- nospy(_).
 * % Removes all spy points
 * true.
 */
public class NoSpy implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 1) {
            throw new PrologEvaluationException("nospy/1 requires exactly 1 argument");
        }
        
        Term predicateIndicator = query.getArguments().get(0).resolveBindings(bindings);
        
        if (predicateIndicator instanceof Variable) {
            // Remove all spy points if argument is a variable
            Spy.clearAllSpyPoints();
            System.out.println("% All spy points removed");
            solutions.add(bindings);
            return true;
        }
        
        String spyPoint = null;
        
        if (predicateIndicator instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) predicateIndicator;
            if ("/".equals(compound.getName()) && compound.getArguments().size() == 2) {
                Term nameTerm = compound.getArguments().get(0);
                Term arityTerm = compound.getArguments().get(1);
                
                if (nameTerm instanceof Atom && arityTerm instanceof it.denzosoft.jprolog.core.terms.Number) {
                    String name = ((Atom) nameTerm).getName();
                    double arityValue = ((it.denzosoft.jprolog.core.terms.Number) arityTerm).getValue();
                    
                    if (arityValue == Math.floor(arityValue) && arityValue >= 0) {
                        int arity = (int) arityValue;
                        spyPoint = name + "/" + arity;
                    }
                }
            }
        }
        
        if (spyPoint == null) {
            return false; // Fail silently for invalid predicate indicators
        }
        
        Spy.removeSpyPoint(spyPoint);
        System.out.println("% Spy point removed from " + spyPoint);
        
        solutions.add(bindings);
        return true;
    }
}