package it.denzosoft.jprolog.builtin.debug;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Implementation of spy/1 predicate.
 * 
 * spy(+PredicateIndicator)
 * 
 * Sets a spy point on the specified predicate. Spy points cause execution
 * to pause and display debugging information when the predicate is called.
 * PredicateIndicator should be in the form Name/Arity.
 * 
 * Examples:
 * ?- spy(member/2).
 * true.
 * 
 * ?- spy(foo/1).
 * true.
 */
public class Spy implements BuiltIn {
    
    // Global set of spy points (simplified implementation)
    private static Set<String> spyPoints = new HashSet<>();
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 1) {
            throw new PrologEvaluationException("spy/1 requires exactly 1 argument");
        }
        
        Term predicateIndicator = query.getArguments().get(0).resolveBindings(bindings);
        
        if (predicateIndicator instanceof Variable) {
            return false; // Fail silently for unbound variables
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
        
        spyPoints.add(spyPoint);
        System.out.println("% Spy point set on " + spyPoint);
        
        solutions.add(bindings);
        return true;
    }
    
    /**
     * Check if a spy point is set for the given predicate.
     */
    public static boolean hasSpyPoint(String name, int arity) {
        return spyPoints.contains(name + "/" + arity);
    }
    
    /**
     * Get all spy points.
     */
    public static Set<String> getSpyPoints() {
        return new HashSet<>(spyPoints);
    }
    
    /**
     * Clear a specific spy point (used by nospy/1).
     */
    public static void removeSpyPoint(String predicateIndicator) {
        spyPoints.remove(predicateIndicator);
    }
    
    /**
     * Clear all spy points.
     */
    public static void clearAllSpyPoints() {
        spyPoints.clear();
    }
}