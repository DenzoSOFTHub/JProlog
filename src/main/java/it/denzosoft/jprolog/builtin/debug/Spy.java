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
    
    // START_CHANGE: ISS-2025-0477 - engine v4 wave W7 (the tail of LIM-034): the spy points are
    // PER ENGINE, not per JVM. `spy(foo/1).` in one Prolog used to set a spy point for every engine
    // in the process. The set now lives on EngineState, reached through the engine current on the
    // calling thread; the static API below is unchanged so nospy/1, spying/0 and debugging/0 keep
    // compiling.
    private static it.denzosoft.jprolog.core.engine.v4.EngineState.Spies spies() {
        return it.denzosoft.jprolog.core.engine.v4.EngineState.current().spies();
    }
    // END_CHANGE: ISS-2025-0477
    
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
        
        spies().add(spyPoint);
        it.denzosoft.jprolog.builtin.io.StreamManager.out().println("% Spy point set on " + spyPoint);
        
        solutions.add(bindings);
        return true;
    }
    
    /**
     * Check if a spy point is set for the given predicate.
     */
    public static boolean hasSpyPoint(String name, int arity) {
        return spies().has(name, arity);
    }
    
    /**
     * Get all spy points.
     */
    public static Set<String> getSpyPoints() {
        return spies().snapshot();
    }
    
    /**
     * Clear a specific spy point (used by nospy/1).
     */
    public static void removeSpyPoint(String predicateIndicator) {
        spies().remove(predicateIndicator);
    }
    
    /**
     * Clear all spy points.
     */
    public static void clearAllSpyPoints() {
        spies().clear();
    }
}