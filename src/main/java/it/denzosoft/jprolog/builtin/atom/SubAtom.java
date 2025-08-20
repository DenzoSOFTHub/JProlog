package it.denzosoft.jprolog.builtin.atom;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of sub_atom/5 predicate.
 * 
 * sub_atom(+Atom, ?Before, ?Length, ?After, ?SubAtom)
 * 
 * True if SubAtom is a sub-atom of Atom that begins Before characters from the start,
 * has Length characters, and is followed by After characters.
 * 
 * Examples:
 * ?- sub_atom(hello, 1, 3, 1, X).
 * X = ell.
 * 
 * ?- sub_atom(hello, X, 2, Y, el).
 * X = 1, Y = 2.
 * 
 * ?- sub_atom(hello, _, _, _, X).
 * X = '' ; X = h ; X = he ; X = hel ; X = hell ; X = hello ; X = e ; X = el ; ...
 */
public class SubAtom implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 5) {
            throw new PrologEvaluationException("sub_atom/5 requires exactly 5 arguments");
        }
        
        Term atomTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term beforeTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term lengthTerm = query.getArguments().get(2).resolveBindings(bindings);
        Term afterTerm = query.getArguments().get(3).resolveBindings(bindings);
        Term subAtomTerm = query.getArguments().get(4).resolveBindings(bindings);
        
        // The main atom must be instantiated
        if (!(atomTerm instanceof Atom)) {
            throw new PrologEvaluationException("sub_atom/5: first argument must be an atom");
        }
        
        String atom = ((Atom) atomTerm).getName();
        int atomLength = atom.length();
        
        // Get values if instantiated, otherwise use variables
        Integer before = getIntegerValue(beforeTerm);
        Integer length = getIntegerValue(lengthTerm);
        Integer after = getIntegerValue(afterTerm);
        String subAtom = getAtomValue(subAtomTerm);
        
        boolean foundSolution = false;
        
        // Generate all possible combinations
        for (int b = 0; b <= atomLength; b++) {
            for (int len = 0; len <= atomLength - b; len++) {
                int a = atomLength - b - len;
                String sub = atom.substring(b, b + len);
                
                // Check constraints
                if (before != null && before != b) continue;
                if (length != null && length != len) continue;
                if (after != null && after != a) continue;
                if (subAtom != null && !subAtom.equals(sub)) continue;
                
                // Create solution
                Map<String, Term> newBindings = new HashMap<>(bindings);
                boolean unificationSuccess = true;
                
                if (!beforeTerm.unify(new it.denzosoft.jprolog.core.terms.Number((double) b), newBindings)) {
                    unificationSuccess = false;
                }
                if (unificationSuccess && !lengthTerm.unify(new it.denzosoft.jprolog.core.terms.Number((double) len), newBindings)) {
                    unificationSuccess = false;
                }
                if (unificationSuccess && !afterTerm.unify(new it.denzosoft.jprolog.core.terms.Number((double) a), newBindings)) {
                    unificationSuccess = false;
                }
                if (unificationSuccess && !subAtomTerm.unify(new Atom(sub), newBindings)) {
                    unificationSuccess = false;
                }
                
                if (unificationSuccess) {
                    solutions.add(newBindings);
                    foundSolution = true;
                }
            }
        }
        
        return foundSolution;
    }
    
    /**
     * Extract integer value from a term if it's a number, null if variable.
     */
    private Integer getIntegerValue(Term term) {
        if (term instanceof it.denzosoft.jprolog.core.terms.Number) {
            double value = ((it.denzosoft.jprolog.core.terms.Number) term).getValue();
            if (value == Math.floor(value) && value >= 0) {
                return (int) value;
            } else {
                throw new PrologEvaluationException("sub_atom/5: numeric arguments must be non-negative integers");
            }
        } else if (term instanceof Variable) {
            return null;
        } else {
            throw new PrologEvaluationException("sub_atom/5: before, length, and after arguments must be integers or variables");
        }
    }
    
    /**
     * Extract atom value from a term if it's an atom, null if variable.
     */
    private String getAtomValue(Term term) {
        if (term instanceof Atom) {
            return ((Atom) term).getName();
        } else if (term instanceof Variable) {
            return null;
        } else {
            throw new PrologEvaluationException("sub_atom/5: sub-atom argument must be an atom or variable");
        }
    }
}