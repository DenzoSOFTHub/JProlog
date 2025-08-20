package it.denzosoft.jprolog.builtin.string;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of atom_string/2 predicate.
 * 
 * atom_string(?Atom, ?String)
 * 
 * True if the characters of Atom are the same as the characters of String.
 * At least one argument must be instantiated.
 */
public class AtomString implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<java.lang.String, Term> bindings, List<Map<java.lang.String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("atom_string/2 requires exactly 2 arguments.");
        }
        
        Term atomTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term stringTerm = query.getArguments().get(1).resolveBindings(bindings);
        
        if (atomTerm.isGround() && !stringTerm.isGround()) {
            // Convert atom to string
            if (!(atomTerm instanceof Atom)) {
                return false;
            }
            
            java.lang.String atomValue = ((Atom) atomTerm).getName();
            PrologString stringValue = new PrologString(atomValue);
            
            Map<java.lang.String, Term> newBindings = new HashMap<>(bindings);
            if (stringTerm.unify(stringValue, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            
        } else if (!atomTerm.isGround() && stringTerm.isGround()) {
            // Convert string to atom
            if (!(stringTerm instanceof PrologString)) {
                return false;
            }
            
            java.lang.String stringValue = ((PrologString) stringTerm).getStringValue();
            Atom atomValue = new Atom(stringValue);
            
            Map<java.lang.String, Term> newBindings = new HashMap<>(bindings);
            if (atomTerm.unify(atomValue, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            
        } else if (atomTerm.isGround() && stringTerm.isGround()) {
            // Both ground - check if they have the same text
            if (!(atomTerm instanceof Atom) || !(stringTerm instanceof PrologString)) {
                return false;
            }
            
            java.lang.String atomValue = ((Atom) atomTerm).getName();
            java.lang.String stringValue = ((PrologString) stringTerm).getStringValue();
            
            if (atomValue.equals(stringValue)) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            return false;
            
        } else {
            throw new PrologEvaluationException("atom_string/2: at least one argument must be instantiated.");
        }
        
        return false;
    }
}