package it.denzosoft.jprolog.builtin.character;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.*;

import java.util.HashMap;
import java.util.Map;
import java.util.List;

/**
 * Implementation of upcase_atom/2 predicate for atom case conversion.
 * 
 * upcase_atom(+Atom, ?UpperCase) - Convert atom to uppercase
 */
public class UpCase implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("upcase_atom/2 requires exactly 2 arguments");
        }
        
        Term inputTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term outputTerm = query.getArguments().get(1);
        
        try {
            if (!(inputTerm instanceof Atom)) {
                throw new PrologEvaluationException("upcase_atom/2: first argument must be an atom");
            }
            
            String inputAtom = ((Atom) inputTerm).getName();
            String upperCaseAtom = inputAtom.toUpperCase();
            
            Atom resultAtom = new Atom(upperCaseAtom);
            
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (outputTerm.unify(resultAtom, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            
            return false;
        } catch (Exception e) {
            throw new PrologEvaluationException("upcase_atom/2 error: " + e.getMessage());
        }
    }
}