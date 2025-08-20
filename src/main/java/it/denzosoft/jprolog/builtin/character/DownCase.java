package it.denzosoft.jprolog.builtin.character;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.*;

import java.util.HashMap;
import java.util.Map;
import java.util.List;

/**
 * Implementation of downcase_atom/2 predicate for atom case conversion.
 * 
 * downcase_atom(+Atom, ?LowerCase) - Convert atom to lowercase
 */
public class DownCase implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("downcase_atom/2 requires exactly 2 arguments");
        }
        
        Term inputTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term outputTerm = query.getArguments().get(1);
        
        try {
            if (!(inputTerm instanceof Atom)) {
                throw new PrologEvaluationException("downcase_atom/2: first argument must be an atom");
            }
            
            String inputAtom = ((Atom) inputTerm).getName();
            String lowerCaseAtom = inputAtom.toLowerCase();
            
            Atom resultAtom = new Atom(lowerCaseAtom);
            
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (outputTerm.unify(resultAtom, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            
            return false;
        } catch (Exception e) {
            throw new PrologEvaluationException("downcase_atom/2 error: " + e.getMessage());
        }
    }
}