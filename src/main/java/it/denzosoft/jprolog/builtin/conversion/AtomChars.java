package it.denzosoft.jprolog.builtin.conversion;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;



public class AtomChars implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("atom_chars/2 requires exactly 2 arguments.");
        }

        Term atomTerm = query.getArguments().get(0);
        Term charsTerm = query.getArguments().get(1);

        if (atomTerm.isGround() && !charsTerm.isGround()) {
            // Convert atom to character list
            if (!(atomTerm instanceof Atom)) {
                return false; // First argument must be an atom
            }
            
            String atomValue = ((Atom) atomTerm).getName();
            Term charList = buildCharList(atomValue);
            
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (charsTerm.unify(charList, newBindings)) {
                solutions.add(new HashMap<>(newBindings));
                return true;
            }
        } else if (!atomTerm.isGround() && charsTerm.isGround()) {
            // Convert character list to atom
            List<String> chars = extractChars(charsTerm.resolveBindings(bindings));
            if (chars != null) {
                StringBuilder sb = new StringBuilder();
                for (String ch : chars) {
                    sb.append(ch);
                }
                
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (atomTerm.unify(new Atom(sb.toString()), newBindings)) {
                    solutions.add(new HashMap<>(newBindings));
                    return true;
                }
            }
        } else if (atomTerm.isGround() && charsTerm.isGround()) {
            // Both ground - check if they represent the same value
            if (!(atomTerm instanceof Atom)) {
                return false;
            }
            
            String atomValue = ((Atom) atomTerm).getName();
            List<String> chars = extractChars(charsTerm.resolveBindings(bindings));
            
            if (chars != null) {
                StringBuilder sb = new StringBuilder();
                for (String ch : chars) {
                    sb.append(ch);
                }
                
                if (atomValue.equals(sb.toString())) {
                    solutions.add(new HashMap<>(bindings));
                    return true;
                }
            }
            return false;
        } else {
            throw new PrologEvaluationException("atom_chars/2: at least one argument must be ground.");
        }
        
        return false;
    }
    
    private Term buildCharList(String str) {
        Term result = new Atom("[]");
        for (int i = str.length() - 1; i >= 0; i--) {
            List<Term> args = new ArrayList<>();
            args.add(new Atom(String.valueOf(str.charAt(i))));
            args.add(result);
            result = new CompoundTerm(new Atom("."), args);
        }
        return result;
    }
    
    private List<String> extractChars(Term list) {
        List<String> chars = new ArrayList<>();
        Term current = list;
        
        while (current instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) current;
            if (compound.getName().equals(".") && compound.getArguments().size() == 2) {
                Term element = compound.getArguments().get(0);
                if (element instanceof Atom) {
                    chars.add(((Atom) element).getName());
                } else {
                    return null; // Invalid character
                }
                current = compound.getArguments().get(1);
            } else {
                break;
            }
        }
        
        if (current instanceof Atom && ((Atom) current).getName().equals("[]")) {
            return chars;
        } else {
            return null; // Malformed list
        }
    }
}
