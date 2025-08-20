package it.denzosoft.jprolog.builtin.conversion;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;



public class AtomNumber implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("atom_number/2 requires exactly 2 arguments.");
        }

        Term atomTerm = query.getArguments().get(0);
        Term numberTerm = query.getArguments().get(1);

        if (atomTerm.isGround() && !numberTerm.isGround()) {
            // Convert atom to number
            if (!(atomTerm instanceof Atom)) {
                return false; // First argument must be an atom
            }
            
            String atomValue = ((Atom) atomTerm).getName();
            try {
                double value = Double.parseDouble(atomValue);
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (numberTerm.unify(new Number(value), newBindings)) {
                    solutions.add(new HashMap<>(newBindings));
                    return true;
                }
            } catch (NumberFormatException e) {
                return false; // Atom is not a valid number
            }
        } else if (!atomTerm.isGround() && numberTerm.isGround()) {
            // Convert number to atom
            if (!(numberTerm instanceof Number)) {
                return false; // Second argument must be a number
            }
            
            double numberValue = ((Number) numberTerm).getValue();
            String atomValue = formatNumber(numberValue);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (atomTerm.unify(new Atom(atomValue), newBindings)) {
                solutions.add(new HashMap<>(newBindings));
                return true;
            }
        } else if (atomTerm.isGround() && numberTerm.isGround()) {
            // Both ground - check if they represent the same value
            if (!(atomTerm instanceof Atom) || !(numberTerm instanceof Number)) {
                return false;
            }
            
            String atomValue = ((Atom) atomTerm).getName();
            double numberValue = ((Number) numberTerm).getValue();
            
            try {
                double atomAsNumber = Double.parseDouble(atomValue);
                if (Math.abs(atomAsNumber - numberValue) < 1e-10) {
                    solutions.add(new HashMap<>(bindings));
                    return true;
                }
                return false;
            } catch (NumberFormatException e) {
                return false; // Atom is not a valid number
            }
        } else {
            throw new PrologEvaluationException("atom_number/2: at least one argument must be ground.");
        }
        
        return false;
    }
    
    private String formatNumber(double value) {
        if (value == Math.floor(value) && !Double.isInfinite(value)) {
            return String.valueOf((long) value);
        } else {
            return String.valueOf(value);
        }
    }
}
