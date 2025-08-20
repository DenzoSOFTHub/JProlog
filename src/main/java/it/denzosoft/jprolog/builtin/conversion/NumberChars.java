package it.denzosoft.jprolog.builtin.conversion;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;



public class NumberChars implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("number_chars/2 requires exactly 2 arguments.");
        }

        Term numberTerm = query.getArguments().get(0);
        Term charsTerm = query.getArguments().get(1);

        if (numberTerm.isGround() && !charsTerm.isGround()) {
            // Convert number to character list
            if (!(numberTerm instanceof Number)) {
                return false; // First argument must be a number
            }
            
            double numberValue = ((Number) numberTerm).getValue();
            String numberStr = formatNumber(numberValue);
            Term charList = buildCharList(numberStr);
            
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (charsTerm.unify(charList, newBindings)) {
                solutions.add(new HashMap<>(newBindings));
                return true;
            }
        } else if (!numberTerm.isGround() && charsTerm.isGround()) {
            // Convert character list to number
            List<String> chars = extractChars(charsTerm.resolveBindings(bindings));
            if (chars != null) {
                StringBuilder sb = new StringBuilder();
                for (String ch : chars) {
                    sb.append(ch);
                }
                
                try {
                    double value = Double.parseDouble(sb.toString());
                    Map<String, Term> newBindings = new HashMap<>(bindings);
                    if (numberTerm.unify(new Number(value), newBindings)) {
                        solutions.add(new HashMap<>(newBindings));
                        return true;
                    }
                } catch (NumberFormatException e) {
                    return false; // Character list is not a valid number
                }
            }
        } else if (numberTerm.isGround() && charsTerm.isGround()) {
            // Both ground - check if they represent the same value
            if (!(numberTerm instanceof Number)) {
                return false;
            }
            
            double numberValue = ((Number) numberTerm).getValue();
            String numberStr = formatNumber(numberValue);
            List<String> chars = extractChars(charsTerm.resolveBindings(bindings));
            
            if (chars != null) {
                StringBuilder sb = new StringBuilder();
                for (String ch : chars) {
                    sb.append(ch);
                }
                
                try {
                    double charListAsNumber = Double.parseDouble(sb.toString());
                    if (Math.abs(numberValue - charListAsNumber) < 1e-10) {
                        solutions.add(new HashMap<>(bindings));
                        return true;
                    }
                } catch (NumberFormatException e) {
                    return false; // Character list is not a valid number
                }
            }
            return false;
        } else {
            throw new PrologEvaluationException("number_chars/2: at least one argument must be ground.");
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
