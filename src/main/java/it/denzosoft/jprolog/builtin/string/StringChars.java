package it.denzosoft.jprolog.builtin.string;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of string_chars/2 predicate.
 * 
 * string_chars(?String, ?Chars)
 * 
 * True if Chars is a list of single-character atoms that comprise String.
 */
public class StringChars implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<java.lang.String, Term> bindings, List<Map<java.lang.String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("string_chars/2 requires exactly 2 arguments.");
        }
        
        Term stringTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term charsTerm = query.getArguments().get(1).resolveBindings(bindings);
        
        if (stringTerm.isGround() && !charsTerm.isGround()) {
            // Convert string to character list
            if (!(stringTerm instanceof PrologString)) {
                return false;
            }
            
            java.lang.String stringValue = ((PrologString) stringTerm).getStringValue();
            Term charList = buildCharList(stringValue);
            
            Map<java.lang.String, Term> newBindings = new HashMap<>(bindings);
            if (charsTerm.unify(charList, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            
        } else if (!stringTerm.isGround() && charsTerm.isGround()) {
            // Convert character list to string
            List<java.lang.String> chars = extractChars(charsTerm);
            if (chars != null) {
                StringBuilder sb = new StringBuilder();
                for (java.lang.String ch : chars) {
                    sb.append(ch);
                }
                
                Map<java.lang.String, Term> newBindings = new HashMap<>(bindings);
                if (stringTerm.unify(new PrologString(sb.toString()), newBindings)) {
                    solutions.add(newBindings);
                    return true;
                }
            }
            
        } else if (stringTerm.isGround() && charsTerm.isGround()) {
            // Both ground - check if they match
            if (!(stringTerm instanceof PrologString)) {
                return false;
            }
            
            java.lang.String stringValue = ((PrologString) stringTerm).getStringValue();
            List<java.lang.String> chars = extractChars(charsTerm);
            
            if (chars != null) {
                StringBuilder sb = new StringBuilder();
                for (java.lang.String ch : chars) {
                    sb.append(ch);
                }
                
                if (stringValue.equals(sb.toString())) {
                    solutions.add(new HashMap<>(bindings));
                    return true;
                }
            }
            return false;
            
        } else {
            throw new PrologEvaluationException("string_chars/2: at least one argument must be instantiated.");
        }
        
        return false;
    }
    
    private Term buildCharList(java.lang.String str) {
        Term result = new Atom("[]");
        for (int i = str.length() - 1; i >= 0; i--) {
            List<Term> args = new ArrayList<>();
            args.add(new Atom(java.lang.String.valueOf(str.charAt(i))));
            args.add(result);
            result = new CompoundTerm(new Atom("."), args);
        }
        return result;
    }
    
    private List<java.lang.String> extractChars(Term list) {
        List<java.lang.String> chars = new ArrayList<>();
        Term current = list;
        
        while (current instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) current;
            if (compound.getName().equals(".") && compound.getArguments().size() == 2) {
                Term element = compound.getArguments().get(0);
                if (element instanceof Atom) {
                    java.lang.String charStr = ((Atom) element).getName();
                    if (charStr.length() == 1) {  // Must be single character
                        chars.add(charStr);
                    } else {
                        return null; // Invalid character
                    }
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