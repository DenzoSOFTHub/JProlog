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
            // START_CHANGE: ISS-2025-0233/0236 - accept atom or string; codepoint-aware
            java.lang.String stringValue;
            if (stringTerm instanceof PrologString) {
                stringValue = ((PrologString) stringTerm).getStringValue();
            } else if (stringTerm instanceof Atom) {
                stringValue = ((Atom) stringTerm).getName();
            } else {
                return false;
            }
            Term charList = buildCharList(stringValue);
            // END_CHANGE: ISS-2025-0233/0236
            
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
            // START_CHANGE: ISS-2025-0405 - SWI text interop: atoms accepted as their text
            java.lang.String stringValue = TextTerm.textOf(stringTerm);
            if (stringValue == null) {
                return false;
            }
            // END_CHANGE: ISS-2025-0405
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
            // START_CHANGE: ISS-2025-0084 - Return false instead of throwing for normal failure
            return false;
            // END_CHANGE: ISS-2025-0084
        }
        
        return false;
    }
    
    private Term buildCharList(java.lang.String str) {
        // START_CHANGE: ISS-2025-0233 - codepoint-aware (supplementary Unicode plane)
        List<java.lang.String> chars = new ArrayList<>();
        int i = 0;
        while (i < str.length()) {
            int cp = str.codePointAt(i);
            chars.add(new java.lang.String(Character.toChars(cp)));
            i += Character.charCount(cp);
        }
        Term result = new Atom("[]");
        for (int k = chars.size() - 1; k >= 0; k--) {
            List<Term> args = new ArrayList<>();
            args.add(new Atom(chars.get(k)));
            args.add(result);
            result = new CompoundTerm(new Atom("."), args);
        }
        return result;
        // END_CHANGE: ISS-2025-0233
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
                    // START_CHANGE: ISS-2025-0233 - allow single codepoint (1 or 2 surrogate chars)
                    if (charStr.length() == 1 || (charStr.length() == 2 && Character.isHighSurrogate(charStr.charAt(0)))) {
                        chars.add(charStr);
                    } else {
                        return null;
                    }
                    // END_CHANGE: ISS-2025-0233
                } else {
                    return null;
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