package it.denzosoft.jprolog.builtin.character;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.*;

import java.util.HashMap;
import java.util.Map;
import java.util.List;

/**
 * Implementation of char_code/2 predicate for character-code conversion.
 * 
 * char_code(?Char, ?Code) - Convert between character and character code
 */
public class CharCode implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("char_code/2 requires exactly 2 arguments");
        }
        
        Term charTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term codeTerm = query.getArguments().get(1).resolveBindings(bindings);
        
        try {
            if (charTerm instanceof Variable && codeTerm instanceof Variable) {
                // Both variables - cannot determine conversion
                return false;
            } else if (charTerm instanceof Variable) {
                // Convert code to character
                if (codeToChar((Variable) charTerm, codeTerm, bindings, solutions)) {
                    return true;
                }
                return false;
            } else if (codeTerm instanceof Variable) {
                // Convert character to code
                if (charToCode(charTerm, (Variable) codeTerm, bindings, solutions)) {
                    return true;
                }
                return false;
            } else {
                // Test conversion
                if (testCharCode(charTerm, codeTerm)) {
                    solutions.add(new HashMap<>(bindings));
                    return true;
                }
                return false;
            }
        } catch (Exception e) {
            throw new PrologEvaluationException("char_code/2 error: " + e.getMessage());
        }
    }
    
    /**
     * Convert character code to character.
     */
    private boolean codeToChar(Variable charVar, Term codeTerm, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (!(codeTerm instanceof it.denzosoft.jprolog.core.terms.Number)) {
            return false;
        }
        
        int code = ((it.denzosoft.jprolog.core.terms.Number) codeTerm).getValue().intValue();
        
        // Validate character code range
        if (code < 0 || code > Character.MAX_VALUE) {
            return false;
        }
        
        char ch = (char) code;
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (charVar.unify(new Atom(String.valueOf(ch)), newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        
        return false;
    }
    
    /**
     * Convert character to character code.
     */
    private boolean charToCode(Term charTerm, Variable codeVar, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        char ch = getCharacter(charTerm);
        if (ch == 0 && !isValidNullChar(charTerm)) {
            return false;
        }
        
        int code = (int) ch;
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (codeVar.unify(new it.denzosoft.jprolog.core.terms.Number((double) code), newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        
        return false;
    }
    
    /**
     * Test character-code conversion.
     */
    private boolean testCharCode(Term charTerm, Term codeTerm) {
        if (!(codeTerm instanceof it.denzosoft.jprolog.core.terms.Number)) {
            return false;
        }
        
        char ch = getCharacter(charTerm);
        int expectedCode = ((it.denzosoft.jprolog.core.terms.Number) codeTerm).getValue().intValue();
        
        return (int) ch == expectedCode;
    }
    
    /**
     * Extract character from term.
     */
    private char getCharacter(Term term) {
        if (term instanceof Atom) {
            String str = ((Atom) term).getName();
            return str.length() == 1 ? str.charAt(0) : 0;
        }
        return 0;
    }
    
    /**
     * Check if term represents valid null character.
     */
    private boolean isValidNullChar(Term term) {
        if (term instanceof Atom) {
            String str = ((Atom) term).getName();
            return str.length() == 1 && str.charAt(0) == 0;
        }
        return false;
    }
}