package it.denzosoft.jprolog.builtin.character;

import it.denzosoft.jprolog.builtin.AbstractBuiltInWithContext;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.*;

import java.util.Map;
import java.util.List;

/**
 * Implementation of char_code/2 predicate for character-code conversion.
 * 
 * char_code(?Char, ?Code) - Convert between character and character code
 */
public class CharCode extends AbstractBuiltInWithContext {
    
    /**
     * Create char_code predicate.
     * 
     * @param solver The query solver
     */
    public CharCode(QuerySolver solver) {
        super(solver);
    }
    
    @Override
    public boolean execute(Term term, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        return solve(solver, bindings);
    }
    
    @Override
    public boolean solve(QuerySolver solver, Map<String, Term> bindings) {
        Term[] args = getArguments();
        if (args.length != 2) {
            return false;
        }
        
        Term charTerm = args[0];
        Term codeTerm = args[1];
        
        try {
            if (charTerm instanceof Variable && codeTerm instanceof Variable) {
                // Both variables - cannot determine conversion
                return false;
            } else if (charTerm instanceof Variable) {
                // Convert code to character
                return codeToChar((Variable) charTerm, codeTerm, bindings);
            } else if (codeTerm instanceof Variable) {
                // Convert character to code
                return charToCode(charTerm, (Variable) codeTerm, bindings);
            } else {
                // Test conversion
                return testCharCode(charTerm, codeTerm);
            }
        } catch (Exception e) {
            return false;
        }
    }
    
    /**
     * Convert character code to character.
     */
    private boolean codeToChar(Variable charVar, Term codeTerm, Map<String, Term> bindings) {
        if (!(codeTerm instanceof it.denzosoft.jprolog.core.terms.Number)) {
            return false;
        }
        
        int code = ((it.denzosoft.jprolog.core.terms.Number) codeTerm).getValue().intValue();
        
        // Validate character code range
        if (code < 0 || code > Character.MAX_VALUE) {
            return false;
        }
        
        char ch = (char) code;
        bindings.put(charVar.getName(), new Atom(String.valueOf(ch)));
        return true;
    }
    
    /**
     * Convert character to character code.
     */
    private boolean charToCode(Term charTerm, Variable codeVar, Map<String, Term> bindings) {
        char ch = getCharacter(charTerm);
        if (ch == 0 && !isValidNullChar(charTerm)) {
            return false;
        }
        
        int code = (int) ch;
        bindings.put(codeVar.getName(), new it.denzosoft.jprolog.core.terms.Number((double) code));
        return true;
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