package it.denzosoft.jprolog.builtin.character;

import it.denzosoft.jprolog.builtin.AbstractBuiltInWithContext;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.*;

import java.util.*;

/**
 * Implementation of char_type/2 predicate for ISO character classification.
 * 
 * char_type(?Char, ?Type) - Character type testing and generation
 */
public class CharType extends AbstractBuiltInWithContext {
    
    /**
     * Character type enumeration according to ISO Prolog.
     */
    private enum CharacterType {
        ALNUM,       // Alphanumeric
        ALPHA,       // Alphabetic
        ASCII,       // ASCII character
        CNTRL,       // Control character
        DIGIT,       // Decimal digit
        GRAPH,       // Graphic character
        LOWER,       // Lowercase letter
        PRINT,       // Printable character
        PUNCT,       // Punctuation
        SPACE,       // Whitespace
        UPPER,       // Uppercase letter
        XDIGIT,      // Hexadecimal digit
        // Extended types
        NEWLINE,     // Newline character
        END_OF_FILE, // End of file
        END_OF_LINE, // End of line
        LAYOUT,      // Layout character
        META,        // Meta character
        SOLO,        // Solo character
        SYMBOL       // Symbol character
    }
    
    /**
     * Create char_type predicate.
     * 
     * @param solver The query solver
     */
    public CharType(QuerySolver solver) {
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
        Term typeTerm = args[1];
        
        try {
            if (charTerm instanceof Variable && typeTerm instanceof Variable) {
                // Generate all character-type pairs
                return generateCharacterTypes((Variable) charTerm, (Variable) typeTerm, bindings);
            } else if (charTerm instanceof Variable) {
                // Generate characters of given type
                return generateCharactersOfType((Variable) charTerm, typeTerm, bindings);
            } else if (typeTerm instanceof Variable) {
                // Find types of given character
                return findCharacterTypes(charTerm, (Variable) typeTerm, bindings);
            } else {
                // Test if character has given type
                return testCharacterType(charTerm, typeTerm);
            }
        } catch (Exception e) {
            return false;
        }
    }
    
    /**
     * Test if a character has a specific type.
     */
    private boolean testCharacterType(Term charTerm, Term typeTerm) {
        char ch = getCharacter(charTerm);
        if (ch == 0) return false;
        
        CharacterType type = getCharacterTypeFromTerm(typeTerm);
        if (type == null) return false;
        
        return hasCharacterType(ch, type);
    }
    
    /**
     * Find all types of a given character.
     */
    private boolean findCharacterTypes(Term charTerm, Variable typeVar, Map<String, Term> bindings) {
        char ch = getCharacter(charTerm);
        if (ch == 0) return false;
        
        // Find first matching type
        for (CharacterType type : CharacterType.values()) {
            if (hasCharacterType(ch, type)) {
                bindings.put(typeVar.getName(), new Atom(type.name().toLowerCase()));
                return true;
            }
        }
        
        return false;
    }
    
    /**
     * Generate characters of a given type.
     */
    private boolean generateCharactersOfType(Variable charVar, Term typeTerm, Map<String, Term> bindings) {
        CharacterType type = getCharacterTypeFromTerm(typeTerm);
        if (type == null) return false;
        
        // Generate first character of this type
        for (int i = 0; i <= 127; i++) { // ASCII range
            char ch = (char) i;
            if (hasCharacterType(ch, type)) {
                bindings.put(charVar.getName(), new Atom(String.valueOf(ch)));
                return true;
            }
        }
        
        return false;
    }
    
    /**
     * Generate all character-type pairs.
     */
    private boolean generateCharacterTypes(Variable charVar, Variable typeVar, Map<String, Term> bindings) {
        // Generate first valid pair
        for (int i = 0; i <= 127; i++) {
            char ch = (char) i;
            for (CharacterType type : CharacterType.values()) {
                if (hasCharacterType(ch, type)) {
                    bindings.put(charVar.getName(), new Atom(String.valueOf(ch)));
                    bindings.put(typeVar.getName(), new Atom(type.name().toLowerCase()));
                    return true;
                }
            }
        }
        
        return false;
    }
    
    /**
     * Check if character has specific type.
     */
    private boolean hasCharacterType(char ch, CharacterType type) {
        switch (type) {
            case ALNUM:
                return Character.isLetterOrDigit(ch);
            case ALPHA:
                return Character.isLetter(ch);
            case ASCII:
                return ch >= 0 && ch <= 127;
            case CNTRL:
                return Character.isISOControl(ch);
            case DIGIT:
                return Character.isDigit(ch);
            case GRAPH:
                return !Character.isWhitespace(ch) && !Character.isISOControl(ch) && ch != ' ';
            case LOWER:
                return Character.isLowerCase(ch);
            case PRINT:
                return !Character.isISOControl(ch);
            case PUNCT:
                return isPunctuation(ch);
            case SPACE:
                return Character.isWhitespace(ch);
            case UPPER:
                return Character.isUpperCase(ch);
            case XDIGIT:
                return isHexDigit(ch);
            case NEWLINE:
                return ch == '\n';
            case END_OF_FILE:
                return ch == (char) -1; // EOF marker
            case END_OF_LINE:
                return ch == '\n' || ch == '\r';
            case LAYOUT:
                return Character.isWhitespace(ch);
            case META:
                return isMetaCharacter(ch);
            case SOLO:
                return isSoloCharacter(ch);
            case SYMBOL:
                return isSymbolCharacter(ch);
            default:
                return false;
        }
    }
    
    /**
     * Check if character is punctuation.
     */
    private boolean isPunctuation(char ch) {
        return "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~".indexOf(ch) >= 0;
    }
    
    /**
     * Check if character is hexadecimal digit.
     */
    private boolean isHexDigit(char ch) {
        return Character.isDigit(ch) || 
               (ch >= 'a' && ch <= 'f') || 
               (ch >= 'A' && ch <= 'F');
    }
    
    /**
     * Check if character is a Prolog meta character.
     */
    private boolean isMetaCharacter(char ch) {
        return "\\^".indexOf(ch) >= 0;
    }
    
    /**
     * Check if character is a solo character.
     */
    private boolean isSoloCharacter(char ch) {
        return "!();[]{}|".indexOf(ch) >= 0;
    }
    
    /**
     * Check if character is a symbol character.
     */
    private boolean isSymbolCharacter(char ch) {
        return "#$&*+-./:<=>?@^~".indexOf(ch) >= 0;
    }
    
    /**
     * Extract character from term.
     */
    private char getCharacter(Term term) {
        if (term instanceof Atom) {
            String str = ((Atom) term).getName();
            return str.length() == 1 ? str.charAt(0) : 0;
        } else if (term instanceof it.denzosoft.jprolog.core.terms.Number) {
            int code = ((it.denzosoft.jprolog.core.terms.Number) term).getValue().intValue();
            return (code >= 0 && code <= Character.MAX_VALUE) ? (char) code : 0;
        }
        return 0;
    }
    
    /**
     * Get character type from term.
     */
    private CharacterType getCharacterTypeFromTerm(Term term) {
        if (term instanceof Atom) {
            String typeName = ((Atom) term).getName().toUpperCase();
            try {
                return CharacterType.valueOf(typeName);
            } catch (IllegalArgumentException e) {
                return null;
            }
        }
        return null;
    }
}