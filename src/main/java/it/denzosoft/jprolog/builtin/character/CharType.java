package it.denzosoft.jprolog.builtin.character;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.*;

import java.util.*;

/**
 * Implementation of char_type/2 predicate for ISO character classification.
 * 
 * char_type(?Char, ?Type) - Character type testing and generation
 */
public class CharType implements BuiltIn {
    
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
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("char_type/2 requires exactly 2 arguments");
        }
        
        Term charTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term typeTerm = query.getArguments().get(1).resolveBindings(bindings);
        
        try {
            if (charTerm instanceof Variable && typeTerm instanceof Variable) {
                // Generate all character-type pairs
                return generateCharacterTypes((Variable) charTerm, (Variable) typeTerm, bindings, solutions);
            } else if (charTerm instanceof Variable) {
                // Generate characters of given type
                return generateCharactersOfType((Variable) charTerm, typeTerm, bindings, solutions);
            } else if (typeTerm instanceof Variable) {
                // Find types of given character
                return findCharacterTypes(charTerm, (Variable) typeTerm, bindings, solutions);
            } else {
                // Test if character has given type
                if (testCharacterType(charTerm, typeTerm)) {
                    solutions.add(new HashMap<>(bindings));
                    return true;
                }
                return false;
            }
        } catch (Exception e) {
            throw new PrologEvaluationException("char_type/2 error: " + e.getMessage());
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
    private boolean findCharacterTypes(Term charTerm, Variable typeVar, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        char ch = getCharacter(charTerm);
        if (ch == 0) return false;
        
        boolean foundSolution = false;
        // Find all matching types
        for (CharacterType type : CharacterType.values()) {
            if (hasCharacterType(ch, type)) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (typeVar.unify(new Atom(type.name().toLowerCase()), newBindings)) {
                    solutions.add(newBindings);
                    foundSolution = true;
                }
            }
        }
        
        return foundSolution;
    }
    
    /**
     * Generate characters of a given type.
     */
    private boolean generateCharactersOfType(Variable charVar, Term typeTerm, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        CharacterType type = getCharacterTypeFromTerm(typeTerm);
        if (type == null) return false;
        
        boolean foundSolution = false;
        // Generate all characters of this type
        for (int i = 0; i <= 127; i++) { // ASCII range
            char ch = (char) i;
            if (hasCharacterType(ch, type)) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (charVar.unify(new Atom(String.valueOf(ch)), newBindings)) {
                    solutions.add(newBindings);
                    foundSolution = true;
                }
            }
        }
        
        return foundSolution;
    }
    
    /**
     * Generate all character-type pairs.
     */
    private boolean generateCharacterTypes(Variable charVar, Variable typeVar, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        boolean foundSolution = false;
        // Generate all valid pairs
        for (int i = 0; i <= 127; i++) {
            char ch = (char) i;
            for (CharacterType type : CharacterType.values()) {
                if (hasCharacterType(ch, type)) {
                    Map<String, Term> newBindings = new HashMap<>(bindings);
                    if (charVar.unify(new Atom(String.valueOf(ch)), newBindings) &&
                        typeVar.unify(new Atom(type.name().toLowerCase()), newBindings)) {
                        solutions.add(newBindings);
                        foundSolution = true;
                    }
                }
            }
        }
        
        return foundSolution;
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