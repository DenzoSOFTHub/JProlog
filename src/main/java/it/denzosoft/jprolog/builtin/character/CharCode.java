package it.denzosoft.jprolog.builtin.character;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
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
                // START_CHANGE: ISS-2025-0401 - ISO 8.16.6.3 a: both unbound -> instantiation_error
                // (was a silent false)
                throw new PrologException(ISOErrorTerms.instantiationError("char_code/2"));
                // END_CHANGE: ISS-2025-0401
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
        // START_CHANGE: ISS-2025-0401 - rethrow PrologException so the ISO error ball survives
        } catch (PrologException e) {
            throw e;
        // END_CHANGE: ISS-2025-0401
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw new PrologEvaluationException("char_code/2 error: " + e.getMessage());
        }
    }

    /**
     * Convert character code to character.
     */
    private boolean codeToChar(Variable charVar, Term codeTerm, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        // START_CHANGE: ISS-2025-0401 - ISO 8.16.6.3: a non-integer code -> type_error(integer, C),
        // an integer that is not a character code -> representation_error(character_code)
        // (both were a silent false)
        int code = requireCharacterCode(codeTerm);
        // END_CHANGE: ISS-2025-0401

        String charStr = new String(Character.toChars(code));
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (charVar.unify(new Atom(charStr), newBindings)) {
            solutions.add(newBindings);
            return true;
        }

        return false;
    }

    /**
     * Convert character to character code.
     */
    private boolean charToCode(Term charTerm, Variable codeVar, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        // START_CHANGE: ISS-2025-0211 - support supplementary codepoints
        // START_CHANGE: ISS-2025-0401 - ISO 8.16.6.3 b: not a one-char atom -> type_error(character)
        int code = getCodepoint(charTerm);
        if (code < 0) {
            throw new PrologException(ISOErrorTerms.typeError("character", charTerm, "char_code/2"));
        }
        // END_CHANGE: ISS-2025-0401
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (codeVar.unify(new it.denzosoft.jprolog.core.terms.Number((long) code), newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
        // END_CHANGE: ISS-2025-0211
    }

    /**
     * Test character-code conversion.
     */
    private boolean testCharCode(Term charTerm, Term codeTerm) {
        // START_CHANGE: ISS-2025-0211 - codepoint comparison
        // START_CHANGE: ISS-2025-0401 - ISO 8.16.6.3: typed errors instead of silent false in
        // the both-bound mode as well
        int code = getCodepoint(charTerm);
        if (code < 0) {
            throw new PrologException(ISOErrorTerms.typeError("character", charTerm, "char_code/2"));
        }
        int expectedCode = requireCharacterCode(codeTerm);
        // END_CHANGE: ISS-2025-0401
        return code == expectedCode;
        // END_CHANGE: ISS-2025-0211
    }

    // START_CHANGE: ISS-2025-0401 - validate a bound code argument per ISO 8.16.6.3
    /** Return the (valid) character code of a bound Code argument, raising
     *  type_error(integer, C) for a non-integer and representation_error(character_code)
     *  for an integer outside the character-code range. */
    private int requireCharacterCode(Term codeTerm) {
        if (!(codeTerm instanceof it.denzosoft.jprolog.core.terms.Number)
                || !((it.denzosoft.jprolog.core.terms.Number) codeTerm).isInteger()) {
            throw new PrologException(ISOErrorTerms.typeError("integer", codeTerm, "char_code/2"));
        }
        long code = ((it.denzosoft.jprolog.core.terms.Number) codeTerm).longValue();
        // START_CHANGE: ISS-2025-0193 - Extend range to full Unicode (up to U+10FFFF)
        if (code < 0 || code > 0x10FFFF) {
            throw new PrologException(ISOErrorTerms.representationError("character_code", "char_code/2"));
        }
        // END_CHANGE: ISS-2025-0193
        return (int) code;
    }
    // END_CHANGE: ISS-2025-0401

    // START_CHANGE: ISS-2025-0211 - extract full codepoint from a single-character atom
    private int getCodepoint(Term term) {
        if (!(term instanceof Atom)) return -1;
        String str = ((Atom) term).getName();
        if (str.isEmpty()) return -1;
        int cp = str.codePointAt(0);
        // Must be exactly one codepoint
        if (Character.charCount(cp) != str.length()) return -1;
        return cp;
    }
    // END_CHANGE: ISS-2025-0211
}
