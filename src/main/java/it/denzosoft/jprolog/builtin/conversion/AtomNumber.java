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

        // START_CHANGE: ISS-2025-0084 - Resolve bindings before type/ground checks
        Term atomTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term numberTerm = query.getArguments().get(1).resolveBindings(bindings);
        // END_CHANGE: ISS-2025-0084

        if (atomTerm.isGround() && !numberTerm.isGround()) {
            // Convert atom to number
            if (!(atomTerm instanceof Atom)) {
                return false; // First argument must be an atom
            }

            String atomValue = ((Atom) atomTerm).getName();
            // START_CHANGE: ISS-2025-0235 - support hex/binary/octal prefixes
            Number parsed = parsePrologNumber(atomValue);
            if (parsed == null) return false;
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (numberTerm.unify(parsed, newBindings)) {
                solutions.add(new HashMap<>(newBindings));
                return true;
            }
            return false;
            // END_CHANGE: ISS-2025-0235
        } else if (!atomTerm.isGround() && numberTerm.isGround()) {
            // Convert number to atom
            if (!(numberTerm instanceof Number)) {
                return false; // Second argument must be a number
            }

            // START_CHANGE: ISS-2025-0365 - format via the Number term (BigInteger-exact for
            // integers); the old double->(long) cast silently saturated past 64 bits
            String atomValue = formatNumberExact((Number) numberTerm);
            // END_CHANGE: ISS-2025-0365
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

            // START_CHANGE: ISS-2025-0365 - compare exactly (BigInteger) when both sides are
            // integers, so big values are not collapsed through double precision
            Number atomAsNumber = parsePrologNumber(atomValue);
            if (atomAsNumber == null) {
                return false; // Atom is not a valid number
            }
            Number numberValue = (Number) numberTerm;
            // START_CHANGE: ISS-2025-0399 - exact type-aware comparison (Number.equals): an
            // integer never equals a float, and floats compare exactly (no 1e-10 epsilon)
            boolean same = atomAsNumber.equals(numberValue);
            // END_CHANGE: ISS-2025-0399
            if (same) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            return false;
            // END_CHANGE: ISS-2025-0365
        } else {
            // START_CHANGE: ISS-2025-0084 - Return false instead of throwing for normal failure
            return false;
            // END_CHANGE: ISS-2025-0084
        }
        
        return false;
    }
    
    // START_CHANGE: ISS-2025-0365 - exact text<->number helpers shared with NumberChars/NumberCodes
    // START_CHANGE: ISS-2025-0399 - floats always keep valid float syntax (123.0 -> "123.0", not
    // "123"), so the text of a float reads back as a float. Delegates to Number.toString(), which
    // already emits canonical ISO float text (lowercase 'e' exponent, ISS-2025-0390).
    /** Format a Number exactly: BigInteger digits for integers, float syntax for floats. */
    public static String formatNumberExact(Number n) {
        if (n.isInteger()) {
            return n.bigIntegerValue().toString();
        }
        return n.toString();
    }
    // END_CHANGE: ISS-2025-0399

    /** Parse optionally-signed all-digit text as an exact (arbitrary precision) integer; null if not. */
    static Number parseExactInteger(String s) {
        if (s == null || s.isEmpty()) return null;
        int i = 0;
        char c0 = s.charAt(0);
        boolean neg = (c0 == '-');
        if (neg || c0 == '+') i = 1;
        if (i >= s.length()) return null;
        for (int j = i; j < s.length(); j++) {
            char c = s.charAt(j);
            if (c < '0' || c > '9') return null;
        }
        java.math.BigInteger bi = new java.math.BigInteger(s.substring(i));
        return new Number(neg ? bi.negate() : bi);
    }
    // END_CHANGE: ISS-2025-0365

    // START_CHANGE: ISS-2025-0235 - parse Prolog number syntax: decimals, floats, hex, binary, octal
    // START_CHANGE: ISS-2025-0400 - delegate to the strict ISO number-token parser, so atom_number
    // also gains 0'c constants and rejects the Java-only spellings (Infinity, NaN, '.5', '3.').
    public static Number parsePrologNumber(String s) {
        if (s == null || s.isEmpty()) return null;
        return parseNumberToken(s.trim());
    }
    // END_CHANGE: ISS-2025-0400
    // END_CHANGE: ISS-2025-0235

    // START_CHANGE: ISS-2025-0400 - strict ISO number-token parser shared by number_chars/2,
    // number_codes/2 and atom_number/2 (mirrors core.parser.v2.Lexer's number tokenization).
    /** Parse text as a Prolog number token (ISO 6.4.4/6.4.5): decimal integers, 0x/0o/0b radix
     *  integers, 0'c character-code constants (including escapes and 0'''), and strict floats
     *  (digits '.' digits and/or exponent). An optional leading layout and a +/- sign are
     *  accepted (ISO 8.16.7.1 / 6.3.1.2); trailing characters and the Java-only spellings
     *  (Infinity, NaN, '.5', '3.', d/f suffixes, '_' separators) are rejected.
     *  Returns null if the text is not a valid Prolog number. Integers parse via BigInteger so
     *  values beyond 64 bits stay exact (ISS-2025-0365); float syntax yields a FLOAT even for
     *  integral values like "1.0e10" (ISS-2025-0399). */
    public static Number parseNumberToken(String s) {
        if (s == null) return null;
        int len = s.length();
        int i = 0;
        while (i < len && Character.isWhitespace(s.charAt(i))) i++;   // leading layout (ISO 8.16.7.1)
        boolean neg = false;
        if (i < len && (s.charAt(i) == '-' || s.charAt(i) == '+')) {
            neg = s.charAt(i) == '-';
            i++;
        }
        if (i >= len || s.charAt(i) < '0' || s.charAt(i) > '9') return null;
        try {
            if (s.charAt(i) == '0' && i + 1 < len) {
                char marker = s.charAt(i + 1);
                if (marker == '\'') {                                 // 0'c character-code constant
                    int code = parseCharCodeConstant(s, i + 2);
                    if (code < 0) return null;
                    return new Number(neg ? -(long) code : (long) code);
                }
                int radix = (marker == 'x' || marker == 'X') ? 16
                          : (marker == 'o' || marker == 'O') ? 8
                          : (marker == 'b' || marker == 'B') ? 2 : 0;
                if (radix != 0) {
                    String digits = s.substring(i + 2);
                    if (digits.isEmpty()) return null;
                    for (int j = 0; j < digits.length(); j++) {
                        if (Character.digit(digits.charAt(j), radix) < 0) return null;
                    }
                    java.math.BigInteger bi = new java.math.BigInteger(digits, radix);
                    return new Number(neg ? bi.negate() : bi);
                }
            }
            int bodyStart = i;
            while (i < len && s.charAt(i) >= '0' && s.charAt(i) <= '9') i++;
            boolean isFloat = false;
            // fraction: '.' must be followed by a digit (so "3." is the integer 3 + end mark)
            if (i + 1 < len && s.charAt(i) == '.' && s.charAt(i + 1) >= '0' && s.charAt(i + 1) <= '9') {
                isFloat = true;
                i += 2;
                while (i < len && s.charAt(i) >= '0' && s.charAt(i) <= '9') i++;
            }
            // exponent: e/E, optional sign, at least one digit
            if (i < len && (s.charAt(i) == 'e' || s.charAt(i) == 'E')) {
                int j = i + 1;
                if (j < len && (s.charAt(j) == '+' || s.charAt(j) == '-')) j++;
                if (j >= len || s.charAt(j) < '0' || s.charAt(j) > '9') return null;
                isFloat = true;
                i = j + 1;
                while (i < len && s.charAt(i) >= '0' && s.charAt(i) <= '9') i++;
            }
            // START_CHANGE: ISS-2025-0712 - a rational literal NrD (SWI), as the v2 lexer reads it
            if (!isFloat && i + 1 < len && s.charAt(i) == 'r' && s.charAt(i + 1) >= '0' && s.charAt(i + 1) <= '9') {
                int denStart = i + 1;
                int j = denStart;
                while (j < len && s.charAt(j) >= '0' && s.charAt(j) <= '9') j++;
                if (j != len) return null;
                java.math.BigInteger n = new java.math.BigInteger(s.substring(bodyStart, i));
                java.math.BigInteger d = new java.math.BigInteger(s.substring(denStart, j));
                if (d.signum() == 0) return null;
                return it.denzosoft.jprolog.core.terms.Rational.of(neg ? n.negate() : n, d);
            }
            // END_CHANGE: ISS-2025-0712
            if (i != len) return null;                                // trailing characters
            String body = s.substring(bodyStart, len);
            if (isFloat) {
                double d = Double.parseDouble(body);
                return new Number(neg ? -d : d, false);
            }
            java.math.BigInteger bi = new java.math.BigInteger(body);
            return new Number(neg ? bi.negate() : bi);
        } catch (NumberFormatException e) {
            return null;
        }
    }

    /** Parse the single-quoted character of a 0'c constant starting at {@code from}; the
     *  character (plain, escape sequence, or '' / ''' for the quote) must extend exactly to the
     *  end of the text. Returns the code point, or -1 if invalid. */
    private static int parseCharCodeConstant(String s, int from) {
        int len = s.length();
        if (from >= len) return -1;
        char c = s.charAt(from);
        if (c == '\\') {
            return parseCharEscape(s, from + 1);
        }
        if (c == '\'') {                                              // 0''' (ISO) or lenient 0''
            int after = from + 1;
            if (after < len && s.charAt(after) == '\'') after++;
            return after == len ? '\'' : -1;
        }
        int cp = s.codePointAt(from);
        return from + Character.charCount(cp) == len ? cp : -1;
    }

    /** Parse a backslash escape body (backslash already consumed) ending exactly at the end of
     *  the text. Mirrors core.parser.v2.Lexer.readEscape. Returns the code point or -1. */
    private static int parseCharEscape(String s, int from) {
        int len = s.length();
        if (from >= len) return -1;
        char e = s.charAt(from);
        switch (e) {
            case 'n': return from + 1 == len ? '\n' : -1;
            case 't': return from + 1 == len ? '\t' : -1;
            case 'r': return from + 1 == len ? '\r' : -1;
            case 'a': return from + 1 == len ? 7 : -1;
            case 'b': return from + 1 == len ? '\b' : -1;
            case 'f': return from + 1 == len ? '\f' : -1;
            case 'v': return from + 1 == len ? 11 : -1;
            case '\\': return from + 1 == len ? '\\' : -1;
            case '\'': return from + 1 == len ? '\'' : -1;
            case '"': return from + 1 == len ? '"' : -1;
            case '`': return from + 1 == len ? '`' : -1;
            case 'x': {                                               // \xHH...\ hex escape
                long val = 0;
                int j = from + 1, digits = 0;
                while (j < len && Character.digit(s.charAt(j), 16) >= 0) {
                    val = val * 16 + Character.digit(s.charAt(j), 16);
                    if (val > Character.MAX_CODE_POINT) return -1;
                    j++; digits++;
                }
                if (digits == 0) return -1;
                if (j < len && s.charAt(j) == '\\') j++;
                return j == len ? (int) val : -1;
            }
            default: {
                if (e >= '0' && e <= '7') {                           // \NNN\ octal escape
                    long val = 0;
                    int j = from;
                    while (j < len && s.charAt(j) >= '0' && s.charAt(j) <= '7') {
                        val = val * 8 + (s.charAt(j) - '0');
                        if (val > Character.MAX_CODE_POINT) return -1;
                        j++;
                    }
                    if (j < len && s.charAt(j) == '\\') j++;
                    return j == len ? (int) val : -1;
                }
                return -1;
            }
        }
    }
    // END_CHANGE: ISS-2025-0400
}
