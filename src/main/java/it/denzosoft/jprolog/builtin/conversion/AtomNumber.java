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
            boolean same;
            if (atomAsNumber.isInteger() && numberValue.isInteger()) {
                same = atomAsNumber.bigIntegerValue().equals(numberValue.bigIntegerValue());
            } else {
                same = Math.abs(atomAsNumber.doubleValue() - numberValue.doubleValue()) < 1e-10;
            }
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
    /** Format a Number exactly: BigInteger digits for integers, double syntax for floats.
     *  Integral floats within long range keep the historical digits-only form (123.0 -> "123");
     *  beyond long range the (long) cast would corrupt, so the double syntax is used instead. */
    static String formatNumberExact(Number n) {
        if (n.isInteger()) {
            return n.bigIntegerValue().toString();
        }
        double v = n.doubleValue();
        if (v == Math.floor(v) && !Double.isInfinite(v) && Math.abs(v) < 9.223372036854776E18) {
            return String.valueOf((long) v);
        }
        return String.valueOf(v);
    }

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
    static Number parsePrologNumber(String s) {
        if (s == null || s.isEmpty()) return null;
        String t = s.trim();
        boolean neg = false;
        int i = 0;
        if (t.startsWith("-")) { neg = true; i = 1; }
        else if (t.startsWith("+")) { i = 1; }
        String body = t.substring(i);
        try {
            if (body.startsWith("0x") || body.startsWith("0X")) {
                java.math.BigInteger bi = new java.math.BigInteger(body.substring(2), 16);
                if (neg) bi = bi.negate();
                return new Number(bi);
            }
            if (body.startsWith("0o") || body.startsWith("0O")) {
                java.math.BigInteger bi = new java.math.BigInteger(body.substring(2), 8);
                if (neg) bi = bi.negate();
                return new Number(bi);
            }
            if (body.startsWith("0b") || body.startsWith("0B")) {
                java.math.BigInteger bi = new java.math.BigInteger(body.substring(2), 2);
                if (neg) bi = bi.negate();
                return new Number(bi);
            }
            // START_CHANGE: ISS-2025-0365 - all-digit decimals parse via BigInteger so integers
            // beyond 64 bits stay exact (the old Double.parseDouble + (long) cast saturated)
            Number exact = parseExactInteger(t);
            if (exact != null) {
                return exact;
            }
            // END_CHANGE: ISS-2025-0365
            // Default: try double
            double d = Double.parseDouble(t);
            return new Number(d, false);
        } catch (NumberFormatException e) {
            return null;
        }
    }
    // END_CHANGE: ISS-2025-0235
}
