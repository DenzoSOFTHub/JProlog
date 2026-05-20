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
            
            double numberValue = ((Number) numberTerm).getValue();
            String atomValue = formatNumber(numberValue);
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
            double numberValue = ((Number) numberTerm).getValue();
            
            try {
                double atomAsNumber = Double.parseDouble(atomValue);
                if (Math.abs(atomAsNumber - numberValue) < 1e-10) {
                    solutions.add(new HashMap<>(bindings));
                    return true;
                }
                return false;
            } catch (NumberFormatException e) {
                return false; // Atom is not a valid number
            }
        } else {
            // START_CHANGE: ISS-2025-0084 - Return false instead of throwing for normal failure
            return false;
            // END_CHANGE: ISS-2025-0084
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

    // START_CHANGE: ISS-2025-0235 - parse Prolog number syntax: decimals, floats, hex, binary, octal
    private static Number parsePrologNumber(String s) {
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
            // Default: try double
            double d = Double.parseDouble(t);
            // Preserve integer type if value has no fractional part and was without exponent/decimal
            boolean hasFraction = t.contains(".") || t.toLowerCase().contains("e");
            if (!hasFraction && d == Math.floor(d) && !Double.isInfinite(d)) {
                return new Number((long) d);
            }
            return new Number(d, false);
        } catch (NumberFormatException e) {
            return null;
        }
    }
    // END_CHANGE: ISS-2025-0235
}
