package it.denzosoft.jprolog.builtin.conversion;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implements number_codes/2 built-in predicate for ISO Prolog.
 * 
 * number_codes(+Number, ?Codes) - Convert number to list of ASCII codes
 * number_codes(?Number, +Codes) - Convert list of ASCII codes to number
 * number_codes(+Number, +Codes) - Check if number and codes match
 * 
 * Examples:
 *   ?- number_codes(123, Codes).
 *   Codes = [49, 50, 51].
 * 
 *   ?- number_codes(N, [49, 50, 51]).
 *   N = 123.
 */
public class NumberCodes implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("number_codes/2 requires exactly 2 arguments.");
        }

        Term numberTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term codesTerm = query.getArguments().get(1).resolveBindings(bindings);

        if (numberTerm.isGround() && !codesTerm.isGround()) {
            // Convert number to ASCII codes list
            return numberToCodes(numberTerm, codesTerm, bindings, solutions);
            
        } else if (!numberTerm.isGround() && codesTerm.isGround()) {
            // Convert ASCII codes list to number
            return codesToNumber(numberTerm, codesTerm, bindings, solutions);
            
        } else if (numberTerm.isGround() && codesTerm.isGround()) {
            // Both ground - check if they represent the same value
            return checkNumberCodes(numberTerm, codesTerm, bindings, solutions);
            
        } else {
            throw new PrologEvaluationException("number_codes/2: at least one argument must be ground.");
        }
    }
    
    private boolean numberToCodes(Term numberTerm, Term codesTerm, Map<String, Term> bindings, 
                                  List<Map<String, Term>> solutions) {
        if (!(numberTerm instanceof Number)) {
            return false; // First argument must be a number
        }

        // START_CHANGE: ISS-2025-0365 - format via the Number term (BigInteger-exact for
        // integers); the old double->(long) cast silently saturated past 64 bits
        String numberStr = AtomNumber.formatNumberExact((Number) numberTerm);
        // END_CHANGE: ISS-2025-0365
        Term codesList = buildCodesList(numberStr);

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (codesTerm.unify(codesList, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }
    
    private boolean codesToNumber(Term numberTerm, Term codesTerm, Map<String, Term> bindings,
                                  List<Map<String, Term>> solutions) {
        List<Integer> codes = extractCodes(codesTerm);
        if (codes == null) {
            return false; // Invalid codes list
        }
        
        StringBuilder sb = new StringBuilder();
        for (int code : codes) {
            // ISS-2025-0187: extend to full Unicode BMP range
            if (code < 0 || code > Character.MAX_VALUE) {
                return false; // Invalid ASCII code
            }
            sb.append((char) code);
        }
        
        try {
            // START_CHANGE: ISS-2025-0365 - all-digit text parses via BigInteger so integers
            // beyond 64 bits stay exact; floats keep the double path
            Number value = AtomNumber.parseExactInteger(sb.toString());
            if (value == null) {
                value = new Number(Double.parseDouble(sb.toString()));
            }
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (numberTerm.unify(value, newBindings)) {
            // END_CHANGE: ISS-2025-0365
                solutions.add(newBindings);
                return true;
            }
        } catch (NumberFormatException e) {
            return false; // Codes don't form a valid number
        }

        return false;
    }
    
    private boolean checkNumberCodes(Term numberTerm, Term codesTerm, Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        if (!(numberTerm instanceof Number)) {
            return false;
        }

        List<Integer> codes = extractCodes(codesTerm);
        if (codes == null) {
            return false;
        }

        // Convert codes to string and compare with number string
        StringBuilder sb = new StringBuilder();
        for (int code : codes) {
            // ISS-2025-0187: extend to full Unicode BMP range
            if (code < 0 || code > Character.MAX_VALUE) {
                return false;
            }
            sb.append((char) code);
        }

        try {
            // START_CHANGE: ISS-2025-0365 - compare exactly (BigInteger) when both sides are
            // integers, so big values are not collapsed through double precision
            if (AtomNumber.formatNumberExact((Number) numberTerm).equals(sb.toString())) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            double codesAsNumber = Double.parseDouble(sb.toString());
            Number exact = AtomNumber.parseExactInteger(sb.toString());
            if (exact != null && ((Number) numberTerm).isInteger()) {
                if (exact.bigIntegerValue().equals(((Number) numberTerm).bigIntegerValue())) {
                    solutions.add(new HashMap<>(bindings));
                    return true;
                }
                return false;
            }
            double numberValue = ((Number) numberTerm).getValue();
            if (Math.abs(numberValue - codesAsNumber) < 1e-10) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            // END_CHANGE: ISS-2025-0365
        } catch (NumberFormatException e) {
            return false;
        }

        return false;
    }


    private Term buildCodesList(String str) {
        Term result = new Atom("[]");
        for (int i = str.length() - 1; i >= 0; i--) {
            List<Term> args = new ArrayList<>();
            args.add(new Number((double) str.charAt(i))); // ASCII code as Number
            args.add(result);
            result = new CompoundTerm(new Atom("."), args);
        }
        return result;
    }
    
    private List<Integer> extractCodes(Term list) {
        List<Integer> codes = new ArrayList<>();
        Term current = list;
        
        while (current instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) current;
            if (compound.getName().equals(".") && compound.getArguments().size() == 2) {
                Term element = compound.getArguments().get(0);
                if (element instanceof Number) {
                    double code = ((Number) element).getValue();
                    // START_CHANGE: ISS-2025-0212 - Extend to full Unicode (U+10FFFF) for consistency with atom_codes
                    if (code == Math.floor(code) && code >= 0 && code <= 0x10FFFF) {
                        codes.add((int) code);
                    } else {
                        return null;
                    }
                    // END_CHANGE: ISS-2025-0212
                } else {
                    return null;
                }
                current = compound.getArguments().get(1);
            } else {
                break;
            }
        }
        
        if (current instanceof Atom && ((Atom) current).getName().equals("[]")) {
            return codes;
        } else {
            return null; // Malformed list
        }
    }
}