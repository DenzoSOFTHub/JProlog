package it.denzosoft.jprolog.builtin.conversion;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

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

        // START_CHANGE: ISS-2025-0399/ISS-2025-0400/ISS-2025-0406 - ISO 8.16.8 semantics: a proper
        // code list (or, SWI-style, a string) is parsed as a Prolog number token and unified with
        // Number, so float text yields a FLOAT and the both-ground mode compares exactly; otherwise
        // the number is formatted to its canonical codes. Error cases raise the ISO error terms.
        if (!(numberTerm instanceof Variable) && !(numberTerm instanceof Number)) {
            throw new PrologException(ISOErrorTerms.typeError("number", numberTerm, "number_codes/2"));
        }

        String text = codesText(codesTerm);
        if (text != null) {
            // Parse the text as an ISO number token (6.4.4/6.4.5) and unify (ISO 8.16.8.1 a)
            Number value = AtomNumber.parseNumberToken(text);
            if (value == null) {
                throw new PrologException(ISOErrorTerms.syntaxError("illegal_number", "number_codes/2"));
            }
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (numberTerm.unify(value, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        }

        // List side is not usable text: a variable / partial list needs Number instantiated
        if (numberTerm instanceof Variable) {
            throw new PrologException(ISOErrorTerms.instantiationError("number_codes/2"));
        }
        String numberStr = AtomNumber.formatNumberExact((Number) numberTerm);
        Term codesList = buildCodesList(numberStr);
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (codesTerm.unify(codesList, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
        // END_CHANGE: ISS-2025-0399/ISS-2025-0400/ISS-2025-0406
    }

    // START_CHANGE: ISS-2025-0406 - list walker with ISO error reporting: returns the text of a
    // proper list of character codes (or of a string, ISS-2025-0405), null for a variable/partial
    // list, and throws representation_error(character_code) / type_error(list, L) for malformed
    // input (ISO 8.16.8.3).
    private String codesText(Term list) {
        if (list instanceof PrologString) {
            return ((PrologString) list).getStringValue();
        }
        if (list instanceof Variable) {
            return null;
        }
        StringBuilder sb = new StringBuilder();
        Term current = list;
        while (current instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) current;
            if (!compound.getName().equals(".") || compound.getArguments().size() != 2) {
                throw new PrologException(ISOErrorTerms.typeError("list", list, "number_codes/2"));
            }
            Term element = compound.getArguments().get(0);
            if (element instanceof Variable) {
                return null; // element unbound -> not usable as text
            }
            if (!(element instanceof Number) || !((Number) element).isInteger()) {
                throw new PrologException(ISOErrorTerms.representationError("character_code", "number_codes/2"));
            }
            long code = ((Number) element).longValue();
            // START_CHANGE: ISS-2025-0212 - Extend to full Unicode (U+10FFFF) for consistency with atom_codes
            if (code < 0 || code > 0x10FFFF) {
                throw new PrologException(ISOErrorTerms.representationError("character_code", "number_codes/2"));
            }
            // END_CHANGE: ISS-2025-0212
            sb.appendCodePoint((int) code);
            current = compound.getArguments().get(1);
        }
        if (current instanceof Variable) {
            return null; // partial list
        }
        if (current instanceof Atom && ((Atom) current).getName().equals("[]")) {
            return sb.toString();
        }
        throw new PrologException(ISOErrorTerms.typeError("list", list, "number_codes/2"));
    }
    // END_CHANGE: ISS-2025-0406

    private Term buildCodesList(String str) {
        Term result = new Atom("[]");
        for (int i = str.length() - 1; i >= 0; i--) {
            List<Term> args = new ArrayList<>();
            args.add(new Number((long) str.charAt(i))   /* ISS-2025-0424 */); // ASCII code as Number
            args.add(result);
            result = new CompoundTerm(new Atom("."), args);
        }
        return result;
    }
}
