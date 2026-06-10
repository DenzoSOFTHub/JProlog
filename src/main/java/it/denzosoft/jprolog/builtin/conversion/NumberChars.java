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



public class NumberChars implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("number_chars/2 requires exactly 2 arguments.");
        }

        // START_CHANGE: ISS-2025-0084 - Resolve bindings before type/ground checks
        Term numberTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term charsTerm = query.getArguments().get(1).resolveBindings(bindings);
        // END_CHANGE: ISS-2025-0084

        // START_CHANGE: ISS-2025-0399/ISS-2025-0400/ISS-2025-0406 - ISO 8.16.7 semantics: a proper
        // char list (or, SWI-style, a string) is parsed as a Prolog number token and unified with
        // Number, so float text yields a FLOAT and the both-ground mode compares exactly; otherwise
        // the number is formatted to its canonical chars. Error cases raise the ISO error terms.
        if (!(numberTerm instanceof Variable) && !(numberTerm instanceof Number)) {
            throw new PrologException(ISOErrorTerms.typeError("number", numberTerm, "number_chars/2"));
        }

        String text = charsText(charsTerm);
        if (text != null) {
            // Parse the text as an ISO number token (6.4.4/6.4.5) and unify (ISO 8.16.7.1 a)
            Number value = AtomNumber.parseNumberToken(text);
            if (value == null) {
                throw new PrologException(ISOErrorTerms.syntaxError("illegal_number", "number_chars/2"));
            }
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (numberTerm.unify(value, newBindings)) {
                solutions.add(new HashMap<>(newBindings));
                return true;
            }
            return false;
        }

        // List side is not usable text: a variable / partial list needs Number instantiated
        if (numberTerm instanceof Variable) {
            throw new PrologException(ISOErrorTerms.instantiationError("number_chars/2"));
        }
        String numberStr = AtomNumber.formatNumberExact((Number) numberTerm);
        Term charList = buildCharList(numberStr);
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (charsTerm.unify(charList, newBindings)) {
            solutions.add(new HashMap<>(newBindings));
            return true;
        }
        return false;
        // END_CHANGE: ISS-2025-0399/ISS-2025-0400/ISS-2025-0406
    }

    // START_CHANGE: ISS-2025-0406 - list walker with ISO error reporting: returns the text of a
    // proper list of one-char atoms (or of a string, ISS-2025-0405), null for a variable/partial
    // list, and throws type_error(character, E) / type_error(list, L) for malformed input.
    private String charsText(Term list) {
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
                throw new PrologException(ISOErrorTerms.typeError("list", list, "number_chars/2"));
            }
            Term element = compound.getArguments().get(0);
            if (element instanceof Variable) {
                return null; // element unbound -> not usable as text
            }
            if (!(element instanceof Atom) || !isOneChar(((Atom) element).getName())) {
                throw new PrologException(ISOErrorTerms.typeError("character", element, "number_chars/2"));
            }
            sb.append(((Atom) element).getName());
            current = compound.getArguments().get(1);
        }
        if (current instanceof Variable) {
            return null; // partial list
        }
        if (current instanceof Atom && ((Atom) current).getName().equals("[]")) {
            return sb.toString();
        }
        throw new PrologException(ISOErrorTerms.typeError("list", list, "number_chars/2"));
    }

    /** True if the text is exactly one character (one code point). */
    private boolean isOneChar(String s) {
        return !s.isEmpty() && Character.charCount(s.codePointAt(0)) == s.length();
    }
    // END_CHANGE: ISS-2025-0406

    private Term buildCharList(String str) {
        Term result = new Atom("[]");
        for (int i = str.length() - 1; i >= 0; i--) {
            List<Term> args = new ArrayList<>();
            args.add(new Atom(String.valueOf(str.charAt(i))));
            args.add(result);
            result = new CompoundTerm(new Atom("."), args);
        }
        return result;
    }
}
