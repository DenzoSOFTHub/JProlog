package it.denzosoft.jprolog.builtin.conversion;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.builtin.string.TextTerm;
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



public class AtomChars implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("atom_chars/2 requires exactly 2 arguments.");
        }

        // START_CHANGE: ISS-2025-0084 - Resolve bindings before type/ground checks
        Term atomTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term charsTerm = query.getArguments().get(1).resolveBindings(bindings);
        // END_CHANGE: ISS-2025-0084

        // START_CHANGE: ISS-2025-0405/ISS-2025-0406 - atom-first ISO 8.16.4 semantics with SWI
        // text interop (strings accepted as text, numbers stringified) and ISO error terms
        // instead of silent failure.
        String atomText = atomText(atomTerm);
        if (atomText != null) {
            // (+Atom, ?Chars): convert the text to a char list and unify (covers check mode too)
            if (charsTerm instanceof PrologString) {
                // SWI interop: compare text directly when the chars side is a string
                if (atomText.equals(((PrologString) charsTerm).getStringValue())) {
                    solutions.add(new HashMap<>(bindings));
                    return true;
                }
                return false;
            }
            Term charList = buildCharList(atomText);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (charsTerm.unify(charList, newBindings)) {
                solutions.add(new HashMap<>(newBindings));
                return true;
            }
            return false;
        }
        if (!(atomTerm instanceof Variable)) {
            throw new PrologException(ISOErrorTerms.typeError("atom", atomTerm, "atom_chars/2"));
        }

        // (-Atom, +Chars): convert the char list (or string) to an atom
        String charsText = charsText(charsTerm);
        if (charsText == null) {
            // both sides unbound / partial char list (ISO 8.16.4.3 a)
            throw new PrologException(ISOErrorTerms.instantiationError("atom_chars/2"));
        }
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (atomTerm.unify(new Atom(charsText), newBindings)) {
            solutions.add(new HashMap<>(newBindings));
            return true;
        }
        return false;
        // END_CHANGE: ISS-2025-0405/ISS-2025-0406
    }

    // START_CHANGE: ISS-2025-0405/ISS-2025-0406 - text extraction helpers
    /** Text of the atom side: atom name, string value (SWI interop), or the canonical text of a
     *  number (SWI/GNU treat numbers as their text here); null for variables/compounds. */
    private String atomText(Term t) {
        String text = TextTerm.textOf(t);
        if (text != null) {
            return text;
        }
        if (t instanceof Number) {
            return AtomNumber.formatNumberExact((Number) t);
        }
        return null;
    }

    /** Text of a proper list of one-char atoms (or of a string, ISS-2025-0405); null for a
     *  variable / partial list / list with unbound elements; throws type_error(character, E) /
     *  type_error(list, L) when malformed. */
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
                throw new PrologException(ISOErrorTerms.typeError("list", list, "atom_chars/2"));
            }
            Term element = compound.getArguments().get(0);
            if (element instanceof Variable) {
                return null; // element unbound -> instantiation_error at the caller
            }
            if (!(element instanceof Atom) || !isOneChar(((Atom) element).getName())) {
                throw new PrologException(ISOErrorTerms.typeError("character", element, "atom_chars/2"));
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
        throw new PrologException(ISOErrorTerms.typeError("list", list, "atom_chars/2"));
    }

    /** True if the text is exactly one character (one code point). */
    private boolean isOneChar(String s) {
        return !s.isEmpty() && Character.charCount(s.codePointAt(0)) == s.length();
    }
    // END_CHANGE: ISS-2025-0405/ISS-2025-0406

    private Term buildCharList(String str) {
        // START_CHANGE: ISS-2025-0211 - iterate by codepoint, not by Java char, to handle surrogate pairs
        List<String> chars = new ArrayList<>();
        int i = 0;
        while (i < str.length()) {
            int cp = str.codePointAt(i);
            chars.add(new String(Character.toChars(cp)));
            i += Character.charCount(cp);
        }
        Term result = new Atom("[]");
        for (int k = chars.size() - 1; k >= 0; k--) {
            List<Term> args = new ArrayList<>();
            args.add(new Atom(chars.get(k)));
            args.add(result);
            result = new CompoundTerm(new Atom("."), args);
        }
        return result;
        // END_CHANGE: ISS-2025-0211
    }
}
