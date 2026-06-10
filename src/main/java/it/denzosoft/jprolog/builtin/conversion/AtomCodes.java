package it.denzosoft.jprolog.builtin.conversion;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.builtin.string.TextTerm;
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
 * Implementation of atom_codes/2 predicate.
 *
 * atom_codes(+Atom, ?Codes) - Convert between atom and list of character codes
 * atom_codes(?Atom, +Codes) - Convert between list of character codes and atom
 *
 * Examples:
 * ?- atom_codes(hello, X).
 * X = [104, 101, 108, 108, 111].
 *
 * ?- atom_codes(X, [104, 101, 108, 108, 111]).
 * X = hello.
 */
public class AtomCodes implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 2) {
            throw new PrologEvaluationException("atom_codes/2 requires exactly 2 arguments");
        }

        Term atomTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term codesTerm = query.getArguments().get(1).resolveBindings(bindings);

        // START_CHANGE: ISS-2025-0405/ISS-2025-0406 - atom-first ISO 8.16.5 semantics with SWI
        // text interop (strings accepted as text, numbers stringified) and ISO error terms
        // instead of silent failure.
        String atomText = atomText(atomTerm);
        if (atomText != null) {
            // (+Atom, ?Codes): convert the text to a code list and unify (covers check mode too)
            if (codesTerm instanceof PrologString) {
                // SWI interop: compare text directly when the codes side is a string
                if (atomText.equals(((PrologString) codesTerm).getStringValue())) {
                    solutions.add(new HashMap<>(bindings));
                    return true;
                }
                return false;
            }
            List<Term> codes = new ArrayList<>();
            // START_CHANGE: ISS-2025-0193 - Use codePoints for correct supplementary Unicode
            atomText.codePoints().forEach(cp ->
                codes.add(new it.denzosoft.jprolog.core.terms.Number((long) cp)));
            // END_CHANGE: ISS-2025-0193
            Term codesList = createList(codes);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (codesTerm.unify(codesList, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        }
        if (!(atomTerm instanceof Variable)) {
            throw new PrologException(ISOErrorTerms.typeError("atom", atomTerm, "atom_codes/2"));
        }

        // (-Atom, +Codes): convert the code list (or string) to an atom
        String codesText = codesText(codesTerm);
        if (codesText == null) {
            // both sides unbound / partial code list (ISO 8.16.5.3 a)
            throw new PrologException(ISOErrorTerms.instantiationError("atom_codes/2"));
        }
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (atomTerm.unify(new Atom(codesText), newBindings)) {
            solutions.add(newBindings);
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

    /** Text of a proper list of character codes (or of a string, ISS-2025-0405); null for a
     *  variable / partial list / list with unbound elements; throws
     *  representation_error(character_code) / type_error(list, L) when malformed (ISO 8.16.5.3). */
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
                throw new PrologException(ISOErrorTerms.typeError("list", list, "atom_codes/2"));
            }
            Term element = compound.getArguments().get(0);
            if (element instanceof Variable) {
                return null; // element unbound -> instantiation_error at the caller
            }
            // START_CHANGE: ISS-2025-0193 - Support supplementary Unicode codepoints
            if (!(element instanceof Number) || !((Number) element).isInteger()) {
                throw new PrologException(ISOErrorTerms.representationError("character_code", "atom_codes/2"));
            }
            long code = ((Number) element).longValue();
            if (code < 0 || code > 0x10FFFF) {
                throw new PrologException(ISOErrorTerms.representationError("character_code", "atom_codes/2"));
            }
            sb.appendCodePoint((int) code);
            // END_CHANGE: ISS-2025-0193
            current = compound.getArguments().get(1);
        }
        if (current instanceof Variable) {
            return null; // partial list
        }
        if (current instanceof Atom && ((Atom) current).getName().equals("[]")) {
            return sb.toString();
        }
        throw new PrologException(ISOErrorTerms.typeError("list", list, "atom_codes/2"));
    }
    // END_CHANGE: ISS-2025-0405/ISS-2025-0406

    /**
     * Create a Prolog list from a Java list of terms.
     */
    private Term createList(List<Term> elements) {
        if (elements.isEmpty()) {
            return new Atom("[]");
        }

        Term result = new Atom("[]");
        for (int i = elements.size() - 1; i >= 0; i--) {
            List<Term> args = new ArrayList<>();
            args.add(elements.get(i));
            args.add(result);
            result = new CompoundTerm(new Atom("."), args);
        }
        return result;
    }
}
