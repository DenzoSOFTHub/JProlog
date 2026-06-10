package it.denzosoft.jprolog.builtin.atom;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.builtin.string.TextTerm;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class AtomLength implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("atom_length/2 requires exactly two arguments.");
        }

        // START_CHANGE: ISS-2025-0080 - Resolve bindings before type/ground checks
        Term atomArg = query.getArguments().get(0).resolveBindings(bindings);
        Term lengthArg = query.getArguments().get(1).resolveBindings(bindings);
        // END_CHANGE: ISS-2025-0080

        // START_CHANGE: ISS-2025-0277 - ISO error terms: instantiation_error for an unbound atom
        // argument, type_error(atom, Culprit) for a non-atom (was a bare PrologEvaluationException).
        if (atomArg instanceof Variable || !atomArg.isGround()) {
            throw new PrologException(ISOErrorTerms.instantiationError("atom_length/2"));
        }
        // START_CHANGE: ISS-2025-0405 - SWI text interop: strings are accepted as their text
        String atomString = TextTerm.textOf(atomArg);
        if (atomString == null) {
            throw new PrologException(ISOErrorTerms.typeError("atom", atomArg, "atom_length/2"));
        }
        // END_CHANGE: ISS-2025-0405
        // END_CHANGE: ISS-2025-0277

        // START_CHANGE: ISS-2025-0406 - ISO 8.16.1.3: Length neither var nor integer ->
        // type_error(integer, Length); a negative integer -> domain_error(not_less_than_zero, L)
        if (!(lengthArg instanceof Variable)) {
            if (!(lengthArg instanceof Number) || !((Number) lengthArg).isInteger()) {
                throw new PrologException(ISOErrorTerms.typeError("integer", lengthArg, "atom_length/2"));
            }
            if (((Number) lengthArg).longValue() < 0) {
                throw new PrologException(ISOErrorTerms.domainError("not_less_than_zero", lengthArg, "atom_length/2"));
            }
        }
        // END_CHANGE: ISS-2025-0406
        // START_CHANGE: ISS-2025-0193 - Use codePointCount for correct Unicode character counting
        double lengthValue = atomString.codePointCount(0, atomString.length());
        // END_CHANGE: ISS-2025-0193

        Term computedLength = new Number(lengthValue);

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (lengthArg.unify(computedLength, newBindings)) {
            solutions.add(new HashMap<>(newBindings));
            return true;
        }

        return false; // Unification failed
    }
}
