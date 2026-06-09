package it.denzosoft.jprolog.builtin.atom;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
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
        Term lengthArg = query.getArguments().get(1);
        // END_CHANGE: ISS-2025-0080

        // START_CHANGE: ISS-2025-0277 - ISO error terms: instantiation_error for an unbound atom
        // argument, type_error(atom, Culprit) for a non-atom (was a bare PrologEvaluationException).
        if (atomArg instanceof Variable || !atomArg.isGround()) {
            throw new PrologException(ISOErrorTerms.instantiationError("atom_length/2"));
        }
        if (!(atomArg instanceof Atom)) {
            throw new PrologException(ISOErrorTerms.typeError("atom", atomArg, "atom_length/2"));
        }
        // END_CHANGE: ISS-2025-0277

        String atomString = ((Atom) atomArg).getName();
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
