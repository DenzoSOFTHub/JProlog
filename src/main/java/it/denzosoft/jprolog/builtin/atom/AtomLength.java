package it.denzosoft.jprolog.builtin.atom;

import it.denzosoft.jprolog.BuiltIn;
import it.denzosoft.jprolog.PrologEvaluationException;
import it.denzosoft.jprolog.terms.Atom;
import it.denzosoft.jprolog.terms.Number;
import it.denzosoft.jprolog.terms.Term;
import it.denzosoft.jprolog.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class AtomLength implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("atom_length/2 requires exactly two arguments.");
        }

        Term atomArg = query.getArguments().get(0);
        Term lengthArg = query.getArguments().get(1);

        if (!atomArg.isGround()) {
            throw new PrologEvaluationException("atom_length/2: First argument (Atom) must be ground.");
        }

        if (!(atomArg instanceof Atom)) {
            throw new PrologEvaluationException("atom_length/2: First argument must be an atom.");
        }

        String atomString = ((Atom) atomArg).getName();
        double lengthValue = atomString.length(); // convert int to double

        Term computedLength = new Number(lengthValue);

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (lengthArg.unify(computedLength, newBindings)) {
            solutions.add(new HashMap<>(newBindings));
            return true;
        }

        return false; // Unification failed
    }
}
