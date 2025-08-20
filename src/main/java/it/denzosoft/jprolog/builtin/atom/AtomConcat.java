package it.denzosoft.jprolog.builtin.atom;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class AtomConcat implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("atom_concat/3 requires exactly three arguments.");
        }

        Term atom1 = query.getArguments().get(0);
        Term atom2 = query.getArguments().get(1);
        Term atom12 = query.getArguments().get(2);

        // Mode (+, +, -) => concatenation
        if (atom1.isGround() && atom2.isGround() && !atom12.isGround()) {
            if (!(atom1 instanceof Atom) || !(atom2 instanceof Atom)) {
                throw new PrologEvaluationException("atom_concat/3 (+,+,-): First two arguments must be atoms.");
            }
            String resultStr = ((Atom) atom1).getName() + ((Atom) atom2).getName();
            Term resultAtom = new Atom(resultStr);

            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (atom12.unify(resultAtom, newBindings)) {
                solutions.add(new HashMap<>(newBindings));
                return true;
            }
            return false; // Failed to unify
        }

        // Mode (-, -, +) => splitting up to whitespace-pos?
        else if (!atom1.isGround() && !atom2.isGround() && atom12.isGround()) {
            if (!(atom12 instanceof Atom)) {
                 throw new PrologEvaluationException("atom_concat/3 (-,-,+): Third argument must be an atom.");
            }
            String fullString = ((Atom) atom12).getName();
            // This implementation generates ALL possible splits
            for (int i = 0; i <= fullString.length(); i++) {
                String part1 = fullString.substring(0, i);
                String part2 = fullString.substring(i);
                Term t1 = new Atom(part1);
                Term t2 = new Atom(part2);

                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (atom1.unify(t1, newBindings) && atom2.unify(t2, newBindings)) {
                    solutions.add(new HashMap<>(newBindings));
                    // Don't break here to find all solutions.
                    // Prolog generator semantics allow for multiple unify success paths.
                }
            }
            // Return true if any successful solution was added
            return !solutions.isEmpty();
        } else {
            throw new PrologEvaluationException("atom_concat/3 mode not supported. "+atom1.isGround()+
                    ", "+atom2.isGround()+", "+atom12.isGround());
        }
    }
}
