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

        // START_CHANGE: ISS-2025-0080 - Resolve bindings before type/ground checks
        Term atom1 = query.getArguments().get(0).resolveBindings(bindings);
        Term atom2 = query.getArguments().get(1).resolveBindings(bindings);
        Term atom12 = query.getArguments().get(2).resolveBindings(bindings);
        // END_CHANGE: ISS-2025-0080

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

        // START_CHANGE: ISS-2025-0021 - Add missing atom_concat modes (+,-,+) and (-,+,+)
        // Mode (+, -, +) => extract suffix
        else if (atom1.isGround() && !atom2.isGround() && atom12.isGround()) {
            if (!(atom1 instanceof Atom) || !(atom12 instanceof Atom)) {
                throw new PrologEvaluationException("atom_concat/3 (+,-,+): First and third arguments must be atoms.");
            }
            String prefix = ((Atom) atom1).getName();
            String full = ((Atom) atom12).getName();
            if (full.startsWith(prefix)) {
                String suffix = full.substring(prefix.length());
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (atom2.unify(new Atom(suffix), newBindings)) {
                    solutions.add(new HashMap<>(newBindings));
                    return true;
                }
            }
            return false;
        }

        // Mode (-, +, +) => extract prefix
        else if (!atom1.isGround() && atom2.isGround() && atom12.isGround()) {
            if (!(atom2 instanceof Atom) || !(atom12 instanceof Atom)) {
                throw new PrologEvaluationException("atom_concat/3 (-,+,+): Second and third arguments must be atoms.");
            }
            String suffix = ((Atom) atom2).getName();
            String full = ((Atom) atom12).getName();
            if (full.endsWith(suffix)) {
                String prefix = full.substring(0, full.length() - suffix.length());
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (atom1.unify(new Atom(prefix), newBindings)) {
                    solutions.add(new HashMap<>(newBindings));
                    return true;
                }
            }
            return false;
        }
        // END_CHANGE: ISS-2025-0021

        // Mode (-, -, +) => all possible splits
        else if (!atom1.isGround() && !atom2.isGround() && atom12.isGround()) {
            if (!(atom12 instanceof Atom)) {
                 throw new PrologEvaluationException("atom_concat/3 (-,-,+): Third argument must be an atom.");
            }
            String fullString = ((Atom) atom12).getName();
            for (int i = 0; i <= fullString.length(); i++) {
                String part1 = fullString.substring(0, i);
                String part2 = fullString.substring(i);
                Term t1 = new Atom(part1);
                Term t2 = new Atom(part2);

                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (atom1.unify(t1, newBindings) && atom2.unify(t2, newBindings)) {
                    solutions.add(new HashMap<>(newBindings));
                }
            }
            return !solutions.isEmpty();

        // Mode (+, +, +) => verify concatenation
        } else if (atom1.isGround() && atom2.isGround() && atom12.isGround()) {
            if (!(atom1 instanceof Atom) || !(atom2 instanceof Atom) || !(atom12 instanceof Atom)) {
                return false;
            }
            String concat = ((Atom) atom1).getName() + ((Atom) atom2).getName();
            if (concat.equals(((Atom) atom12).getName())) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            return false;
        } else {
            throw new PrologEvaluationException("atom_concat/3 mode not supported. "+atom1.isGround()+
                    ", "+atom2.isGround()+", "+atom12.isGround());
        }
    }
}
