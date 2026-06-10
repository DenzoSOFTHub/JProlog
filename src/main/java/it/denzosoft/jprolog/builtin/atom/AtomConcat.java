package it.denzosoft.jprolog.builtin.atom;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.builtin.string.TextTerm;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
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
            // START_CHANGE: ISS-2025-0405/ISS-2025-0406 - strings accepted as text; ISO
            // type_error(atom, Culprit) naming the offending argument (was a bare message ball)
            String resultStr = requireText(atom1) + requireText(atom2);
            // END_CHANGE: ISS-2025-0405/ISS-2025-0406
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
            // START_CHANGE: ISS-2025-0405/ISS-2025-0406 - text interop + ISO type errors
            String prefix = requireText(atom1);
            String full = requireText(atom12);
            // END_CHANGE: ISS-2025-0405/ISS-2025-0406
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
            // START_CHANGE: ISS-2025-0405/ISS-2025-0406 - text interop + ISO type errors
            String suffix = requireText(atom2);
            String full = requireText(atom12);
            // END_CHANGE: ISS-2025-0405/ISS-2025-0406
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
            // START_CHANGE: ISS-2025-0405/ISS-2025-0406 - text interop + ISO type errors
            String fullString = requireText(atom12);
            // END_CHANGE: ISS-2025-0405/ISS-2025-0406
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
            // START_CHANGE: ISS-2025-0405/ISS-2025-0406 - text interop + ISO type errors
            // (was a silent false for non-atom arguments)
            String concat = requireText(atom1) + requireText(atom2);
            if (concat.equals(requireText(atom12))) {
            // END_CHANGE: ISS-2025-0405/ISS-2025-0406
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            return false;
        } else {
            // START_CHANGE: ISS-2025-0406 - ISO 8.16.2.3 a: A1_2 unbound together with A1 or A2
            // unbound is an instantiation error (was a silent false, ISS-2025-0191)
            throw new PrologException(ISOErrorTerms.instantiationError("atom_concat/3"));
            // END_CHANGE: ISS-2025-0406
        }
    }

    // START_CHANGE: ISS-2025-0405/ISS-2025-0406 - shared text extraction with ISO type error
    /** Text of an atom or (SWI interop) string argument; any other bound term raises
     *  type_error(atom, Culprit) naming the actual offending argument (ISO 8.16.2.3 b-d). */
    private String requireText(Term t) {
        String text = TextTerm.textOf(t);
        if (text == null) {
            throw new PrologException(ISOErrorTerms.typeError("atom", t, "atom_concat/3"));
        }
        return text;
    }
    // END_CHANGE: ISS-2025-0405/ISS-2025-0406
}
