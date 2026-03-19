package it.denzosoft.jprolog.builtin.conversion;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * string_to_atom(?String, ?Atom) - Convert between string and atom.
 * Also handles atom_to_term-like conversions for simple atoms.
 */
public class StringToAtom implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) return false;

        Term strTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term atomTerm = query.getArguments().get(1).resolveBindings(bindings);

        if (strTerm.isGround() && !atomTerm.isGround()) {
            String value;
            if (strTerm instanceof PrologString) {
                value = ((PrologString) strTerm).getStringValue();
            } else if (strTerm instanceof Atom) {
                value = ((Atom) strTerm).getName();
            } else {
                value = strTerm.toString();
            }
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (atomTerm.unify(new Atom(value), newBindings)) {
                solutions.add(newBindings);
                return true;
            }
        } else if (!strTerm.isGround() && atomTerm.isGround()) {
            String value;
            if (atomTerm instanceof Atom) {
                value = ((Atom) atomTerm).getName();
            } else {
                value = atomTerm.toString();
            }
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (strTerm.unify(new Atom(value), newBindings)) {
                solutions.add(newBindings);
                return true;
            }
        } else if (strTerm.isGround() && atomTerm.isGround()) {
            String s1 = strTerm instanceof Atom ? ((Atom) strTerm).getName() :
                        strTerm instanceof PrologString ? ((PrologString) strTerm).getStringValue() :
                        strTerm.toString();
            String s2 = atomTerm instanceof Atom ? ((Atom) atomTerm).getName() : atomTerm.toString();
            if (s1.equals(s2)) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
        }
        return false;
    }
}
