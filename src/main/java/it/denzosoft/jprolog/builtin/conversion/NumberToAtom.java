package it.denzosoft.jprolog.builtin.conversion;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * number_to_atom(+Number, -Atom) / atom_to_number(+Atom, -Number)
 * Convert between number and atom representations.
 */
public class NumberToAtom implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) return false;

        Term arg1 = query.getArguments().get(0).resolveBindings(bindings);
        Term arg2 = query.getArguments().get(1).resolveBindings(bindings);

        if (arg1.isGround() && !arg2.isGround()) {
            if (arg1 instanceof Number) {
                double val = ((Number) arg1).getValue();
                String str = (val == Math.floor(val) && !Double.isInfinite(val))
                    ? String.valueOf((long) val) : String.valueOf(val);
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (arg2.unify(new Atom(str), newBindings)) {
                    solutions.add(newBindings);
                    return true;
                }
            } else if (arg1 instanceof Atom) {
                // atom_to_number mode
                try {
                    double val = Double.parseDouble(((Atom) arg1).getName());
                    Map<String, Term> newBindings = new HashMap<>(bindings);
                    if (arg2.unify(new Number(val), newBindings)) {
                        solutions.add(newBindings);
                        return true;
                    }
                } catch (NumberFormatException e) {
                    return false;
                }
            }
        } else if (!arg1.isGround() && arg2.isGround()) {
            if (arg2 instanceof Atom) {
                try {
                    double val = Double.parseDouble(((Atom) arg2).getName());
                    Map<String, Term> newBindings = new HashMap<>(bindings);
                    if (arg1.unify(new Number(val), newBindings)) {
                        solutions.add(newBindings);
                        return true;
                    }
                } catch (NumberFormatException e) {
                    return false;
                }
            } else if (arg2 instanceof Number) {
                double val = ((Number) arg2).getValue();
                String str = (val == Math.floor(val) && !Double.isInfinite(val))
                    ? String.valueOf((long) val) : String.valueOf(val);
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (arg1.unify(new Atom(str), newBindings)) {
                    solutions.add(newBindings);
                    return true;
                }
            }
        }
        return false;
    }
}
