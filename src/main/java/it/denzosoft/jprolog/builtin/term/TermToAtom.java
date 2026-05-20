package it.denzosoft.jprolog.builtin.term;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.parser.Parser;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * term_to_atom(?Term, ?Atom) - Convert between term and its atom representation.
 */
public class TermToAtom implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) return false;

        Term termArg = query.getArguments().get(0).resolveBindings(bindings);
        Term atomArg = query.getArguments().get(1).resolveBindings(bindings);

        if (termArg.isGround()) {
            // START_CHANGE: ISS-2025-0243 - operator-aware roundtrip via TermFormatter
            String str = it.denzosoft.jprolog.core.util.TermFormatter.format(termArg, true, false, false, 1200);
            // END_CHANGE: ISS-2025-0243
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (atomArg.unify(new Atom(str), newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        } else if (atomArg.isGround() && atomArg instanceof Atom) {
            // Atom -> Term: parse atom string
            String str = ((Atom) atomArg).getName();
            try {
                Parser parser = new Parser();
                Term parsed = parser.parseTerm(str);
                if (parsed != null) {
                    Map<String, Term> newBindings = new HashMap<>(bindings);
                    if (termArg.unify(parsed, newBindings)) {
                        solutions.add(newBindings);
                        return true;
                    }
                }
            } catch (Exception e) {
                return false;
            }
            return false;
        }

        return false;
    }
}
