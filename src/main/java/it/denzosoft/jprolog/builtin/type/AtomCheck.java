package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.BuiltIn;
import it.denzosoft.jprolog.PrologEvaluationException;
import it.denzosoft.jprolog.terms.Atom;
import it.denzosoft.jprolog.terms.Term;
import it.denzosoft.jprolog.terms.Variable;

import java.util.List;
import java.util.Map;

public class AtomCheck implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("atom/1 requires exactly one argument.");
        }

        Term termArg = query.getArguments().get(0);

        if (termArg.isGround()) {
            // Ground -> deterministic check
            boolean isAtom = (termArg instanceof Atom);

            if (isAtom) {
                solutions.add(bindings); // Add current (unchanged) binding set
                return true;
            } else {
                return false; // Not an atom
            }
        } else {
            // Non-ground -> disallow unless assumption changed.
            // Some Prolog systems allow meta-programming on untyped variables.
            // Here: Raise error or silently fail. Choosing explicit exception.
            throw new PrologEvaluationException("atom/1: Argument must be ground.");
        }
    }
}
