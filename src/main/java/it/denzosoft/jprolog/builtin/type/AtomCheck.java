package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.List;
import java.util.Map;

public class AtomCheck implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("atom/1 requires exactly one argument.");
        }

        Term termArg = query.getArguments().get(0);

        // Resolve bindings first
        Term resolvedTerm = termArg.resolveBindings(bindings);
        
        if (resolvedTerm.isGround()) {
            // Ground -> deterministic check
            boolean isAtom = (resolvedTerm instanceof Atom);

            if (isAtom) {
                solutions.add(bindings); // Add current (unchanged) binding set
                return true;
            } else {
                return false; // Not an atom
            }
        } else {
            // In ISO Prolog, type checks should fail (not throw) for unbound variables
            return false;
        }
    }
}
