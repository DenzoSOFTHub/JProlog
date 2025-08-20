package it.denzosoft.jprolog.builtin.system;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.system.PrologFlags;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

/**
 * set_prolog_flag/2 - set_prolog_flag(+Flag, +Value)
 * Sets a Prolog system flag.
 */
public class SetPrologFlag implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("set_prolog_flag/2 requires exactly 2 arguments: set_prolog_flag(+Flag, +Value).");
        }

        Term flagTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term valueTerm = query.getArguments().get(1).resolveBindings(bindings);

        if (!(flagTerm instanceof Atom)) {
            throw new PrologEvaluationException("set_prolog_flag/2: Flag must be an atom.");
        }

        String flagName = ((Atom) flagTerm).getName();

        if (PrologFlags.setFlag(flagName, valueTerm)) {
            solutions.add(bindings);
            return true;
        } else {
            throw new PrologEvaluationException("set_prolog_flag/2: Cannot set flag '" + flagName + "' (read-only or invalid value).");
        }
    }
}