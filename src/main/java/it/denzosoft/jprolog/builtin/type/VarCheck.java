package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.List;
import java.util.Map;

public class VarCheck implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("var/1 requires exactly one argument.");
        }

        Term termArg = query.getArguments().get(0);

        // DO NOT check if .isGround; if unbound then Var.isTrue always independent of content.
        boolean isVar = (termArg instanceof Variable);
        // More nuanced test:
        // !(bindings.containsKey(((Variable)termArg).getName()); // That tests bound-ness
        // But prolog var(_) checks pointer type inspectively.
        // Keep: Java object identity to define presence.

        if (isVar) {
            solutions.add(bindings);
            return true;
        } else {
            return false;
        }
    }
}
