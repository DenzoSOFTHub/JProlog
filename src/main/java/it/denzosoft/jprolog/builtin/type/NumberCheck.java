package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.List;
import java.util.Map;

public class NumberCheck implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("number/1 requires exactly one argument.");
        }

        Term termArg = query.getArguments().get(0);

        // Resolve bindings first
        Term resolvedTerm = termArg.resolveBindings(bindings);
        
        if (resolvedTerm.isGround()) {
            boolean isNumber = (resolvedTerm instanceof Number);

            if (isNumber) {
                solutions.add(bindings);
                return true;
            } else {
                return false;
            }
        } else {
           // In ISO Prolog, type checks should fail (not throw) for unbound variables
           return false;
        }
    }
}
