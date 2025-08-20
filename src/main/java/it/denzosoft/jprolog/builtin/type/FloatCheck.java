package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;


public class FloatCheck implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("float/1 requires exactly one argument.");
        }

        Term termArg = query.getArguments().get(0);

        if (termArg.isGround()) {
            boolean isFloat = (termArg instanceof Number) && 
                             !(((Number) termArg).getValue() == Math.floor(((Number) termArg).getValue()) &&
                               !Double.isInfinite(((Number) termArg).getValue()));
            
            if (isFloat) {
                solutions.add(bindings);
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }
}
