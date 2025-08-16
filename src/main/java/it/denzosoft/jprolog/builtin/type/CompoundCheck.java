package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.BuiltIn;
import it.denzosoft.jprolog.PrologEvaluationException;
import it.denzosoft.jprolog.terms.CompoundTerm;
import it.denzosoft.jprolog.terms.Term;

import java.util.List;
import java.util.Map;

public class CompoundCheck implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("compound/1 requires exactly one argument.");
        }

        Term termArg = query.getArguments().get(0);

        if (termArg.isGround()) {
            boolean isCompound = (termArg instanceof CompoundTerm);

            if (isCompound) {
                solutions.add(bindings);
                return true;
            } else {
                return false;
            }
        } else {
             throw new PrologEvaluationException("compound/1: Argument must be ground.");
        }
    }
}
