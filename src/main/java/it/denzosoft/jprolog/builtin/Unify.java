package it.denzosoft.jprolog.builtin;

import it.denzosoft.jprolog.BuiltIn;
import it.denzosoft.jprolog.PrologEvaluationException;
import it.denzosoft.jprolog.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class Unify implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("=/2 requires exactly 2 arguments.");
        }

        Term term1 = query.getArguments().get(0);
        Term term2 = query.getArguments().get(1);

        // Create a copy of bindings for unification
        Map<String, Term> newBindings = new HashMap<>(bindings);
        
        // Attempt unification
        if (term1.unify(term2, newBindings)) {
            solutions.add(new HashMap<>(newBindings));
            return true;
        }
        
        return false;
    }
}
