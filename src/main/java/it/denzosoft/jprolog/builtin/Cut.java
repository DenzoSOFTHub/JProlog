package it.denzosoft.jprolog.builtin;

import it.denzosoft.jprolog.BuiltIn;
import it.denzosoft.jprolog.CutStatus;
import it.denzosoft.jprolog.PrologEvaluationException;
import it.denzosoft.jprolog.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class Cut implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 0) {
            throw new PrologEvaluationException("cut/0 takes no arguments.");
        }
        // Add current bindings to solutions before cutting
        solutions.add(new HashMap<>(bindings));
        // Note: The actual cut behavior is handled in QuerySolver
        return true;
    }
}
