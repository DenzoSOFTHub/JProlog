package it.denzosoft.jprolog.builtin.control;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class Repeat implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() != null && query.getArguments().size() != 0) {
            throw new PrologEvaluationException("repeat/0 takes no arguments.");
        }
        // START_CHANGE: ISS-2025-0052 - Fix repeat/0 to generate multiple choice points
        // ISO Prolog: repeat/0 generates infinite solutions on backtracking.
        // Since the architecture collects solutions eagerly, provide a large bound.
        for (int i = 0; i < 1000; i++) {
            solutions.add(new HashMap<>(bindings));
        }
        // END_CHANGE: ISS-2025-0052
        return true;
    }
}
