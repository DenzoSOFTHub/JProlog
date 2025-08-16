package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.BuiltIn;
import it.denzosoft.jprolog.PrologEvaluationException;
import it.denzosoft.jprolog.terms.Term;

import java.util.List;
import java.util.Map;

public class Nl implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 0) {
             throw new PrologEvaluationException("nl/0 takes no arguments.");
        }

        System.out.println(); // Writes a newline

        solutions.add(bindings);
        return true;
    }
}
