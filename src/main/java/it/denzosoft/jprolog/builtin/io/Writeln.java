package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

public class Writeln implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("writeln/1 requires exactly 1 argument.");
        }

        Term termToWrite = query.getArguments().get(0);
        Term resolvedTerm = termToWrite.resolveBindings(bindings);
        System.out.println(resolvedTerm.toString());

        solutions.add(bindings);
        return true;
    }
}
