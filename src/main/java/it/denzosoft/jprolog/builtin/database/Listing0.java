package it.denzosoft.jprolog.builtin.database;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;


public class Listing0 implements BuiltInWithContext {

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        // Expect zero arguments for this predicate
        if (query.getArguments() != null && query.getArguments().size() != 0) {
            throw new PrologEvaluationException("listing/0 takes no arguments.");
        }

        // Get Prolog engine instance and access its listing feature
        Prolog engine = solver.getPrologContext();
        if (engine != null) {
            engine.listing();
        } else {
             throw new PrologEvaluationException("listing/0: Prolog engine context not available.");
        }

        // listing is side-effect, succeeds trivially if executed
        solutions.add(new java.util.HashMap<>(bindings)); // Add current binding set
        return true;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("Context-dependent built-in 'listing/0' must be invoked with context");
    }
}
