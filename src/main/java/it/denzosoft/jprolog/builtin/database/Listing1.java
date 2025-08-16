package it.denzosoft.jprolog.builtin.database;

import it.denzosoft.jprolog.BuiltInWithContext;
import it.denzosoft.jprolog.QuerySolver;
import it.denzosoft.jprolog.terms.Term;
import it.denzosoft.jprolog.terms.Variable;

import java.util.List;
import java.util.Map;


public class Listing1 implements BuiltInWithContext {

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new it.denzosoft.jprolog.PrologEvaluationException("listing/1 requires exactly one argument.");
        }

        Term indicatorTerm = query.getArguments().get(0);

        String predicateIndicatorString;
        if (indicatorTerm.isGround()) {
            // Resolve possible variable bindings before reading as string
            Term resolvedTerm = indicatorTerm.resolveBindings(bindings);
            predicateIndicatorString = resolvedTerm.toString();
        } else {
            throw new it.denzosoft.jprolog.PrologEvaluationException("listing/1: Argument must be ground.");
        }

        // Get Prolog engine instance
        it.denzosoft.jprolog.Prolog engine = solver.getPrologContext();
        if (engine != null) {
            engine.listing(predicateIndicatorString);
        } else {
             throw new it.denzosoft.jprolog.PrologEvaluationException("listing/1: Prolog engine context not available.");
        }

        // Successfully displayed listing
        solutions.add(new java.util.HashMap<>(bindings));
        return true;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("Context-dependent built-in 'listing/1' must be invoked with context");
    }
}
