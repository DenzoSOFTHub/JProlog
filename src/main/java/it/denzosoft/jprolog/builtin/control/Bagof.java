package it.denzosoft.jprolog.builtin.control;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.utils.CollectionUtils;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;


public class Bagof implements BuiltInWithContext {
    private final QuerySolver solver;

    public Bagof(QuerySolver solver) {
        this.solver = solver;
    }

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        return CollectionUtils.genericListCollector("bagof", query, bindings, solutions, solver);
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("Context-dependent built-in 'bagof' must be invoked with context");
    }
}
