package it.denzosoft.jprolog.builtin;

import it.denzosoft.jprolog.BuiltInWithContext;
import it.denzosoft.jprolog.CollectionUtils;
import it.denzosoft.jprolog.QuerySolver;
import it.denzosoft.jprolog.terms.Term;

import java.util.List;
import java.util.Map;


public class Findall implements BuiltInWithContext {
    private final QuerySolver solver;

    public Findall(QuerySolver solver) {
        this.solver = solver;
    }

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        return CollectionUtils.genericListCollector("findall", query, bindings, solutions, solver);
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("Context-dependent built-in 'findall' must be invoked with context");
    }
}
