package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;


public class CollectionBuiltInAdapter implements BuiltIn {
    private final BuiltInWithContext builtIn;
    private final QuerySolver querySolver;

    public CollectionBuiltInAdapter(BuiltInWithContext builtIn, QuerySolver querySolver) {
        this.builtIn = builtIn;
        this.querySolver = querySolver;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        return builtIn.executeWithContext(querySolver, query, bindings, solutions);
    }
}
