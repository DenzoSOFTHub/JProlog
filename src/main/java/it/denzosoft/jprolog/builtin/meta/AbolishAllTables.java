package it.denzosoft.jprolog.builtin.meta;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.*;

// START_CHANGE: ISS-2025-0092 - abolish_all_tables/0 built-in predicate
/**
 * Clears all tabling caches.
 */
public class AbolishAllTables implements BuiltInWithContext {

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query,
            Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        solver.getPrologContext().getTableStore().abolishAllTables();
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        return false; // Requires context
    }
}
// END_CHANGE: ISS-2025-0092
