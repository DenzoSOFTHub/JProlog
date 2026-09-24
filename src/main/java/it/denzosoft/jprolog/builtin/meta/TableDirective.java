package it.denzosoft.jprolog.builtin.meta;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.terms.*;

import java.util.*;

// START_CHANGE: ISS-2025-0092 - table/1 built-in predicate
/**
 * Declares a predicate as tabled (memoized).
 * Usage: table(Functor/Arity) or :- table Functor/Arity.
 */
public class TableDirective implements BuiltInWithContext {

    @Override
    public boolean executeWithContext(SolverContext solver, Term query,
            Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        // START_CHANGE: ISS-2025-0572 - every table/1 form, through the one spec parser
        if (query.getArguments() != null && query.getArguments().size() == 1) {
            for (String w : solver.getPrologContext().getTableStore().declareSpec(          // ISS-2025-0753
                    query.getArguments().get(0).resolveBindings(bindings), "table/1")) {
                solver.getPrologContext().warnUser(w);
            }
            solutions.add(new HashMap<>(bindings));
            return true;
        }
        // END_CHANGE: ISS-2025-0572
        return false;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        return false; // Requires context
    }
}
// END_CHANGE: ISS-2025-0092
