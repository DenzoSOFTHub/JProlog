package it.denzosoft.jprolog.builtin.meta;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.*;

import java.util.*;

// START_CHANGE: ISS-2025-0092 - table/1 built-in predicate
/**
 * Declares a predicate as tabled (memoized).
 * Usage: table(Functor/Arity) or :- table Functor/Arity.
 */
public class TableDirective implements BuiltInWithContext {

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query,
            Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() != null && query.getArguments().size() == 1) {
            Term arg = query.getArguments().get(0).resolveBindings(bindings);
            if (arg instanceof CompoundTerm && "/".equals(arg.getName())
                && arg.getArguments().size() == 2) {
                Term functorTerm = arg.getArguments().get(0);
                Term arityTerm = arg.getArguments().get(1);
                if (functorTerm instanceof Atom && arityTerm instanceof it.denzosoft.jprolog.core.terms.Number) {
                    String functor = ((Atom) functorTerm).getName();
                    int arity = (int) Math.round(((it.denzosoft.jprolog.core.terms.Number) arityTerm).getValue());
                    solver.getPrologContext().getTableStore().declareTable(functor, arity);
                    solutions.add(new HashMap<>(bindings));
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        return false; // Requires context
    }
}
// END_CHANGE: ISS-2025-0092
