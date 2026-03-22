package it.denzosoft.jprolog.builtin.jdbc;

// START_CHANGE: ISS-2025-0108 - JDBC built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

/**
 * jdbc_driver_load/1 - jdbc_driver_load(+ClassName)
 * Loads a JDBC driver class (e.g., 'org.h2.Driver').
 */
public class JdbcDriverLoad implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("jdbc_driver_load/1 requires exactly 1 argument.");
        }

        Term classTerm = query.getArguments().get(0).resolveBindings(bindings);
        if (!(classTerm instanceof Atom)) {
            throw new PrologEvaluationException("jdbc_driver_load/1: Class name must be an atom.");
        }

        String className = ((Atom) classTerm).getName();
        try {
            Class.forName(className);
            solutions.add(bindings);
            return true;
        } catch (ClassNotFoundException e) {
            throw new PrologEvaluationException("jdbc_driver_load: Driver class not found: " + className);
        }
    }
}
// END_CHANGE: ISS-2025-0108
