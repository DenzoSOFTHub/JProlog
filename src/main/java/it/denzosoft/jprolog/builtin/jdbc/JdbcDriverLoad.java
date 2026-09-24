package it.denzosoft.jprolog.builtin.jdbc;

// START_CHANGE: ISS-2025-0108 - JDBC built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.v4.Errors;

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
            throw LibArgs.unknownArity(query);   // ISS-2025-0692
        }

        Term classTerm = query.getArguments().get(0).resolveBindings(bindings);
        if (!(classTerm instanceof Atom)) {
            throw LibArgs.notA("atom", classTerm, "jdbc_driver_load", 1, "Class name must be an atom");   // ISS-2025-0692
        }

        String className = ((Atom) classTerm).getName();
        try {
            Class.forName(className);
            solutions.add(bindings);
            return true;
        } catch (ClassNotFoundException e) {
            throw Errors.existence("class", new Atom(className), "jdbc_driver_load", 1, "driver class not found");   // ISS-2025-0692
        }
    }
}
// END_CHANGE: ISS-2025-0108
