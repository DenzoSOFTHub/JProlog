package it.denzosoft.jprolog.builtin.jdbc;

// START_CHANGE: ISS-2025-0108 - JDBC built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.v4.Errors;

import java.sql.SQLException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * jdbc_execute_update/3 - jdbc_execute_update(+Connection, +SQL, -AffectedRows)
 * Executes an INSERT, UPDATE, or DELETE statement.
 */
public class JdbcExecuteUpdate implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0692
        }

        Term connTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term sqlTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term rowsTerm = query.getArguments().get(2);

        if (!(connTerm instanceof Atom)) {
            throw LibArgs.notA("atom", connTerm, "jdbc_execute_update", 3, "Connection must be an atom handle");   // ISS-2025-0692
        }
        if (!(sqlTerm instanceof Atom)) {
            throw LibArgs.notA("atom", sqlTerm, "jdbc_execute_update", 3, "SQL must be an atom");   // ISS-2025-0692
        }

        try {
            int affected = JdbcConnectionManager.getInstance().executeUpdate(
                ((Atom) connTerm).getName(), ((Atom) sqlTerm).getName());

            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (rowsTerm.resolveBindings(bindings).unify(new Number(affected), newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        } catch (SQLException e) {
            throw Errors.host(e, "execute", "sql", null, "jdbc_execute_update", LibArgs.arity(query));   // ISS-2025-0692
        }
    }
}
// END_CHANGE: ISS-2025-0108
