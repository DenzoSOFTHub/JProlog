package it.denzosoft.jprolog.builtin.jdbc;

// START_CHANGE: ISS-2025-0108 - JDBC built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

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
            throw new PrologEvaluationException(
                "jdbc_execute_update/3 requires exactly 3 arguments: jdbc_execute_update(+Conn, +SQL, -Rows).");
        }

        Term connTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term sqlTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term rowsTerm = query.getArguments().get(2);

        if (!(connTerm instanceof Atom)) {
            throw new PrologEvaluationException("jdbc_execute_update/3: Connection must be an atom handle.");
        }
        if (!(sqlTerm instanceof Atom)) {
            throw new PrologEvaluationException("jdbc_execute_update/3: SQL must be an atom.");
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
            throw new PrologEvaluationException("jdbc_execute_update: " + e.getMessage());
        }
    }
}
// END_CHANGE: ISS-2025-0108
