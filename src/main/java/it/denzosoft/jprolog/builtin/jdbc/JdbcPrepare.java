package it.denzosoft.jprolog.builtin.jdbc;

// START_CHANGE: ISS-2025-0110 - Prepared statements with parameters and stored procedures
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.sql.SQLException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * jdbc_prepare/3 - jdbc_prepare(+Connection, +SQL, -Statement)
 * Prepares a parameterized SQL statement for later execution.
 *
 * Example:
 *   jdbc_prepare(Conn, 'SELECT * FROM users WHERE age > ? AND city = ?', Stmt)
 */
public class JdbcPrepare implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException(
                "jdbc_prepare/3 requires 3 arguments: jdbc_prepare(+Conn, +SQL, -Stmt).");
        }

        Term connTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term sqlTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term stmtTerm = query.getArguments().get(2);

        if (!(connTerm instanceof Atom)) {
            throw new PrologEvaluationException("jdbc_prepare/3: Connection must be an atom handle.");
        }
        if (!(sqlTerm instanceof Atom)) {
            throw new PrologEvaluationException("jdbc_prepare/3: SQL must be an atom.");
        }

        try {
            String handle = JdbcConnectionManager.getInstance().prepareStatement(
                ((Atom) connTerm).getName(), ((Atom) sqlTerm).getName());

            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (stmtTerm.resolveBindings(bindings).unify(new Atom(handle), newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            JdbcConnectionManager.getInstance().closeStatement(handle);
            return false;
        } catch (SQLException e) {
            throw new PrologEvaluationException("jdbc_prepare: " + e.getMessage());
        }
    }
}
// END_CHANGE: ISS-2025-0110
