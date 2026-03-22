package it.denzosoft.jprolog.builtin.jdbc;

// START_CHANGE: ISS-2025-0108 - JDBC built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.sql.SQLException;
import java.util.List;
import java.util.Map;

/**
 * jdbc_disconnect/1 - jdbc_disconnect(+Connection)
 * Closes an open JDBC connection.
 */
public class JdbcDisconnect implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("jdbc_disconnect/1 requires exactly 1 argument.");
        }

        Term connTerm = query.getArguments().get(0).resolveBindings(bindings);
        if (!(connTerm instanceof Atom)) {
            throw new PrologEvaluationException("jdbc_disconnect/1: Connection must be an atom handle.");
        }

        try {
            JdbcConnectionManager.getInstance().closeConnection(((Atom) connTerm).getName());
            solutions.add(bindings);
            return true;
        } catch (SQLException e) {
            throw new PrologEvaluationException("jdbc_disconnect: " + e.getMessage());
        }
    }
}
// END_CHANGE: ISS-2025-0108
