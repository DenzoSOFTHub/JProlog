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
 * Transaction management predicates:
 *   jdbc_set_autocommit/2 - jdbc_set_autocommit(+Conn, +Bool)
 *   jdbc_commit/1         - jdbc_commit(+Conn)
 *   jdbc_rollback/1       - jdbc_rollback(+Conn)
 */
public class JdbcTransaction implements BuiltIn {

    public enum Mode { SET_AUTOCOMMIT, COMMIT, ROLLBACK }

    private final Mode mode;

    public JdbcTransaction(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();

        Term connTerm = args.get(0).resolveBindings(bindings);
        if (!(connTerm instanceof Atom)) {
            throw new PrologEvaluationException(modeName() + ": Connection must be an atom handle.");
        }
        String handle = ((Atom) connTerm).getName();

        try {
            switch (mode) {
                case SET_AUTOCOMMIT:
                    if (args.size() != 2) {
                        throw new PrologEvaluationException("jdbc_set_autocommit/2 requires 2 arguments.");
                    }
                    Term boolTerm = args.get(1).resolveBindings(bindings);
                    if (!(boolTerm instanceof Atom)) {
                        throw new PrologEvaluationException("jdbc_set_autocommit/2: second arg must be true or false.");
                    }
                    boolean auto = "true".equals(((Atom) boolTerm).getName());
                    JdbcConnectionManager.getInstance().setAutoCommit(handle, auto);
                    break;
                case COMMIT:
                    if (args.size() != 1) {
                        throw new PrologEvaluationException("jdbc_commit/1 requires 1 argument.");
                    }
                    JdbcConnectionManager.getInstance().commit(handle);
                    break;
                case ROLLBACK:
                    if (args.size() != 1) {
                        throw new PrologEvaluationException("jdbc_rollback/1 requires 1 argument.");
                    }
                    JdbcConnectionManager.getInstance().rollback(handle);
                    break;
            }
            solutions.add(bindings);
            return true;
        } catch (SQLException e) {
            throw new PrologEvaluationException(modeName() + ": " + e.getMessage());
        }
    }

    private String modeName() {
        switch (mode) {
            case SET_AUTOCOMMIT: return "jdbc_set_autocommit";
            case COMMIT: return "jdbc_commit";
            case ROLLBACK: return "jdbc_rollback";
            default: return "jdbc_transaction";
        }
    }
}
// END_CHANGE: ISS-2025-0108
