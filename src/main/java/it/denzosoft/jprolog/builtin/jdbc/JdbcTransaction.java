package it.denzosoft.jprolog.builtin.jdbc;

// START_CHANGE: ISS-2025-0108 - JDBC built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.v4.Errors;

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
            throw LibArgs.notA("atom", connTerm, modeName(), LibArgs.nameArity(modeName()), "the connection");   // ISS-2025-0692
        }
        String handle = ((Atom) connTerm).getName();

        try {
            switch (mode) {
                case SET_AUTOCOMMIT:
                    if (args.size() != 2) {
                        throw LibArgs.unknownArity(query);   // ISS-2025-0692
                    }
                    Term boolTerm = args.get(1).resolveBindings(bindings);
                    if (!(boolTerm instanceof Atom)) {
            throw LibArgs.notA("atom", boolTerm, "jdbc_set_autocommit", 2, "second arg must be true or false");   // ISS-2025-0692
        }
                    boolean auto = "true".equals(((Atom) boolTerm).getName());
                    JdbcConnectionManager.getInstance().setAutoCommit(handle, auto);
                    break;
                case COMMIT:
                    if (args.size() != 1) {
                        throw LibArgs.unknownArity(query);   // ISS-2025-0692
                    }
                    JdbcConnectionManager.getInstance().commit(handle);
                    break;
                case ROLLBACK:
                    if (args.size() != 1) {
                        throw LibArgs.unknownArity(query);   // ISS-2025-0692
                    }
                    JdbcConnectionManager.getInstance().rollback(handle);
                    break;
            }
            solutions.add(bindings);
            return true;
        } catch (SQLException e) {
            throw Errors.host(e, "execute", "sql", null, modeName(), LibArgs.arity(query));   // ISS-2025-0692
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
