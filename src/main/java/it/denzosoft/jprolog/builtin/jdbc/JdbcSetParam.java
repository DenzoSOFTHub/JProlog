package it.denzosoft.jprolog.builtin.jdbc;

// START_CHANGE: ISS-2025-0110 - Prepared statements with parameters and stored procedures
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.v4.Errors;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;

/**
 * jdbc_set_param/3 - jdbc_set_param(+Statement, +Index, +Value)
 * Sets a parameter on a prepared statement.
 *
 * Index is 1-based. Value type is auto-detected:
 *   - Number -> setDouble/setLong
 *   - Atom 'null' -> setNull
 *   - Atom -> setString
 *
 * Example:
 *   jdbc_set_param(Stmt, 1, 25),        % sets ? #1 to integer 25
 *   jdbc_set_param(Stmt, 2, 'Milan')    % sets ? #2 to string "Milan"
 */
public class JdbcSetParam implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0692
        }

        Term stmtTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term indexTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term valueTerm = query.getArguments().get(2).resolveBindings(bindings);

        if (!(stmtTerm instanceof Atom)) {
            throw LibArgs.notA("atom", stmtTerm, "jdbc_set_param", 3, "Statement must be an atom handle");   // ISS-2025-0692
        }
        if (!(indexTerm instanceof Number)) {
            throw LibArgs.notA("number", indexTerm, "jdbc_set_param", 3, "Index must be a number");   // ISS-2025-0692
        }

        String handle = ((Atom) stmtTerm).getName();
        int index = ((Number) indexTerm).getValue().intValue();

        try {
            PreparedStatement ps = JdbcConnectionManager.getInstance().getStatement(handle);
            setParameter(ps, index, valueTerm);
            solutions.add(bindings);
            return true;
        } catch (SQLException e) {
            throw Errors.host(e, "execute", "sql", null, "jdbc_set_param", LibArgs.arity(query));   // ISS-2025-0692
        }
    }

    static void setParameter(PreparedStatement ps, int index, Term value) throws SQLException {
        if (value instanceof Number) {
            double d = ((Number) value).getValue();
            if (d == Math.floor(d) && !Double.isInfinite(d)) {
                ps.setLong(index, (long) d);
            } else {
                ps.setDouble(index, d);
            }
        } else if (value instanceof Atom) {
            String name = ((Atom) value).getName();
            if ("null".equals(name)) {
                ps.setNull(index, java.sql.Types.NULL);
            } else {
                ps.setString(index, name);
            }
        } else {
            ps.setString(index, value.toString());
        }
    }
}
// END_CHANGE: ISS-2025-0110
