package it.denzosoft.jprolog.builtin.jdbc;

// START_CHANGE: ISS-2025-0110 - Prepared statements with parameters and stored procedures
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

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
            throw new PrologEvaluationException(
                "jdbc_set_param/3 requires 3 arguments: jdbc_set_param(+Stmt, +Index, +Value).");
        }

        Term stmtTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term indexTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term valueTerm = query.getArguments().get(2).resolveBindings(bindings);

        if (!(stmtTerm instanceof Atom)) {
            throw new PrologEvaluationException("jdbc_set_param/3: Statement must be an atom handle.");
        }
        if (!(indexTerm instanceof Number)) {
            throw new PrologEvaluationException("jdbc_set_param/3: Index must be a number.");
        }

        String handle = ((Atom) stmtTerm).getName();
        int index = ((Number) indexTerm).getValue().intValue();

        try {
            PreparedStatement ps = JdbcConnectionManager.getInstance().getStatement(handle);
            setParameter(ps, index, valueTerm);
            solutions.add(bindings);
            return true;
        } catch (SQLException e) {
            throw new PrologEvaluationException("jdbc_set_param: " + e.getMessage());
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
