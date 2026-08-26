package it.denzosoft.jprolog.builtin.jdbc;

// START_CHANGE: ISS-2025-0110 - Prepared statements with parameters and stored procedures
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.utils.CollectionUtils;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.*;

/**
 * Prepared statement execution predicates:
 *
 *   jdbc_execute_prepared_query/2  - jdbc_execute_prepared_query(+Stmt, -Rows)
 *     Executes a prepared SELECT statement and returns rows as list of row(...) terms.
 *
 *   jdbc_execute_prepared_update/2 - jdbc_execute_prepared_update(+Stmt, -AffectedRows)
 *     Executes a prepared INSERT/UPDATE/DELETE and returns affected row count.
 *
 *   jdbc_close_statement/1 - jdbc_close_statement(+Stmt)
 *     Closes a prepared statement.
 */
public class JdbcExecutePrepared implements BuiltIn {

    public enum Mode { QUERY, UPDATE, CLOSE }

    private final Mode mode;

    public JdbcExecutePrepared(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();

        switch (mode) {
            case QUERY:  return execQuery(args, bindings, solutions);
            case UPDATE: return execUpdate(args, bindings, solutions);
            case CLOSE:  return execClose(args, bindings, solutions);
            default: return false;
        }
    }

    private boolean execQuery(List<Term> args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        if (args.size() != 2) {
            throw new PrologEvaluationException(
                "jdbc_execute_prepared_query/2 requires 2 arguments: jdbc_execute_prepared_query(+Stmt, -Rows).");
        }

        Term stmtTerm = args.get(0).resolveBindings(bindings);
        Term rowsTerm = args.get(1);

        if (!(stmtTerm instanceof Atom)) {
            throw new PrologEvaluationException("jdbc_execute_prepared_query/2: Statement must be an atom handle.");
        }

        String handle = ((Atom) stmtTerm).getName();

        try {
            String rsHandle = JdbcConnectionManager.getInstance().executePreparedQuery(handle);
            ResultSet rs = JdbcConnectionManager.getInstance().getResultSet(rsHandle);
            ResultSetMetaData meta = rs.getMetaData();
            int colCount = meta.getColumnCount();

            List<Term> rowTerms = new ArrayList<>();
            while (rs.next()) {
                List<Term> colValues = new ArrayList<>(colCount);
                for (int i = 1; i <= colCount; i++) {
                    colValues.add(resultValueToTerm(rs, i, meta.getColumnType(i)));
                }
                rowTerms.add(new CompoundTerm(new Atom("row"), colValues));
            }

            JdbcConnectionManager.getInstance().closeResultSet(rsHandle);

            Term resultList = CollectionUtils.createListTerm(rowTerms);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (rowsTerm.resolveBindings(bindings).unify(resultList, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        } catch (SQLException e) {
            throw new PrologEvaluationException("jdbc_execute_prepared_query: " + e.getMessage());
        }
    }

    private boolean execUpdate(List<Term> args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        if (args.size() != 2) {
            throw new PrologEvaluationException(
                "jdbc_execute_prepared_update/2 requires 2 arguments: jdbc_execute_prepared_update(+Stmt, -Rows).");
        }

        Term stmtTerm = args.get(0).resolveBindings(bindings);
        Term rowsTerm = args.get(1);

        if (!(stmtTerm instanceof Atom)) {
            throw new PrologEvaluationException("jdbc_execute_prepared_update/2: Statement must be an atom handle.");
        }

        try {
            int affected = JdbcConnectionManager.getInstance()
                .executePreparedUpdate(((Atom) stmtTerm).getName());

            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (rowsTerm.resolveBindings(bindings).unify(new Number(affected), newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        } catch (SQLException e) {
            throw new PrologEvaluationException("jdbc_execute_prepared_update: " + e.getMessage());
        }
    }

    private boolean execClose(List<Term> args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        if (args.size() != 1) {
            throw new PrologEvaluationException(
                "jdbc_close_statement/1 requires 1 argument.");
        }

        Term stmtTerm = args.get(0).resolveBindings(bindings);
        if (!(stmtTerm instanceof Atom)) {
            throw new PrologEvaluationException("jdbc_close_statement/1: Statement must be an atom handle.");
        }

        try {
            String handle = ((Atom) stmtTerm).getName();
            if (handle.startsWith("$jdbc_call_")) {
                JdbcConnectionManager.getInstance().closeCallableStatement(handle);
            } else {
                JdbcConnectionManager.getInstance().closeStatement(handle);
            }
            solutions.add(bindings);
            return true;
        } catch (SQLException e) {
            throw new PrologEvaluationException("jdbc_close_statement: " + e.getMessage());
        }
    }

    private Term resultValueToTerm(ResultSet rs, int col, int sqlType) throws SQLException {
        if (rs.getObject(col) == null) {
            return new Atom("null");
        }
        switch (sqlType) {
            case java.sql.Types.INTEGER:
            case java.sql.Types.SMALLINT:
            case java.sql.Types.TINYINT:
            case java.sql.Types.BIGINT:
                return new Number(rs.getLong(col));
            case java.sql.Types.FLOAT:
            case java.sql.Types.DOUBLE:
            case java.sql.Types.REAL:
            case java.sql.Types.DECIMAL:
            case java.sql.Types.NUMERIC:
                // ISS-2025-0424 - ENG-02: read as a double => an ISO FLOAT (integral SQL types are
                // handled by the long branch above); Number(double) no longer collapses 1.0 to 1.
                return new Number(rs.getDouble(col));
            default:
                return new Atom(rs.getString(col));
        }
    }
}
// END_CHANGE: ISS-2025-0110
