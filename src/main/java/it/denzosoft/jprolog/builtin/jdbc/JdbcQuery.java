package it.denzosoft.jprolog.builtin.jdbc;

// START_CHANGE: ISS-2025-0108 - JDBC built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.core.utils.CollectionUtils;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.*;

/**
 * jdbc_query/3 - jdbc_query(+Connection, +SQL, -Rows)
 * Executes a SELECT query and returns all rows as a Prolog list of row(...) terms.
 * Each row is a compound term row(Val1, Val2, ...).
 */
public class JdbcQuery implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0692
        }

        Term connTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term sqlTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term rowsTerm = query.getArguments().get(2);

        if (!(connTerm instanceof Atom)) {
            throw LibArgs.notA("atom", connTerm, "jdbc_query", 3, "Connection must be an atom handle");   // ISS-2025-0692
        }
        if (!(sqlTerm instanceof Atom)) {
            throw LibArgs.notA("atom", sqlTerm, "jdbc_query", 3, "SQL must be an atom");   // ISS-2025-0692
        }

        String connHandle = ((Atom) connTerm).getName();
        String sql = ((Atom) sqlTerm).getName();
        String rsHandle = null;

        try {
            rsHandle = JdbcConnectionManager.getInstance().executeQuery(connHandle, sql);
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

            Term resultList = CollectionUtils.createListTerm(rowTerms);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (rowsTerm.resolveBindings(bindings).unify(resultList, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        } catch (SQLException e) {
            throw Errors.host(e, "execute", "sql", null, "jdbc_query", LibArgs.arity(query));   // ISS-2025-0692
        } finally {
            if (rsHandle != null) {
                try { JdbcConnectionManager.getInstance().closeResultSet(rsHandle); }
                catch (SQLException ignored) {}
            }
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
// END_CHANGE: ISS-2025-0108
