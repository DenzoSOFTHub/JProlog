package it.denzosoft.jprolog.builtin.jdbc;

// START_CHANGE: ISS-2025-0108 - JDBC built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.utils.CollectionUtils;

import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;

/**
 * Database metadata predicates:
 *   jdbc_tables/2  - jdbc_tables(+Conn, -Tables)       returns list of table names
 *   jdbc_columns/3 - jdbc_columns(+Conn, +Table, -Cols) returns list of column(Name,Type,Size) terms
 */
public class JdbcMetadata implements BuiltIn {

    public enum Mode { TABLES, COLUMNS }

    private final Mode mode;

    public JdbcMetadata(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        Term connTerm = args.get(0).resolveBindings(bindings);
        if (!(connTerm instanceof Atom)) {
            throw new PrologEvaluationException(modeName() + ": Connection must be an atom handle.");
        }
        String connHandle = ((Atom) connTerm).getName();

        try {
            DatabaseMetaData meta = JdbcConnectionManager.getInstance()
                .getConnection(connHandle).getMetaData();

            switch (mode) {
                case TABLES: return executeTables(meta, args, bindings, solutions);
                case COLUMNS: return executeColumns(meta, args, bindings, solutions);
                default: return false;
            }
        } catch (SQLException e) {
            throw new PrologEvaluationException(modeName() + ": " + e.getMessage());
        }
    }

    private boolean executeTables(DatabaseMetaData meta, List<Term> args,
            Map<String, Term> bindings, List<Map<String, Term>> solutions) throws SQLException {
        if (args.size() != 2) {
            throw new PrologEvaluationException("jdbc_tables/2 requires 2 arguments.");
        }
        ResultSet rs = meta.getTables(null, null, "%", new String[]{"TABLE"});
        List<Term> tables = new ArrayList<>();
        while (rs.next()) {
            tables.add(new Atom(rs.getString("TABLE_NAME")));
        }
        rs.close();

        Term result = CollectionUtils.createListTerm(tables);
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (args.get(1).resolveBindings(bindings).unify(result, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    private boolean executeColumns(DatabaseMetaData meta, List<Term> args,
            Map<String, Term> bindings, List<Map<String, Term>> solutions) throws SQLException {
        if (args.size() != 3) {
            throw new PrologEvaluationException("jdbc_columns/3 requires 3 arguments.");
        }
        Term tableTerm = args.get(1).resolveBindings(bindings);
        if (!(tableTerm instanceof Atom)) {
            throw new PrologEvaluationException("jdbc_columns/3: Table must be an atom.");
        }
        String tableName = ((Atom) tableTerm).getName();

        ResultSet rs = meta.getColumns(null, null, tableName, "%");
        List<Term> columns = new ArrayList<>();
        while (rs.next()) {
            Term colTerm = new CompoundTerm(new Atom("column"), Arrays.asList(
                new Atom(rs.getString("COLUMN_NAME")),
                new Atom(rs.getString("TYPE_NAME")),
                new Number(rs.getInt("COLUMN_SIZE"))
            ));
            columns.add(colTerm);
        }
        rs.close();

        Term result = CollectionUtils.createListTerm(columns);
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (args.get(2).resolveBindings(bindings).unify(result, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    private String modeName() {
        return mode == Mode.TABLES ? "jdbc_tables" : "jdbc_columns";
    }
}
// END_CHANGE: ISS-2025-0108
