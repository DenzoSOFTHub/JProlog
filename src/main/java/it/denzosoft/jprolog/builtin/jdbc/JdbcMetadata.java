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
            throw LibArgs.notA("atom", connTerm, modeName(), LibArgs.nameArity(modeName()), "the connection");   // ISS-2025-0692
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
            throw Errors.host(e, "execute", "sql", null, modeName(), LibArgs.arity(query));   // ISS-2025-0692
        }
    }

    private boolean executeTables(DatabaseMetaData meta, List<Term> args,
            Map<String, Term> bindings, List<Map<String, Term>> solutions) throws SQLException {
        if (args.size() != 2) {
            throw LibArgs.unknownArity(modeName(), args.size());   // ISS-2025-0692
        }
        // START_CHANGE: ISS-2025-0260 - try-with-resources so the metadata ResultSet is closed
        // even if rs.next()/getString throws mid-iteration.
        List<Term> tables = new ArrayList<>();
        try (ResultSet rs = meta.getTables(null, null, "%", new String[]{"TABLE"})) {
            while (rs.next()) {
                tables.add(new Atom(rs.getString("TABLE_NAME")));
            }
        }
        // END_CHANGE: ISS-2025-0260

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
            throw LibArgs.unknownArity(modeName(), args.size());   // ISS-2025-0692
        }
        Term tableTerm = args.get(1).resolveBindings(bindings);
        if (!(tableTerm instanceof Atom)) {
            throw LibArgs.notA("atom", tableTerm, "jdbc_columns", 3, "Table must be an atom");   // ISS-2025-0692
        }
        String tableName = ((Atom) tableTerm).getName();

        // START_CHANGE: ISS-2025-0260 - try-with-resources (see executeTables).
        List<Term> columns = new ArrayList<>();
        try (ResultSet rs = meta.getColumns(null, null, tableName, "%")) {
            while (rs.next()) {
                Term colTerm = new CompoundTerm(new Atom("column"), Arrays.asList(
                    new Atom(rs.getString("COLUMN_NAME")),
                    new Atom(rs.getString("TYPE_NAME")),
                    new Number(rs.getInt("COLUMN_SIZE"))
                ));
                columns.add(colTerm);
            }
        }
        // END_CHANGE: ISS-2025-0260

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
