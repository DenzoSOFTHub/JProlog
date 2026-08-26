package it.denzosoft.jprolog.builtin.jdbc;

// START_CHANGE: ISS-2025-0110 - Prepared statements with parameters and stored procedures
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.utils.CollectionUtils;

import java.sql.CallableStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.*;

/**
 * Stored procedure invocation predicates:
 *
 *   jdbc_prepare_call/3 - jdbc_prepare_call(+Conn, +SQL, -CallStmt)
 *     Prepares a callable statement for stored procedure invocation.
 *     SQL format: '{call procedure_name(?, ?, ?)}'
 *
 *   jdbc_call_set_param/3 - jdbc_call_set_param(+CallStmt, +Index, +Value)
 *     Sets an IN parameter on a callable statement.
 *
 *   jdbc_call_register_out/3 - jdbc_call_register_out(+CallStmt, +Index, +SqlType)
 *     Registers an OUT parameter. SqlType: integer, double, varchar.
 *
 *   jdbc_call_execute/1 - jdbc_call_execute(+CallStmt)
 *     Executes the stored procedure.
 *
 *   jdbc_call_get_result/3 - jdbc_call_get_result(+CallStmt, +Index, -Value)
 *     Retrieves an OUT parameter value after execution.
 *
 *   jdbc_call_get_resultset/2 - jdbc_call_get_resultset(+CallStmt, -Rows)
 *     Retrieves the ResultSet from a procedure that returns one.
 */
public class JdbcCallProcedure implements BuiltIn {

    public enum Mode {
        PREPARE_CALL, SET_PARAM, REGISTER_OUT, EXECUTE,
        GET_RESULT, GET_RESULTSET
    }

    private final Mode mode;

    public JdbcCallProcedure(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        try {
            switch (mode) {
                case PREPARE_CALL:   return doPrepareCall(query, bindings, solutions);
                case SET_PARAM:      return doSetParam(query, bindings, solutions);
                case REGISTER_OUT:   return doRegisterOut(query, bindings, solutions);
                case EXECUTE:        return doExecute(query, bindings, solutions);
                case GET_RESULT:     return doGetResult(query, bindings, solutions);
                case GET_RESULTSET:  return doGetResultSet(query, bindings, solutions);
                default: return false;
            }
        } catch (SQLException e) {
            throw new PrologEvaluationException(modeName() + ": " + e.getMessage());
        }
    }

    private boolean doPrepareCall(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws SQLException {
        List<Term> args = query.getArguments();
        if (args.size() != 3) {
            throw new PrologEvaluationException(
                "jdbc_prepare_call/3 requires 3 arguments: jdbc_prepare_call(+Conn, +SQL, -CallStmt).");
        }

        String connHandle = resolveAtom(args.get(0), bindings, "Connection");
        String sql = resolveAtom(args.get(1), bindings, "SQL");

        String handle = JdbcConnectionManager.getInstance().prepareCall(connHandle, sql);

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (args.get(2).resolveBindings(bindings).unify(new Atom(handle), newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        JdbcConnectionManager.getInstance().closeCallableStatement(handle);
        return false;
    }

    private boolean doSetParam(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws SQLException {
        List<Term> args = query.getArguments();
        if (args.size() != 3) {
            throw new PrologEvaluationException(
                "jdbc_call_set_param/3 requires 3 arguments: jdbc_call_set_param(+CallStmt, +Index, +Value).");
        }

        String handle = resolveAtom(args.get(0), bindings, "CallStmt");
        int index = resolveInt(args.get(1), bindings, "Index");
        Term value = args.get(2).resolveBindings(bindings);

        CallableStatement cs = JdbcConnectionManager.getInstance().getCallableStatement(handle);
        JdbcSetParam.setParameter(cs, index, value);

        solutions.add(bindings);
        return true;
    }

    private boolean doRegisterOut(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws SQLException {
        List<Term> args = query.getArguments();
        if (args.size() != 3) {
            throw new PrologEvaluationException(
                "jdbc_call_register_out/3 requires 3 arguments: jdbc_call_register_out(+CallStmt, +Index, +SqlType).");
        }

        String handle = resolveAtom(args.get(0), bindings, "CallStmt");
        int index = resolveInt(args.get(1), bindings, "Index");
        String typeName = resolveAtom(args.get(2), bindings, "SqlType");

        int sqlType = mapSqlType(typeName);
        CallableStatement cs = JdbcConnectionManager.getInstance().getCallableStatement(handle);
        cs.registerOutParameter(index, sqlType);

        solutions.add(bindings);
        return true;
    }

    private boolean doExecute(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws SQLException {
        List<Term> args = query.getArguments();
        if (args.size() != 1) {
            throw new PrologEvaluationException(
                "jdbc_call_execute/1 requires 1 argument: jdbc_call_execute(+CallStmt).");
        }

        String handle = resolveAtom(args.get(0), bindings, "CallStmt");
        CallableStatement cs = JdbcConnectionManager.getInstance().getCallableStatement(handle);
        cs.execute();

        solutions.add(bindings);
        return true;
    }

    private boolean doGetResult(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws SQLException {
        List<Term> args = query.getArguments();
        if (args.size() != 3) {
            throw new PrologEvaluationException(
                "jdbc_call_get_result/3 requires 3 arguments: jdbc_call_get_result(+CallStmt, +Index, -Value).");
        }

        String handle = resolveAtom(args.get(0), bindings, "CallStmt");
        int index = resolveInt(args.get(1), bindings, "Index");

        CallableStatement cs = JdbcConnectionManager.getInstance().getCallableStatement(handle);
        Object value = cs.getObject(index);

        Term valueTerm;
        if (value == null) {
            valueTerm = new Atom("null");
        } else if (value instanceof java.lang.Number) {
            valueTerm = sqlNumberToTerm((java.lang.Number) value);   /* ISS-2025-0424 */
        } else {
            valueTerm = new Atom(value.toString());
        }

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (args.get(2).resolveBindings(bindings).unify(valueTerm, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    private boolean doGetResultSet(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws SQLException {
        List<Term> args = query.getArguments();
        if (args.size() != 2) {
            throw new PrologEvaluationException(
                "jdbc_call_get_resultset/2 requires 2 arguments: jdbc_call_get_resultset(+CallStmt, -Rows).");
        }

        String handle = resolveAtom(args.get(0), bindings, "CallStmt");
        CallableStatement cs = JdbcConnectionManager.getInstance().getCallableStatement(handle);
        ResultSet rs = cs.getResultSet();

        if (rs == null) {
            throw new PrologEvaluationException("jdbc_call_get_resultset: No ResultSet available.");
        }

        // START_CHANGE: ISS-2025-0265 - try-with-resources so the ResultSet is closed even if
        // rs.next()/getObject throws mid-iteration (rs.close() was only reached on normal exit).
        List<Term> rowTerms = new ArrayList<>();
        try (ResultSet r = rs) {
            ResultSetMetaData meta = r.getMetaData();
            int colCount = meta.getColumnCount();

            while (r.next()) {
                List<Term> colValues = new ArrayList<>(colCount);
                for (int i = 1; i <= colCount; i++) {
                    Object val = r.getObject(i);
                    if (val == null) {
                        colValues.add(new Atom("null"));
                    } else if (val instanceof java.lang.Number) {
                        colValues.add(sqlNumberToTerm((java.lang.Number) val));   /* ISS-2025-0424 */
                    } else {
                        colValues.add(new Atom(val.toString()));
                    }
                }
                rowTerms.add(new CompoundTerm(new Atom("row"), colValues));
            }
        }
        // END_CHANGE: ISS-2025-0265

        Term resultList = CollectionUtils.createListTerm(rowTerms);
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (args.get(1).resolveBindings(bindings).unify(resultList, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    private int mapSqlType(String typeName) {
        switch (typeName.toLowerCase()) {
            case "integer": case "int":     return java.sql.Types.INTEGER;
            case "bigint": case "long":     return java.sql.Types.BIGINT;
            case "double": case "float":    return java.sql.Types.DOUBLE;
            case "decimal": case "numeric": return java.sql.Types.DECIMAL;
            case "varchar": case "string":  return java.sql.Types.VARCHAR;
            case "boolean": case "bool":    return java.sql.Types.BOOLEAN;
            case "date":                    return java.sql.Types.DATE;
            case "timestamp":               return java.sql.Types.TIMESTAMP;
            default:
                throw new PrologEvaluationException(
                    "jdbc_call_register_out: Unknown SQL type: " + typeName +
                    ". Valid types: integer, bigint, double, decimal, varchar, boolean, date, timestamp.");
        }
    }

    private String resolveAtom(Term term, Map<String, Term> bindings, String argName) {
        Term resolved = term.resolveBindings(bindings);
        if (!(resolved instanceof Atom)) {
            throw new PrologEvaluationException(modeName() + ": " + argName + " must be an atom.");
        }
        return ((Atom) resolved).getName();
    }

    private int resolveInt(Term term, Map<String, Term> bindings, String argName) {
        Term resolved = term.resolveBindings(bindings);
        if (!(resolved instanceof Number)) {
            throw new PrologEvaluationException(modeName() + ": " + argName + " must be a number.");
        }
        return ((Number) resolved).getValue().intValue();
    }

    private String modeName() {
        switch (mode) {
            case PREPARE_CALL:   return "jdbc_prepare_call";
            case SET_PARAM:      return "jdbc_call_set_param";
            case REGISTER_OUT:   return "jdbc_call_register_out";
            case EXECUTE:        return "jdbc_call_execute";
            case GET_RESULT:     return "jdbc_call_get_result";
            case GET_RESULTSET:  return "jdbc_call_get_resultset";
            default: return "jdbc_call";
        }
    }

    // START_CHANGE: ISS-2025-0424 - ENG-02: map a java.lang.Number to an ISO integer when the SQL
    // value is integral (Integer/Long/Short/Byte/BigInteger) and to an ISO float otherwise.
    // Previously everything went through Number(double), which auto-classified integral doubles as
    // integers — so a DECIMAL 1.0 came back as the integer 1.
    private static Term sqlNumberToTerm(java.lang.Number value) {
        if (value instanceof Integer || value instanceof Long
                || value instanceof Short || value instanceof Byte) {
            return new Number(value.longValue());
        }
        if (value instanceof java.math.BigInteger) {
            return new Number((java.math.BigInteger) value);
        }
        return new Number(value.doubleValue());
    }
    // END_CHANGE: ISS-2025-0424
}
// END_CHANGE: ISS-2025-0110
