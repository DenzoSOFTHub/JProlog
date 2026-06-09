package it.denzosoft.jprolog.builtin.jdbc;

// START_CHANGE: ISS-2025-0108 - JDBC built-in predicates for database connectivity
import java.sql.*;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Manages JDBC connections, statements, and result sets.
 * Thread-safe singleton with named connection handles.
 */
public final class JdbcConnectionManager {

    private static final JdbcConnectionManager INSTANCE = new JdbcConnectionManager();

    private final Map<String, Connection> connections = new ConcurrentHashMap<>();
    private final Map<String, PreparedStatement> statements = new ConcurrentHashMap<>();
    private final Map<String, ResultSet> resultSets = new ConcurrentHashMap<>();
    private final AtomicInteger connCounter = new AtomicInteger(0);
    private final AtomicInteger stmtCounter = new AtomicInteger(0);
    private final AtomicInteger rsCounter = new AtomicInteger(0);

    private JdbcConnectionManager() {}

    public static JdbcConnectionManager getInstance() {
        return INSTANCE;
    }

    // ---- Connections ----

    public String openConnection(String url, String user, String password) throws SQLException {
        Connection conn = DriverManager.getConnection(url, user, password);
        String handle = "$jdbc_conn_" + connCounter.incrementAndGet();
        connections.put(handle, conn);
        return handle;
    }

    public String openConnection(String url) throws SQLException {
        Connection conn = DriverManager.getConnection(url);
        String handle = "$jdbc_conn_" + connCounter.incrementAndGet();
        connections.put(handle, conn);
        return handle;
    }

    public void closeConnection(String handle) throws SQLException {
        Connection conn = connections.remove(handle);
        if (conn != null && !conn.isClosed()) {
            conn.close();
        }
    }

    public Connection getConnection(String handle) {
        Connection conn = connections.get(handle);
        if (conn == null) {
            throw new IllegalArgumentException("Unknown JDBC connection handle: " + handle);
        }
        return conn;
    }

    // ---- Transactions ----

    public void setAutoCommit(String handle, boolean auto) throws SQLException {
        getConnection(handle).setAutoCommit(auto);
    }

    public void commit(String handle) throws SQLException {
        getConnection(handle).commit();
    }

    public void rollback(String handle) throws SQLException {
        getConnection(handle).rollback();
    }

    // ---- Statements / Queries ----

    public String prepareStatement(String connHandle, String sql) throws SQLException {
        PreparedStatement ps = getConnection(connHandle).prepareStatement(sql);
        String handle = "$jdbc_stmt_" + stmtCounter.incrementAndGet();
        statements.put(handle, ps);
        return handle;
    }

    public PreparedStatement getStatement(String handle) {
        PreparedStatement ps = statements.get(handle);
        if (ps == null) {
            throw new IllegalArgumentException("Unknown JDBC statement handle: " + handle);
        }
        return ps;
    }

    public void closeStatement(String handle) throws SQLException {
        PreparedStatement ps = statements.remove(handle);
        if (ps != null && !ps.isClosed()) {
            ps.close();
        }
    }

    public String executeQuery(String connHandle, String sql) throws SQLException {
        // START_CHANGE: ISS-2025-0259 - close the ad-hoc Statement if executeQuery throws
        // (e.g. invalid SQL); otherwise it (and its server-side cursor) leaks since it is never
        // stored in any map.
        Statement stmt = getConnection(connHandle).createStatement();
        try {
            ResultSet rs = stmt.executeQuery(sql);
            String handle = "$jdbc_rs_" + rsCounter.incrementAndGet();
            resultSets.put(handle, rs);
            return handle;
        } catch (SQLException | RuntimeException e) {
            try { stmt.close(); } catch (SQLException ignore) { /* preserve original error */ }
            throw e;
        }
        // END_CHANGE: ISS-2025-0259
    }

    public int executeUpdate(String connHandle, String sql) throws SQLException {
        try (Statement stmt = getConnection(connHandle).createStatement()) {
            return stmt.executeUpdate(sql);
        }
    }

    // ---- Prepared Statements with Parameters ----

    public String executePreparedQuery(String stmtHandle) throws SQLException {
        PreparedStatement ps = getStatement(stmtHandle);
        ResultSet rs = ps.executeQuery();
        String handle = "$jdbc_rs_" + rsCounter.incrementAndGet();
        resultSets.put(handle, rs);
        return handle;
    }

    public int executePreparedUpdate(String stmtHandle) throws SQLException {
        return getStatement(stmtHandle).executeUpdate();
    }

    // ---- Callable Statements (Stored Procedures) ----

    private final Map<String, CallableStatement> callableStatements = new ConcurrentHashMap<>();

    public String prepareCall(String connHandle, String sql) throws SQLException {
        CallableStatement cs = getConnection(connHandle).prepareCall(sql);
        String handle = "$jdbc_call_" + stmtCounter.incrementAndGet();
        callableStatements.put(handle, cs);
        return handle;
    }

    public CallableStatement getCallableStatement(String handle) {
        CallableStatement cs = callableStatements.get(handle);
        if (cs == null) {
            throw new IllegalArgumentException("Unknown JDBC callable statement handle: " + handle);
        }
        return cs;
    }

    public void closeCallableStatement(String handle) throws SQLException {
        CallableStatement cs = callableStatements.remove(handle);
        if (cs != null && !cs.isClosed()) {
            cs.close();
        }
    }

    // ---- ResultSets ----

    public ResultSet getResultSet(String handle) {
        ResultSet rs = resultSets.get(handle);
        if (rs == null) {
            throw new IllegalArgumentException("Unknown JDBC result set handle: " + handle);
        }
        return rs;
    }

    public void closeResultSet(String handle) throws SQLException {
        ResultSet rs = resultSets.remove(handle);
        if (rs != null && !rs.isClosed()) {
            Statement stmt = rs.getStatement();
            rs.close();
            // START_CHANGE: ISS-2025-0259 - Only close the parent statement when it is an ad-hoc
            // (unmanaged) statement created for this result set by executeQuery(). A result set
            // produced by a managed prepared/callable statement shares that statement; closing it
            // here would corrupt the user's still-registered handle (and leak on its own close path).
            if (stmt != null && !stmt.isClosed()
                    && !statements.containsValue(stmt)
                    && !callableStatements.containsValue(stmt)) {
                stmt.close();
            }
            // END_CHANGE: ISS-2025-0259
        }
    }

    /** Close all open resources. */
    public void closeAll() {
        for (String h : resultSets.keySet()) {
            try { closeResultSet(h); } catch (SQLException ignored) {}
        }
        for (String h : statements.keySet()) {
            try { closeStatement(h); } catch (SQLException ignored) {}
        }
        for (String h : callableStatements.keySet()) {
            try { closeCallableStatement(h); } catch (SQLException ignored) {}
        }
        for (String h : connections.keySet()) {
            try { closeConnection(h); } catch (SQLException ignored) {}
        }
    }
}
// END_CHANGE: ISS-2025-0108
