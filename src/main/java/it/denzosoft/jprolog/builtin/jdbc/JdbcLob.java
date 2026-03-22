package it.denzosoft.jprolog.builtin.jdbc;

// START_CHANGE: ISS-2025-0111 - CLOB and BLOB support for JDBC
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.utils.CollectionUtils;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.*;

/**
 * CLOB/BLOB handling predicates:
 *
 *   jdbc_set_clob/3  - jdbc_set_clob(+Stmt, +Index, +TextAtom)
 *     Sets a CLOB parameter from a Prolog atom (string content).
 *
 *   jdbc_set_blob/3  - jdbc_set_blob(+Stmt, +Index, +FilePath)
 *     Sets a BLOB parameter from a file.
 *
 *   jdbc_set_blob_bytes/3 - jdbc_set_blob_bytes(+Stmt, +Index, +ByteList)
 *     Sets a BLOB parameter from a list of byte values (0-255).
 *
 *   jdbc_get_clob/3  - jdbc_get_clob(+ConnHandle, +SQL, -TextAtom)
 *     Executes a query and reads the first CLOB column of the first row.
 *
 *   jdbc_get_blob_to_file/3 - jdbc_get_blob_to_file(+ConnHandle, +SQL, +FilePath)
 *     Executes a query and writes the first BLOB column of the first row to a file.
 *
 *   jdbc_get_blob_bytes/3 - jdbc_get_blob_bytes(+ConnHandle, +SQL, -ByteList)
 *     Executes a query and returns the first BLOB column as a list of byte values.
 */
public class JdbcLob implements BuiltIn {

    public enum Mode {
        SET_CLOB, SET_BLOB_FILE, SET_BLOB_BYTES,
        GET_CLOB, GET_BLOB_FILE, GET_BLOB_BYTES
    }

    private final Mode mode;

    public JdbcLob(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        try {
            switch (mode) {
                case SET_CLOB:       return doSetClob(query, bindings, solutions);
                case SET_BLOB_FILE:  return doSetBlobFile(query, bindings, solutions);
                case SET_BLOB_BYTES: return doSetBlobBytes(query, bindings, solutions);
                case GET_CLOB:       return doGetClob(query, bindings, solutions);
                case GET_BLOB_FILE:  return doGetBlobFile(query, bindings, solutions);
                case GET_BLOB_BYTES: return doGetBlobBytes(query, bindings, solutions);
                default: return false;
            }
        } catch (SQLException e) {
            throw new PrologEvaluationException(modeName() + ": " + e.getMessage());
        } catch (IOException e) {
            throw new PrologEvaluationException(modeName() + ": I/O error: " + e.getMessage());
        }
    }

    // ---- SET operations (on prepared statements) ----

    private boolean doSetClob(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws SQLException {
        List<Term> args = query.getArguments();
        if (args.size() != 3) {
            throw new PrologEvaluationException("jdbc_set_clob/3 requires 3 arguments.");
        }

        String handle = resolveAtom(args.get(0), bindings, "Statement");
        int index = resolveInt(args.get(1), bindings, "Index");
        String text = resolveAtom(args.get(2), bindings, "Text");

        PreparedStatement ps = JdbcConnectionManager.getInstance().getStatement(handle);
        ps.setClob(index, new StringReader(text));

        solutions.add(bindings);
        return true;
    }

    private boolean doSetBlobFile(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws SQLException, IOException {
        List<Term> args = query.getArguments();
        if (args.size() != 3) {
            throw new PrologEvaluationException("jdbc_set_blob/3 requires 3 arguments.");
        }

        String handle = resolveAtom(args.get(0), bindings, "Statement");
        int index = resolveInt(args.get(1), bindings, "Index");
        String filePath = resolveAtom(args.get(2), bindings, "FilePath");

        File file = new File(filePath);
        if (!file.exists()) {
            throw new PrologEvaluationException("jdbc_set_blob/3: File not found: " + filePath);
        }

        // START_CHANGE: ISS-2025-0173 - Read file into byte array to avoid FileInputStream leak
        // Cannot use try-with-resources directly because setBinaryStream may need the stream
        // open until execute. Instead, read the file into a byte array and use setBytes.
        PreparedStatement ps = JdbcConnectionManager.getInstance().getStatement(handle);
        byte[] fileBytes;
        try (FileInputStream fis = new FileInputStream(file)) {
            ByteArrayOutputStream baos = new ByteArrayOutputStream((int) file.length());
            byte[] buffer = new byte[8192];
            int bytesRead;
            while ((bytesRead = fis.read(buffer)) != -1) {
                baos.write(buffer, 0, bytesRead);
            }
            fileBytes = baos.toByteArray();
        }
        ps.setBytes(index, fileBytes);
        // END_CHANGE: ISS-2025-0173

        solutions.add(bindings);
        return true;
    }

    private boolean doSetBlobBytes(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws SQLException {
        List<Term> args = query.getArguments();
        if (args.size() != 3) {
            throw new PrologEvaluationException("jdbc_set_blob_bytes/3 requires 3 arguments.");
        }

        String handle = resolveAtom(args.get(0), bindings, "Statement");
        int index = resolveInt(args.get(1), bindings, "Index");
        List<Term> byteTerms = termToList(args.get(2).resolveBindings(bindings));

        byte[] bytes = new byte[byteTerms.size()];
        for (int i = 0; i < byteTerms.size(); i++) {
            if (!(byteTerms.get(i) instanceof Number)) {
                throw new PrologEvaluationException("jdbc_set_blob_bytes/3: Byte list must contain numbers.");
            }
            int b = ((Number) byteTerms.get(i)).getValue().intValue();
            if (b < 0 || b > 255) {
                throw new PrologEvaluationException("jdbc_set_blob_bytes/3: Byte value out of range (0-255): " + b);
            }
            bytes[i] = (byte) b;
        }

        PreparedStatement ps = JdbcConnectionManager.getInstance().getStatement(handle);
        ps.setBytes(index, bytes);

        solutions.add(bindings);
        return true;
    }

    // ---- GET operations (execute query + read LOB) ----

    private boolean doGetClob(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws SQLException, IOException {
        List<Term> args = query.getArguments();
        if (args.size() != 3) {
            throw new PrologEvaluationException("jdbc_get_clob/3 requires 3 arguments.");
        }

        String connHandle = resolveAtom(args.get(0), bindings, "Connection");
        String sql = resolveAtom(args.get(1), bindings, "SQL");

        String rsHandle = JdbcConnectionManager.getInstance().executeQuery(connHandle, sql);
        try {
            java.sql.ResultSet rs = JdbcConnectionManager.getInstance().getResultSet(rsHandle);
            if (!rs.next()) {
                return false; // No rows
            }

            Clob clob = rs.getClob(1);
            if (clob == null) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (args.get(2).resolveBindings(bindings).unify(new Atom("null"), newBindings)) {
                    solutions.add(newBindings);
                    return true;
                }
                return false;
            }

            String text = clobToString(clob);
            clob.free();

            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (args.get(2).resolveBindings(bindings).unify(new Atom(text), newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        } finally {
            JdbcConnectionManager.getInstance().closeResultSet(rsHandle);
        }
    }

    private boolean doGetBlobFile(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws SQLException, IOException {
        List<Term> args = query.getArguments();
        if (args.size() != 3) {
            throw new PrologEvaluationException("jdbc_get_blob_to_file/3 requires 3 arguments.");
        }

        String connHandle = resolveAtom(args.get(0), bindings, "Connection");
        String sql = resolveAtom(args.get(1), bindings, "SQL");
        String filePath = resolveAtom(args.get(2), bindings, "FilePath");

        String rsHandle = JdbcConnectionManager.getInstance().executeQuery(connHandle, sql);
        try {
            java.sql.ResultSet rs = JdbcConnectionManager.getInstance().getResultSet(rsHandle);
            if (!rs.next()) {
                return false;
            }

            Blob blob = rs.getBlob(1);
            if (blob == null) {
                return false;
            }

            try (InputStream is = blob.getBinaryStream();
                 FileOutputStream fos = new FileOutputStream(filePath)) {
                byte[] buffer = new byte[8192];
                int bytesRead;
                while ((bytesRead = is.read(buffer)) != -1) {
                    fos.write(buffer, 0, bytesRead);
                }
            }
            blob.free();

            solutions.add(bindings);
            return true;
        } finally {
            JdbcConnectionManager.getInstance().closeResultSet(rsHandle);
        }
    }

    private boolean doGetBlobBytes(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws SQLException, IOException {
        List<Term> args = query.getArguments();
        if (args.size() != 3) {
            throw new PrologEvaluationException("jdbc_get_blob_bytes/3 requires 3 arguments.");
        }

        String connHandle = resolveAtom(args.get(0), bindings, "Connection");
        String sql = resolveAtom(args.get(1), bindings, "SQL");

        String rsHandle = JdbcConnectionManager.getInstance().executeQuery(connHandle, sql);
        try {
            java.sql.ResultSet rs = JdbcConnectionManager.getInstance().getResultSet(rsHandle);
            if (!rs.next()) {
                return false;
            }

            byte[] bytes = rs.getBytes(1);
            if (bytes == null) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (args.get(2).resolveBindings(bindings).unify(new Atom("null"), newBindings)) {
                    solutions.add(newBindings);
                    return true;
                }
                return false;
            }

            List<Term> byteTerms = new ArrayList<>(bytes.length);
            for (byte b : bytes) {
                byteTerms.add(new Number(b & 0xFF));
            }

            Term byteList = CollectionUtils.createListTerm(byteTerms);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (args.get(2).resolveBindings(bindings).unify(byteList, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        } finally {
            JdbcConnectionManager.getInstance().closeResultSet(rsHandle);
        }
    }

    // ---- Utilities ----

    private String clobToString(Clob clob) throws SQLException, IOException {
        StringBuilder sb = new StringBuilder();
        try (Reader reader = clob.getCharacterStream()) {
            char[] buffer = new char[4096];
            int charsRead;
            while ((charsRead = reader.read(buffer)) != -1) {
                sb.append(buffer, 0, charsRead);
            }
        }
        return sb.toString();
    }

    private List<Term> termToList(Term term) {
        List<Term> result = new ArrayList<>();
        Term current = term;
        while (current instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) current;
            if (".".equals(ct.getName()) && ct.getArguments().size() == 2) {
                result.add(ct.getArguments().get(0));
                current = ct.getArguments().get(1);
            } else {
                break;
            }
        }
        return result;
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
            case SET_CLOB:       return "jdbc_set_clob";
            case SET_BLOB_FILE:  return "jdbc_set_blob";
            case SET_BLOB_BYTES: return "jdbc_set_blob_bytes";
            case GET_CLOB:       return "jdbc_get_clob";
            case GET_BLOB_FILE:  return "jdbc_get_blob_to_file";
            case GET_BLOB_BYTES: return "jdbc_get_blob_bytes";
            default: return "jdbc_lob";
        }
    }
}
// END_CHANGE: ISS-2025-0111
