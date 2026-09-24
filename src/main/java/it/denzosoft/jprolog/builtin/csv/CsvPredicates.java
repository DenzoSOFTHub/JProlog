package it.denzosoft.jprolog.builtin.csv;

// START_CHANGE: ISS-2025-0120 - CSV built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.core.utils.CollectionUtils;

import java.io.*;
import java.nio.file.*;
import java.util.*;

/**
 * CSV predicates:
 *   csv_read_file/2    - csv_read_file(+Path, -Rows)         read CSV into list of row(...) terms
 *   csv_write_file/2   - csv_write_file(+Path, +Rows)        write list of row(...) terms to CSV
 *   csv_parse/2        - csv_parse(+CsvString, -Rows)        parse CSV string
 *   csv_serialize/2    - csv_serialize(+Rows, -CsvString)     serialize rows to CSV string
 */
public class CsvPredicates implements BuiltIn {

    public enum Mode { CSV_READ_FILE, CSV_WRITE_FILE, CSV_PARSE, CSV_SERIALIZE }

    private final Mode mode;

    public CsvPredicates(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        try {
            switch (mode) {
                case CSV_READ_FILE:  return doReadFile(query, bindings, solutions);
                case CSV_WRITE_FILE: return doWriteFile(query, bindings, solutions);
                case CSV_PARSE:      return doParse(query, bindings, solutions);
                case CSV_SERIALIZE:  return doSerialize(query, bindings, solutions);
                default: return false;
            }
        } catch (IOException e) {
            // ISS-2025-0686: a host failure of the file, not an argument fault
            throw Errors.host(e, mode.name().contains("WRITE") ? "write" : "read", "file", null, modeName(), arityOf(query));
        }
    }

    private boolean doReadFile(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 2);
        String path = resolveAtom(query, 0, bindings);
        String content = new String(Files.readAllBytes(Paths.get(it.denzosoft.jprolog.core.engine.v4.EngineState.path(path))));
        List<Term> rows = parseCsv(content);
        return unify(query.getArguments().get(1), CollectionUtils.createListTerm(rows), bindings, solutions);
    }

    private boolean doWriteFile(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 2);
        String path = resolveAtom(query, 0, bindings);
        Term rowsTerm = query.getArguments().get(1).resolveBindings(bindings);
        String csv = serializeCsv(rowsTerm);
        Files.write(Paths.get(it.denzosoft.jprolog.core.engine.v4.EngineState.path(path)), csv.getBytes());
        solutions.add(bindings);
        return true;
    }

    private boolean doParse(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 2);
        String csvStr = resolveAtom(query, 0, bindings);
        List<Term> rows = parseCsv(csvStr);
        return unify(query.getArguments().get(1), CollectionUtils.createListTerm(rows), bindings, solutions);
    }

    private boolean doSerialize(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 2);
        Term rowsTerm = query.getArguments().get(0).resolveBindings(bindings);
        String csv = serializeCsv(rowsTerm);
        return unify(query.getArguments().get(1), new Atom(csv), bindings, solutions);
    }

    private List<Term> parseCsv(String content) {
        List<Term> rows = new ArrayList<>();
        String[] lines = content.split("\n");
        for (String line : lines) {
            String trimmed = line.trim();
            if (trimmed.isEmpty()) continue;
            List<String> fields = parseCsvLine(trimmed);
            List<Term> fieldTerms = new ArrayList<>();
            for (String field : fields) {
                // START_CHANGE: ISS-2025-0424 - ENG-02: a CSV field "1.0" is a FLOAT and "1" an
                // INTEGER; Double.parseDouble + Number(double) collapsed both to the integer 1.
                Number val = it.denzosoft.jprolog.builtin.conversion.AtomNumber.parseNumberToken(field);
                if (val != null) {
                    fieldTerms.add(val);
                } else {
                    fieldTerms.add(new Atom(field));
                }
                // END_CHANGE: ISS-2025-0424
            }
            rows.add(new CompoundTerm(new Atom("row"), fieldTerms));
        }
        return rows;
    }

    private List<String> parseCsvLine(String line) {
        List<String> fields = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        boolean inQuotes = false;

        for (int i = 0; i < line.length(); i++) {
            char c = line.charAt(i);
            if (inQuotes) {
                if (c == '"') {
                    if (i + 1 < line.length() && line.charAt(i + 1) == '"') {
                        current.append('"');
                        i++;
                    } else {
                        inQuotes = false;
                    }
                } else {
                    current.append(c);
                }
            } else {
                if (c == '"') {
                    inQuotes = true;
                } else if (c == ',') {
                    fields.add(current.toString().trim());
                    current = new StringBuilder();
                } else {
                    current.append(c);
                }
            }
        }
        fields.add(current.toString().trim());
        return fields;
    }

    private String serializeCsv(Term rowsTerm) {
        List<Term> rows = CollectionUtils.termToList(rowsTerm);
        // ISS-2025-0686
        if (rows == null) {
            if (rowsTerm instanceof it.denzosoft.jprolog.core.terms.Variable) {
                throw Errors.instantiation(modeName(), 2, "the rows must be bound");
            }
            throw Errors.type("list", rowsTerm, modeName(), 2, "a list of row(...) terms expected");
        }
        StringBuilder sb = new StringBuilder();
        for (Term row : rows) {
            if (row instanceof CompoundTerm && "row".equals(((CompoundTerm) row).getName())) {
                List<Term> fields = row.getArguments();
                for (int i = 0; i < fields.size(); i++) {
                    if (i > 0) sb.append(',');
                    String val = termToString(fields.get(i));
                    if (val.contains(",") || val.contains("\"") || val.contains("\n")) {
                        sb.append('"').append(val.replace("\"", "\"\"")).append('"');
                    } else {
                        sb.append(val);
                    }
                }
                sb.append('\n');
            }
        }
        return sb.toString();
    }

    private String termToString(Term t) {
        if (t instanceof Atom) return ((Atom) t).getName();
        if (t instanceof Number) {
            double v = ((Number) t).getValue();
            if (v == Math.floor(v) && !Double.isInfinite(v)) return String.valueOf((long) v);
            return String.valueOf(v);
        }
        return t.toString();
    }


    // START_CHANGE: ISS-2025-0686 - wave Q1.1: ISO error terms error(Formal, context(Name/Arity, Msg)),
    // not message atoms (LIM-038)
    private static int arityOf(Term query) {
        return query.getArguments() == null ? 0 : query.getArguments().size();
    }

    /** Unreachable through the registry since ISS-2025-0685 (exact arities); kept for direct calls. */
    private void checkArity(Term query, int expected) {
        int n = arityOf(query);
        if (n != expected) throw Errors.existence("procedure", Errors.pi(modeName(), n), modeName(), n, null);
    }

    /** Argument {@code i} as text: an atom (or a string); unbound is an instantiation error. */
    private String resolveAtom(Term query, int i, Map<String, Term> bindings) {
        int n = arityOf(query);
        Term resolved = query.getArguments().get(i).resolveBindings(bindings);
        if (resolved instanceof it.denzosoft.jprolog.core.terms.Variable) {
            throw Errors.instantiation(modeName(), n, "argument " + (i + 1) + " must be bound");
        }
        if (resolved instanceof it.denzosoft.jprolog.core.terms.PrologString) {
            return ((it.denzosoft.jprolog.core.terms.PrologString) resolved).getStringValue();
        }
        if (!(resolved instanceof Atom)) {
            throw Errors.type("atom", resolved, modeName(), n, "argument " + (i + 1) + " must be an atom");
        }
        return ((Atom) resolved).getName();
    }
    // END_CHANGE: ISS-2025-0686

    private boolean unify(Term target, Term value, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        Map<String, Term> nb = new HashMap<>(bindings);
        if (target.resolveBindings(bindings).unify(value, nb)) { solutions.add(nb); return true; }
        return false;
    }

    private String modeName() { return mode.name().toLowerCase(); }
}
// END_CHANGE: ISS-2025-0120
