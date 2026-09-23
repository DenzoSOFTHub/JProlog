package it.denzosoft.jprolog.core.compiled;

import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.operator.Operator;
import it.denzosoft.jprolog.core.operator.OperatorTable;
import it.denzosoft.jprolog.core.terms.*;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.*;

/**
 * Serializes Rules and operator definitions to the JPC binary format.
 *
 * Usage:
 *   JpcWriter writer = new JpcWriter();
 *   writer.write(rules, operatorTable, sourceHash, outputStream);
 */
// START_CHANGE: ISS-2025-0085 - Binary compiled format writer
public class JpcWriter {

    /** Intern table built during serialization */
    private final List<String> stringTable = new ArrayList<>();
    private final Map<String, Integer> stringIndex = new HashMap<>();

    /**
     * Write compiled rules to an output stream.
     *
     * @param rules         the list of rules to serialize
     * @param operatorTable custom operators (may be null)
     * @param sourceHash    hash of the original source for cache invalidation
     * @param out           target output stream
     */
    public void write(List<Rule> rules, OperatorTable operatorTable,
                      long sourceHash, OutputStream out) throws IOException {
        // Phase 1: collect all strings
        stringTable.clear();
        stringIndex.clear();
        for (Rule rule : rules) {
            collectStrings(rule.getHead());
            for (Term t : rule.getBody()) {
                collectStrings(t);
            }
        }
        if (operatorTable != null) {
            for (String name : operatorTable.getAllOperatorNames()) {
                intern(name);
            }
        }

        DataOutputStream dos = new DataOutputStream(new BufferedOutputStream(out));

        // Header
        dos.write(JpcFormat.MAGIC);
        dos.writeByte(JpcFormat.VERSION);
        dos.writeLong(sourceHash);

        // String table
        writeVarint(dos, stringTable.size());
        for (String s : stringTable) {
            byte[] bytes = s.getBytes(StandardCharsets.UTF_8);
            writeVarint(dos, bytes.length);
            dos.write(bytes);
        }

        // Operators
        List<Operator> ops = new ArrayList<>();
        if (operatorTable != null) {
            for (String name : operatorTable.getAllOperatorNames()) {
                ops.addAll(operatorTable.getOperators(name));
            }
        }
        writeVarint(dos, ops.size());
        for (Operator op : ops) {
            writeVarint(dos, op.getPrecedence());
            dos.writeByte(op.getType().ordinal());
            writeVarint(dos, indexOf(op.getName()));
        }

        // Rules
        writeVarint(dos, rules.size());
        for (Rule rule : rules) {
            // START_CHANGE: ISS-2025-0447 - one variable-index scope per CLAUSE (format 0x03)
            clauseVars.clear();
            // END_CHANGE: ISS-2025-0447
            writeTerm(dos, rule.getHead());
            writeVarint(dos, rule.getBody().size());
            for (Term t : rule.getBody()) {
                writeTerm(dos, t);
            }
            // ISS-2025-0447 - source line (+1 so the "unknown" -1 stays a non-negative varint)
            writeVarint(dos, rule.getSourceLine() + 1);
        }

        dos.flush();
    }

    /**
     * Compute a source hash for cache invalidation.
     */
    public static long computeSourceHash(String source) {
        // START_CHANGE: Round5 minor - upgrade MD5 → SHA-256 (MD5 is collision-broken)
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            byte[] digest = md.digest(source.getBytes(StandardCharsets.UTF_8));
            long hash = 0;
            for (int i = 0; i < 8; i++) {
                hash = (hash << 8) | (digest[i] & 0xFF);
            }
            return hash;
        } catch (NoSuchAlgorithmException e) {
            // Fallback: simple hash (defensive; SHA-256 always available on JREs ≥ 1.7)
            return source.hashCode();
            // END_CHANGE: Round5 minor
        }
    }

    // ---------- internals ----------

    // START_CHANGE: ISS-2025-0447 - per-clause variable numbering (format 0x03)
    private final Map<String, Integer> clauseVars = new HashMap<>();

    private int clauseVarIndex(String name) {
        Integer k = clauseVars.get(name);
        if (k != null) return k;
        int i = clauseVars.size();
        clauseVars.put(name, i);
        return i;
    }
    // END_CHANGE: ISS-2025-0447

    private int intern(String s) {
        Integer idx = stringIndex.get(s);
        if (idx != null) return idx;
        int i = stringTable.size();
        stringTable.add(s);
        stringIndex.put(s, i);
        return i;
    }

    // START_CHANGE: ISS-2025-0188 - Null safety for un-interned strings
    private int indexOf(String s) {
        Integer idx = stringIndex.get(s);
        if (idx == null) {
            return intern(s);
        }
        return idx;
    }
    // END_CHANGE: ISS-2025-0188

    private void collectStrings(Term term) {
        // START_CHANGE: Round5 - cycle detection via IdentityHashMap
        collectStrings(term, new java.util.IdentityHashMap<>());
    }
    private void collectStrings(Term term, java.util.IdentityHashMap<Term, Boolean> visited) {
        // START_CHANGE: ISS-2025-0561 - P3.12: iterative (a deep clause overflowed the recursion);
        // the Round5 rule is kept: a compound met twice is skipped
        java.util.ArrayDeque<Term> work = new java.util.ArrayDeque<>();
        if (term != null) work.push(term);
        while (!work.isEmpty()) {
            Term t = work.pop();
            if (t instanceof CompoundTerm) {
                if (visited.put(t, Boolean.TRUE) != null) continue;
                CompoundTerm ct = (CompoundTerm) t;
                intern(ct.getFunctor().getName());
                List<Term> as = ct.getArguments();
                for (int i = as.size() - 1; i >= 0; i--) if (as.get(i) != null) work.push(as.get(i));
            } else if (t instanceof Atom) {
                intern(((Atom) t).getName());
            } else if (t instanceof Variable) {
                intern(((Variable) t).getName());
            } else if (t instanceof PrologString) {
                intern(((PrologString) t).getStringValue());
            }
            // Number and Rational have no strings
        }
        // END_CHANGE: ISS-2025-0561
    }

    private void writeTerm(DataOutputStream dos, Term term) throws IOException {
        // START_CHANGE: Round5 - cycle detection (throws IOException on cyclic terms instead of SOE)
        writeTerm(dos, term, new java.util.IdentityHashMap<>());
    }
    private void writeTerm(DataOutputStream dos, Term term, java.util.IdentityHashMap<Term, Boolean> visited) throws IOException {
        // START_CHANGE: ISS-2025-0561 - P3.12: pre-order with an explicit stack (a compound's
        // header, then its arguments left to right — the same bytes the recursion wrote)
        java.util.ArrayDeque<Term> work = new java.util.ArrayDeque<>();
        work.push(term);
        while (!work.isEmpty()) {
            Term t = work.pop();
            if (t instanceof CompoundTerm) {
                if (visited.put(t, Boolean.TRUE) != null) {
                    throw new IOException("Cannot serialize cyclic term");
                }
                CompoundTerm ct = (CompoundTerm) t;
                dos.writeByte(JpcFormat.TERM_COMPOUND);
                writeVarint(dos, indexOf(ct.getFunctor().getName()));
                List<Term> as = ct.getArguments();
                writeVarint(dos, as.size());
                for (int i = as.size() - 1; i >= 0; i--) work.push(as.get(i));
            } else {
                writeTermInner(dos, t, visited);
            }
        }
        // END_CHANGE: ISS-2025-0561
        // END_CHANGE: Round5
    }
    private void writeTermInner(DataOutputStream dos, Term term, java.util.IdentityHashMap<Term, Boolean> visited) throws IOException {
        // START_CHANGE: ISS-2025-0185 - Handle Rational before Number (Rational extends Number)
        if (term instanceof Rational) {
            Rational r = (Rational) term;
            dos.writeByte(JpcFormat.TERM_RATIONAL);
            byte[] numBytes = r.getNumerator().toByteArray();
            byte[] denBytes = r.getDenominator().toByteArray();
            writeVarint(dos, numBytes.length);
            dos.write(numBytes);
            writeVarint(dos, denBytes.length);
            dos.write(denBytes);
        // END_CHANGE: ISS-2025-0185
        } else if (term instanceof Atom) {
            dos.writeByte(JpcFormat.TERM_ATOM);
            writeVarint(dos, indexOf(((Atom) term).getName()));
        } else if (term instanceof it.denzosoft.jprolog.core.terms.Number) {
            it.denzosoft.jprolog.core.terms.Number num = (it.denzosoft.jprolog.core.terms.Number) term;
            dos.writeByte(JpcFormat.TERM_NUMBER);
            // START_CHANGE: ISS-2025-0261 - preserve int/float type and BigInteger precision
            // instead of collapsing every number to a double.
            if (num.isInteger()) {
                if (num.isBigInteger()) {
                    dos.writeByte(JpcFormat.NUM_BIGINT);
                    byte[] b = num.bigIntegerValue().toByteArray();
                    writeVarint(dos, b.length);
                    dos.write(b);
                } else {
                    dos.writeByte(JpcFormat.NUM_VARLONG);                     // ISS-2025-0553
                    long v = num.longValue();
                    writeVarlong(dos, (v << 1) ^ (v >> 63));
                }
            } else {
                dos.writeByte(JpcFormat.NUM_FLOAT);
                dos.writeDouble(num.doubleValue());
            }
            // END_CHANGE: ISS-2025-0261
        } else if (term instanceof Variable) {
            // START_CHANGE: ISS-2025-0447 - by INDEX within the clause, plus the name for display.
            Variable var = (Variable) term;
            // ISS-2025-0553: the name only at the first occurrence in the clause
            boolean first = !clauseVars.containsKey(var.getName());
            int slot = clauseVarIndex(var.getName());
            if (first) {
                dos.writeByte(JpcFormat.TERM_VARIABLE);
                writeVarint(dos, slot);
                writeVarint(dos, indexOf(var.getName()));
            } else {
                dos.writeByte(JpcFormat.TERM_VAR_AGAIN);
                writeVarint(dos, slot);
            }
            // END_CHANGE: ISS-2025-0447
        } else if (term instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) term;
            dos.writeByte(JpcFormat.TERM_COMPOUND);
            writeVarint(dos, indexOf(ct.getFunctor().getName()));
            writeVarint(dos, ct.getArguments().size());
            for (Term arg : ct.getArguments()) {
                // START_CHANGE: Round5 - propagate visited map for cycle detection
                writeTerm(dos, arg, visited);
                // END_CHANGE: Round5
            }
        } else if (term instanceof PrologString) {
            dos.writeByte(JpcFormat.TERM_PROLOG_STRING);
            writeVarint(dos, indexOf(((PrologString) term).getStringValue()));
        } else {
            throw new IOException("Unknown term type: " + term.getClass().getName());
        }
    }

    /** Write an unsigned variable-length integer (1-5 bytes). */
    // START_CHANGE: ISS-2025-0553
    static void writeVarlong(DataOutputStream dos, long value) throws IOException {
        while ((value & ~0x7FL) != 0) {
            dos.writeByte((int) ((value & 0x7F) | 0x80));
            value >>>= 7;
        }
        dos.writeByte((int) value);
    }
    // END_CHANGE: ISS-2025-0553

    static void writeVarint(DataOutputStream dos, int value) throws IOException {
        while ((value & ~0x7F) != 0) {
            dos.writeByte((value & 0x7F) | 0x80);
            value >>>= 7;
        }
        dos.writeByte(value);
    }

    // START_CHANGE: Round5 minor - signed varint (zigzag encoding) for future use
    /** Write a signed variable-length integer using zigzag encoding (1-5 bytes). */
    public static void writeSignedVarint(DataOutputStream dos, int value) throws IOException {
        int zigzag = (value << 1) ^ (value >> 31);
        writeVarint(dos, zigzag);
    }
    // END_CHANGE: Round5 minor
}
// END_CHANGE: ISS-2025-0085
