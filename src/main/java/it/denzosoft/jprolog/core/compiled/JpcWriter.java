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
            writeTerm(dos, rule.getHead());
            writeVarint(dos, rule.getBody().size());
            for (Term t : rule.getBody()) {
                writeTerm(dos, t);
            }
        }

        dos.flush();
    }

    /**
     * Compute a source hash for cache invalidation.
     */
    public static long computeSourceHash(String source) {
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            byte[] digest = md.digest(source.getBytes(StandardCharsets.UTF_8));
            long hash = 0;
            for (int i = 0; i < 8; i++) {
                hash = (hash << 8) | (digest[i] & 0xFF);
            }
            return hash;
        } catch (NoSuchAlgorithmException e) {
            // Fallback: simple hash
            return source.hashCode();
        }
    }

    // ---------- internals ----------

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
        if (term == null) return;
        if (term instanceof CompoundTerm) {
            if (visited.put(term, Boolean.TRUE) != null) {
                // cycle: skip
                return;
            }
        }
        if (term instanceof Atom) {
            intern(((Atom) term).getName());
        } else if (term instanceof Variable) {
            intern(((Variable) term).getName());
        } else if (term instanceof PrologString) {
            intern(((PrologString) term).getStringValue());
        } else if (term instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) term;
            intern(ct.getFunctor().getName());
            for (Term arg : ct.getArguments()) {
                collectStrings(arg, visited);
            }
        }
        // Number and Rational have no strings
        // END_CHANGE: Round5
    }

    private void writeTerm(DataOutputStream dos, Term term) throws IOException {
        // START_CHANGE: Round5 - cycle detection (throws IOException on cyclic terms instead of SOE)
        writeTerm(dos, term, new java.util.IdentityHashMap<>());
    }
    private void writeTerm(DataOutputStream dos, Term term, java.util.IdentityHashMap<Term, Boolean> visited) throws IOException {
        if (term instanceof CompoundTerm) {
            if (visited.put(term, Boolean.TRUE) != null) {
                throw new IOException("Cannot serialize cyclic term: " + term);
            }
        }
        writeTermInner(dos, term, visited);
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
            dos.writeDouble(num.getValue());
        } else if (term instanceof Variable) {
            Variable var = (Variable) term;
            dos.writeByte(JpcFormat.TERM_VARIABLE);
            writeVarint(dos, indexOf(var.getName()));
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
    static void writeVarint(DataOutputStream dos, int value) throws IOException {
        while ((value & ~0x7F) != 0) {
            dos.writeByte((value & 0x7F) | 0x80);
            value >>>= 7;
        }
        dos.writeByte(value);
    }
}
// END_CHANGE: ISS-2025-0085
