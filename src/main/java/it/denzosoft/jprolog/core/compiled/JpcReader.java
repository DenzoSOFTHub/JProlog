package it.denzosoft.jprolog.core.compiled;

import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.operator.Operator;
import it.denzosoft.jprolog.core.operator.OperatorTable;
import it.denzosoft.jprolog.core.terms.*;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

/**
 * Deserializes Rules and operator definitions from the JPC binary format.
 *
 * Usage:
 *   JpcReader reader = new JpcReader();
 *   JpcReader.CompiledProgram program = reader.read(inputStream);
 *   // program.rules, program.operators, program.sourceHash
 */
// START_CHANGE: ISS-2025-0085 - Binary compiled format reader
public class JpcReader {

    /**
     * Result of reading a compiled program.
     */
    public static class CompiledProgram {
        public final long sourceHash;
        public final List<Operator> operators;
        public final List<Rule> rules;

        public CompiledProgram(long sourceHash, List<Operator> operators, List<Rule> rules) {
            this.sourceHash = sourceHash;
            this.operators = operators;
            this.rules = rules;
        }
    }

    /**
     * Read a compiled program from an input stream.
     *
     * @param in the input stream
     * @return the compiled program
     * @throws IOException if reading fails or format is invalid
     */
    // START_CHANGE: ISS-2025-0553 - wave P2.14: the reader decodes from ONE byte array. It used to
    // pull every byte through DataInputStream.readByte over a BufferedInputStream (a third of the
    // load time was that call chain), allocate a new Atom per atom/functor occurrence and build
    // each compound through a List that the CompoundTerm constructor then copied again. Now: the
    // file is read in one go, atoms are made once per string-table entry (they are immutable),
    // compounds adopt their argument array, and each clause reuses one variable frame. Format
    // 0x04 (see JpcFormat) is read, and so is 0x03, which is a valid 0x04 file that never uses
    // the two new encodings.
    public CompiledProgram read(InputStream in) throws IOException {
        Buf b = new Buf(readAll(in));

        // Header
        if (b.remaining() < 12) throw new IOException("Invalid JPC file: truncated header");
        byte m0 = b.u8b(), m1 = b.u8b(), m2 = b.u8b();
        if (m0 != JpcFormat.MAGIC[0] || m1 != JpcFormat.MAGIC[1] || m2 != JpcFormat.MAGIC[2]) {
            throw new IOException("Invalid JPC file: bad magic bytes");
        }
        byte version = b.u8b();
        if (version != JpcFormat.VERSION && version != JpcFormat.VERSION_V3) {
            throw new IOException("Unsupported JPC version: " + version);
        }
        long sourceHash = b.int64();

        // String table
        int stringCount = b.varint();
        if (stringCount < 0 || stringCount > b.remaining()) throw new IOException("Invalid string table size: " + stringCount);
        String[] strings = new String[stringCount];
        for (int i = 0; i < stringCount; i++) {
            int len = b.varint();
            strings[i] = b.utf8(len);
        }
        this.atoms = new Atom[stringCount];

        // Operators
        int opCount = b.varint();
        List<Operator> operators = new ArrayList<>(Math.min(opCount, 4096));
        Operator.Type[] types = Operator.Type.values();
        for (int i = 0; i < opCount; i++) {
            int precedence = b.varint();
            int typeOrdinal = b.u8();
            int nameIdx = b.varint();
            if (typeOrdinal >= types.length) {
                throw new IOException("Invalid operator type ordinal: " + typeOrdinal);
            }
            checkStringIndex(nameIdx, strings.length, "operator");
            operators.add(new Operator(precedence, types[typeOrdinal], strings[nameIdx]));
        }

        // Rules
        int ruleCount = b.varint();
        List<Rule> rules = new ArrayList<>(Math.min(ruleCount, 1 << 20));
        for (int i = 0; i < ruleCount; i++) {
            // START_CHANGE: ISS-2025-0447 - one shared Variable per index, per CLAUSE (format 0x03)
            frameSize = 0;
            Term head = readTerm(b, strings);
            int bodyCount = b.varint();
            List<Term> body = new ArrayList<>(bodyCount);
            for (int j = 0; j < bodyCount; j++) {
                body.add(readTerm(b, strings));
            }
            Rule r = new Rule(head, body);
            r.setSourceLine(b.varint() - 1);
            rules.add(r);
            // END_CHANGE: ISS-2025-0447
        }

        return new CompiledProgram(sourceHash, operators, rules);
    }

    // ---------- internals ----------

    /** One atom per string-table entry, made on first use. */
    private Atom[] atoms;
    /** The current clause's variables by slot; {@code frameSize} slots are valid. */
    private Variable[] frame = new Variable[16];
    private int frameSize;

    private Atom atom(String[] strings, int idx) {
        Atom a = atoms[idx];
        if (a == null) { a = new Atom(strings[idx]); atoms[idx] = a; }
        return a;
    }

    private static byte[] readAll(InputStream in) throws IOException {
        ByteArrayOutputStream bos = new ByteArrayOutputStream(1 << 16);
        byte[] chunk = new byte[1 << 16];
        int n;
        while ((n = in.read(chunk)) > 0) bos.write(chunk, 0, n);
        return bos.toByteArray();
    }

    /** A cursor over the file's bytes; every read is bounds-checked (a truncated file is an IOException). */
    private static final class Buf {
        final byte[] d;
        int p;
        Buf(byte[] d) { this.d = d; }
        int remaining() { return d.length - p; }
        private void need(int n) throws IOException {
            if (n < 0 || p + n > d.length) throw new EOFException("Truncated JPC file");
        }
        byte u8b() throws IOException { need(1); return d[p++]; }
        int u8() throws IOException { need(1); return d[p++] & 0xFF; }
        int varint() throws IOException {
            int value = 0, shift = 0;
            while (true) {
                need(1);
                byte x = d[p++];
                value |= (x & 0x7F) << shift;
                if ((x & 0x80) == 0) return value;
                shift += 7;
                if (shift > 35) throw new IOException("Varint too large");
            }
        }
        long varlong() throws IOException {
            long value = 0;
            int shift = 0;
            while (true) {
                need(1);
                byte x = d[p++];
                value |= (long) (x & 0x7F) << shift;
                if ((x & 0x80) == 0) return value;
                shift += 7;
                if (shift > 63) throw new IOException("Varlong too large");
            }
        }
        long int64() throws IOException {
            need(8);
            long v = 0;
            for (int i = 0; i < 8; i++) v = (v << 8) | (d[p++] & 0xFF);
            return v;
        }
        byte[] bytes(int n) throws IOException {
            need(n);
            byte[] out = java.util.Arrays.copyOfRange(d, p, p + n);
            p += n;
            return out;
        }
        String utf8(int n) throws IOException {
            need(n);
            String s = new String(d, p, n, StandardCharsets.UTF_8);
            p += n;
            return s;
        }
    }

    private Variable slotVar(int slot, String name) throws IOException {
        if (slot < 0 || slot > 1_000_000) throw new IOException("Invalid variable slot: " + slot);
        if (slot >= frame.length) frame = java.util.Arrays.copyOf(frame, Math.max(slot + 1, frame.length * 2));
        while (frameSize <= slot) frame[frameSize++] = null;
        Variable v = frame[slot];
        if (v == null) {
            if (name == null) throw new IOException("Variable slot " + slot + " used before its first occurrence");
            v = new Variable(name);
            frame[slot] = v;
        }
        return v;
    }
    // END_CHANGE: ISS-2025-0553

    // START_CHANGE: ISS-2025-0190 - Add bounds checking on string table indices
    // START_CHANGE: ISS-2025-0561 - P3.12: compounds are decoded with an explicit stack, so a
    // clause nested 100 000 deep loads from a .jpc as it consults (the recursion overflowed).
    private static final class Frame {
        final Atom f; final Term[] args; int i;
        Frame(Atom f, Term[] args) { this.f = f; this.args = args; }
    }

    private Frame compoundFrame(Buf b, String[] strings) throws IOException {
        int functorIdx = b.varint();
        checkStringIndex(functorIdx, strings.length, "compound functor");
        int argCount = b.varint();
        if (argCount < 0 || argCount > b.remaining()) throw new IOException("Invalid arity: " + argCount);
        return new Frame(atom(strings, functorIdx), new Term[argCount]);   // ISS-2025-0553: adopted
    }

    private Term readTerm(Buf b, String[] strings) throws IOException {
        return readTerm(b, strings, 0);
    }

    /** Recursive (allocation-free) to depth 256, then the explicit stack for that subterm. */
    private Term readTerm(Buf b, String[] strings, int depth) throws IOException {
        byte type = b.u8b();
        if (type != JpcFormat.TERM_COMPOUND) return readLeaf(b, strings, type);
        if (depth < 256) {
            Frame f = compoundFrame(b, strings);
            for (int i = 0; i < f.args.length; i++) f.args[i] = readTerm(b, strings, depth + 1);
            return new CompoundTerm(f.f, f.args);
        }
        ArrayList<Frame> stack = new ArrayList<Frame>();
        stack.add(compoundFrame(b, strings));
        for (;;) {
            Frame f = stack.get(stack.size() - 1);
            if (f.i == f.args.length) {
                Term built = new CompoundTerm(f.f, f.args);
                stack.remove(stack.size() - 1);
                if (stack.isEmpty()) return built;
                Frame parent = stack.get(stack.size() - 1);
                parent.args[parent.i++] = built;
                continue;
            }
            byte t = b.u8b();
            if (t == JpcFormat.TERM_COMPOUND) stack.add(compoundFrame(b, strings));
            else f.args[f.i++] = readLeaf(b, strings, t);
        }
    }
    // END_CHANGE: ISS-2025-0561

    private Term readLeaf(Buf b, String[] strings, byte type) throws IOException {
        switch (type) {
            case JpcFormat.TERM_ATOM: {
                int idx = b.varint();
                checkStringIndex(idx, strings.length, "atom");
                return atom(strings, idx);                                // ISS-2025-0553
            }
            case JpcFormat.TERM_NUMBER: {
                // START_CHANGE: ISS-2025-0261 - read the subtype byte so int/float type and
                // BigInteger precision are restored (v0x02 format).
                byte subtype = b.u8b();
                switch (subtype) {
                    case JpcFormat.NUM_LONG:
                        return new it.denzosoft.jprolog.core.terms.Number(b.int64());
                    case JpcFormat.NUM_VARLONG: {                             // ISS-2025-0553
                        long z = b.varlong();
                        return it.denzosoft.jprolog.core.terms.Number.valueOf((z >>> 1) ^ -(z & 1));
                    }
                    case JpcFormat.NUM_FLOAT:
                        return new it.denzosoft.jprolog.core.terms.Number(Double.longBitsToDouble(b.int64()), false);
                    case JpcFormat.NUM_BIGINT: {
                        int len = b.varint();
                        return new it.denzosoft.jprolog.core.terms.Number(new java.math.BigInteger(b.bytes(len)));
                    }
                    default:
                        throw new IOException("Unknown JPC number subtype: " + subtype);
                }
                // END_CHANGE: ISS-2025-0261
            }
            case JpcFormat.TERM_VARIABLE: {
                // START_CHANGE: ISS-2025-0447 - index first, then the name; one cell per index.
                int slot = b.varint();
                int idx = b.varint();
                checkStringIndex(idx, strings.length, "variable");
                return slotVar(slot, strings[idx]);
                // END_CHANGE: ISS-2025-0447
            }
            case JpcFormat.TERM_VAR_AGAIN:                                // ISS-2025-0553
                return slotVar(b.varint(), null);
            case JpcFormat.TERM_PROLOG_STRING: {
                int idx = b.varint();
                checkStringIndex(idx, strings.length, "prolog string");
                return new PrologString(strings[idx]);
            }
    // END_CHANGE: ISS-2025-0190
            // START_CHANGE: ISS-2025-0185 - Rational number deserialization
            case JpcFormat.TERM_RATIONAL: {
                int numLen = b.varint();
                byte[] numBytes = b.bytes(numLen);
                int denLen = b.varint();
                byte[] denBytes = b.bytes(denLen);
                return Rational.of(new java.math.BigInteger(numBytes), new java.math.BigInteger(denBytes));   // ISS-2025-0712: normalised
            }
            // END_CHANGE: ISS-2025-0185
            default:
                throw new IOException("Unknown term type tag: " + type);
        }
    }

    // START_CHANGE: ISS-2025-0190 - Bounds checking helper
    private static void checkStringIndex(int idx, int tableSize, String context) throws IOException {
        if (idx < 0 || idx >= tableSize) {
            throw new IOException("Invalid " + context + " string index: " + idx + " (table size: " + tableSize + ")");
        }
    }
    // END_CHANGE: ISS-2025-0190

    /** Read an unsigned variable-length integer (1-5 bytes). */
    static int readVarint(DataInputStream dis) throws IOException {
        int value = 0;
        int shift = 0;
        byte b;
        do {
            b = dis.readByte();
            value |= (b & 0x7F) << shift;
            shift += 7;
            if (shift > 35) {
                throw new IOException("Varint too large");
            }
        } while ((b & 0x80) != 0);
        return value;
        // START_CHANGE: Round5 minor - readSignedVarint for zigzag-decoded ints
    }

    /** Read a signed variable-length integer (zigzag-encoded). */
    public static int readSignedVarint(DataInputStream dis) throws IOException {
        int zigzag = readVarint(dis);
        return (zigzag >>> 1) ^ -(zigzag & 1);
        // END_CHANGE: Round5 minor
    }
}
// END_CHANGE: ISS-2025-0085
