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
    public CompiledProgram read(InputStream in) throws IOException {
        DataInputStream dis = new DataInputStream(new BufferedInputStream(in));

        // Header
        byte[] magic = new byte[3];
        dis.readFully(magic);
        if (magic[0] != JpcFormat.MAGIC[0] || magic[1] != JpcFormat.MAGIC[1] || magic[2] != JpcFormat.MAGIC[2]) {
            throw new IOException("Invalid JPC file: bad magic bytes");
        }
        byte version = dis.readByte();
        if (version != JpcFormat.VERSION) {
            throw new IOException("Unsupported JPC version: " + version);
        }
        long sourceHash = dis.readLong();

        // String table
        int stringCount = readVarint(dis);
        String[] strings = new String[stringCount];
        for (int i = 0; i < stringCount; i++) {
            int len = readVarint(dis);
            byte[] bytes = new byte[len];
            dis.readFully(bytes);
            strings[i] = new String(bytes, StandardCharsets.UTF_8);
        }

        // Operators
        int opCount = readVarint(dis);
        List<Operator> operators = new ArrayList<>(opCount);
        Operator.Type[] types = Operator.Type.values();
        for (int i = 0; i < opCount; i++) {
            int precedence = readVarint(dis);
            int typeOrdinal = dis.readByte() & 0xFF;
            int nameIdx = readVarint(dis);
            if (typeOrdinal >= types.length) {
                throw new IOException("Invalid operator type ordinal: " + typeOrdinal);
            }
            operators.add(new Operator(precedence, types[typeOrdinal], strings[nameIdx]));
        }

        // Rules
        int ruleCount = readVarint(dis);
        List<Rule> rules = new ArrayList<>(ruleCount);
        for (int i = 0; i < ruleCount; i++) {
            Term head = readTerm(dis, strings);
            int bodyCount = readVarint(dis);
            List<Term> body = new ArrayList<>(bodyCount);
            for (int j = 0; j < bodyCount; j++) {
                body.add(readTerm(dis, strings));
            }
            rules.add(new Rule(head, body));
        }

        return new CompiledProgram(sourceHash, operators, rules);
    }

    // ---------- internals ----------

    private Term readTerm(DataInputStream dis, String[] strings) throws IOException {
        byte type = dis.readByte();
        switch (type) {
            case JpcFormat.TERM_ATOM: {
                int idx = readVarint(dis);
                return new Atom(strings[idx]);
            }
            case JpcFormat.TERM_NUMBER: {
                double value = dis.readDouble();
                return new it.denzosoft.jprolog.core.terms.Number(value);
            }
            case JpcFormat.TERM_VARIABLE: {
                int idx = readVarint(dis);
                return new Variable(strings[idx]);
            }
            case JpcFormat.TERM_COMPOUND: {
                int functorIdx = readVarint(dis);
                int argCount = readVarint(dis);
                List<Term> args = new ArrayList<>(argCount);
                for (int i = 0; i < argCount; i++) {
                    args.add(readTerm(dis, strings));
                }
                return new CompoundTerm(new Atom(strings[functorIdx]), args);
            }
            case JpcFormat.TERM_PROLOG_STRING: {
                int idx = readVarint(dis);
                return new PrologString(strings[idx]);
            }
            default:
                throw new IOException("Unknown term type tag: " + type);
        }
    }

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
    }
}
// END_CHANGE: ISS-2025-0085
