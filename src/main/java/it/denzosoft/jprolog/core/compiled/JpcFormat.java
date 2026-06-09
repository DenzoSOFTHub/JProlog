package it.denzosoft.jprolog.core.compiled;

/**
 * Constants for the JProlog Compiled (.jpc) binary format.
 *
 * Format layout:
 *   Header:   MAGIC (3 bytes) + VERSION (1 byte) + SOURCE_HASH (8 bytes)
 *   Strings:  string count (varint) + [length (varint) + UTF-8 bytes] ...
 *   Operators: op count (varint) + [precedence (varint) + type_idx (varint) + name_idx (varint)] ...
 *   Rules:    rule count (varint) + [term ...] ...
 *
 * Terms are encoded as:
 *   type_byte + type-specific payload
 *
 * String interning: all strings (atom names, variable names, etc.) are stored
 * once in the string table and referenced by index (varint).
 */
// START_CHANGE: ISS-2025-0085 - Binary compiled format for faster loading
public final class JpcFormat {

    private JpcFormat() {}

    /** Magic bytes: "JPC" */
    public static final byte[] MAGIC = { 0x4A, 0x50, 0x43 };

    /** Current format version */
    // START_CHANGE: ISS-2025-0261 - bumped to 0x02: TERM_NUMBER now carries a subtype byte
    // (long / float / bigint) so int/float type and BigInteger precision round-trip. Older 0x01
    // .jpc files are rejected by the version check and transparently recompiled.
    public static final byte VERSION = 0x02;
    // END_CHANGE: ISS-2025-0261

    /** File extension */
    public static final String EXTENSION = ".jpc";

    // Term type tags
    public static final byte TERM_ATOM          = 0x01;
    public static final byte TERM_NUMBER        = 0x02;
    public static final byte TERM_VARIABLE      = 0x03;
    public static final byte TERM_COMPOUND      = 0x04;
    public static final byte TERM_PROLOG_STRING = 0x05;
    // START_CHANGE: ISS-2025-0185 - Rational number serialization
    public static final byte TERM_RATIONAL      = 0x06;

    // START_CHANGE: ISS-2025-0261 - TERM_NUMBER subtype tags (preserve int/float + BigInteger)
    public static final byte NUM_LONG   = 0x00;  // signed 64-bit integer
    public static final byte NUM_FLOAT  = 0x01;  // IEEE-754 double
    public static final byte NUM_BIGINT = 0x02;  // arbitrary-precision integer (two's-complement bytes)
    // END_CHANGE: ISS-2025-0261
    // END_CHANGE: ISS-2025-0185

    // Operator type indices (matches Operator.Type ordinal)
    public static final byte OP_FX  = 0;
    public static final byte OP_FY  = 1;
    public static final byte OP_XF  = 2;
    public static final byte OP_YF  = 3;
    public static final byte OP_XFX = 4;
    public static final byte OP_XFY = 5;
    public static final byte OP_YFX = 6;
}
// END_CHANGE: ISS-2025-0085
