package it.denzosoft.jprolog.core.engine.v4;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CoderResult;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashSet;
import java.util.Set;

// START_CHANGE: ISS-2025-0472 - engine v4 wave W7, design B.11: one open stream.
/**
 * One open Prolog stream: a byte channel plus, for a text stream, a private decode buffer that
 * tracks <b>byte position, character count, line number and line position</b>.
 *
 * <p>That buffer is what fixes limit <b>L-07</b>. The previous implementation handed out a
 * {@code PushbackReader} wrapped around the raw {@code FileInputStream}: the reader had its own
 * 8 KB buffer, so {@code seek/4} and {@code set_stream_position/2} moved the underlying channel
 * while the next {@code get_char/2} kept consuming the stale buffer —
 * {@code get_char(S,C1), seek(S,0,bof,_), get_char(S,C2)} answered {@code C2 = e} after
 * {@code C1 = h}. Here every reposition flushes the decode buffer and resets the decoder, and the
 * reported position is always {@code basePos + buffer.position()} minus any peeked character, so
 * it is exact on text streams as well as binary ones.
 *
 * <p>Peek is a one-character lookahead on the decoder ({@link #peekCodePoint()}), not a pushback on
 * a reader, so a peek costs no state that a reposition could invalidate.
 */
public final class PrologStream {

    private static final int NONE = -2;
    private static final int BUF = 8192;

    /** The numeric identity behind the {@code '$stream'(N)} term. */
    public final int id;
    /** The canonical handle, {@code stream_<id>} — also the key of the legacy string-alias API. */
    public final String handle;

    String fileName;
    String mode = "read";          // read | write | append
    boolean input;
    String type = "text";          // text | binary
    String encoding = "utf8";
    String eofAction = "eof_code"; // error | eof_code | reset
    boolean reposition;
    boolean systemStream;
    boolean closed;
    final Set<String> aliases = new LinkedHashSet<String>();

    // ---- input side ---------------------------------------------------
    private FileChannel inChannel;
    private InputStream rawIn;
    private ByteBuffer bytes;
    private long basePos;
    private Charset charset = StandardCharsets.UTF_8;
    private CharsetDecoder decoder;
    private final CharBuffer cb1 = CharBuffer.allocate(1);
    private final CharBuffer cb2 = CharBuffer.allocate(2);
    private boolean flushed;
    private boolean eofSeen;
    private boolean pastEof;
    private int pendingCp = NONE;
    private int pendingBytes;
    private long charCount;
    private long lineCount = 1;
    private long linePos;

    // ---- output side --------------------------------------------------
    private OutputStream rawOut;
    private FileChannel outChannel;
    private PrintStream printOut;
    private Counter counter;

    PrologStream(int id, boolean input) {
        this.id = id;
        this.handle = "stream_" + id;
        this.input = input;
    }

    // ------------------------------------------------------------------
    // construction
    // ------------------------------------------------------------------

    static PrologStream forFileRead(int id, String file, Charset cs) throws IOException {
        PrologStream s = new PrologStream(id, true);
        FileInputStream fis = new FileInputStream(file);
        s.fileName = file;
        s.mode = "read";
        s.rawIn = fis;
        s.inChannel = fis.getChannel();
        s.reposition = true;
        s.setCharset(cs);
        s.bytes = ByteBuffer.allocate(BUF);
        s.bytes.limit(0);
        return s;
    }

    static PrologStream forFileWrite(int id, String file, boolean append, Charset cs) throws IOException {
        PrologStream s = new PrologStream(id, false);
        FileOutputStream fos = new FileOutputStream(file, append);
        s.fileName = file;
        s.mode = append ? "append" : "write";
        s.outChannel = fos.getChannel();
        s.counter = new Counter(fos);
        s.rawOut = s.counter;
        s.printOut = new PrintStream(s.counter, true);
        s.reposition = true;
        s.setCharset(cs);
        return s;
    }

    static PrologStream forSystemInput(int id, InputStream in) {
        PrologStream s = new PrologStream(id, true);
        s.fileName = null;
        s.mode = "read";
        s.rawIn = in;
        s.systemStream = true;
        s.reposition = false;
        s.setCharset(StandardCharsets.UTF_8);
        s.bytes = ByteBuffer.allocate(BUF);
        s.bytes.limit(0);
        return s;
    }

    static PrologStream forSystemOutput(int id, PrintStream out) {
        PrologStream s = new PrologStream(id, false);
        s.mode = "append";
        s.printOut = out;
        s.rawOut = out;
        s.systemStream = true;
        s.reposition = false;
        s.setCharset(StandardCharsets.UTF_8);
        return s;
    }

    /** A stream over a raw byte source registered by a library (sockets, pipes, in-memory). */
    static PrologStream forRawInput(int id, InputStream in) {
        PrologStream s = new PrologStream(id, true);
        s.rawIn = in;
        s.reposition = false;
        s.setCharset(StandardCharsets.UTF_8);
        s.bytes = ByteBuffer.allocate(BUF);
        s.bytes.limit(0);
        return s;
    }

    /** A stream over a raw byte sink registered by a library. */
    static PrologStream forRawOutput(int id, OutputStream out) {
        PrologStream s = new PrologStream(id, false);
        s.mode = "append";
        s.counter = new Counter(out);
        s.rawOut = s.counter;
        s.printOut = new PrintStream(s.counter, true);
        s.reposition = false;
        s.setCharset(StandardCharsets.UTF_8);
        return s;
    }

    // START_CHANGE: ISS-2025-0552 - wave P2.13: in UTF-8 and US-ASCII a byte below 0x80 IS its
    // code point, and in ISO-8859-1 every byte is; decodeOne() answers those without a
    // CharsetDecoder call (which cost a decode round-trip through a one-char buffer per character:
    // get_char/2 over 1.6 MB took 3 s). Both decoders are stateless between whole characters, and
    // decodeOne() only ever stops between whole characters, so skipping the decoder is exact.
    /** 0: always decode; 1: bytes < 0x80 are code points; 2: every byte is its code point. */
    private int byteFast;
    /** Test hook: characters that went through the CharsetDecoder (all streams, racy). */
    static long decoderCalls;
    // END_CHANGE: ISS-2025-0552

    void setCharset(Charset cs) {
        this.charset = (cs == null) ? StandardCharsets.UTF_8 : cs;
        this.byteFast = (StandardCharsets.UTF_8.equals(this.charset) || StandardCharsets.US_ASCII.equals(this.charset)) ? 1
            : StandardCharsets.ISO_8859_1.equals(this.charset) ? 2 : 0;                 // ISS-2025-0552
        this.decoder = this.charset.newDecoder()
            .onMalformedInput(CodingErrorAction.REPLACE)
            .onUnmappableCharacter(CodingErrorAction.REPLACE);
        this.encoding = Streams.encodingName(this.charset);
    }

    // ------------------------------------------------------------------
    // properties
    // ------------------------------------------------------------------

    public boolean isInput() { return input; }
    public boolean isOutput() { return !input; }
    public String mode() { return mode; }
    public String type() { return type; }
    public String encoding() { return encoding; }
    public String eofAction() { return eofAction; }
    public boolean canReposition() { return reposition && (inChannel != null || outChannel != null); }
    public String fileName() { return fileName; }
    public Set<String> aliases() { return aliases; }
    public boolean isSystemStream() { return systemStream; }
    public boolean isClosed() { return closed; }

    void setType(String t) { this.type = t; }
    void setEofAction(String a) { this.eofAction = a; }

    /** The writable view. Never null for an output stream. */
    public PrintStream out() { return printOut; }

    /** The underlying raw input stream (legacy compatibility). */
    public InputStream rawInput() { return rawIn; }

    /** Swap the raw input (only used to wrap stdin in a PushbackInputStream for peek_char/1). */
    public void replaceRawInput(InputStream in) { this.rawIn = in; }

    /** The underlying raw output stream (legacy compatibility). */
    public OutputStream rawOutput() { return rawOut; }

    // ------------------------------------------------------------------
    // positions
    // ------------------------------------------------------------------

    /** The byte offset of the next character to be read/written. */
    public long bytePosition() {
        if (input) {
            long p = basePos + (bytes == null ? 0 : bytes.position());
            if (pendingCp != NONE) p -= pendingBytes;
            return p;
        }
        if (outChannel != null) {
            try { return outChannel.position(); } catch (IOException e) { return counter == null ? 0 : counter.bytes; }
        }
        return counter == null ? 0 : counter.bytes;
    }

    /** Characters read (or written) so far. */
    public long charCount() { return input ? charCount : (counter == null ? 0 : counter.chars); }

    /** The 1-based line number of the read/write head. */
    public long lineCount() { return input ? lineCount : (counter == null ? 1 : counter.lines); }

    /** The 0-based column of the read/write head. */
    public long linePosition() { return input ? linePos : (counter == null ? 0 : counter.linePos); }

    /** True when the last read hit the end of the stream. */
    public boolean atEndOfStream() {
        if (!input) return false;
        try { return peekCodePoint() < 0; } catch (IOException e) { return true; }
    }

    /** True when a read has already been attempted past the end. */
    public boolean pastEndOfStream() { return pastEof; }

    // ------------------------------------------------------------------
    // reading
    // ------------------------------------------------------------------

    /** Read one code point, or -1 at end of stream. */
    public int getCodePoint() throws IOException {
        if (pendingCp != NONE) {
            int cp = pendingCp;
            pendingCp = NONE;
            if (cp < 0) { pastEof = true; return -1; }
            account(cp);
            return cp;
        }
        int cp = decodeOne();
        if (cp < 0) { pastEof = true; return -1; }
        account(cp);
        return cp;
    }

    /** Look at the next code point without consuming it; -1 at end of stream. */
    public int peekCodePoint() throws IOException {
        if (pendingCp != NONE) return pendingCp;
        long before = basePos + (bytes == null ? 0 : bytes.position());
        int cp = decodeOne();
        long after = basePos + (bytes == null ? 0 : bytes.position());
        pendingBytes = (int) (after - before);
        pendingCp = cp;
        return cp;
    }

    /** Read one raw byte, or -1 at end of stream (binary streams; also valid on text streams). */
    public int getByte() throws IOException {
        if (pendingCp != NONE) {
            // a peeked character invalidates byte-level reading; drop back to its byte offset
            reposition(bytePosition());
        }
        if (bytes == null) return -1;
        if (!bytes.hasRemaining() && !refill()) { pastEof = true; return -1; }
        if (!bytes.hasRemaining()) { pastEof = true; return -1; }
        int b = bytes.get() & 0xFF;
        charCount++;
        return b;
    }

    /** Look at the next raw byte without consuming it. */
    public int peekByte() throws IOException {
        if (pendingCp != NONE) reposition(bytePosition());
        if (bytes == null) return -1;
        if (!bytes.hasRemaining() && !refill()) return -1;
        if (!bytes.hasRemaining()) return -1;
        return bytes.get(bytes.position()) & 0xFF;
    }

    private void account(int cp) {
        charCount++;
        if (cp == '\n') { lineCount++; linePos = 0; } else { linePos++; }
    }

    /**
     * Decode exactly ONE code point, advancing the byte buffer by exactly that character's bytes.
     *
     * <p>The one-character output buffer is the point: a bigger {@code CharBuffer} would let the
     * decoder consume as many bytes as fit, and the extra characters would be dropped while the
     * byte position ran ahead — which is how a first cut of this class turned
     * {@code get_char, get_char} on "foo(bar)" into {@code f} then {@code b}. A supplementary
     * character does not fit in one char, so the decoder answers OVERFLOW without consuming
     * anything; the second attempt uses a two-char buffer and returns the code point.
     */
    private int decodeOne() throws IOException {
        if (bytes == null || flushed) return -1;
        // START_CHANGE: ISS-2025-0552 - the byte fast path (see byteFast)
        if (byteFast != 0 && bytes.hasRemaining()) {
            int b = bytes.get(bytes.position());
            if (b >= 0 || byteFast == 2) { bytes.position(bytes.position() + 1); return b & 0xFF; }
        }
        decoderCalls++;                                                   // test hook
        // END_CHANGE: ISS-2025-0552
        int spins = 0;
        while (true) {
            cb1.clear();
            CoderResult cr = decoder.decode(bytes, cb1, eofSeen);
            if (cb1.position() > 0) { cb1.flip(); return cb1.get(); }
            if (cr.isOverflow()) {
                cb2.clear();
                decoder.decode(bytes, cb2, eofSeen);
                if (cb2.position() >= 2) {
                    cb2.flip();
                    char hi = cb2.get();
                    char lo = cb2.get();
                    return Character.isSurrogatePair(hi, lo) ? Character.toCodePoint(hi, lo) : hi;
                }
                if (cb2.position() == 1) { cb2.flip(); return cb2.get(); }
                if (++spins > 4) return -1;
                continue;
            }
            if (cr.isUnderflow()) {
                if (eofSeen) {
                    flushed = true;
                    cb1.clear();
                    decoder.flush(cb1);
                    if (cb1.position() > 0) { cb1.flip(); return cb1.get(); }
                    return -1;
                }
                refill();
                if (++spins > 4096) return -1;
                continue;
            }
            // REPLACE is configured, so a malformed result should not reach here; skip defensively
            if (bytes.hasRemaining()) bytes.get();
            return 0xFFFD;
        }
    }

    private boolean refill() throws IOException {
        if (eofSeen) return bytes.hasRemaining();
        basePos += bytes.position();
        bytes.compact();
        int n;
        if (inChannel != null) {
            n = inChannel.read(bytes);
        } else if (rawIn != null) {
            byte[] tmp = new byte[bytes.remaining()];
            n = (tmp.length == 0) ? 0 : rawIn.read(tmp);
            if (n > 0) bytes.put(tmp, 0, n);
        } else {
            n = -1;
        }
        bytes.flip();
        if (n < 0) { eofSeen = true; return bytes.hasRemaining(); }
        return true;
    }

    // ------------------------------------------------------------------
    // repositioning — the heart of L-07
    // ------------------------------------------------------------------

    /** The absolute size of a file-backed stream, or -1. */
    public long size() throws IOException {
        if (inChannel != null) return inChannel.size();
        if (outChannel != null) return outChannel.size();
        return -1;
    }

    /**
     * Move the read/write head to the absolute byte offset {@code pos}. On a text input stream the
     * decode buffer is dropped and the decoder reset, so the next {@code get_char/2} really does
     * read from {@code pos}.
     */
    public void reposition(long pos) throws IOException {
        if (input) {
            if (inChannel == null) throw new IOException("stream is not repositionable");
            inChannel.position(pos);
            bytes.clear();
            bytes.limit(0);
            basePos = pos;
            eofSeen = false;
            pastEof = false;
            flushed = false;
            pendingCp = NONE;
            pendingBytes = 0;
            decoder.reset();
            recountTo(pos);
        } else {
            if (outChannel == null) throw new IOException("stream is not repositionable");
            if (printOut != null) printOut.flush();
            outChannel.position(pos);
            if (counter != null) counter.reposition(pos);
        }
    }

    /**
     * Recompute character count, line number and line position for the byte offset {@code pos} by
     * re-decoding the prefix of the file. Bounded: a reposition inside a very large file leaves the
     * counters at their byte-derived approximation rather than costing a full re-scan.
     */
    private void recountTo(long pos) {
        if (pos == 0) { charCount = 0; lineCount = 1; linePos = 0; return; }
        if (inChannel == null || pos > (8L << 20)) { charCount = pos; lineCount = 1; linePos = pos; return; }
        long savedPos;
        try { savedPos = inChannel.position(); } catch (IOException e) { return; }
        long cc = 0, lc = 1, lp = 0;
        try {
            inChannel.position(0);
            ByteBuffer b = ByteBuffer.allocate(BUF);
            CharsetDecoder d = charset.newDecoder()
                .onMalformedInput(CodingErrorAction.REPLACE)
                .onUnmappableCharacter(CodingErrorAction.REPLACE);
            CharBuffer c = CharBuffer.allocate(1);
            long consumed = 0;
            long base = 0;
            b.limit(0);
            boolean eof = false;
            while (consumed < pos) {
                c.clear();
                CoderResult cr = d.decode(b, c, eof);
                if (c.position() > 0) {
                    c.flip();
                    char ch = c.get();
                    cc++;
                    if (ch == '\n') { lc++; lp = 0; } else { lp++; }
                    consumed = base + b.position();
                    continue;
                }
                if (cr.isUnderflow()) {
                    if (eof) break;
                    base += b.position();
                    b.compact();
                    int n = inChannel.read(b);
                    b.flip();
                    if (n < 0) eof = true;
                } else if (cr.isOverflow()) {
                    // c has capacity 1 and we drained it above
                    continue;
                } else {
                    if (b.hasRemaining()) b.get();
                    consumed = base + b.position();
                }
            }
        } catch (IOException e) {
            // leave the best-effort counters
        } finally {
            try { inChannel.position(savedPos); } catch (IOException ignore) { }
        }
        charCount = cc;
        lineCount = lc;
        linePos = lp;
    }

    // ------------------------------------------------------------------
    // writing / closing
    // ------------------------------------------------------------------

    /** Flush anything buffered. */
    public void flush() {
        if (printOut != null) printOut.flush();
        try { if (rawOut != null) rawOut.flush(); } catch (IOException ignore) { }
    }

    void close() {
        if (closed) return;
        closed = true;
        flush();
        try { if (rawIn != null && !systemStream) rawIn.close(); } catch (IOException ignore) { }
        try { if (rawOut != null && !systemStream) rawOut.close(); } catch (IOException ignore) { }
    }

    @Override
    public String toString() { return handle; }

    /** Byte / character / line counter for output streams. */
    private static final class Counter extends FilterOutputStream {
        long bytes;
        long chars;
        long lines = 1;
        long linePos;

        Counter(OutputStream out) { super(out); }

        void reposition(long pos) { bytes = pos; }

        @Override
        public void write(int b) throws IOException {
            out.write(b);
            bytes++;
            if ((b & 0xC0) != 0x80) chars++;          // count UTF-8 lead bytes only
            if (b == '\n') { lines++; linePos = 0; } else if ((b & 0xC0) != 0x80) linePos++;
        }

        @Override
        public void write(byte[] b, int off, int len) throws IOException {
            out.write(b, off, len);
            for (int i = 0; i < len; i++) {
                int v = b[off + i] & 0xFF;
                bytes++;
                if ((v & 0xC0) != 0x80) chars++;
                if (v == '\n') { lines++; linePos = 0; } else if ((v & 0xC0) != 0x80) linePos++;
            }
        }
    }
}
// END_CHANGE: ISS-2025-0472
