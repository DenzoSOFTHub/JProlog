package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.v4.PrologStream;
import it.denzosoft.jprolog.core.engine.v4.Streams;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.Reader;

// START_CHANGE: ISS-2025-0472 - engine v4 wave W7 (design B.11): StreamManager is now a THIN STATIC
// FACADE over the per-engine {@link Streams} table. It used to own the stream state itself — two
// static maps of open streams, a static property map, a static reader cache and a static
// PrintStream-wrapper cache — so every {@code Prolog} in the JVM shared one set of streams and one
// set of aliases (limit L-06), and text reads went through a PushbackReader over the raw file,
// which is what made seek/4 and set_stream_position/2 ineffective (limit L-07).
//
// The signatures are unchanged, so the ~30 legacy I/O built-ins (and any embedder code) keep
// compiling and keep working; every call now routes to the engine current on the calling thread,
// exactly as PrologFlags has since ISS-2025-0437.
/**
 * Static facade over the stream table of the engine current on the calling thread.
 *
 * <p>New code should prefer {@link Streams} and {@link PrologStream} directly; this class exists so
 * the legacy built-ins and the two older engines keep working unchanged for the release in which
 * v2 is still selectable (design decision 1, B.17).
 */
public class StreamManager {

    /** Property names of the historical per-stream property map. */
    public static final String PROP_TYPE = "type";
    public static final String PROP_ENCODING = "encoding";
    public static final String PROP_EOF_ACTION = "eof_action";

    private StreamManager() { }

    /** The stream table of the engine current on this thread. */
    public static Streams streams() { return Streams.current(); }

    // ------------------------------------------------------------------
    // thread-local output capture (the IDE contract — unchanged semantics)
    // ------------------------------------------------------------------

    /** Redirect this thread's output to {@code ps} (null clears it). */
    public static void setThreadLocalOutput(PrintStream ps) { Streams.setThreadLocalOutput(ps); }

    /** The thread-local output override, or null when none is installed. */
    public static PrintStream threadLocalOutput() { return Streams.threadLocalOutput(); }

    /**
     * The effective output stream for the calling thread: a thread-local override if set, otherwise
     * the current output stream ({@code user_output} resolves to {@code System.out} live, so test
     * and console redirections are honoured). Output built-ins must write here, never to
     * {@code System.out}.
     */
    public static PrintStream out() { return Streams.current().out(); }

    /** Resolve a stream alias to a writable PrintStream, or null when it names no output stream. */
    public static PrintStream resolveOutput(String alias) { return Streams.current().resolveOutput(alias); }

    // ------------------------------------------------------------------
    // opening / closing
    // ------------------------------------------------------------------

    /** Open a file stream and return its handle. */
    public static String openStream(String filename, String mode) throws IOException {
        PrologStream s = Streams.current().open(filename, mode == null ? null : mode.toLowerCase(),
            null, null, null, null, null);
        return s.handle;
    }

    /** Register {@code userAlias} as another name for the stream currently named {@code existingAlias}. */
    public static void aliasStream(String existingAlias, String userAlias) {
        Streams st = Streams.current();
        PrologStream s = st.byName(existingAlias);
        if (s != null) st.addAlias(s, userAlias);
    }

    /** Close the stream named by {@code streamAlias}; false for an unknown or system stream. */
    public static boolean closeStream(String streamAlias) {
        Streams st = Streams.current();
        PrologStream s = st.byName(streamAlias);
        return s != null && st.close(s);
    }

    // ------------------------------------------------------------------
    // lookup
    // ------------------------------------------------------------------

    /** The stream named by an alias/handle, or null. */
    public static PrologStream stream(String alias) { return Streams.current().byName(alias); }

    /** The stream a Prolog term denotes ({@code '$stream'(N)}, {@code stream(A)} or an atom alias). */
    public static PrologStream stream(Term t) { return Streams.current().byTerm(t); }

    /** True when {@code streamAlias} names an open stream of this engine. */
    public static boolean hasStream(String streamAlias) { return Streams.current().byName(streamAlias) != null; }

    /** True when the named stream can be repositioned. */
    public static boolean supportsReposition(String streamAlias) {
        PrologStream s = Streams.current().byName(streamAlias);
        return s != null && s.canReposition();
    }

    /** The raw input stream behind an alias, or null (legacy compatibility). */
    public static InputStream getInputStream(String streamAlias) {
        PrologStream s = Streams.current().byName(streamAlias);
        return (s == null || !s.isInput()) ? null : s.rawInput();
    }

    /** The raw output stream behind an alias, or null (legacy compatibility). */
    public static OutputStream getOutputStream(String streamAlias) {
        PrologStream s = Streams.current().byName(streamAlias);
        return (s == null || !s.isOutput()) ? null : s.rawOutput();
    }

    /** Register an externally-created byte source under {@code streamAlias}. */
    public static void registerInputStream(String streamAlias, InputStream stream) {
        Streams st = Streams.current();
        PrologStream existing = st.byName(streamAlias);
        if (existing != null && existing.rawInput() == stream) return;
        st.registerInput(streamAlias, stream);
    }

    /** Register an externally-created byte sink under {@code streamAlias}. */
    public static void setOutputStreamRaw(String streamAlias, OutputStream stream) {
        Streams.current().registerOutput(streamAlias, stream);
    }

    /**
     * A character reader over the named input stream, sharing the stream's decoder — so a read
     * through this reader advances the same character/line counters as {@code get_char/2} and is
     * invalidated by a reposition, which is exactly what limit L-07 was about.
     */
    public static Reader getReader(String alias) {
        PrologStream s = Streams.current().byName(alias);
        return (s == null || !s.isInput()) ? null : reader(s);
    }

    /** A character reader over {@code s} (see {@link #getReader(String)}). */
    public static Reader reader(final PrologStream s) {
        return new Reader() {
            @Override
            public int read() throws IOException {
                int cp = s.getCodePoint();
                if (cp < 0) return -1;
                return (cp <= 0xFFFF) ? cp : Character.highSurrogate(cp);
            }

            @Override
            public int read(char[] cbuf, int off, int len) throws IOException {
                if (len == 0) return 0;
                int c = read();
                if (c < 0) return -1;
                cbuf[off] = (char) c;
                return 1;
            }

            @Override
            public void close() { /* the stream owns its channel */ }
        };
    }

    // ------------------------------------------------------------------
    // per-stream properties (the historical string-keyed accessors)
    // ------------------------------------------------------------------

    public static void setProperty(String alias, String prop, String value) {
        PrologStream s = Streams.current().byName(alias);
        if (s == null) return;
        Streams.setProperty(s, prop, value);
    }

    public static String getProperty(String alias, String prop) {
        PrologStream s = Streams.current().byName(alias);
        if (s == null) return null;
        return Streams.getProperty(s, prop);
    }

    // ------------------------------------------------------------------
    // current input / output
    // ------------------------------------------------------------------

    public static void setCurrentInput(String streamAlias) {
        Streams st = Streams.current();
        PrologStream s = st.byName(streamAlias);
        if (s != null && s.isInput()) st.setCurrentInput(s);
    }

    public static void setCurrentOutput(String streamAlias) {
        Streams st = Streams.current();
        PrologStream s = st.byName(streamAlias);
        if (s != null && s.isOutput()) st.setCurrentOutput(s);
    }

    public static String getCurrentInput() {
        Streams st = Streams.current();
        PrologStream s = st.currentInput();
        return (s == st.userInput()) ? "user_input" : Streams.nameOf(s);
    }

    public static String getCurrentOutput() {
        Streams st = Streams.current();
        PrologStream s = st.currentOutput();
        return (s == st.userOutput()) ? "user_output" : Streams.nameOf(s);
    }
}
// END_CHANGE: ISS-2025-0472
