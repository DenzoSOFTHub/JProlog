package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

// START_CHANGE: ISS-2025-0472 - engine v4 wave W7, design B.11: the stream table of ONE engine.
/**
 * The stream table of one engine (limit <b>L-06</b>: it used to be a set of statics on
 * {@code builtin.io.StreamManager}, shared by every {@code Prolog} in the JVM — a stream opened by
 * engine A was visible to engine B, and an alias registered by one leaked into all of them).
 *
 * <p>Reached from the ~400 legacy built-ins through the unchanged {@code StreamManager} static
 * facade, which delegates to the engine current on the calling thread exactly as
 * {@code PrologFlags} has since ISS-2025-0437 — so this works on the legacy and v2 engines too,
 * which design decision 1 (B.17) requires for one more release.
 *
 * <p>{@code current_input}/{@code current_output} are per <b>thread</b> within an engine, and the
 * IDE's per-thread output capture ({@link #setThreadLocalOutput}) is a process-wide thread-local
 * that wins over both — that is the contract {@code RunPanel} and {@code DebugPanel} rely on.
 */
public final class Streams {

    /** The stream table of the engine current on this thread. */
    public static Streams current() { return EngineState.current().streams(); }

    // The handle counter is process-global on purpose: `stream_1001` must denote at most one
    // stream in the whole JVM, or a stale per-alias cache in an older built-in (Read, ReadTerm)
    // would hand engine B the closed file of engine A.
    private static final AtomicInteger counter = new AtomicInteger(1000);
    private final Map<Integer, PrologStream> byId = new ConcurrentHashMap<Integer, PrologStream>();
    private final Map<String, PrologStream> byAlias = new ConcurrentHashMap<String, PrologStream>();

    private final PrologStream userInput;
    private final PrologStream userOutput;
    private final PrologStream userError;

    private final ThreadLocal<PrologStream> currentInput = new ThreadLocal<PrologStream>();
    private final ThreadLocal<PrologStream> currentOutput = new ThreadLocal<PrologStream>();

    // START_CHANGE: ISS-2025-0327 (moved here from StreamManager) - per-thread output override.
    // Process-wide by design: the IDE installs it around a background solve, before any engine is
    // current on that thread, and it must win over every engine's current_output.
    private static final ThreadLocal<PrintStream> THREAD_OUTPUT = new ThreadLocal<PrintStream>();

    /** Redirect this thread's output to {@code ps} (null clears it). */
    public static void setThreadLocalOutput(PrintStream ps) {
        if (ps == null) THREAD_OUTPUT.remove(); else THREAD_OUTPUT.set(ps);
        // START_CHANGE: ISS-2025-0606 - P4.13: remember WHICH current output the override
        // replaces; an explicit set_output/1 to another stream afterwards wins over it.
        if (ps == null) {
            THREAD_OUTPUT_BASE.remove();
        } else {
            PrologStream base = null;
            try { base = current().currentOutput(); } catch (RuntimeException e) { base = null; }
            if (base == null) THREAD_OUTPUT_BASE.remove(); else THREAD_OUTPUT_BASE.set(base);
        }
        // END_CHANGE: ISS-2025-0606
    }

    // START_CHANGE: ISS-2025-0606 - the current output the thread-local override stands for
    private static final ThreadLocal<PrologStream> THREAD_OUTPUT_BASE = new ThreadLocal<PrologStream>();
    // END_CHANGE: ISS-2025-0606

    /** The thread-local output override, or null when none is installed. */
    public static PrintStream threadLocalOutput() { return THREAD_OUTPUT.get(); }
    // END_CHANGE: ISS-2025-0327

    public Streams() {
        userInput = PrologStream.forSystemInput(0, System.in);
        userInput.aliases.add("user_input");
        userOutput = PrologStream.forSystemOutput(1, System.out);
        userOutput.aliases.add("user_output");
        userError = PrologStream.forSystemOutput(2, System.err);
        userError.aliases.add("user_error");
        register(userInput);
        register(userOutput);
        register(userError);
    }

    private void register(PrologStream s) {
        byId.put(Integer.valueOf(s.id), s);
        byAlias.put(s.handle, s);
        for (String a : s.aliases) byAlias.put(a, s);
    }

    public PrologStream userInput() { return userInput; }
    public PrologStream userOutput() { return userOutput; }
    public PrologStream userError() { return userError; }

    /** Every open stream of this engine. */
    public List<PrologStream> all() {
        List<PrologStream> out = new ArrayList<PrologStream>(byId.values());
        Collections.sort(out, new java.util.Comparator<PrologStream>() {
            public int compare(PrologStream a, PrologStream b) { return Integer.compare(a.id, b.id); }
        });
        return out;
    }

    // ------------------------------------------------------------------
    // opening / closing
    // ------------------------------------------------------------------

    /**
     * Open {@code file} in {@code mode}, honouring the ISO {@code open/4} options.
     *
     * @param alias      an extra alias to register, or null
     * @param type       {@code text} or {@code binary}, or null for the default
     * @param encoding   an encoding name, or null for UTF-8
     * @param eofAction  {@code error}, {@code eof_code} or {@code reset}, or null
     * @param reposition {@code Boolean.FALSE} to refuse repositioning, or null for the default
     */
    public PrologStream open(String file, String mode, String alias, String type,
                             String encoding, String eofAction, Boolean reposition) throws IOException {
        int id = counter.incrementAndGet();
        Charset cs = resolveCharset(encoding);
        PrologStream s;
        if ("read".equals(mode)) {
            s = PrologStream.forFileRead(id, file, cs);
        } else if ("write".equals(mode)) {
            s = PrologStream.forFileWrite(id, file, false, cs);
        } else if ("append".equals(mode)) {
            s = PrologStream.forFileWrite(id, file, true, cs);
        } else {
            throw new IllegalArgumentException("Invalid stream mode: " + mode);
        }
        if (type != null) s.setType(type);
        if (eofAction != null) s.setEofAction(eofAction);
        if (reposition != null) s.reposition = reposition.booleanValue();
        if (alias != null) s.aliases.add(alias);
        register(s);
        return s;
    }

    /** Register a stream over a raw byte source that some library opened (sockets, pipes). */
    public PrologStream registerInput(String name, InputStream in) {
        PrologStream s = PrologStream.forRawInput(counter.incrementAndGet(), in);
        if (name != null) s.aliases.add(name);
        register(s);
        return s;
    }

    /** Register a stream over a raw byte sink that some library opened. */
    public PrologStream registerOutput(String name, OutputStream out) {
        PrologStream s = PrologStream.forRawOutput(counter.incrementAndGet(), out);
        if (name != null) s.aliases.add(name);
        register(s);
        return s;
    }

    /** Add {@code alias} as another name for {@code s}. */
    public void addAlias(PrologStream s, String alias) {
        if (s == null || alias == null) return;
        s.aliases.add(alias);
        byAlias.put(alias, s);
    }

    /** Close {@code s} and drop every alias that referenced it. Returns false for a system stream. */
    public boolean close(PrologStream s) {
        if (s == null) return false;
        if (s.systemStream) return false;
        s.close();
        byId.remove(Integer.valueOf(s.id));
        List<String> drop = new ArrayList<String>();
        for (Map.Entry<String, PrologStream> e : byAlias.entrySet()) {
            if (e.getValue() == s) drop.add(e.getKey());
        }
        for (String k : drop) byAlias.remove(k);
        if (currentInput.get() == s) currentInput.remove();
        if (currentOutput.get() == s) currentOutput.remove();
        return true;
    }

    // ------------------------------------------------------------------
    // lookup
    // ------------------------------------------------------------------

    /** The stream named by an alias or by its {@code stream_<id>} handle, or null. */
    public PrologStream byName(String name) {
        if (name == null) return null;
        if ("current_input".equals(name)) return currentInput();
        if ("current_output".equals(name)) return currentOutput();
        return byAlias.get(name);
    }

    /** The stream identified by {@code id}, or null. */
    public PrologStream byId(int id) { return byId.get(Integer.valueOf(id)); }

    /**
     * The stream a Prolog term denotes: {@code '$stream'(N)}, the legacy {@code stream(Alias)}
     * wrapper, or a plain atom alias / handle. Returns null when the term names no open stream.
     */
    public PrologStream byTerm(Term t) {
        if (t == null) return null;
        if (t instanceof Atom) return byName(((Atom) t).getName());
        if (t instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) t;
            List<Term> as = c.getArguments();
            if (as != null && as.size() == 1) {
                Term inner = as.get(0);
                if ("$stream".equals(c.getName()) && inner instanceof Number) {
                    return byId((int) ((Number) inner).longValue());
                }
                if ("stream".equals(c.getName()) && inner instanceof Atom) {
                    return byName(((Atom) inner).getName());
                }
            }
        }
        return null;
    }

    /** The canonical Prolog term for {@code s}: {@code '$stream'(N)}. */
    public static Term termFor(PrologStream s) {
        return new CompoundTerm(new Atom("$stream"), Arrays.<Term>asList(new Number((long) s.id)));
    }

    /**
     * The name the legacy string-keyed API uses for {@code s}: its first user alias if it has one,
     * otherwise its {@code stream_<id>} handle.
     */
    public static String nameOf(PrologStream s) {
        for (String a : s.aliases) return a;
        return s.handle;
    }

    // ------------------------------------------------------------------
    // current input / output (per thread, within this engine)
    // ------------------------------------------------------------------

    public PrologStream currentInput() {
        PrologStream s = currentInput.get();
        return (s == null || s.closed) ? userInput : s;
    }

    public PrologStream currentOutput() {
        PrologStream s = currentOutput.get();
        return (s == null || s.closed) ? userOutput : s;
    }

    public void setCurrentInput(PrologStream s) {
        if (s == null || s == userInput) currentInput.remove(); else currentInput.set(s);
    }

    public void setCurrentOutput(PrologStream s) {
        if (s == null || s == userOutput) currentOutput.remove(); else currentOutput.set(s);
    }

    /**
     * The effective output for the calling thread: the thread-local override if one is installed,
     * otherwise the current output stream. {@code user_output} resolves to {@code System.out} live,
     * so a test or console redirection of {@code System.out} is honoured.
     */
    public PrintStream out() {
        PrintStream tl = THREAD_OUTPUT.get();
        PrologStream s = currentOutput();
        // START_CHANGE: ISS-2025-0606 - P4.13: the override captures the current output it was
        // installed over (and user_output); a set_output/1 to a file inside the captured goal
        // redirects to that file, as in SWI (with_output_to/2 only replaces current_output).
        if (tl != null) {
            if (s == userOutput) return tl;
            PrologStream base = THREAD_OUTPUT_BASE.get();
            if (base == null || base == s) return tl;
        }
        // END_CHANGE: ISS-2025-0606
        if (s == userOutput) return sysOut();   // ISS-2025-0714
        if (s == userError) return sysErr();   // ISS-2025-0714
        PrintStream ps = s.out();
        return (ps != null) ? ps : sysOut();   // ISS-2025-0714
    }

    /**
     * Resolve a stream name to a writable {@link PrintStream}, or null when it names no open
     * output stream. {@code current_output} (or null) follows the current output;
     * {@code user_output}/{@code user_error} map to the live system streams (honouring the
     * thread-local override).
     */
    public PrintStream resolveOutput(String name) {
        if (name == null || "current_output".equals(name)) return out();
        if ("user_output".equals(name)) {
            PrintStream tl = THREAD_OUTPUT.get();
            return (tl != null) ? tl : sysOut();   // ISS-2025-0714
        }
        if ("user_error".equals(name)) return sysErr();   // ISS-2025-0714
        PrologStream s = byAlias.get(name);
        if (s == null || s.input) return null;
        return s.out();
    }

    // START_CHANGE: ISS-2025-0714 - wave Q2.6: the live System.out/System.err, each wrapped ONCE in
    // a ColumnPrintStream (re-wrapped when a test or the console swaps the system stream), so the
    // console has a column for line_position/2 and format/2's column stops.
    private static PrintStream sysOutRaw, sysErrRaw;
    private static ColumnPrintStream sysOutCol, sysErrCol;

    static PrintStream sysOut() {
        PrintStream cur = System.out;
        if (cur instanceof ColumnPrintStream) return cur;
        synchronized (Streams.class) {
            if (cur != sysOutRaw || sysOutCol == null) { sysOutRaw = cur; sysOutCol = new ColumnPrintStream(cur, false); }
            return sysOutCol;
        }
    }

    static PrintStream sysErr() {
        PrintStream cur = System.err;
        if (cur instanceof ColumnPrintStream) return cur;
        synchronized (Streams.class) {
            if (cur != sysErrRaw || sysErrCol == null) { sysErrRaw = cur; sysErrCol = new ColumnPrintStream(cur, false); }
            return sysErrCol;
        }
    }

    /** The column {@code ps} has reached, or -1 when it does not track one. */
    public static long columnOf(PrintStream ps) {
        return (ps instanceof ColumnPrintStream) ? ((ColumnPrintStream) ps).column() : -1;
    }

    /** line_position/2 of user_output (id 1, honouring the capture) and user_error (id 2). */
    static long systemColumn(int id) {
        PrintStream ps;
        if (id == 2) ps = sysErr();
        else {
            PrintStream tl = THREAD_OUTPUT.get();
            ps = (tl != null) ? tl : sysOut();
        }
        long c = columnOf(ps);
        return c < 0 ? 0 : c;
    }
    // END_CHANGE: ISS-2025-0714

    /** The {@link PrintStream} to write {@code s} through, honouring the thread-local override. */
    public PrintStream writerFor(PrologStream s) {
        if (s == userOutput) {
            PrintStream tl = THREAD_OUTPUT.get();
            return (tl != null) ? tl : sysOut();   // ISS-2025-0714
        }
        if (s == userError) return sysErr();   // ISS-2025-0714
        return s.out();
    }

    // ------------------------------------------------------------------
    // helpers
    // ------------------------------------------------------------------

    /**
     * Set one of the historical string-keyed properties ({@code type}, {@code encoding},
     * {@code eof_action}) on a stream. Backs {@code set_stream/2} and the legacy accessors.
     */
    public static void setProperty(PrologStream s, String prop, String value) {
        if (s == null || prop == null) return;
        if ("type".equals(prop)) s.setType(value);
        else if ("encoding".equals(prop)) s.setCharset(resolveCharset(value));
        else if ("eof_action".equals(prop)) s.setEofAction(value);
        else if ("alias".equals(prop)) s.aliases.add(value);
    }

    /** Read one of the historical string-keyed properties, or null. */
    public static String getProperty(PrologStream s, String prop) {
        if (s == null || prop == null) return null;
        if ("type".equals(prop)) return s.type();
        if ("encoding".equals(prop)) return s.encoding();
        if ("eof_action".equals(prop)) return s.eofAction();
        return null;
    }

    /** Map an ISO/SWI encoding name to a {@link Charset}; unknown names fall back to UTF-8. */
    public static Charset resolveCharset(String enc) {
        if (enc == null) return StandardCharsets.UTF_8;
        String s = enc.toLowerCase();
        if ("utf8".equals(s) || "utf-8".equals(s)) return StandardCharsets.UTF_8;
        if ("ascii".equals(s) || "us-ascii".equals(s)) return StandardCharsets.US_ASCII;
        if ("iso_latin_1".equals(s) || "latin1".equals(s) || "iso-8859-1".equals(s)) return StandardCharsets.ISO_8859_1;
        if ("utf16".equals(s) || "utf-16".equals(s)) return StandardCharsets.UTF_16;
        if ("octet".equals(s) || "binary".equals(s)) return StandardCharsets.ISO_8859_1;
        try { return Charset.forName(enc); } catch (Exception e) { return StandardCharsets.UTF_8; }
    }

    /** The ISO/SWI name of a {@link Charset}. */
    public static String encodingName(Charset cs) {
        if (cs == null) return "utf8";
        String n = cs.name();
        if ("UTF-8".equals(n)) return "utf8";
        if ("US-ASCII".equals(n)) return "ascii";
        if ("ISO-8859-1".equals(n)) return "iso_latin_1";
        if (n.startsWith("UTF-16")) return "utf16";
        return n.toLowerCase();
    }
}
// END_CHANGE: ISS-2025-0472
