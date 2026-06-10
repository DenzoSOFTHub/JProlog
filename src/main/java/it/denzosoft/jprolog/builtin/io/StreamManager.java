package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.*;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Manages I/O streams for ISO Prolog compliance.
 * Handles standard streams and file-based streams.
 */
public class StreamManager {
    // START_CHANGE: ISS-2025-0258 - These static maps are shared across the solver thread, the
    // debug solver thread, and HTTP/TCP handler threads. Plain HashMap mutation from multiple
    // threads can corrupt the table; use ConcurrentHashMap (matching the other resource managers).
    private static final Map<String, InputStream> INPUT_STREAMS = new java.util.concurrent.ConcurrentHashMap<>();
    private static final Map<String, OutputStream> OUTPUT_STREAMS = new java.util.concurrent.ConcurrentHashMap<>();
    private static final AtomicInteger STREAM_COUNTER = new AtomicInteger(1000);
    // START_CHANGE: R3 - per-stream properties (type/encoding/eof_action)
    public static final String PROP_TYPE = "type";          // "text" | "binary"
    public static final String PROP_ENCODING = "encoding";  // e.g. "utf8" / "iso_latin_1"
    public static final String PROP_EOF_ACTION = "eof_action"; // "error" | "eof_code" | "reset"
    private static final Map<String, Map<String, String>> STREAM_PROPS = new java.util.concurrent.ConcurrentHashMap<>();
    private static final Map<String, java.io.Reader> READERS = new java.util.concurrent.ConcurrentHashMap<>();
    // END_CHANGE: ISS-2025-0258

    public static void setProperty(String alias, String prop, String value) {
        STREAM_PROPS.computeIfAbsent(alias, k -> new HashMap<>()).put(prop, value);
    }
    public static String getProperty(String alias, String prop) {
        Map<String, String> p = STREAM_PROPS.get(alias);
        return (p != null) ? p.get(prop) : null;
    }
    public static java.io.Reader getReader(String alias) {
        java.io.Reader r = READERS.get(alias);
        if (r != null) return r;
        InputStream is = INPUT_STREAMS.get(alias);
        if (is == null) return null;
        String enc = getProperty(alias, PROP_ENCODING);
        java.nio.charset.Charset cs = resolveCharset(enc);
        // START_CHANGE: ISS-2025-0376 - wrap in a PushbackReader so peek_char/2 and peek_code/2 can
        // look ahead without consuming; get_char/2 reads through the same reader, keeping peek+get
        // on the same stream consistent. Buffer of 2 chars covers surrogate pairs.
        java.io.Reader nr = new java.io.PushbackReader(new java.io.InputStreamReader(is, cs), 2);
        // END_CHANGE: ISS-2025-0376
        READERS.put(alias, nr);
        return nr;
    }
    private static java.nio.charset.Charset resolveCharset(String enc) {
        if (enc == null) return java.nio.charset.StandardCharsets.UTF_8;
        String s = enc.toLowerCase();
        switch (s) {
            case "utf8": case "utf-8": return java.nio.charset.StandardCharsets.UTF_8;
            case "ascii": return java.nio.charset.StandardCharsets.US_ASCII;
            case "iso_latin_1": case "latin1": case "iso-8859-1": return java.nio.charset.StandardCharsets.ISO_8859_1;
            case "utf16": case "utf-16": return java.nio.charset.StandardCharsets.UTF_16;
            default:
                try { return java.nio.charset.Charset.forName(enc); } catch (Exception e) {
                    return java.nio.charset.StandardCharsets.UTF_8;
                }
        }
    }
    // END_CHANGE: R3
    
    private static String currentInputStream = "user_input";
    private static String currentOutputStream = "user_output";

    // START_CHANGE: ISS-2025-0327 - per-thread output override. Lets a caller (e.g. the IDE) capture
    // write/1 output for ONE thread without a process-wide System.setOut (which garbles other threads).
    private static final ThreadLocal<java.io.PrintStream> THREAD_OUTPUT = new ThreadLocal<>();

    /** Redirect this thread's output to {@code ps} (null clears it). */
    public static void setThreadLocalOutput(java.io.PrintStream ps) {
        if (ps == null) THREAD_OUTPUT.remove(); else THREAD_OUTPUT.set(ps);
    }

    /**
     * The effective output stream for the calling thread: a thread-local override if set, otherwise the
     * current user output (read live from {@code System.out} so test/console redirections are honoured),
     * otherwise the explicitly-selected stream. Output built-ins must write here instead of System.out.
     */
    public static java.io.PrintStream out() {
        java.io.PrintStream tl = THREAD_OUTPUT.get();
        if (tl != null) return tl;
        if (currentOutputStream == null || "user_output".equals(currentOutputStream)) return System.out;
        // START_CHANGE: ISS-2025-0375 - openStream stores a raw FileOutputStream, which the previous
        // "instanceof PrintStream" check rejected, silently falling back to System.out and making
        // set_output/1 a no-op. Wrap non-PrintStream output streams in a cached PrintStream instead.
        OutputStream s = OUTPUT_STREAMS.get(currentOutputStream);
        if (s == null) return System.out;
        if (s instanceof java.io.PrintStream) return (java.io.PrintStream) s;
        return printWrapper(currentOutputStream, s);
        // END_CHANGE: ISS-2025-0375
    }
    // END_CHANGE: ISS-2025-0327

    // START_CHANGE: ISS-2025-0375 - cached PrintStream wrappers around raw file OutputStreams.
    // OUTPUT_STREAMS keeps the raw FileOutputStream (seek/4, stream positioning and reposition
    // detection rely on "instanceof FileOutputStream"); writers obtain a per-alias PrintStream view.
    private static final Map<String, java.io.PrintStream> PRINT_WRAPPERS = new java.util.concurrent.ConcurrentHashMap<>();

    private static java.io.PrintStream printWrapper(String alias, OutputStream os) {
        java.io.PrintStream cached = PRINT_WRAPPERS.get(alias);
        if (cached != null) return cached;
        java.io.PrintStream ps = new java.io.PrintStream(os, true);
        PRINT_WRAPPERS.put(alias, ps);
        return ps;
    }
    // END_CHANGE: ISS-2025-0375

    // START_CHANGE: ISS-2025-0373 - shared stream-argument dispatch for the output predicates
    // (write/2, writeln/2, nl/1, put_char/2, tab/2, write_term/3, print/2, format/3).
    /**
     * Resolve a stream alias to a writable PrintStream.
     * {@code current_output} (or null) follows the current output stream; {@code user_output} and
     * {@code user_error} map to the live System streams (honouring the thread-local override);
     * any other alias is looked up in the open output streams.
     *
     * @param alias the stream alias, or null for the current output
     * @return the PrintStream, or null if the alias names no open output stream
     */
    public static java.io.PrintStream resolveOutput(String alias) {
        if (alias == null || "current_output".equals(alias)) return out();
        if ("user_output".equals(alias)) {
            java.io.PrintStream tl = THREAD_OUTPUT.get();
            return (tl != null) ? tl : System.out;
        }
        if ("user_error".equals(alias)) return System.err;
        OutputStream s = OUTPUT_STREAMS.get(alias);
        if (s == null) return null;
        if (s instanceof java.io.PrintStream) return (java.io.PrintStream) s;
        return printWrapper(alias, s);
    }
    // END_CHANGE: ISS-2025-0373

    static {
        // Initialize standard streams
        INPUT_STREAMS.put("user_input", System.in);
        OUTPUT_STREAMS.put("user_output", System.out);
        OUTPUT_STREAMS.put("user_error", System.err);
    }
    
    /**
     * Open a file stream for reading or writing.
     */
    public static String openStream(String filename, String mode) throws IOException {
        String streamAlias = "stream_" + STREAM_COUNTER.incrementAndGet();
        
        // START_CHANGE: ISS-2025-0188 - Close stream on exception to prevent resource leak
        switch (mode.toLowerCase()) {
            case "read": {
                FileInputStream fis = new FileInputStream(filename);
                INPUT_STREAMS.put(streamAlias, fis);
                break;
            }
            case "write": {
                FileOutputStream fos = new FileOutputStream(filename);
                OUTPUT_STREAMS.put(streamAlias, fos);
                break;
            }
            case "append": {
                FileOutputStream aos = new FileOutputStream(filename, true);
                OUTPUT_STREAMS.put(streamAlias, aos);
                break;
            }
            default:
                throw new IllegalArgumentException("Invalid stream mode: " + mode);
        }
        // END_CHANGE: ISS-2025-0188
        
        return streamAlias;
    }
    
    // START_CHANGE: ISS-2025-0252 - alias support: map user-friendly name to underlying stream
    public static void aliasStream(String existingAlias, String userAlias) {
        InputStream is = INPUT_STREAMS.get(existingAlias);
        if (is != null) {
            INPUT_STREAMS.put(userAlias, is);
            return;
        }
        OutputStream os = OUTPUT_STREAMS.get(existingAlias);
        if (os != null) {
            OUTPUT_STREAMS.put(userAlias, os);
        }
    }
    // END_CHANGE: ISS-2025-0252

    /**
     * Close a stream by alias.
     */
    public static boolean closeStream(String streamAlias) {
        // START_CHANGE: ISS-2025-0305 - remove EVERY alias referencing the same underlying stream
        // (open/4 with alias(A) registers both stream_N and A; closing one left the other dangling,
        // pointing at a now-closed stream). Find all aliases for the target object and drop them all.
        InputStream tis = INPUT_STREAMS.get(streamAlias);
        OutputStream tos = OUTPUT_STREAMS.get(streamAlias);
        java.util.List<String> aliases = new java.util.ArrayList<>();
        aliases.add(streamAlias);
        if (tis != null) {
            for (Map.Entry<String, InputStream> e : INPUT_STREAMS.entrySet()) {
                if (e.getValue() == tis && !aliases.contains(e.getKey())) aliases.add(e.getKey());
            }
        }
        if (tos != null) {
            for (Map.Entry<String, OutputStream> e : OUTPUT_STREAMS.entrySet()) {
                if (e.getValue() == tos && !aliases.contains(e.getKey())) aliases.add(e.getKey());
            }
        }
        // invalidate cached Reader + per-stream properties for every alias (ISS-2025-0287)
        for (String a : aliases) {
            java.io.Reader cachedReader = READERS.remove(a);
            if (cachedReader != null) { try { cachedReader.close(); } catch (IOException ignore) {} }
            STREAM_PROPS.remove(a);
            INPUT_STREAMS.remove(a);
            OUTPUT_STREAMS.remove(a);
            // START_CHANGE: ISS-2025-0375 - flush and drop the cached PrintStream wrapper
            java.io.PrintStream wrapper = PRINT_WRAPPERS.remove(a);
            if (wrapper != null) wrapper.flush();
            // END_CHANGE: ISS-2025-0375
        }
        // START_CHANGE: ISS-2025-0375 - closing the current output/input stream reverts to the user streams
        if (tos != null && aliases.contains(currentOutputStream)) {
            currentOutputStream = "user_output";
        }
        if (tis != null && aliases.contains(currentInputStream)) {
            currentInputStream = "user_input";
        }
        // END_CHANGE: ISS-2025-0375
        boolean closed = false;
        try {
            if (tis != null && tis != System.in) { tis.close(); closed = true; }
            if (tos != null && tos != System.out && tos != System.err) { tos.close(); closed = true; }
        } catch (IOException e) {
            return false;
        }
        return closed;
        // END_CHANGE: ISS-2025-0305
    }
    
    /**
     * Get input stream by alias.
     */
    public static InputStream getInputStream(String streamAlias) {
        return INPUT_STREAMS.get(streamAlias);
    }

    // START_CHANGE: ISS-2025-0193 - Allow replacing stream with wrapped version (e.g. PushbackInputStream)
    public static void registerInputStream(String streamAlias, InputStream stream) {
        INPUT_STREAMS.put(streamAlias, stream);
    }
    // END_CHANGE: ISS-2025-0193

    // START_CHANGE: Round5 final - allow temp replace of output stream (for portray hook capture)
    public static void setOutputStreamRaw(String streamAlias, OutputStream stream) {
        OUTPUT_STREAMS.put(streamAlias, stream);
        // START_CHANGE: ISS-2025-0375 - drop any stale PrintStream wrapper over the replaced stream
        PRINT_WRAPPERS.remove(streamAlias);
        // END_CHANGE: ISS-2025-0375
    }
    // END_CHANGE: Round5 final
    
    /**
     * Get output stream by alias.
     */
    public static OutputStream getOutputStream(String streamAlias) {
        return OUTPUT_STREAMS.get(streamAlias);
    }
    
    /**
     * Set current input stream.
     */
    public static void setCurrentInput(String streamAlias) {
        if (INPUT_STREAMS.containsKey(streamAlias)) {
            currentInputStream = streamAlias;
        }
    }
    
    /**
     * Set current output stream.
     */
    public static void setCurrentOutput(String streamAlias) {
        if (OUTPUT_STREAMS.containsKey(streamAlias)) {
            currentOutputStream = streamAlias;
        }
    }
    
    /**
     * Get current input stream alias.
     */
    public static String getCurrentInput() {
        return currentInputStream;
    }
    
    /**
     * Get current output stream alias.
     */
    public static String getCurrentOutput() {
        return currentOutputStream;
    }
    
    /**
     * Check if stream exists.
     */
    public static boolean hasStream(String streamAlias) {
        return INPUT_STREAMS.containsKey(streamAlias) || OUTPUT_STREAMS.containsKey(streamAlias);
    }

    // START_CHANGE: LIM-007 - Stream Repositioning support
    /**
     * Check if a stream supports repositioning.
     * Only file-backed streams (FileInputStream/FileOutputStream) support repositioning.
     *
     * @param streamAlias the stream alias to check
     * @return true if the stream supports repositioning
     */
    public static boolean supportsReposition(String streamAlias) {
        // Standard streams do not support repositioning
        if ("user_input".equals(streamAlias) || "user_output".equals(streamAlias)
                || "user_error".equals(streamAlias)) {
            return false;
        }
        InputStream is = INPUT_STREAMS.get(streamAlias);
        if (is instanceof FileInputStream) {
            return true;
        }
        OutputStream os = OUTPUT_STREAMS.get(streamAlias);
        if (os instanceof FileOutputStream) {
            return true;
        }
        return false;
    }
    // END_CHANGE: LIM-007
}