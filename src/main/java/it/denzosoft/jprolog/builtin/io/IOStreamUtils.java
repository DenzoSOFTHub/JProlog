package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.io.PrintStream;
import java.util.Map;

// START_CHANGE: ISS-2025-0373 - shared stream-argument resolution for the I/O built-ins
/**
 * Helpers shared by the stream-argument forms of the I/O built-ins
 * (write/2, writeln/2, nl/1, put_char/2, tab/2, write_term/3, print/2, format/3).
 */
public final class IOStreamUtils {

    private IOStreamUtils() {
    }

    /**
     * Extract the stream alias from a stream argument: either an atom alias
     * (e.g. {@code stream_1001}, {@code user_output}) or a {@code stream(Alias)} wrapper.
     *
     * @param streamTerm the resolved stream argument
     * @return the alias, or null if the term has no recognizable alias
     */
    public static String streamAlias(Term streamTerm) {
        if (streamTerm instanceof Atom) {
            return ((Atom) streamTerm).getName();
        }
        if (streamTerm instanceof CompoundTerm
                && streamTerm.getArguments() != null && streamTerm.getArguments().size() == 1) {
            Term inner = streamTerm.getArguments().get(0);
            if ("stream".equals(streamTerm.getName()) && inner instanceof Atom) {
                return ((Atom) inner).getName();
            }
            // START_CHANGE: ISS-2025-0472 - wave W7: '$stream'(N) is the canonical stream term
            if ("$stream".equals(streamTerm.getName())) {
                it.denzosoft.jprolog.core.engine.v4.PrologStream s = StreamManager.stream(streamTerm);
                if (s != null) return it.denzosoft.jprolog.core.engine.v4.Streams.nameOf(s);
            }
            // END_CHANGE: ISS-2025-0472
        }
        return null;
    }

    /**
     * Resolve a stream argument to a writable {@link PrintStream}, raising the ISO errors
     * mandated for stream arguments (8.14.2.3): instantiation_error for an unbound argument,
     * domain_error(stream_or_alias, S) for a non-stream term, existence_error(stream, S)
     * for an alias that names no open output stream.
     *
     * @param streamArg the raw stream argument (will be resolved against bindings)
     * @param bindings  current variable bindings
     * @param context   predicate indicator for the error context (e.g. "write/2")
     * @return the resolved PrintStream (never null)
     */
    public static PrintStream resolveOutputStream(Term streamArg, Map<String, Term> bindings, String context) {
        Term streamTerm = streamArg.resolveBindings(bindings);
        if (streamTerm instanceof Variable) {
            throw new PrologException(ISOErrorTerms.instantiationError(context));
        }
        // START_CHANGE: ISS-2025-0472 - resolve through the engine's stream table, so the
        // thread-local capture and the per-engine isolation both apply.
        it.denzosoft.jprolog.core.engine.v4.Streams st = StreamManager.streams();
        it.denzosoft.jprolog.core.engine.v4.PrologStream s = st.byTerm(streamTerm);
        if (s != null) {
            if (!s.isOutput()) {
                throw new PrologException(ISOErrorTerms.permissionError("output", "stream", streamTerm, context));
            }
            return st.writerFor(s);
        }
        String alias = streamAlias(streamTerm);
        if (alias == null) {
            if (isStreamHandle(streamTerm)) {                 // ISS-2025-0605: a closed stream
                throw new PrologException(ISOErrorTerms.existenceError("stream", streamTerm, context));
            }
            throw new PrologException(ISOErrorTerms.domainError("stream_or_alias", streamTerm, context));
        }
        PrintStream out = StreamManager.resolveOutput(alias);
        if (out == null) {
            throw new PrologException(ISOErrorTerms.existenceError("stream", streamTerm, context));
        }
        return out;
        // END_CHANGE: ISS-2025-0472
    }

    // START_CHANGE: ISS-2025-0472 - engine v4 wave W7: resolve a stream argument to the engine's
    // own PrologStream. The character/byte I/O built-ins read and write through the stream's own
    // decoder, so peek, get, seek and the position properties all agree (limit L-07).
    /**
     * Resolve a stream argument to an <b>input</b> stream of the current engine, raising the ISO
     * errors of 8.11.7: instantiation_error, domain_error(stream_or_alias, S),
     * existence_error(stream, S), permission_error(input, stream, S).
     *
     * @param streamArg the raw stream argument (null selects the current input)
     */
    public static it.denzosoft.jprolog.core.engine.v4.PrologStream inputStream(
            Term streamArg, Map<String, Term> bindings, String context) {
        it.denzosoft.jprolog.core.engine.v4.Streams st = StreamManager.streams();
        if (streamArg == null) return st.currentInput();
        Term t = streamArg.resolveBindings(bindings);
        if (t instanceof Variable) throw new PrologException(ISOErrorTerms.instantiationError(context));
        it.denzosoft.jprolog.core.engine.v4.PrologStream s = st.byTerm(t);
        if (s == null) {
            if (streamAlias(t) == null && !isStreamHandle(t)) {   // ISS-2025-0605: closed -> existence
                throw new PrologException(ISOErrorTerms.domainError("stream_or_alias", t, context));
            }
            throw new PrologException(ISOErrorTerms.existenceError("stream", t, context));
        }
        if (!s.isInput()) {
            throw new PrologException(ISOErrorTerms.permissionError("input", "stream", t, context));
        }
        return s;
    }

    /**
     * Resolve a stream argument to an <b>output</b> stream of the current engine, raising the ISO
     * errors of 8.11.7 (see {@link #inputStream}).
     *
     * @param streamArg the raw stream argument (null selects the current output)
     */
    public static it.denzosoft.jprolog.core.engine.v4.PrologStream outputStream(
            Term streamArg, Map<String, Term> bindings, String context) {
        it.denzosoft.jprolog.core.engine.v4.Streams st = StreamManager.streams();
        if (streamArg == null) return st.currentOutput();
        Term t = streamArg.resolveBindings(bindings);
        if (t instanceof Variable) throw new PrologException(ISOErrorTerms.instantiationError(context));
        it.denzosoft.jprolog.core.engine.v4.PrologStream s = st.byTerm(t);
        if (s == null) {
            if (streamAlias(t) == null && !isStreamHandle(t)) {   // ISS-2025-0605: closed -> existence
                throw new PrologException(ISOErrorTerms.domainError("stream_or_alias", t, context));
            }
            throw new PrologException(ISOErrorTerms.existenceError("stream", t, context));
        }
        if (!s.isOutput()) {
            throw new PrologException(ISOErrorTerms.permissionError("output", "stream", t, context));
        }
        return s;
    }

    /**
     * The ISO reaction to reading past the end of a stream: {@code eof_action(error)} raises
     * permission_error(input, past_end_of_stream, S), the other actions return the eof value.
     */
    public static void checkPastEof(it.denzosoft.jprolog.core.engine.v4.PrologStream s, String context) {
        // START_CHANGE: ISS-2025-0605 - P4.12: ISO 7.10.2.9 / 8.13.1: the FIRST read at the end
        // returns end_of_file and moves the stream to end_of_stream(past); only a read attempted
        // while the stream is already past raises. The check moved BEFORE the read
        // (beforeRead); this post-read hook is kept for source compatibility and does nothing.
        // END_CHANGE: ISS-2025-0605
    }

    // START_CHANGE: ISS-2025-0605 - P4.12: the checks every input built-in makes before reading.
    /**
     * Raise permission_error(input, past_end_of_stream, S) when the stream is already past its
     * end and its eof_action is {@code error}; S is the stream TERM ('$stream'(N)), not its
     * internal handle name.
     */
    public static void beforeRead(it.denzosoft.jprolog.core.engine.v4.PrologStream s, String context) {
        if (s.pastEndOfStream() && "error".equals(s.eofAction())) {
            throw new PrologException(ISOErrorTerms.permissionError(
                "input", "past_end_of_stream", it.denzosoft.jprolog.core.engine.v4.Streams.termFor(s), context));
        }
    }

    /**
     * ISO 8.12 / 8.13: a character built-in on a binary stream is permission_error(input,
     * binary_stream, S); a byte built-in on a text stream is permission_error(input, text_stream,
     * S). The interactive user_input is exempt (it serves both).
     */
    public static void checkStreamType(it.denzosoft.jprolog.core.engine.v4.PrologStream s, boolean bytes,
                                       String direction, String context) {
        if (s == null || s.isSystemStream()) return;
        boolean binary = "binary".equals(s.type());
        if (bytes && !binary) {
            throw new PrologException(ISOErrorTerms.permissionError(
                direction, "text_stream", it.denzosoft.jprolog.core.engine.v4.Streams.termFor(s), context));
        }
        if (!bytes && binary) {
            throw new PrologException(ISOErrorTerms.permissionError(
                direction, "binary_stream", it.denzosoft.jprolog.core.engine.v4.Streams.termFor(s), context));
        }
    }

    /** A {@code '$stream'(N)} term that names no open stream is existence_error(stream, S). */
    private static boolean isStreamHandle(Term t) {
        return t instanceof CompoundTerm && "$stream".equals(t.getName())
            && t.getArguments() != null && t.getArguments().size() == 1;
    }
    // END_CHANGE: ISS-2025-0605

    /**
     * True when {@code t} looks like a stream argument rather than a term to write / a variable to
     * read into: the canonical {@code '$stream'(N)}, the legacy {@code stream(A)} wrapper, one of
     * the reserved aliases, or an atom that names an open stream of this engine.
     */
    public static boolean isStreamTerm(Term t) {
        if (t instanceof Atom) {
            String n = ((Atom) t).getName();
            if ("current_input".equals(n) || "current_output".equals(n)
                    || "user_input".equals(n) || "user_output".equals(n) || "user_error".equals(n)) {
                return true;
            }
            return StreamManager.hasStream(n);
        }
        if (t instanceof CompoundTerm && t.getArguments() != null && t.getArguments().size() == 1) {
            return "$stream".equals(t.getName()) || "stream".equals(t.getName());
        }
        return false;
    }

    /** True when {@code s} is the engine's {@code user_input} (the interactive stdin path). */
    public static boolean isStdin(it.denzosoft.jprolog.core.engine.v4.PrologStream s) {
        return s == StreamManager.streams().userInput();
    }
    // END_CHANGE: ISS-2025-0472
}
// END_CHANGE: ISS-2025-0373
