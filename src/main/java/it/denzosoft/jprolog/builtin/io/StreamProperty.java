package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.PrologStream;
import it.denzosoft.jprolog.core.engine.v4.Streams;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * stream_property/2 - stream_property(?Stream, ?Property), ISO 8.11.8.
 *
 * <p>START_CHANGE: ISS-2025-0473 - wave W7 (design B.11): the complete property set —
 * {@code file_name}, {@code mode}, {@code input}, {@code output}, {@code alias}, {@code position},
 * {@code end_of_stream}, {@code eof_action}, {@code reposition}, {@code type}, {@code encoding},
 * {@code line_count} — over every stream of the CURRENT ENGINE. It used to report six hard-coded
 * properties for the three standard streams plus whatever the (process-global) alias tables held.
 */
public class StreamProperty implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("stream_property/2 requires exactly 2 arguments");
        }

        Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term propertyTerm = query.getArguments().get(1).resolveBindings(bindings);

        // START_CHANGE: ISS-2025-0605 - P4.12: ISO 8.11.8.3 (c): a bound Property that is not a
        // stream property is domain_error(stream_property, P), not a silent failure.
        if (!(propertyTerm instanceof Variable) && !isStreamProperty(propertyTerm)) {
            throw new PrologException(
                ISOErrorTerms.domainError("stream_property", propertyTerm, "stream_property/2"));
        }
        // END_CHANGE: ISS-2025-0605
        List<PrologStream> candidates = new ArrayList<>();
        boolean bindStream = streamTerm instanceof Variable;
        if (bindStream) {
            candidates.addAll(StreamManager.streams().all());
        } else {
            PrologStream s = StreamManager.stream(streamTerm);
            if (s == null) {
                throw new PrologException(
                    ISOErrorTerms.existenceError("stream", streamTerm, "stream_property/2"));
            }
            candidates.add(s);
        }

        boolean found = false;
        for (PrologStream s : candidates) {
            // START_CHANGE: ISS-2025-0473 - only BIND the stream argument when it was unbound.
            // A bound argument has already selected the stream (it may be an alias atom such as
            // `myin`, which of course does not unify with the canonical '$stream'(N) term).
            Term streamValue = Streams.termFor(s);
            for (Term property : propertiesOf(s)) {
                Map<String, Term> nb = new HashMap<>(bindings);
                if (bindStream && !streamTerm.unify(streamValue, nb)) continue;
                if (!propertyTerm.unify(property, nb)) continue;
                solutions.add(nb);
                found = true;
            }
            // END_CHANGE: ISS-2025-0473
        }
        return found;
    }

    // START_CHANGE: ISS-2025-0605 - the ISO properties plus the SWI ones a program may ask about
    private static final java.util.Set<String> UNARY_PROPERTIES = new java.util.HashSet<>(java.util.Arrays.asList(
        "file_name", "mode", "alias", "position", "end_of_stream", "eof_action", "reposition",
        "type", "encoding", "line_count", "buffer", "buffer_size", "bom", "close_on_abort",
        "newline", "representation_errors", "timeout", "tty", "file_no", "locale", "nlink",
        "write_errors", "close_on_exec"));

    private static boolean isStreamProperty(Term p) {
        if (p instanceof Atom) return "input".equals(((Atom) p).getName()) || "output".equals(((Atom) p).getName());
        return p instanceof it.denzosoft.jprolog.core.terms.CompoundTerm
            && p.getArguments().size() == 1 && UNARY_PROPERTIES.contains(p.getName());
    }
    // END_CHANGE: ISS-2025-0605

    /** Every property of {@code s}, in ISO order. */
    public static List<Term> propertiesOf(PrologStream s) {
        List<Term> out = new ArrayList<>();
        if (s.fileName() != null) out.add(one("file_name", new Atom(s.fileName())));
        out.add(one("mode", new Atom(s.mode())));
        out.add(new Atom(s.isInput() ? "input" : "output"));
        for (String a : s.aliases()) out.add(one("alias", new Atom(a)));
        out.add(one("position", positionTerm(s)));
        if (s.isInput()) out.add(one("end_of_stream", new Atom(endOfStream(s))));
        out.add(one("eof_action", new Atom(s.eofAction())));
        out.add(one("reposition", new Atom(s.canReposition() ? "true" : "false")));
        out.add(one("type", new Atom(s.type())));
        out.add(new Atom(s.type()));                       // legacy bare `text` / `binary`
        out.add(one("encoding", new Atom(s.encoding())));
        out.add(one("line_count", new Number(s.lineCount())));
        return out;
    }

    /** The opaque position term: {@code '$stream_position'(CharCount, LineCount, LinePos, ByteCount)}. */
    public static Term positionTerm(PrologStream s) {
        return new CompoundTerm(new Atom("$stream_position"), Arrays.<Term>asList(
            new Number(s.charCount()), new Number(s.lineCount()),
            new Number(s.linePosition()), new Number(s.bytePosition())));
    }

    // START_CHANGE: ISS-2025-0489 - enumerating stream properties must never BLOCK.
    // `at_end_of_stream/0,1` may legitimately wait for input; `stream_property/2` may not — with an
    // unbound first argument it walks EVERY open stream, and on an interactive `user_input` the
    // one-character lookahead behind `atEndOfStream()` blocks until the user types something. That
    // made the documented `stream_property(S, alias(user_error))` hang forever, and no inference
    // budget or Stop interrupt could break it because the wait is inside a bridged built-in.
    // A stream that cannot be repositioned (stdin, a socket, a pipe) is reported as `not` unless a
    // read has already run past its end; a file is peeked as before.
    private static String endOfStream(PrologStream s) {
        if (s.pastEndOfStream()) return "past";
        if (!s.canReposition()) return "not";
        return s.atEndOfStream() ? "at" : "not";
    }
    // END_CHANGE: ISS-2025-0489

    private static Term one(String name, Term arg) {
        return new CompoundTerm(new Atom(name), Arrays.asList(arg));
    }
}
