package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.v4.Errors;
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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0473 - engine v4 wave W7 (design B.11): the stream-introspection and
// stream-configuration predicates the design asks for. They all read the per-engine stream table
// and the per-stream decoder counters, which is what makes them meaningful on a text stream.
/**
 * The stream introspection / configuration family:
 * <ul>
 *   <li>{@code set_stream(+Stream, +Property)} — {@code alias(A)}, {@code type(T)},
 *       {@code eof_action(A)}, {@code encoding(E)};</li>
 *   <li>{@code stream_position_data(+Field, +Position, ?Data)} — {@code char_count},
 *       {@code line_count}, {@code line_position}, {@code byte_count};</li>
 *   <li>{@code character_count/2}, {@code line_count/2}, {@code line_position/2};</li>
 *   <li>{@code current_stream(?File, ?Mode, ?Stream)}.</li>
 * </ul>
 */
public class StreamInfo implements BuiltIn {

    /** Which predicate this instance implements. */
    public enum Kind {
        SET_STREAM("set_stream/2"),
        POSITION_DATA("stream_position_data/3"),
        CHARACTER_COUNT("character_count/2"),
        LINE_COUNT("line_count/2"),
        LINE_POSITION("line_position/2"),
        CURRENT_STREAM("current_stream/3");

        private final String pi;
        Kind(String pi) { this.pi = pi; }
        public String indicator() { return pi; }
    }

    private final Kind kind;

    public StreamInfo(Kind kind) { this.kind = kind; }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        switch (kind) {
            case SET_STREAM: return setStream(args, bindings, solutions);
            case POSITION_DATA: return positionData(args, bindings, solutions);
            case CHARACTER_COUNT: return counter(args, bindings, solutions, 'c');
            case LINE_COUNT: return counter(args, bindings, solutions, 'l');
            case LINE_POSITION: return counter(args, bindings, solutions, 'p');
            case CURRENT_STREAM: return currentStream(args, bindings, solutions);
            default: return false;
        }
    }

    private boolean setStream(List<Term> args, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        require(args, 2);
        PrologStream s = resolve(args.get(0), bindings, "set_stream/2");
        Term prop = args.get(1).resolveBindings(bindings);
        if (prop instanceof Variable) {
            throw new PrologException(ISOErrorTerms.instantiationError("set_stream/2"));
        }
        if (!(prop instanceof CompoundTerm) || prop.getArguments() == null || prop.getArguments().size() != 1) {
            throw new PrologException(ISOErrorTerms.domainError("stream_property", prop, "set_stream/2"));
        }
        Term v = prop.getArguments().get(0).resolveBindings(bindings);
        String value = (v instanceof Atom) ? ((Atom) v).getName() : null;
        String name = prop.getName();
        if ("alias".equals(name)) {
            if (value == null) throw new PrologException(ISOErrorTerms.typeError("atom", v, "set_stream/2"));
            StreamManager.streams().addAlias(s, value);
        } else if ("type".equals(name) || "eof_action".equals(name) || "encoding".equals(name)) {
            if (value == null) throw new PrologException(ISOErrorTerms.typeError("atom", v, "set_stream/2"));
            Streams.setProperty(s, name, value);
        } else {
            throw new PrologException(ISOErrorTerms.domainError("stream_property", prop, "set_stream/2"));
        }
        solutions.add(bindings);
        return true;
    }

    private boolean positionData(List<Term> args, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        require(args, 3);
        Term field = args.get(0).resolveBindings(bindings);
        Term pos = args.get(1).resolveBindings(bindings);
        Term data = args.get(2);
        if (field instanceof Variable) {
            throw new PrologException(ISOErrorTerms.instantiationError("stream_position_data/3"));
        }
        if (!(pos instanceof CompoundTerm) || !"$stream_position".equals(pos.getName())
                || pos.getArguments() == null || pos.getArguments().size() != 4) {
            throw new PrologException(ISOErrorTerms.domainError("stream_position", pos, "stream_position_data/3"));
        }
        String f = (field instanceof Atom) ? ((Atom) field).getName() : "";
        Term value;
        if ("char_count".equals(f)) value = pos.getArguments().get(0);
        else if ("line_count".equals(f)) value = pos.getArguments().get(1);
        else if ("line_position".equals(f)) value = pos.getArguments().get(2);
        else if ("byte_count".equals(f)) value = pos.getArguments().get(3);
        else throw new PrologException(ISOErrorTerms.domainError("stream_position_field", field, "stream_position_data/3"));
        Map<String, Term> nb = new HashMap<>(bindings);
        if (data.resolveBindings(bindings).unify(value, nb)) {
            solutions.add(nb);
            return true;
        }
        return false;
    }

    private boolean counter(List<Term> args, Map<String, Term> bindings,
                            List<Map<String, Term>> solutions, char which) {
        require(args, 2);
        PrologStream s = resolve(args.get(0), bindings, kind.indicator());
        long v = (which == 'c') ? s.charCount() : (which == 'l') ? s.lineCount() : s.linePosition();
        Map<String, Term> nb = new HashMap<>(bindings);
        if (args.get(1).resolveBindings(bindings).unify(new Number(v), nb)) {
            solutions.add(nb);
            return true;
        }
        return false;
    }

    private boolean currentStream(List<Term> args, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        require(args, 3);
        Term fileT = args.get(0);
        Term modeT = args.get(1);
        Term streamT = args.get(2);
        Term boundStream = streamT.resolveBindings(bindings);
        PrologStream only = (boundStream instanceof Variable) ? null : StreamManager.stream(boundStream);
        boolean found = false;
        for (PrologStream s : StreamManager.streams().all()) {
            if (s.fileName() == null) continue;             // SWI: only file streams
            if (only != null && only != s) continue;
            Map<String, Term> nb = new HashMap<>(bindings);
            if (!fileT.resolveBindings(bindings).unify(new Atom(s.fileName()), nb)) continue;
            if (!modeT.resolveBindings(bindings).unify(new Atom(s.mode()), nb)) continue;
            if (only == null && !boundStream.unify(Streams.termFor(s), nb)) continue;
            solutions.add(nb);
            found = true;
        }
        return found;
    }

    private static void require(List<Term> args, int n) {
        if (args == null || args.size() != n) {
            throw Errors.existence("procedure", Errors.pi("stream_info", args == null ? 0 : args.size()), "stream_info");   // ISS-2025-0697
        }
    }

    private static PrologStream resolve(Term t, Map<String, Term> bindings, String ctx) {
        Term st = t.resolveBindings(bindings);
        if (st instanceof Variable) throw new PrologException(ISOErrorTerms.instantiationError(ctx));
        PrologStream s = StreamManager.stream(st);
        if (s == null) throw new PrologException(ISOErrorTerms.existenceError("stream", st, ctx));
        return s;
    }
}
// END_CHANGE: ISS-2025-0473
