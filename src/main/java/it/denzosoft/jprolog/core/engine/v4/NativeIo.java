package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.builtin.io.IOStreamUtils;
import it.denzosoft.jprolog.builtin.io.StreamManager;
import it.denzosoft.jprolog.builtin.io.WriteOptions;
import it.denzosoft.jprolog.core.engine.ControlFlow;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0496 - 4.1 wave B: the io family on the v4 SPI (design B.5/B.11/B.12).
/**
 * The output half of the {@code io} family as v4 natives: {@code format/1,2,3}, the whole
 * {@code write/1} family, {@code nl}, {@code tab}, the character I/O predicates and the current
 * input/output accessors.
 *
 * <h3>Why these first</h3>
 * A bridged built-in reaches the machine through {@link LegacyBuiltinAdapter}, whose first act is
 * {@link Unify#resolve} of the WHOLE goal — for {@code write(BigTerm)} and
 * {@code format("~w", [BigTerm])} that is a complete copy of the term before a single character is
 * printed, and it is paid on every call in a printing loop. A native sees the argument cells and
 * hands them straight to {@link Writer}, which derefs as it walks: no resolve, no
 * {@code Map<String,Term>}, no solution list.
 *
 * <h3>What is preserved exactly</h3>
 * Every directive of the legacy {@code builtin.io.Format} (including the column stops
 * {@code ~t}/{@code ~N|}/{@code ~N+}, {@code ~e}/{@code ~f}/{@code ~g}, {@code ~c}, {@code ~Nr},
 * {@code ~*c}, {@code ~i}, {@code ~p} with the {@code portray/1} hook and {@code ~@}), the
 * {@code atom(A)}/{@code string(S)}/{@code codes(C)}/{@code chars(C)} capture sinks, the
 * {@code error(format(Message), format/2)} shape for a bad directive or a missing argument, the
 * {@code type_error(integer, X)} of {@code ~d}/{@code ~D}, and the silent failure modes of
 * {@code tab/1}, {@code put_char/1} and {@code put_code/1}.
 *
 * <h3>Two deliberate corrections</h3>
 * <ul>
 *   <li>{@code writeq/2} used {@code StreamManager.getOutputStream(alias)}, the static map that
 *       captured {@code System.out} at class-load: it therefore escaped the thread-local capture
 *       ({@code with_output_to/2}, the IDE console) that {@code writeq/1} already honoured, and it
 *       raised a non-ISO evaluation error for a non-atom stream. It now resolves its stream exactly
 *       like {@code write/2}, so it reports the ISO stream errors and is captured.</li>
 *   <li>{@code put_code/2} existed only as an arity entry — the legacy class threw
 *       "put_code/1 requires exactly 1 argument" for it. It writes to the given stream now.</li>
 * </ul>
 */
final class NativeIo {

    private NativeIo() {}

    private static final Map<String, Term> NOB = Collections.emptyMap();
    private static final Atom NIL = new Atom("[]");
    private static final Atom DOT = new Atom(".");

    static void register(BuiltinTable t) {
        t.register("write", 1, new WriteB(Kind.WRITE, false));
        t.register("write", 2, new WriteB(Kind.WRITE, true));
        t.register("writeln", 1, new WriteB(Kind.WRITELN, false));
        t.register("writeln", 2, new WriteB(Kind.WRITELN, true));
        t.register("writeq", 1, new WriteB(Kind.WRITEQ, false));
        t.register("writeq", 2, new WriteB(Kind.WRITEQ, true));
        t.register("write_canonical", 1, new WriteB(Kind.CANONICAL, false));
        t.register("write_canonical", 2, new WriteB(Kind.CANONICAL, true));
        t.register("print", 1, new WriteB(Kind.PRINT, false));
        t.register("print", 2, new WriteB(Kind.PRINT, true));
        t.register("write_term", 2, new WriteTermB(2));
        t.register("write_term", 3, new WriteTermB(3));
        t.register("nl", 0, new NlB(false));
        t.register("nl", 1, new NlB(true));
        t.register("tab", 1, new TabB(false));
        t.register("tab", 2, new TabB(true));
        t.register("put_char", 1, new PutCharB(false));
        t.register("put_char", 2, new PutCharB(true));
        t.register("put_code", 1, new PutCodeB(false));
        t.register("put_code", 2, new PutCodeB(true));
        t.register("get_char", 1, new GetB(true, false, false));
        t.register("get_char", 2, new GetB(true, false, true));
        t.register("get_code", 1, new GetB(false, false, false));
        t.register("get_code", 2, new GetB(false, false, true));
        t.register("peek_char", 1, new GetB(true, true, false));
        t.register("peek_char", 2, new GetB(true, true, true));
        t.register("peek_code", 1, new GetB(false, true, false));
        t.register("peek_code", 2, new GetB(false, true, true));
        t.register("flush_output", 0, new FlushB(false));
        t.register("flush_output", 1, new FlushB(true));
        t.register("current_input", 1, new CurrentStreamB(true));
        t.register("current_output", 1, new CurrentStreamB(false));
        t.register("set_input", 1, new SetStreamB(true));
        t.register("set_output", 1, new SetStreamB(false));
        t.register("at_end_of_stream", 0, new AtEofB(false));
        t.register("at_end_of_stream", 1, new AtEofB(true));
        t.register("format", 1, new FormatB(1));
        t.register("format", 2, new FormatB(2));
        t.register("format", 3, new FormatB(3));
        t.register("print_message", 2, new PrintMessageB());          // ISS-2025-0607
    }

    // START_CHANGE: ISS-2025-0607 - P4.14: print_message/2 renders its message SWI-style:
    // format(Format, Args) is formatted, error(Formal, Context) is described, any other term is
    // "Unknown message: T"; error and warning messages go to user_error with the "ERROR: " /
    // "Warning: " prefix on every line, the others to the current output with "% ". The
    // registry version printed format(F, A) as a raw term.
    private static final class PrintMessageB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term kindT = m.deref(args[0]);
            if (kindT instanceof Variable) throw Errors.instantiation("print_message/2");
            String kind = (kindT instanceof Atom) ? ((Atom) kindT).getName()
                        : (kindT instanceof CompoundTerm) ? ((CompoundTerm) kindT).getName() : "informational";
            if ("silent".equals(kind)) return Outcome.SUCCESS;
            Term msg = m.deref(args[1]);
            String body = messageText(m, msg);
            String prefix = "error".equals(kind) ? "ERROR: " : "warning".equals(kind) ? "Warning: " : "% ";
            StringBuilder sb = new StringBuilder();
            String[] lines = body.split("\n", -1);
            for (int i = 0; i < lines.length; i++) {
                if (i == lines.length - 1 && lines[i].isEmpty() && i > 0) break;
                sb.append(prefix).append(lines[i]).append('\n');
            }
            Streams st = StreamManager.streams();
            PrintStream out = ("error".equals(kind) || "warning".equals(kind))
                ? st.writerFor(st.userError()) : StreamManager.out();
            out.print(sb);
            out.flush();
            return Outcome.SUCCESS;
        }

        /** The text of a message term, without the kind prefix. */
        static String messageText(Machine m, Term msg) {
            if (msg instanceof CompoundTerm && "format".equals(((CompoundTerm) msg).getName())
                    && ((CompoundTerm) msg).arity() == 2) {
                String fmt = formatString(((CompoundTerm) msg).arg(0), m);
                if (fmt != null) {
                    return new Fmt(m, "print_message/2").run(fmt, argumentList(((CompoundTerm) msg).arg(1), m));
                }
            }
            if (msg instanceof CompoundTerm && "error".equals(((CompoundTerm) msg).getName())
                    && ((CompoundTerm) msg).arity() == 2) {
                Term formal = m.resolve(((CompoundTerm) msg).arg(0));
                Term ctx = m.deref(((CompoundTerm) msg).arg(1));
                String where = "", extra = "";
                if (ctx instanceof CompoundTerm && "context".equals(((CompoundTerm) ctx).getName())
                        && ((CompoundTerm) ctx).arity() == 2) {
                    Term p = m.deref(((CompoundTerm) ctx).arg(0));
                    Term x = m.deref(((CompoundTerm) ctx).arg(1));
                    if (!(p instanceof Variable)) where = Writer.format(m.resolve(p), Writer.Options.writeq(), 1200) + ": ";
                    if (!(x instanceof Variable)) extra = " (" + Writer.format(m.resolve(x), new Writer.Options(), 1200) + ")";
                    ctx = new Variable();
                } else if (!(ctx instanceof Variable)) {
                    ctx = m.resolve(ctx);
                }
                String s = Writer.message("error", new CompoundTerm(new Atom("error"), new Term[] { formal, ctx }));
                return where + (s.startsWith("ERROR: ") ? s.substring(7) : s) + extra;
            }
            return "Unknown message: " + Writer.format(msg, Writer.Options.writeq(), 1200);
        }
    }
    // END_CHANGE: ISS-2025-0607

    // ------------------------------------------------------------------ shared helpers

    /** The current output, or the stream named by {@code streamArg} (a TERM, never a string). */
    private static PrintStream out(Machine m, Term streamArg, String ctx) {
        if (streamArg == null) return StreamManager.out();
        return IOStreamUtils.resolveOutputStream(m.resolve(streamArg), NOB, ctx);
    }

    private static PrologStream inStream(Machine m, Term streamArg, String ctx) {
        return IOStreamUtils.inputStream(streamArg == null ? null : m.resolve(streamArg), NOB, ctx);
    }

    /** The ONE stdin reader of the engine: get_char/1 and get_code/1 must not buffer separately. */
    private static BufferedReader stdin;

    private static synchronized BufferedReader stdin() {
        if (stdin == null) stdin = new BufferedReader(new InputStreamReader(System.in));
        return stdin;
    }

    /** ISS-2025-0566: the same stdin reader for the term readers (NativeRead). */
    static BufferedReader stdinReader() { return stdin(); }

    // ------------------------------------------------------------------ the write family

    private enum Kind { WRITE, WRITELN, WRITEQ, CANONICAL, PRINT }

    private static final class WriteB implements Builtin {
        private final Kind kind;
        private final boolean streamArg;
        WriteB(Kind kind, boolean streamArg) { this.kind = kind; this.streamArg = streamArg; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            String ctx = name() + "/" + (streamArg ? 2 : 1);
            PrintStream ps = out(m, streamArg ? args[0] : null, ctx);
            Term t = streamArg ? args[1] : args[0];
            Writer.Options o;
            switch (kind) {
                case WRITEQ:    o = Writer.Options.writeq(); break;
                case CANONICAL: o = Writer.Options.canonical(); break;
                // START_CHANGE: ISS-2025-0600 - P4.7 (decision §8): print/1 is portray + writeq
                case PRINT:     o = printOptions(m); break;
                // END_CHANGE: ISS-2025-0600
                default:
                    o = new Writer.Options();               // numbervars(true), ISO write/1
                    break;
            }
            String s = Writer.format(t, o, 1200);
            if (kind == Kind.WRITELN) ps.println(s); else ps.print(s);
            ps.flush();
            return Outcome.SUCCESS;
        }

        private String name() {
            switch (kind) {
                case WRITELN:   return "writeln";
                case WRITEQ:    return "writeq";
                case CANONICAL: return "write_canonical";
                case PRINT:     return "print";
                default:        return "write";
            }
        }
    }

    /** {@code write_term(+Term, +Options)}, the legacy {@code write_term(+Stream, +Term)}, and /3. */
    private static final class WriteTermB implements Builtin {
        private final int arity;
        WriteTermB(int arity) { this.arity = arity; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            String ctx = "write_term/" + arity;
            Term streamTerm;
            Term term;
            Writer.Options o;
            if (arity == 2) {
                Term first = m.resolve(args[0]);
                if (isStream(first)) {                       // legacy write_term(+Stream, +Term)
                    streamTerm = first;
                    term = args[1];
                    o = new Writer.Options();
                    o.numbervars = false;
                } else {
                    streamTerm = null;
                    term = args[0];
                    o = WriteOptions.parse(m.resolve(args[1]), NOB, m.facade(), ctx);
                }
            } else {
                streamTerm = m.resolve(args[0]);
                term = args[1];
                o = WriteOptions.parse(m.resolve(args[2]), NOB, m.facade(), ctx);
            }
            PrintStream ps = (streamTerm == null)
                ? StreamManager.out()
                : IOStreamUtils.resolveOutputStream(streamTerm, NOB, ctx);
            ps.print(Writer.format(term, o, 1200));
            ps.flush();
            return Outcome.SUCCESS;
        }

        private boolean isStream(Term t) {
            if (t instanceof Atom) {
                String n = ((Atom) t).getName();
                return "current_output".equals(n) || "user_output".equals(n) || "user_error".equals(n);
            }
            return IOStreamUtils.isStreamTerm(t);
        }
    }

    private static final class NlB implements Builtin {
        private final boolean streamArg;
        NlB(boolean streamArg) { this.streamArg = streamArg; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            PrintStream ps = out(m, streamArg ? args[0] : null, "nl/1");
            ps.println();
            ps.flush();
            return Outcome.SUCCESS;
        }
    }

    /** {@code tab(+N)} — a non-integer or a negative count FAILS, as the registry version did. */
    private static final class TabB implements Builtin {
        private final boolean streamArg;
        TabB(boolean streamArg) { this.streamArg = streamArg; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            String ctx = streamArg ? "tab/2" : "tab/1";
            PrintStream ps = out(m, streamArg ? args[0] : null, ctx);
            // START_CHANGE: ISS-2025-0604 - P4.11 (decision §8): the count is an arithmetic
            // expression (tab(1+1) prints two spaces; tab(a) is type_error(evaluable, a/0)); a
            // float result is type_error(integer, F); a negative count prints nothing (SWI).
            Number nv = m.evalNum(streamArg ? args[1] : args[0], ctx);
            if (!nv.isInteger()) throw Errors.type("integer", nv, ctx);
            long n = nv.fitsInLong() ? nv.longValue() : (nv.bigIntegerValue().signum() < 0 ? -1 : Long.MAX_VALUE);
            if (n > Integer.MAX_VALUE) throw Errors.resource("memory", ctx);
            StringBuilder sb = new StringBuilder();
            for (long i = 0; i < n; i++) sb.append(' ');
            ps.print(sb);
            // END_CHANGE: ISS-2025-0604
            ps.flush();
            return Outcome.SUCCESS;
        }
    }

    private static final class PutCharB implements Builtin {
        private final boolean streamArg;
        PutCharB(boolean streamArg) { this.streamArg = streamArg; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            String ctx = "put_char/" + (streamArg ? 2 : 1);
            PrintStream ps = out(m, streamArg ? args[0] : null, ctx);
            Term ct = m.deref(streamArg ? args[1] : args[0]);
            // START_CHANGE: ISS-2025-0505 - ISO 8.12.3.3: an unbound character is
            // instantiation_error and anything that is not a one-character atom is
            // type_error(character, C). Both used to fail silently.
            if (ct instanceof Variable) throw Errors.instantiation(ctx);
            if (!(ct instanceof Atom) || ((Atom) ct).getName().length() != 1) {
                throw Errors.type("character", m.resolve(ct), ctx);
            }
            // END_CHANGE: ISS-2025-0505
            String s = ((Atom) ct).getName();
            ps.print(s);
            ps.flush();
            return Outcome.SUCCESS;
        }
    }

    private static final class PutCodeB implements Builtin {
        private final boolean streamArg;
        PutCodeB(boolean streamArg) { this.streamArg = streamArg; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            String ctx = "put_code/" + (streamArg ? 2 : 1);
            PrintStream ps = out(m, streamArg ? args[0] : null, ctx);
            Term ct = m.deref(streamArg ? args[1] : args[0]);
            // START_CHANGE: ISS-2025-0505 - ISO 8.12.3.3 / 7.12.2: instantiation_error,
            // type_error(integer, C), representation_error(character_code).
            if (ct instanceof Variable) throw Errors.instantiation(ctx);
            if (!(ct instanceof Number) || !((Number) ct).isInteger()) {
                throw Errors.type("integer", m.resolve(ct), ctx);
            }
            double v = ((Number) ct).getValue();
            if (v != Math.floor(v) || v < 0 || v > 1114111) {
                throw Errors.representation("character_code", ctx);
            }
            // END_CHANGE: ISS-2025-0505
            ps.print(new String(Character.toChars((int) v)));
            ps.flush();
            return Outcome.SUCCESS;
        }
    }

    /** get_char/1,2, get_code/1,2, peek_char/1,2, peek_code/1,2 over the engine's own decoder. */
    private static final class GetB implements Builtin {
        private final boolean asChar;
        private final boolean peek;
        private final boolean streamArg;
        private final String ctx;                                // ISS-2025-0552: built once
        GetB(boolean asChar, boolean peek, boolean streamArg) {
            this.asChar = asChar; this.peek = peek; this.streamArg = streamArg;
            this.ctx = (peek ? "peek_" : "get_") + (asChar ? "char/" : "code/") + (streamArg ? 2 : 1);
        }

        @Override
        public Outcome call(Machine m, Term[] args) {
            String ctx = this.ctx;
            Term target = args[streamArg ? 1 : 0];
            PrologStream s = inStream(m, streamArg ? args[0] : null, ctx);
            // START_CHANGE: ISS-2025-0605 - P4.12: the ISO argument and stream checks
            Term tgt = m.deref(target);
            if (!(tgt instanceof Variable)) {
                if (asChar) {
                    if (!(tgt instanceof Atom) || !(isOneChar(((Atom) tgt).getName())
                            || "end_of_file".equals(((Atom) tgt).getName()))) {
                        throw Errors.type("in_character", m.resolve(tgt), ctx);
                    }
                } else if (!(tgt instanceof Number) || !((Number) tgt).isInteger()) {
                    throw Errors.type("integer", m.resolve(tgt), ctx);
                } else {
                    long v = ((Number) tgt).longValue();
                    if (v < -1 || v > 0x10FFFF) throw Errors.representation("in_character_code", ctx);
                }
            }
            IOStreamUtils.checkStreamType(s, false, "input", ctx);
            IOStreamUtils.beforeRead(s, ctx);
            // END_CHANGE: ISS-2025-0605
            int cp;
            try {
                if (IOStreamUtils.isStdin(s)) {
                    cp = peek ? peekStdin() : stdin().read();
                } else {
                    cp = peek ? s.peekCodePoint() : s.getCodePoint();
                }
            } catch (IOException e) {
                throw Errors.system("io_error: " + e.getMessage(), ctx);
            }
            Term value;
            if (asChar) {
                value = (cp < 0) ? END_OF_FILE : charAtom(cp);           // ISS-2025-0552
            } else {
                value = Number.valueOf((long) cp);
            }
            return m.unify(target, value) ? Outcome.SUCCESS : Outcome.FAILURE;
        }

        // START_CHANGE: ISS-2025-0552 - atoms are immutable, so the one-character atoms of the
        // Latin-1 range are made once and shared by every get_char/peek_char.
        private static final Atom END_OF_FILE = new Atom("end_of_file");
        private static final Atom[] CHAR_ATOMS = new Atom[256];
        static {
            for (int i = 0; i < CHAR_ATOMS.length; i++) CHAR_ATOMS[i] = new Atom(String.valueOf((char) i));
        }

        static Atom charAtom(int cp) {
            return (cp >= 0 && cp < CHAR_ATOMS.length) ? CHAR_ATOMS[cp] : new Atom(new String(Character.toChars(cp)));
        }
        // END_CHANGE: ISS-2025-0552

        /** stdin keeps the one-byte pushback path of the registry version. */
        private static int peekStdin() throws IOException {
            java.io.InputStream in = StreamManager.streams().userInput().rawInput();
            java.io.PushbackInputStream pb;
            if (in instanceof java.io.PushbackInputStream) {
                pb = (java.io.PushbackInputStream) in;
            } else {
                pb = new java.io.PushbackInputStream(in);
                StreamManager.streams().userInput().replaceRawInput(pb);
            }
            int b = pb.read();
            if (b >= 0) pb.unread(b);
            return b;
        }
    }

    private static final class FlushB implements Builtin {
        private final boolean streamArg;
        FlushB(boolean streamArg) { this.streamArg = streamArg; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            if (!streamArg) {
                StreamManager.out().flush();
            } else {
                PrologStream s = IOStreamUtils.outputStream(m.resolve(args[0]), NOB, "flush_output/1");
                StreamManager.streams().writerFor(s).flush();
                s.flush();
            }
            return Outcome.SUCCESS;
        }
    }

    private static final class CurrentStreamB implements Builtin {
        private final boolean input;
        CurrentStreamB(boolean input) { this.input = input; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            // START_CHANGE: ISS-2025-0505 - ISO 8.11.1.3 / 8.11.2.3: an argument that is neither a
            // variable nor a stream is domain_error(stream, S), not a silent failure.
            Term a0 = m.deref(args[0]);
            if (!(a0 instanceof Variable) && !IOStreamUtils.isStreamTerm(a0)) {
                throw Errors.domain("stream", m.resolve(a0),
                                    input ? "current_input/1" : "current_output/1");
            }
            // END_CHANGE: ISS-2025-0505
            Streams st = StreamManager.streams();
            PrologStream s = input ? st.currentInput() : st.currentOutput();
            Term value = (s == (input ? st.userInput() : st.userOutput()))
                ? new Atom(input ? "user_input" : "user_output")
                : Streams.termFor(s);
            return m.unify(args[0], value) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class SetStreamB implements Builtin {
        private final boolean input;
        SetStreamB(boolean input) { this.input = input; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            Term t = m.resolve(args[0]);
            if (input) {
                StreamManager.streams().setCurrentInput(IOStreamUtils.inputStream(t, NOB, "set_input/1"));
            } else {
                StreamManager.streams().setCurrentOutput(IOStreamUtils.outputStream(t, NOB, "set_output/1"));
            }
            return Outcome.SUCCESS;
        }
    }

    private static final class AtEofB implements Builtin {
        private final boolean streamArg;
        AtEofB(boolean streamArg) { this.streamArg = streamArg; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            PrologStream s = inStream(m, streamArg ? args[0] : null,
                                      "at_end_of_stream/" + (streamArg ? 1 : 0));
            return s.atEndOfStream() ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ format/1,2,3

    private static final class FormatB implements Builtin {
        private final int arity;
        FormatB(int arity) { this.arity = arity; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            Term sink = (arity == 3) ? m.resolve(args[0]) : new Atom("current_output");
            Term fmtT = m.deref(args[arity == 3 ? 1 : 0]);
            Term argsT = (arity == 1) ? NIL : m.deref(args[arity == 3 ? 2 : 1]);

            // START_CHANGE: ISS-2025-0505 - an unbound format string is instantiation_error
            // (ISO 7.12.2 a); every other shape keeps the historical silent failure.
            if (fmtT instanceof Variable) throw Errors.instantiation("format/" + arity);
            // END_CHANGE: ISS-2025-0505
            String fmt = formatString(fmtT, m);
            if (fmt == null) return Outcome.FAILURE;
            List<Term> list = argumentList(argsT, m);

            String output;
            // START_CHANGE: ISS-2025-0595 - P4.6: errors name format/1,2,3; the sinks include the
            // difference lists codes(C, Tail) and chars(C, Tail) (SWI).
            try {
                output = new Fmt(m, "format/" + arity).run(fmt, list);
            } catch (PrologException pe) {
                throw pe;
            } catch (RuntimeException e) {
                ControlFlow.rethrowIfControl(e);
                return Outcome.FAILURE;
            }

            if (sink instanceof CompoundTerm) {
                CompoundTerm sc = (CompoundTerm) sink;
                String f = sc.getName();
                if (sc.arity() == 1) {
                    Term target = sc.arg(0);
                    if ("atom".equals(f))   return m.unify(target, new Atom(output)) ? Outcome.SUCCESS : Outcome.FAILURE;
                    if ("string".equals(f)) return m.unify(target, new PrologString(output)) ? Outcome.SUCCESS : Outcome.FAILURE;
                    if ("codes".equals(f))  return m.unify(target, textToList(output, true)) ? Outcome.SUCCESS : Outcome.FAILURE;
                    if ("chars".equals(f))  return m.unify(target, textToList(output, false)) ? Outcome.SUCCESS : Outcome.FAILURE;
                } else if (sc.arity() == 2 && ("codes".equals(f) || "chars".equals(f))) {
                    return m.unify(sc.arg(0), textToList(output, "codes".equals(f), args[0]))
                        ? Outcome.SUCCESS : Outcome.FAILURE;
                }
            }
            // END_CHANGE: ISS-2025-0595
            PrintStream ps = IOStreamUtils.resolveOutputStream(sink, NOB, "format/3");
            ps.print(output);
            ps.flush();
            return Outcome.SUCCESS;
        }
    }

    private static boolean isOneChar(String s) {                       // ISS-2025-0605
        return !s.isEmpty() && Character.charCount(s.codePointAt(0)) == s.length();
    }

    static Term textToList(String text, boolean codes) {
        return textToList(text, codes, null);
    }

    // START_CHANGE: ISS-2025-0595 - code points (not UTF-16 units); an optional tail for the
    // codes(C, T) / chars(C, T) sinks, whose tail is the sink's second argument.
    private static Term textToList(String text, boolean codes, Term sinkArg) {
        Term list = NIL;
        if (sinkArg != null) {
            Term sk = Unify.deref(sinkArg);
            if (sk instanceof CompoundTerm && ((CompoundTerm) sk).arity() == 2) list = ((CompoundTerm) sk).arg(1);
        }
        int i = text.length();
        while (i > 0) {
            int cp = text.codePointBefore(i);
            i -= Character.charCount(cp);
            Term head = codes ? (Term) Number.valueOf((long) cp)
                              : (Term) new Atom(new String(Character.toChars(cp)));
            list = new CompoundTerm(DOT, new Term[] { head, list });
        }
        return list;
    }
    // END_CHANGE: ISS-2025-0595

    /** An atom, a string or a character/code list; null when the term is none of those. */
    private static String formatString(Term t, Machine m) {
        Term d = Unify.deref(t);
        if (d instanceof Atom) return ((Atom) d).getName();
        if (d instanceof PrologString) return ((PrologString) d).getStringValue();
        if (NativeLibrary.isCons(d)) return charListText(d, m);
        return null;
    }

    /** The text of a list of codes or one-character atoms; stops at the first foreign element. */
    private static String charListText(Term list, Machine m) {
        StringBuilder sb = new StringBuilder();
        Term cur = Unify.deref(list);
        int n = 0;
        while (NativeLibrary.isCons(cur)) {
            Term h = Unify.deref(NativeLibrary.head(cur));
            if (h instanceof Number) {
                sb.append(Character.toChars(((Number) h).getValue().intValue()));
            } else if (h instanceof Atom && ((Atom) h).getName().length() == 1) {
                sb.append(((Atom) h).getName());
            } else {
                break;
            }
            cur = Unify.deref(NativeLibrary.tail(cur));
            if ((++n & 0x3FF) == 0 && m != null) m.guard().step();
        }
        return sb.toString();
    }

    /** ISO/SWI: a list is the argument list, {@code []} is empty, anything else is ONE argument. */
    private static List<Term> argumentList(Term t, Machine m) {
        Term d = Unify.deref(t);
        if (NativeLibrary.isCons(d)) {
            List<Term> out = new ArrayList<Term>();
            Term cur = d;
            int n = 0;
            while (NativeLibrary.isCons(cur)) {
                out.add(NativeLibrary.head(cur));
                cur = Unify.deref(NativeLibrary.tail(cur));
                if ((++n & 0x3FF) == 0 && m != null) m.guard().step();
            }
            return out;
        }
        if (NativeLibrary.isNil(d)) return Collections.emptyList();
        return Collections.singletonList(d);
    }

    // START_CHANGE: ISS-2025-0595 - P4.6: format/2,3 rewritten after SWI-Prolog's pl-fmt.c.
    // Integers are exact (BigInteger) for ~d/~D/~r/~R/~f; ~e/~f/~g follow C printf on the exact
    // binary value (round half even, %g strips trailing zeros); column stops honour fill
    // characters (~`ct, ~Nt) and ~+ defaults to 8 columns; ~Nn, ~Nc, ~*c, ~i, ~W, ~k, ~p (print:
    // portray + writeq) are complete; every argument fault and a surplus or missing argument
    // raises error(format(Message), _) instead of printing a placeholder.
    private static final class Fmt {
        private final Machine m;
        private final String ctx;
        private final StringBuilder out = new StringBuilder();
        private List<Term> args;
        private int ai;
        /** Index in {@code out} where the current column segment starts, and its column. */
        private int segStart;
        private int segCol;
        /** Fill points of the pending segment: {position in out, fill code point}. */
        private final List<int[]> fills = new ArrayList<int[]>();

        Fmt(Machine m, String ctx) { this.m = m; this.ctx = ctx; }

        String run(String fmt, List<Term> arguments) {
            this.args = arguments;
            int n = fmt.length();
            int i = 0;
            while (i < n) {
                int ch = fmt.codePointAt(i);
                i += Character.charCount(ch);
                if (ch != '~') { emit(ch); continue; }
                if (i >= n) throw fmtError("truncated format specification");
                Integer numArg = null;
                int c = fmt.codePointAt(i);
                if (c == '*') {
                    Term a = nextArg();
                    if (!(a instanceof Number) || !((Number) a).isInteger()
                            || ((Number) a).bigIntegerValue().signum() < 0
                            || ((Number) a).bigIntegerValue().bitLength() > 31) {
                        throw fmtError("no or negative integer for `*' argument");
                    }
                    numArg = Integer.valueOf(((Number) a).bigIntegerValue().intValue());
                    i++;
                } else if (c == '`') {
                    i++;
                    if (i >= n) throw fmtError("truncated format specification");
                    int fc = fmt.codePointAt(i);
                    numArg = Integer.valueOf(fc);
                    i += Character.charCount(fc);
                } else if (c >= '0' && c <= '9') {
                    long v = 0;
                    while (i < n && fmt.charAt(i) >= '0' && fmt.charAt(i) <= '9') {
                        v = Math.min(Integer.MAX_VALUE, v * 10 + (fmt.charAt(i) - '0'));
                        i++;
                    }
                    numArg = Integer.valueOf((int) v);
                }
                if (i >= n) throw fmtError("truncated format specification");
                int d = fmt.codePointAt(i);
                i += Character.charCount(d);
                directive(d, numArg);
            }
            if (ai < args.size()) throw fmtError("too many arguments");
            return out.toString();
        }

        // ---------------------------------------------------------------- output + columns

        private void emit(int cp) {
            out.appendCodePoint(cp);
            if (cp == '\n') newLine();
        }

        private void emit(String s) {
            out.append(s);
            int nl = s.lastIndexOf('\n');
            if (nl >= 0) {
                segStart = out.length() - (s.length() - nl - 1);
                segCol = 0;
                fills.clear();
            }
        }

        private void newLine() {
            segStart = out.length();
            segCol = 0;
            fills.clear();
        }

        private int column() {
            return segCol + out.codePointCount(segStart, out.length());
        }

        /** A column stop at {@code target}: pad the pending segment at its fill points. */
        private void columnStop(int target) {
            int cur = column();
            int pad = target - cur;
            if (pad > 0) {
                if (fills.isEmpty()) {
                    for (int k = 0; k < pad; k++) out.append(' ');
                } else {
                    int slots = fills.size();
                    int per = pad / slots;
                    int rem = pad - per * slots;
                    for (int f = slots - 1; f >= 0; f--) {
                        int[] fp = fills.get(f);
                        int count = per + (f == slots - 1 ? rem : 0);
                        StringBuilder sb = new StringBuilder();
                        for (int k = 0; k < count; k++) sb.appendCodePoint(fp[1]);
                        out.insert(fp[0], sb);
                    }
                }
            }
            segCol = Math.max(cur, target);
            segStart = out.length();
            fills.clear();
        }

        // ---------------------------------------------------------------- directives

        private Term nextArg() {
            if (ai >= args.size()) throw fmtError("not enough arguments");
            return Unify.deref(args.get(ai++));
        }

        private void directive(int d, Integer numArg) {
            switch (d) {
                // ~Nw / ~Nq right-align in N columns: a JProlog extension (ISS-2025-0249) SWI
                // does not have (it ignores the argument); kept for compatibility.
                case 'w': emit(padLeft(Writer.format(nextArg(), new Writer.Options(), 1200), numArg)); return;
                case 'q': emit(padLeft(Writer.format(nextArg(), Writer.Options.writeq(), 1200), numArg)); return;
                case 'p': emit(Writer.format(nextArg(), printOptions(m), 1200)); return;
                case 'k': emit(Writer.format(nextArg(), Writer.Options.canonical(), 1200)); return;
                case 'W': {
                    Term t = nextArg();
                    Term opts = nextArg();
                    emit(Writer.format(t, WriteOptions.parse(m.resolve(opts), NOB, m.facade(), ctx), 1200));
                    return;
                }
                case 'a': {
                    Term a = nextArg();
                    if (a instanceof Atom) emit(((Atom) a).getName());
                    else if (a instanceof PrologString) emit(((PrologString) a).getStringValue());
                    else if (a instanceof Number) emit(Writer.format(a, new Writer.Options(), 1200));
                    else throw argError('a', a);
                    return;
                }
                case 'd': case 'D': {
                    Term a = nextArg();
                    if (!(a instanceof Number) || !((Number) a).isInteger()) throw argError((char) d, a);
                    emit(groupedInteger(((Number) a).bigIntegerValue(), numArg == null ? 0 : numArg.intValue(), d == 'D'));
                    return;
                }
                case 'f': case 'e': case 'g': {
                    Term a = nextArg();
                    if (!(a instanceof Number)) throw argError((char) d, a);
                    emit(cFloat((Number) a, numArg == null ? 6 : numArg.intValue(), (char) d));
                    return;
                }
                case 's': {
                    Term a = nextArg();
                    String s = stringArg(a);
                    if (s == null) throw argError('s', a);
                    emit(s);
                    return;
                }
                case 'c': {
                    Term a = nextArg();
                    if (!(a instanceof Number) || !((Number) a).isInteger()
                            || ((Number) a).bigIntegerValue().signum() < 0
                            || ((Number) a).bigIntegerValue().compareTo(java.math.BigInteger.valueOf(0x10FFFF)) > 0) {
                        throw argError('c', a);
                    }
                    int cp = ((Number) a).bigIntegerValue().intValue();
                    int reps = numArg == null ? 1 : numArg.intValue();
                    for (int k = 0; k < reps; k++) emit(cp);
                    return;
                }
                case 'r': case 'R': {
                    Term a = nextArg();
                    if (numArg == null) throw fmtError("~" + (char) d + " requires a radix argument");
                    int base = numArg.intValue();
                    if (base < 2 || base > 36) throw fmtError("radix must be in 2..36 for ~" + (char) d);
                    if (!(a instanceof Number) || !((Number) a).isInteger()) throw argError((char) d, a);
                    String s = ((Number) a).bigIntegerValue().toString(base);
                    emit(d == 'R' ? s.toUpperCase(java.util.Locale.ROOT) : s);
                    return;
                }
                case 'n': {
                    int reps = numArg == null ? 1 : numArg.intValue();
                    for (int k = 0; k < reps; k++) emit('\n');
                    return;
                }
                case '~': emit('~'); return;
                case 'i': nextArg(); return;
                case '@': {
                    if (ai >= args.size()) throw fmtError("not enough arguments");
                    Term goal = args.get(ai++);
                    emit(viaCall(goal));
                    return;
                }
                case 't':
                    fills.add(new int[] { out.length(), numArg == null ? ' ' : numArg.intValue() });
                    return;
                case '|':
                    columnStop(numArg == null ? column() : numArg.intValue());
                    return;
                case '+':
                    columnStop(segCol + (numArg == null ? 8 : numArg.intValue()));
                    return;
                default:
                    throw fmtError("unknown directive: ~" + new String(Character.toChars(d)));
            }
        }

        private static String padLeft(String s, Integer width) {
            if (width == null) return s;
            int len = s.codePointCount(0, s.length());
            if (len >= width.intValue()) return s;
            StringBuilder sb = new StringBuilder();
            for (int k = len; k < width.intValue(); k++) sb.append(' ');
            return sb.append(s).toString();
        }

        /** The text of a code list, a char list or a string (an atom is accepted too). */
        private String stringArg(Term a) {
            if (a instanceof PrologString) return ((PrologString) a).getStringValue();
            if (a instanceof Atom) return NativeLibrary.isNil(a) ? "" : ((Atom) a).getName();
            if (!NativeLibrary.isCons(a)) return null;
            StringBuilder sb = new StringBuilder();
            Term cur = a;
            int k = 0;
            while (NativeLibrary.isCons(cur)) {
                Term h = Unify.deref(NativeLibrary.head(cur));
                if (h instanceof Number && ((Number) h).isInteger()) {
                    long cp = ((Number) h).longValue();
                    if (cp < 0 || cp > 0x10FFFF) return null;
                    sb.appendCodePoint((int) cp);
                } else if (h instanceof Atom && ((Atom) h).getName().codePointCount(0, ((Atom) h).getName().length()) == 1) {
                    sb.append(((Atom) h).getName());
                } else {
                    return null;
                }
                cur = Unify.deref(NativeLibrary.tail(cur));
                if ((++k & 0x3FF) == 0) m.guard().step();
            }
            return NativeLibrary.isNil(cur) ? sb.toString() : null;
        }

        private PrologException fmtError(String message) {
            Term formal = new CompoundTerm(new Atom("format"),
                Collections.singletonList((Term) new Atom(message)));
            return new PrologException(ISOErrorTerms.error(formal, new Atom(ctx)));
        }

        private PrologException argError(char d, Term a) {
            if (a instanceof Variable) return Errors.instantiation(ctx);
            return fmtError("~" + d + " expects " + expects(d) + " argument, found "
                + Writer.format(m.resolve(a), Writer.Options.writeq(), 1200));
        }

        private static String expects(char d) {
            switch (d) {
                case 'a': return "an atomic";
                case 'c': return "a character code";
                case 's': return "a string or a list of codes or characters";
                case 'e': case 'f': case 'g': return "a numeric";
                default: return "an integer";
            }
        }

        /** {@code ~@}: run the goal and splice in everything it printed. */
        private String viaCall(Term goal) {
            java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
            PrintStream capture = new PrintStream(baos, true);
            PrintStream prev = StreamManager.threadLocalOutput();
            try {
                StreamManager.setThreadLocalOutput(capture);
                m.runSubQuery(goal, new Machine.SolutionSink() {
                    @Override public boolean onSolution(Map<String, Term> s) { return false; }
                });
            } finally {
                capture.flush();
                StreamManager.setThreadLocalOutput(prev);
            }
            return baos.toString();
        }
    }

    /** {@code ~Nd} / {@code ~ND}: an exact integer, N digits after an inserted decimal point,
     *  the integer part grouped by thousands for {@code ~D}. */
    static String groupedInteger(java.math.BigInteger v, int decimals, boolean group) {
        boolean neg = v.signum() < 0;
        String digits = v.abs().toString();
        String frac = "";
        if (decimals > 0) {
            StringBuilder sb = new StringBuilder(digits);
            while (sb.length() < decimals + 1) sb.insert(0, '0');
            digits = sb.substring(0, sb.length() - decimals);
            frac = "." + sb.substring(sb.length() - decimals);
        }
        if (group && digits.length() > 3) {
            StringBuilder g = new StringBuilder();
            int lead = digits.length() % 3;
            if (lead > 0) g.append(digits, 0, lead);
            for (int k = lead; k < digits.length(); k += 3) {
                if (g.length() > 0) g.append(',');
                g.append(digits, k, k + 3);
            }
            digits = g.toString();
        }
        return (neg ? "-" : "") + digits + frac;
    }

    /** C printf {@code %.Nf}, {@code %.Ne}, {@code %.Ng} on the exact value of {@code n}. */
    static String cFloat(Number n, int prec, char conv) {
        java.math.BigDecimal x;
        boolean negZero = false;
        if (n.isInteger()) {
            x = new java.math.BigDecimal(n.bigIntegerValue());
        } else {
            double v = n.doubleValue();
            if (Double.isNaN(v)) return "nan";
            if (Double.isInfinite(v)) return v > 0 ? "inf" : "-inf";
            negZero = (v == 0.0 && 1.0 / v < 0);
            x = new java.math.BigDecimal(v);
        }
        String s;
        if (conv == 'f') {
            s = x.setScale(prec, java.math.RoundingMode.HALF_EVEN).toPlainString();
        } else if (conv == 'e') {
            s = eFormat(x, prec);
        } else {
            int p = prec == 0 ? 1 : prec;
            int exp;
            if (x.signum() == 0) {
                exp = 0;
            } else {
                java.math.BigDecimal r = x.round(new java.math.MathContext(p, java.math.RoundingMode.HALF_EVEN));
                exp = r.precision() - r.scale() - 1;
            }
            if (exp < p && exp >= -4) {
                s = stripZeros(x.setScale(p - 1 - exp, java.math.RoundingMode.HALF_EVEN).toPlainString());
            } else {
                String e = eFormat(x, p - 1);
                int ei = e.indexOf('e');
                s = stripZeros(e.substring(0, ei)) + e.substring(ei);
            }
        }
        if (negZero && !s.startsWith("-")) s = "-" + s;
        return s;
    }

    private static String eFormat(java.math.BigDecimal x, int prec) {
        if (x.signum() == 0) {
            StringBuilder sb = new StringBuilder("0");
            if (prec > 0) { sb.append('.'); for (int k = 0; k < prec; k++) sb.append('0'); }
            return sb.append("e+00").toString();
        }
        java.math.BigDecimal r = x.round(new java.math.MathContext(prec + 1, java.math.RoundingMode.HALF_EVEN));
        int exp = r.precision() - r.scale() - 1;
        String digits = r.unscaledValue().abs().toString();
        StringBuilder d = new StringBuilder(digits);
        while (d.length() < prec + 1) d.append('0');
        StringBuilder sb = new StringBuilder();
        if (r.signum() < 0) sb.append('-');
        sb.append(d.charAt(0));
        if (prec > 0) sb.append('.').append(d, 1, prec + 1);
        sb.append('e').append(exp < 0 ? '-' : '+');
        int ae = Math.abs(exp);
        if (ae < 10) sb.append('0');
        sb.append(ae);
        return sb.toString();
    }

    private static String stripZeros(String s) {
        if (s.indexOf('.') < 0) return s;
        int end = s.length();
        while (end > 0 && s.charAt(end - 1) == '0') end--;
        if (end > 0 && s.charAt(end - 1) == '.') end--;
        return s.substring(0, end);
    }

    /** print/1,2 and {@code ~p}: the portray hook, quoted, numbervars (SWI). */
    private static Writer.Options printOptions(Machine m) {
        Writer.Options o = Writer.Options.writeq();
        o.numbervars = true;
        o.portray = true;
        o.portrayHook = portrayHook(m);
        return o;
    }
    // END_CHANGE: ISS-2025-0595

    /** The {@code portray/1} hook {@code print/1,2} installs, or null when there is no portray/1. */
    private static Writer.Portray portrayHook(final Machine m) {
        if (m.engine().kb() == null) return null;
        List<it.denzosoft.jprolog.core.engine.Rule> rules =
            m.engine().kb().getRulesForPredicate("portray", 1);
        if (rules == null || rules.isEmpty()) return null;
        return new Writer.Portray() {
            @Override
            public String portray(Term t) {
                java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
                PrintStream capture = new PrintStream(baos, true);
                PrintStream prev = StreamManager.threadLocalOutput();
                boolean ok;
                try {
                    StreamManager.setThreadLocalOutput(capture);
                    ok = m.runSubQuery(new CompoundTerm(new Atom("portray"),
                            Collections.singletonList(t)), new Machine.SolutionSink() {
                        @Override public boolean onSolution(Map<String, Term> s) { return true; }
                    });
                } catch (RuntimeException e) {
                    ControlFlow.rethrowIfControl(e);
                    return null;
                } finally {
                    capture.flush();
                    StreamManager.setThreadLocalOutput(prev);
                }
                return (ok && baos.size() > 0) ? baos.toString() : null;
            }
        };
    }
}
// END_CHANGE: ISS-2025-0496
