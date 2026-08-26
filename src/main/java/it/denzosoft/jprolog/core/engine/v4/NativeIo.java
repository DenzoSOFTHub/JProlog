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
    }

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
                case PRINT:
                    o = new Writer.Options();
                    o.numbervars = true;
                    o.portray = true;
                    o.portrayHook = portrayHook(m);
                    break;
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
            PrintStream ps = out(m, streamArg ? args[0] : null, "tab/2");
            Term nt = m.deref(streamArg ? args[1] : args[0]);
            if (!(nt instanceof Number)) return Outcome.FAILURE;
            int n = ((Number) nt).getValue().intValue();
            if (n < 0) return Outcome.FAILURE;
            for (int i = 0; i < n; i++) ps.print(' ');
            ps.flush();
            return Outcome.SUCCESS;
        }
    }

    private static final class PutCharB implements Builtin {
        private final boolean streamArg;
        PutCharB(boolean streamArg) { this.streamArg = streamArg; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            PrintStream ps = out(m, streamArg ? args[0] : null, "put_char/2");
            Term ct = m.deref(streamArg ? args[1] : args[0]);
            if (!(ct instanceof Atom)) return Outcome.FAILURE;      // includes the unbound case
            String s = ((Atom) ct).getName();
            if (s.length() != 1) return Outcome.FAILURE;
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
            PrintStream ps = out(m, streamArg ? args[0] : null, "put_code/2");
            Term ct = m.deref(streamArg ? args[1] : args[0]);
            if (!(ct instanceof Number)) return Outcome.FAILURE;
            double v = ((Number) ct).getValue();
            if (v != Math.floor(v) || v < 0 || v > 1114111) return Outcome.FAILURE;
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
        GetB(boolean asChar, boolean peek, boolean streamArg) {
            this.asChar = asChar; this.peek = peek; this.streamArg = streamArg;
        }

        @Override
        public Outcome call(Machine m, Term[] args) {
            String ctx = (peek ? "peek_" : "get_") + (asChar ? "char/" : "code/")
                       + (streamArg ? 2 : 1);
            Term target = args[streamArg ? 1 : 0];
            PrologStream s = inStream(m, streamArg ? args[0] : null, ctx);
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
            if (cp < 0 && !peek) IOStreamUtils.checkPastEof(s, ctx);
            Term value;
            if (asChar) {
                value = (cp < 0) ? new Atom("end_of_file") : new Atom(new String(Character.toChars(cp)));
            } else {
                value = Number.valueOf((long) cp);
            }
            return m.unify(target, value) ? Outcome.SUCCESS : Outcome.FAILURE;
        }

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

            String fmt = formatString(fmtT, m);
            if (fmt == null) return Outcome.FAILURE;
            List<Term> list = argumentList(argsT, m);

            String output;
            try {
                output = new Fmt(m).run(fmt, list);
            } catch (PrologException pe) {
                throw pe;
            } catch (RuntimeException e) {
                ControlFlow.rethrowIfControl(e);
                return Outcome.FAILURE;
            }

            if (sink instanceof CompoundTerm && sink.getArguments() != null
                    && sink.getArguments().size() == 1) {
                Term target = ((CompoundTerm) sink).getArguments().get(0);
                String f = ((CompoundTerm) sink).getName();
                if ("atom".equals(f))   return m.unify(target, new Atom(output)) ? Outcome.SUCCESS : Outcome.FAILURE;
                if ("string".equals(f)) return m.unify(target, new PrologString(output)) ? Outcome.SUCCESS : Outcome.FAILURE;
                if ("codes".equals(f))  return m.unify(target, textToList(output, true)) ? Outcome.SUCCESS : Outcome.FAILURE;
                if ("chars".equals(f))  return m.unify(target, textToList(output, false)) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            PrintStream ps = IOStreamUtils.resolveOutputStream(sink, NOB, "format/3");
            ps.print(output);
            ps.flush();
            return Outcome.SUCCESS;
        }
    }

    static Term textToList(String text, boolean codes) {
        Term list = NIL;
        for (int i = text.length() - 1; i >= 0; i--) {
            Term head = codes ? (Term) Number.valueOf((long) text.charAt(i))
                              : (Term) new Atom(String.valueOf(text.charAt(i)));
            list = new CompoundTerm(DOT, Arrays.asList(head, list));
        }
        return list;
    }

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

    /**
     * The format-directive engine. A faithful port of {@code builtin.io.Format}'s
     * {@code processFormat}/{@code processFormatCode}, reading dereferenced cells instead of a
     * {@code Map<String,Term>}, and calling back into the running machine for {@code ~@}/{@code ~p}.
     */
    private static final class Fmt {
        /** Directives handled by {@link #directive}; ~t, ~| and ~+ are consumed by {@link #run}. */
        private static final String KNOWN = "adDfegswqnt~ipcrR@";

        private final Machine m;
        Fmt(Machine m) { this.m = m; }

        String run(String fmt, List<Term> arguments) {
            StringBuilder result = new StringBuilder();
            int argIdx = 0;
            List<Integer> tabMarks = new ArrayList<Integer>();
            int segmentStart = 0;
            int segmentBaseCol = 0;

            for (int i = 0; i < fmt.length(); i++) {
                char ch = fmt.charAt(i);
                if (ch != '~') {
                    result.append(ch);
                    if (ch == '\n') {
                        segmentBaseCol = 0;
                        segmentStart = result.length();
                        tabMarks.clear();
                    }
                    continue;
                }
                int j = i + 1;
                Integer numArg = null;
                if (j < fmt.length() && fmt.charAt(j) == '*') {
                    if (argIdx < arguments.size()) {
                        Term a = Unify.deref(arguments.get(argIdx));
                        if (a instanceof Number) numArg = (int) ((Number) a).longValue();
                        argIdx++;
                    }
                    j++;
                } else {
                    StringBuilder num = new StringBuilder();
                    while (j < fmt.length() && Character.isDigit(fmt.charAt(j))) num.append(fmt.charAt(j++));
                    if (num.length() > 0) {
                        try { numArg = Integer.valueOf(num.toString()); }
                        catch (NumberFormatException e) { /* ignore */ }
                    }
                }
                if (j >= fmt.length()) { result.append(ch); continue; }   // lone ~ at the end

                char code = fmt.charAt(j);
                if (code == 't') {
                    tabMarks.add(Integer.valueOf(result.length()));
                    i = j;
                    continue;
                }
                if (code == '|' || code == '+') {
                    int currentColInSegment = result.length() - segmentStart;
                    int targetColInSegment;
                    if (code == '|') {
                        int absTarget = (numArg != null) ? numArg.intValue()
                                                         : (segmentBaseCol + currentColInSegment);
                        targetColInSegment = absTarget - segmentBaseCol;
                    } else {
                        targetColInSegment = (numArg != null) ? numArg.intValue() : currentColInSegment;
                    }
                    int padding = targetColInSegment - currentColInSegment;
                    if (padding > 0 && !tabMarks.isEmpty()) {
                        int slots = tabMarks.size();
                        int perTab = padding / slots;
                        int remainder = padding - perTab * slots;
                        StringBuilder pad = new StringBuilder();
                        for (int k = 0; k < perTab; k++) pad.append(' ');
                        String padStr = pad.toString();
                        for (int t = tabMarks.size() - 1; t >= 0; t--) {
                            int pos = tabMarks.get(t).intValue();
                            if (t == tabMarks.size() - 1 && remainder > 0) {
                                StringBuilder r = new StringBuilder(padStr);
                                for (int k = 0; k < remainder; k++) r.append(' ');
                                result.insert(pos, r);
                            } else {
                                result.insert(pos, padStr);
                            }
                        }
                    } else if (padding > 0) {
                        for (int k = 0; k < padding; k++) result.append(' ');
                    }
                    tabMarks.clear();
                    segmentBaseCol = segmentBaseCol + Math.max(currentColInSegment, targetColInSegment);
                    segmentStart = result.length();
                    i = j;
                    continue;
                }
                String formatted = directive(code, arguments, argIdx, numArg);
                result.append(formatted);
                if (consumes(code)) argIdx++;
                if (formatted.indexOf('\n') >= 0) {
                    segmentBaseCol = 0;
                    segmentStart = result.length();
                    tabMarks.clear();
                }
                i = j;
            }
            return result.toString();
        }

        private static boolean consumes(char c) { return c != 'n' && c != 't' && c != '~'; }

        private String directive(char code, List<Term> arguments, int argIndex, Integer numArg) {
            Term arg = (argIndex < arguments.size()) ? Unify.deref(arguments.get(argIndex)) : null;
            if (KNOWN.indexOf(code) < 0) throw formatError("unknown directive: ~" + code);
            if (arg == null && consumes(code)) throw formatError("not enough arguments");

            switch (code) {
                case 'a': return (arg != null) ? text(arg) : "";
                case 'd':
                    if (arg == null) return "0";
                    requireInteger(arg);
                    if (numArg != null && numArg.intValue() > 0) {
                        int nd = numArg.intValue();
                        String s = integerText(arg);
                        boolean neg = s.startsWith("-");
                        if (neg) s = s.substring(1);
                        while (s.length() < nd + 1) s = "0" + s;
                        s = s.substring(0, s.length() - nd) + "." + s.substring(s.length() - nd);
                        return neg ? "-" + s : s;
                    }
                    return integerText(arg);
                case 'D':
                    if (arg == null) return "0";
                    requireInteger(arg);
                    return String.format("%,d", Long.valueOf(((Number) arg).longValue()));
                case 'f': return (arg == null) ? "0.0" : fixed(arg, numArg, "f");
                case 'e': return (arg == null) ? "0.0" : fixed(arg, numArg, "e");
                case 'g': return (arg == null) ? "0.0" : fixed(arg, numArg, "g");
                case 's': return (arg != null) ? textOfString(arg) : "";
                case 'w': return (arg == null) ? "" : pad(Writer.format(arg, new Writer.Options(), 1200), numArg);
                case 'q': return (arg == null) ? "" : pad(Writer.format(arg, Writer.Options.writeq(), 1200), numArg);
                case 'n': return "\n";
                case 't': return "\t";
                case '~': return "~";
                case 'i': return "";
                case 'p': return (arg == null) ? "" : viaPortray(arg);
                case '@': return (arg == null) ? "" : viaCall(arguments.get(argIndex));
                case 'c': {
                    if (arg == null) return "";
                    int reps = (numArg != null && numArg.intValue() > 0) ? numArg.intValue() : 1;
                    StringBuilder sb = new StringBuilder();
                    String s = charText(arg);
                    for (int k = 0; k < reps; k++) sb.append(s);
                    return sb.toString();
                }
                case 'r':
                case 'R': {
                    if (arg == null) return "";
                    if (numArg == null) {
                        return (arg instanceof Number)
                            ? Integer.toString(((Number) arg).getValue().intValue(), 16) : "";
                    }
                    return radix(arg, numArg.intValue(), code == 'R');
                }
                default: throw formatError("unknown directive: ~" + code);
            }
        }

        private String pad(String s, Integer width) {
            if (width == null || s.length() >= width.intValue()) return s;
            StringBuilder sb = new StringBuilder();
            for (int k = 0; k < width.intValue() - s.length(); k++) sb.append(' ');
            return sb.append(s).toString();
        }

        private String fixed(Term t, Integer prec, String conv) {
            double v = (t instanceof Number) ? ((Number) t).doubleValue() : 0.0;
            int p = (prec != null && prec.intValue() >= 0) ? prec.intValue() : 6;
            return String.format("%." + p + conv, Double.valueOf(v));
        }

        private String radix(Term t, int base, boolean upper) {
            if (!(t instanceof Number)) return "";
            if (base < 2 || base > 36) return t.toString();
            String s = Long.toString(((Number) t).longValue(), base);
            return upper ? s.toUpperCase() : s;
        }

        private String text(Term t) {
            if (t instanceof Atom) return ((Atom) t).getName();
            return m.resolve(t).toString();
        }

        private String integerText(Term t) {
            return (t instanceof Number) ? String.valueOf(((Number) t).getValue().longValue()) : "0";
        }

        private String charText(Term t) {
            if (!(t instanceof Number)) return "";
            int cp = (int) ((Number) t).longValue();
            if (cp < 0 || cp > 0x10FFFF) return "";
            return new String(Character.toChars(cp));
        }

        private String textOfString(Term t) {
            if (t instanceof Atom) return ((Atom) t).getName();
            if (t instanceof PrologString) return ((PrologString) t).getStringValue();
            if (NativeLibrary.isCons(t)) return charListText(t, m);
            return m.resolve(t).toString();
        }

        private void requireInteger(Term t) {
            if (!(t instanceof Number) || !((Number) t).isInteger()) {
                throw new PrologException(ISOErrorTerms.typeError("integer", t, "format/2"));
            }
        }

        private PrologException formatError(String message) {
            Term formal = new CompoundTerm(new Atom("format"),
                Collections.singletonList((Term) new Atom(message)));
            return new PrologException(ISOErrorTerms.error(formal, new Atom("format/2")));
        }

        /** {@code ~p}: the user's {@code portray/1}, or {@code ~w} when there is none. */
        private String viaPortray(Term arg) {
            String s = runCaptured(new CompoundTerm(new Atom("portray"),
                Collections.singletonList(arg)), true);
            return (s != null) ? s : Writer.format(arg, new Writer.Options(), 1200);
        }

        /** {@code ~@}: run the goal and splice in everything it printed. */
        private String viaCall(Term goal) {
            String s = runCaptured(goal, false);
            return (s == null) ? "" : s;
        }

        /**
         * Run {@code goal} on the machine with the output captured. Returns null when the goal
         * failed (or, for the portray hook, printed nothing).
         */
        private String runCaptured(Term goal, boolean portray) {
            if (portray) {
                List<it.denzosoft.jprolog.core.engine.Rule> rules =
                    (m.engine().kb() == null) ? null : m.engine().kb().getRulesForPredicate("portray", 1);
                if (rules == null || rules.isEmpty()) return null;
            }
            java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
            PrintStream capture = new PrintStream(baos, true);
            PrintStream prev = StreamManager.threadLocalOutput();
            boolean ok;
            try {
                StreamManager.setThreadLocalOutput(capture);
                ok = m.runSubQuery(goal, new Machine.SolutionSink() {
                    @Override public boolean onSolution(Map<String, Term> s) { return true; }
                });
            } finally {
                capture.flush();
                StreamManager.setThreadLocalOutput(prev);
            }
            if (portray) return (ok && baos.size() > 0) ? baos.toString() : null;
            return baos.toString();
        }
    }

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
