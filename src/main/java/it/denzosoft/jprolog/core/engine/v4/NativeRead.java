package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.builtin.io.IOStreamUtils;
import it.denzosoft.jprolog.builtin.io.StreamManager;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.parser.v2.Lexer;
import it.denzosoft.jprolog.core.parser.v2.TermReader;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0566 - 4.5 wave P3.4/P3.5: every term reader on the v2 parser.
/**
 * The term readers of the engine, all on the clean-room v2 parser ({@code core.parser.v2}) with the
 * engine's operator table and flags: {@code read/1,2}, {@code read_term/2,3},
 * {@code read_term_from_atom/3}, and (through {@link #parseText}) {@code term_to_atom/2},
 * {@code term_string/2} and {@code atom_to_term/3}. Plus the in-memory input of P3.5:
 * {@code open_string/2}, {@code with_input_from/2}, {@code read_line_to_string/2},
 * {@code read_line_to_codes/2,3} and {@code read_string/3,5}.
 *
 * <p>A term is read from a stream in two steps: {@link #collect} takes the characters of ONE
 * clause off the stream (quote-, comment- and {@code 0'c}-aware, up to and including the end
 * token and the layout character after it, so the next read starts on the next term), then the
 * text is tokenised and parsed. A syntax error therefore consumes exactly the faulty term, as ISO
 * 8.14.1.1 asks, and is raised as {@code error(syntax_error(Description), Context)}.
 *
 * <p>Variables of a term read at run time are FRESH unnamed cells: their names live only in the
 * {@code variable_names/1} option. (Named cells would alias across two reads on the assert path,
 * which numbers skeleton variables by name — invariant 5.)
 */
final class NativeRead {

    private NativeRead() { }

    static void register(BuiltinTable t) {
        t.register("read", 1, new ReadB(false, false));
        t.register("read", 2, new ReadB(true, false));
        t.register("read_term", 2, new ReadB(false, true));
        t.register("read_term", 3, new ReadB(true, true));
        t.register("read_term_from_atom", 3, new ReadTermFromAtomB());
        // START_CHANGE: ISS-2025-0569 - P3.5 in-memory input
        t.register("open_string", 2, new OpenStringB());
        t.register("with_input_from", 2, new WithInputFromB());
        t.register("read_line_to_string", 2, new ReadLineB(Line.STRING));
        t.register("read_line_to_codes", 2, new ReadLineB(Line.CODES));
        t.register("read_line_to_codes", 3, new ReadLineB(Line.CODES_DIFF));
        t.register("read_string", 3, new ReadString3B());
        t.register("read_string", 5, new ReadString5B());
        // END_CHANGE: ISS-2025-0569
    }

    // ------------------------------------------------------------------ character sources

    /** One code point at a time, with a one-code-point lookahead. */
    interface CharSource {
        int read() throws IOException;
        int peek() throws IOException;
    }

    static final class StreamSource implements CharSource {
        private final PrologStream s;
        StreamSource(PrologStream s) { this.s = s; }
        @Override public int read() throws IOException { return s.getCodePoint(); }
        @Override public int peek() throws IOException { return s.peekCodePoint(); }
    }

    /** The console: the engine's single stdin reader (shared with get_char/1). */
    static final class StdinSource implements CharSource {
        private final BufferedReader r;
        StdinSource(BufferedReader r) { this.r = r; }
        @Override public int read() throws IOException { return r.read(); }
        @Override public int peek() throws IOException {
            r.mark(2);
            int c = r.read();
            r.reset();
            return c;
        }
    }

    static final class TextSource implements CharSource {
        private final String s;
        private int i;
        TextSource(String s) { this.s = s; }
        @Override public int read() {
            if (i >= s.length()) return -1;
            int cp = s.codePointAt(i);
            i += Character.charCount(cp);
            return cp;
        }
        @Override public int peek() { return i >= s.length() ? -1 : s.codePointAt(i); }
    }

    static CharSource sourceFor(PrologStream s) {
        if (IOStreamUtils.isStdin(s)) return new StdinSource(NativeIo.stdinReader());
        return new StreamSource(s);
    }

    // ------------------------------------------------------------------ the clause collector

    private static final String GRAPHIC = "#$&*+-./:<=>?@^~\\";

    private static boolean isLayout(int c) {
        return c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == 11;
    }

    private static boolean isAlnum(int c) {
        return c == '_' || Character.isLetterOrDigit(c);
    }

    /**
     * The text of the next clause on {@code src}, including its end token, or null when only
     * layout and comments remain. At end of file an unterminated term is returned as it is (SWI
     * reads {@code foo} from a stream holding just {@code foo}).
     */
    static String collect(CharSource src) throws IOException {
        StringBuilder sb = new StringBuilder();
        boolean token = false;                       // a non-layout character has been seen
        for (;;) {
            int c = src.read();
            if (c < 0) return token ? sb.toString() : null;
            if (isLayout(c)) { sb.appendCodePoint(c); continue; }
            if (c == '%') {                          // line comment
                while ((c = src.read()) >= 0 && c != '\n') { /* skip */ }
                sb.append('\n');
                if (c < 0) return token ? sb.toString() : null;
                continue;
            }
            if (c == '/' && src.peek() == '*') {     // block comment
                src.read();
                int prev = 0;
                for (;;) {
                    int d = src.read();
                    if (d < 0) return token ? sb.toString() : null;
                    if (d == '\n') sb.append('\n');
                    if (prev == '*' && d == '/') break;
                    prev = d;
                }
                sb.append(' ');
                continue;
            }
            token = true;
            if (c == '\'' || c == '"' || c == '`') {
                sb.appendCodePoint(c);
                quoted(src, sb, c);
                continue;
            }
            if (isAlnum(c)) {
                int runStart = sb.length();
                sb.appendCodePoint(c);
                while (isAlnum(src.peek())) sb.appendCodePoint(src.read());
                if (sb.length() - runStart == 1 && c == '0' && src.peek() == '\'') {
                    sb.appendCodePoint(src.read());  // 0'
                    charLiteral(src, sb);
                }
                continue;
            }
            if (GRAPHIC.indexOf(c) >= 0) {
                int runStart = sb.length();
                sb.appendCodePoint(c);
                while (src.peek() >= 0 && GRAPHIC.indexOf(src.peek()) >= 0) {
                    // a '/' starting a block comment ends the graphic run
                    sb.appendCodePoint(src.read());
                }
                if (sb.length() - runStart == 1 && c == '.') {
                    int n = src.peek();
                    if (n < 0 || n == '%' || isLayout(n)) {
                        if (n >= 0 && n != '%') src.read();   // the layout char after the end
                        return sb.toString();
                    }
                }
                continue;
            }
            sb.appendCodePoint(c);                   // punctuation and anything else
        }
    }

    /** The rest of a quoted item (the opening quote already appended). */
    private static void quoted(CharSource src, StringBuilder sb, int q) throws IOException {
        for (;;) {
            int c = src.read();
            if (c < 0) return;                        // unterminated: the parser reports it
            sb.appendCodePoint(c);
            if (c == '\\') {
                int d = src.read();
                if (d < 0) return;
                sb.appendCodePoint(d);
                continue;
            }
            if (c == q) {
                if (src.peek() == q) { sb.appendCodePoint(src.read()); continue; }
                return;
            }
        }
    }

    /** After {@code 0'}: one character, an escape sequence, or a doubled quote. */
    private static void charLiteral(CharSource src, StringBuilder sb) throws IOException {
        int c = src.read();
        if (c < 0) return;
        sb.appendCodePoint(c);
        if (c == '\\') {
            int d = src.read();
            if (d < 0) return;
            sb.appendCodePoint(d);
            if (d == 'x' || (d >= '0' && d <= '7')) {
                while (Character.digit(src.peek(), 16) >= 0) sb.appendCodePoint(src.read());
                if (src.peek() == '\\') sb.appendCodePoint(src.read());
            } else if (d == 'u' || d == 'U') {
                for (int i = 0; i < (d == 'u' ? 4 : 8) && Character.digit(src.peek(), 16) >= 0; i++) {
                    sb.appendCodePoint(src.read());
                }
            }
        } else if (c == '\'' && src.peek() == '\'') {
            sb.appendCodePoint(src.read());
        }
    }

    // ------------------------------------------------------------------ parsing text

    /** A parsed term with the variable information read_term/2,3 reports. */
    static final class Parsed {
        final Term term;
        final TermReader reader;
        Parsed(Term term, TermReader reader) { this.term = term; this.reader = reader; }
    }

    /**
     * Parse ONE term from {@code text} (an optional end token, then nothing) with the engine's
     * operators and flags. A syntax error raises {@code error(syntax_error(D), ctx)}.
     *
     * @param dq a {@code double_quotes} override, or null for the flag
     */
    static Parsed parse(String text, String dq, String ctx) {
        TermReader r;
        try {
            r = new TermReader(Lexer.tokenize(text), Ops.current().table()).withFreshVariables(true);
            if (dq != null) r.withDoubleQuotes(dq);
            return new Parsed(r.readSingle(), r);
        } catch (Lexer.LexException | TermReader.ParseException e) {
            throw syntax(e.getMessage(), ctx);
        }
    }

    /** {@link #parse} for the text built-ins (term_to_atom/2, term_string/2, atom_to_term/3). */
    static Term parseText(String text, String ctx) {
        return parse(text, null, ctx).term;
    }

    static PrologException syntax(String message, String ctx) {
        String m = message == null ? "syntax error" : message.replaceAll(" at line \\d+$", "");
        return Errors.syntax(m, ctx);
    }

    /** {@code ['X'=_A, 'Y'=_B]} for the named variables of the last read, in order. */
    static Term bindingList(TermReader r, boolean singletonsOnly) {
        List<Term> pairs = new ArrayList<Term>();
        Map<String, Integer> occ = r.variableOccurrences();
        for (Map.Entry<String, Variable> e : r.variableNames().entrySet()) {
            if (singletonsOnly) {
                Integer n = occ.get(e.getKey());
                if (n == null || n != 1 || e.getKey().startsWith("_")) continue;
            }
            pairs.add(new CompoundTerm(new Atom("="), Arrays.<Term>asList(new Atom(e.getKey()), e.getValue())));
        }
        return NativeLibrary.listOf(pairs, NIL);
    }

    private static final Atom NIL = new Atom("[]");
    private static final Atom END_OF_FILE = new Atom("end_of_file");

    // ------------------------------------------------------------------ read/1,2, read_term/2,3

    /** The parsed read_term option list. */
    private static final class ReadOptions {
        Term variables, variableNames, singletons, termPosition, comments;
        String syntaxErrors = "error";
        String doubleQuotes;
    }

    private static ReadOptions readOptions(Machine m, Term list, String ctx) {
        ReadOptions o = new ReadOptions();
        Term cur = m.deref(list);
        if (cur instanceof Variable) throw Errors.instantiation(ctx);
        while (cur instanceof CompoundTerm && ".".equals(((CompoundTerm) cur).getName())
                && ((CompoundTerm) cur).getArguments().size() == 2) {
            Term opt = m.deref(NativeLibrary.head(cur));
            if (opt instanceof Variable) throw Errors.instantiation(ctx);
            if (!(opt instanceof CompoundTerm) || ((CompoundTerm) opt).getArguments().size() != 1) {
                throw Errors.domain("read_option", m.resolve(opt), ctx);
            }
            Term a = ((CompoundTerm) opt).getArguments().get(0);
            Term av = m.deref(a);
            switch (((CompoundTerm) opt).getName()) {
                case "variables": o.variables = a; break;
                case "variable_names": o.variableNames = a; break;
                case "singletons": o.singletons = a; break;
                case "term_position": o.termPosition = a; break;
                case "subterm_positions": break;
                case "comments": o.comments = a; break;
                case "module": case "backquoted_string": case "cycles": case "dotlists":
                case "var_prefix": case "quasi_quotations": case "process_comment":
                    break;                                    // accepted, no effect
                case "syntax_errors":
                    if (!(av instanceof Atom)) throw Errors.domain("read_option", m.resolve(opt), ctx);
                    o.syntaxErrors = ((Atom) av).getName();
                    break;
                case "double_quotes":
                    if (!(av instanceof Atom)) throw Errors.domain("read_option", m.resolve(opt), ctx);
                    o.doubleQuotes = ((Atom) av).getName();
                    break;
                default:
                    throw Errors.domain("read_option", m.resolve(opt), ctx);
            }
            cur = m.deref(NativeLibrary.tail(cur));
        }
        if (cur instanceof Variable) throw Errors.instantiation(ctx);
        if (!NativeLibrary.isNil(cur)) throw Errors.type("list", m.resolve(list), ctx);
        return o;
    }

    private static final ReadB READ_TERM_3 = new ReadB(true, true);

    private static final class ReadB implements Builtin {
        private final boolean streamArg;
        private final boolean options;
        private final String ctx;
        ReadB(boolean streamArg, boolean options) {
            this.streamArg = streamArg;
            this.options = options;
            this.ctx = (options ? "read_term/" : "read/") + ((streamArg ? 1 : 0) + (options ? 2 : 1));
        }

        @Override
        public Outcome call(Machine m, Term[] args) {
            // ISS-2025-0352 compatibility: read_term(+Stream, -Term) (a JProlog extension; ISO's
            // read_term/2 is read_term(-Term, +Options)) keeps working
            if (options && !streamArg && m.deref(args[1]) instanceof Variable
                    && IOStreamUtils.isStreamTerm(m.deref(args[0]))) {
                return READ_TERM_3.call(m, new Term[] {args[0], args[1], NIL});
            }
            int i = 0;
            // ISS-2025-0567: a non-stream argument raises the ISO stream errors (8.14.1.3)
            PrologStream s = streamArg
                ? IOStreamUtils.inputStream(m.resolve(args[i++]), java.util.Collections.<String, Term>emptyMap(), ctx)
                : StreamManager.streams().currentInput();
            Term target = args[i++];
            ReadOptions o = options ? readOptions(m, args[i], ctx) : new ReadOptions();
            IOStreamUtils.checkStreamType(s, false, "input", ctx);
            IOStreamUtils.beforeRead(s, ctx);
            Term position = null;
            if (o.termPosition != null) {
                position = new CompoundTerm(new Atom("$stream_position"), Arrays.<Term>asList(
                    Number.valueOf(s.charCount()), Number.valueOf(s.lineCount()),
                    Number.valueOf(s.linePosition()), Number.valueOf(s.bytePosition())));
            }
            String text;
            try {
                text = collect(sourceFor(s));
            } catch (IOException e) {
                throw Errors.system("io_error: " + e.getMessage(), ctx);
            }
            Term term;
            TermReader reader = null;
            if (text == null) {
                term = END_OF_FILE;
            } else {
                try {
                    Parsed p = parse(text, o.doubleQuotes, ctx);
                    term = p.term;
                    reader = p.reader;
                } catch (PrologException pe) {
                    if ("error".equals(o.syntaxErrors) || !isSyntaxError(pe)) throw pe;
                    if ("fail".equals(o.syntaxErrors)) return Outcome.FAILURE;
                    return Outcome.FAILURE;                   // quiet / dec10: fail silently
                }
            }
            if (!m.unifyOrUndo(target, term)) return Outcome.FAILURE;
            if (o.variables != null
                    && !m.unifyOrUndo(o.variables, variableList(reader))) return Outcome.FAILURE;
            if (o.variableNames != null
                    && !m.unifyOrUndo(o.variableNames, reader == null ? NIL : bindingList(reader, false))) return Outcome.FAILURE;
            if (o.singletons != null
                    && !m.unifyOrUndo(o.singletons, reader == null ? NIL : bindingList(reader, true))) return Outcome.FAILURE;
            if (o.termPosition != null && !m.unifyOrUndo(o.termPosition, position)) return Outcome.FAILURE;
            if (o.comments != null && !m.unifyOrUndo(o.comments, NIL)) return Outcome.FAILURE;
            return Outcome.SUCCESS;
        }
    }

    static boolean isSyntaxError(PrologException pe) {
        Term t = pe.getErrorTerm();
        if (!(t instanceof CompoundTerm) || !"error".equals(t.getName())) return false;
        Term f = ((CompoundTerm) t).getArguments().get(0);
        return f instanceof CompoundTerm && "syntax_error".equals(f.getName());
    }

    private static Term variableList(TermReader r) {
        if (r == null) return NIL;
        List<Term> vs = new ArrayList<Term>(r.variables());
        return NativeLibrary.listOf(vs, NIL);
    }

    // ------------------------------------------------------------------ read_term_from_atom/3

    private static final class ReadTermFromAtomB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            String ctx = "read_term_from_atom/3";
            Term a = m.deref(args[0]);
            if (a instanceof Variable) throw Errors.instantiation(ctx);
            String text = NativeText.atomicText(a);
            if (text == null) throw Errors.type("atom", m.resolve(a), ctx);
            ReadOptions o = readOptions(m, args[2], ctx);
            Parsed p = parse(text, o.doubleQuotes, ctx);
            if (!m.unifyOrUndo(args[1], p.term)) return Outcome.FAILURE;
            if (o.variableNames != null && !m.unifyOrUndo(o.variableNames, bindingList(p.reader, false))) return Outcome.FAILURE;
            if (o.variables != null && !m.unifyOrUndo(o.variables, variableList(p.reader))) return Outcome.FAILURE;
            if (o.singletons != null && !m.unifyOrUndo(o.singletons, bindingList(p.reader, true))) return Outcome.FAILURE;
            return Outcome.SUCCESS;
        }
    }

    // ------------------------------------------------------------------ P3.5 in-memory input

    // START_CHANGE: ISS-2025-0569
    /** The text of an in-memory source: an atom, a string, a number, a code or a char list. */
    static String sourceText(Machine m, Term t, String ctx) {
        Term d = m.deref(t);
        if (d instanceof Variable) throw Errors.instantiation(ctx);
        String s = NativeText.atomicText(d);
        if (s != null) return s;
        if (NativeLibrary.isCons(d)) {
            Term first = m.deref(NativeLibrary.head(d));
            String text = NativeText.textOfList(m, d, first instanceof Atom, ctx);
            if (text == null) throw Errors.instantiation(ctx);
            return text;
        }
        throw Errors.type("string", m.resolve(d), ctx);
    }

    /** A new input stream over {@code text}, registered in this engine's stream table. */
    static PrologStream openText(String text) {
        return StreamManager.streams().registerInput(null,
            new ByteArrayInputStream(text.getBytes(StandardCharsets.UTF_8)));
    }

    private static final class OpenStringB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            String ctx = "open_string/2";
            Term sArg = m.deref(args[1]);
            if (!(sArg instanceof Variable)) throw Errors.uninstantiation(m.resolve(sArg), ctx);
            PrologStream s = openText(sourceText(m, args[0], ctx));
            return m.unify(args[1], Streams.termFor(s)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class WithInputFromB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            String ctx = "with_input_from/2";
            Term src = m.deref(args[0]);
            if (src instanceof Variable) throw Errors.instantiation(ctx);
            String text;
            if (src instanceof CompoundTerm && ((CompoundTerm) src).getArguments().size() == 1
                    && Arrays.asList("atom", "string", "codes", "chars").contains(src.getName())) {
                text = sourceText(m, ((CompoundTerm) src).getArguments().get(0), ctx);
            } else {
                throw Errors.domain("input_source", m.resolve(src), ctx);
            }
            Streams st = StreamManager.streams();
            PrologStream s = openText(text);
            PrologStream prev = st.currentInput();
            boolean ok;
            try {
                st.setCurrentInput(s);
                ok = m.runOnce(args[1]);
            } finally {
                st.setCurrentInput(prev);
                st.close(s);
            }
            return ok ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private enum Line { STRING, CODES, CODES_DIFF }

    private static final class ReadLineB implements Builtin {
        private final Line kind;
        ReadLineB(Line kind) { this.kind = kind; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            String ctx = kind == Line.STRING ? "read_line_to_string/2"
                : kind == Line.CODES ? "read_line_to_codes/2" : "read_line_to_codes/3";
            PrologStream s = IOStreamUtils.inputStream(m.resolve(args[0]),
                java.util.Collections.<String, Term>emptyMap(), ctx);
            IOStreamUtils.checkStreamType(s, false, "input", ctx);
            IOStreamUtils.beforeRead(s, ctx);
            CharSource src = sourceFor(s);
            StringBuilder sb = new StringBuilder();
            boolean newline = false;
            int c;
            try {
                while ((c = src.read()) >= 0) {
                    if (c == '\n') { newline = true; break; }
                    sb.appendCodePoint(c);
                }
            } catch (IOException e) {
                throw Errors.system("io_error: " + e.getMessage(), ctx);
            }
            boolean eof = !newline && sb.length() == 0;
            switch (kind) {
                case STRING: {
                    if (eof) return m.unify(args[1], END_OF_FILE) ? Outcome.SUCCESS : Outcome.FAILURE;
                    String line = stripCr(sb);
                    return m.unify(args[1], new PrologString(line)) ? Outcome.SUCCESS : Outcome.FAILURE;
                }
                case CODES: {
                    if (eof) return m.unify(args[1], Number.valueOf(-1L)) ? Outcome.SUCCESS : Outcome.FAILURE;
                    return m.unify(args[1], NativeText.codeList(stripCr(sb))) ? Outcome.SUCCESS : Outcome.FAILURE;
                }
                default: {
                    // SWI library(readutil): the newline is kept; at end of file the list is closed
                    List<Term> codes = new ArrayList<Term>();
                    String text = sb.toString();
                    for (int i = 0; i < text.length(); ) {
                        int cp = text.codePointAt(i);
                        codes.add(Number.valueOf((long) cp));
                        i += Character.charCount(cp);
                    }
                    if (newline) {
                        codes.add(Number.valueOf(10L));
                        return m.unify(args[1], NativeLibrary.listOf(codes, args[2])) ? Outcome.SUCCESS : Outcome.FAILURE;
                    }
                    return m.unify(args[1], NativeLibrary.listOf(codes, NIL)) ? Outcome.SUCCESS : Outcome.FAILURE;
                }
            }
        }

        private static String stripCr(StringBuilder sb) {
            int n = sb.length();
            return (n > 0 && sb.charAt(n - 1) == '\r') ? sb.substring(0, n - 1) : sb.toString();
        }
    }

    /** read_string(+Stream, ?Length, -String). */
    private static final class ReadString3B implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            String ctx = "read_string/3";
            PrologStream s = IOStreamUtils.inputStream(m.resolve(args[0]),
                java.util.Collections.<String, Term>emptyMap(), ctx);
            IOStreamUtils.checkStreamType(s, false, "input", ctx);
            Term len = m.deref(args[1]);
            long max = Long.MAX_VALUE;
            if (!(len instanceof Variable)) {
                if (!(len instanceof Number) || !((Number) len).isInteger()) throw Errors.type("integer", m.resolve(len), ctx);
                max = ((Number) len).longValue();
                if (max < 0) throw Errors.domain("not_less_than_zero", len, ctx);
            }
            CharSource src = sourceFor(s);
            StringBuilder sb = new StringBuilder();
            long n = 0;
            try {
                int c;
                while (n < max && (c = src.read()) >= 0) {
                    sb.appendCodePoint(c);
                    n++;
                    if ((n & 0xFFF) == 0) m.guard().step();
                }
            } catch (IOException e) {
                throw Errors.system("io_error: " + e.getMessage(), ctx);
            }
            if (len instanceof Variable && !m.unifyOrUndo(args[1], Number.valueOf(n))) return Outcome.FAILURE;
            return m.unify(args[2], new PrologString(sb.toString())) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    /** read_string(+Stream, +SepChars, +PadChars, -Sep, -String) (SWI library(strings)). */
    private static final class ReadString5B implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            String ctx = "read_string/5";
            PrologStream s = IOStreamUtils.inputStream(m.resolve(args[0]),
                java.util.Collections.<String, Term>emptyMap(), ctx);
            IOStreamUtils.checkStreamType(s, false, "input", ctx);
            String seps = sourceText(m, args[1], ctx);
            String pads = sourceText(m, args[2], ctx);
            CharSource src = sourceFor(s);
            StringBuilder sb = new StringBuilder();
            int sep = -1;
            try {
                int c;
                while ((c = src.read()) >= 0) {
                    if (seps.indexOf(c) >= 0 || (c > 0xFFFF && seps.contains(new String(Character.toChars(c))))) {
                        sep = c;
                        break;
                    }
                    sb.appendCodePoint(c);
                }
            } catch (IOException e) {
                throw Errors.system("io_error: " + e.getMessage(), ctx);
            }
            int from = 0, to = sb.length();
            while (from < to && pads.indexOf(sb.charAt(from)) >= 0) from++;
            while (to > from && pads.indexOf(sb.charAt(to - 1)) >= 0) to--;
            if (!m.unifyOrUndo(args[3], Number.valueOf((long) sep))) return Outcome.FAILURE;
            return m.unify(args[4], new PrologString(sb.substring(from, to))) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }
    // END_CHANGE: ISS-2025-0569
}
// END_CHANGE: ISS-2025-0566
