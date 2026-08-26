package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologParserException;
import it.denzosoft.jprolog.core.parser.TermParser;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class Read implements BuiltInWithContext {

    // ISS-2025-0472 - wave W7: the per-alias BufferedReader cache is gone; the stream owns its
    // decoder (see readTermTextFromStream).
    // END_CHANGE: ISS-2025-0203

    @Override
    public boolean executeWithContext(SolverContext solver, Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = query.getArguments().size();
        if (arity != 1 && arity != 2) {
            throw new PrologEvaluationException("read/1 or read/2 expected.");
        }

        // START_CHANGE: ISS-2025-0203 - read/2 reads from given stream
        Term termVar;
        String streamAlias = null;
        if (arity == 1) {
            termVar = query.getArguments().get(0);
        } else {
            Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);
            termVar = query.getArguments().get(1);
            // ISS-2025-0472 - wave W7: '$stream'(N), the legacy stream(A) wrapper, or an atom alias
            streamAlias = IOStreamUtils.streamAlias(streamTerm);
            if (streamAlias == null) {
                throw new PrologEvaluationException("read/2: invalid stream argument");
            }
        }

        // START_CHANGE: ISS-2025-0408 - read up to the ISO end token instead of one physical line
        String inputLine = readTermTextFromStream(streamAlias);
        // END_CHANGE: ISS-2025-0408
        if (inputLine == null) {
            // EOF
            Map<String, Term> nb = new HashMap<>(bindings);
            if (termVar.unify(new Atom("end_of_file"), nb)) {
                solutions.add(nb);
                return true;
            }
            return false;
        }
        // END_CHANGE: ISS-2025-0203

        inputLine = inputLine.trim();
        if (inputLine.isEmpty()) {
            throw new PrologEvaluationException("read: No input provided.");
        }
        // START_CHANGE: ISS-2025-0408 - the end token '.' is already consumed by readTermText;
        // no trailing-period strip needed (a '.' here belongs to the term text itself).
        // END_CHANGE: ISS-2025-0408

        Prolog prolog = solver.getPrologContext();
        if (prolog == null) {
            throw new PrologEvaluationException("Prolog context not available for read");
        }
        try {
            TermParser termParser = prolog.getTermParser();
            Term parsedTerm = termParser.parseTerm(inputLine);
            if (parsedTerm != null) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (termVar.unify(parsedTerm, newBindings)) {
                    solutions.add(newBindings);
                    return true;
                }
            }
            return false;
        } catch (PrologParserException e) {
            throw new PrologEvaluationException("Error parsing term in read: " + e.getMessage(), e);
        }
    }

    // START_CHANGE: ISS-2025-0408 - read the text of ONE term (up to the ISO end token) from a
    // named stream or stdin. Replaces the line-based readLineFromStream (ISS-2025-0203): a term
    // may now span several lines, a line may hold several terms, and leading %-comment lines are
    // skipped. The per-alias BufferedReader persists in READERS, so characters after the end
    // token stay buffered for the next read on the same stream.
    private String readTermTextFromStream(String alias) {
        // START_CHANGE: ISS-2025-0375 - honour set_input/1: read/1 (and an explicit current_input)
        // must read from the CURRENT input stream; only user_input falls through to the
        // interactive stdin path below.
        if (alias == null || "current_input".equals(alias)) {
            String cur = StreamManager.getCurrentInput();
            alias = (cur == null) ? "user_input" : cur;
        }
        // END_CHANGE: ISS-2025-0375
        if (alias == null || "current_input".equals(alias) || "user_input".equals(alias)) {
            StreamManager.out().print("?- ");
            try {
                return readTermText(STDIN_TERM_READER);
            } catch (java.io.IOException e) {
                return null;
            }
        }
        // START_CHANGE: ISS-2025-0472 - wave W7: read through the STREAM's own decoder instead of a
        // private static per-alias BufferedReader cache. That cache was process-global state (so a
        // second engine reusing a handle got the first engine's closed file) and it buffered ahead
        // of the stream, so read/1 and get_char/2 disagreed about the position and a seek in
        // between was invisible to it (limit L-07).
        it.denzosoft.jprolog.core.engine.v4.PrologStream ps = StreamManager.stream(alias);
        if (ps == null || !ps.isInput()) {
            throw new PrologEvaluationException("existence_error(stream, " + alias + ")");
        }
        try {
            return readTermText(StreamManager.reader(ps));
        } catch (java.io.IOException e) {
            throw new PrologEvaluationException("io_error(read, " + alias + "): " + e.getMessage());
        }
    }

    /**
     * Persistent character reader over stdin for interactive read/1. Reading stops right after
     * the end token (and the single layout character that closes it), so the CLI prompt is not
     * deadlocked and trailing user input is not swallowed wholesale like the old per-call Scanner.
     */
    private static final java.io.Reader STDIN_TERM_READER = new InputStreamReader(System.in);

    /**
     * Consume characters from {@code r} up to and including the ISO end token: a {@code '.'}
     * followed by a layout character, a %-comment, or EOF (ISO 13211-1 §8.14.1). Tracks quoted
     * atoms {@code '...'}, double-quoted strings {@code "..."}, back-quoted strings, {@code 0'c}
     * character literals, {@code %} line comments and {@code /*..*}{@code /} block comments so
     * embedded dots never terminate the term early; a dot inside a graphic token ({@code =..})
     * or a float ({@code 3.14}) is not an end token either. Characters after the end token are
     * left unread on the (persistent) reader.
     *
     * @return the term text WITHOUT the end {@code '.'} (ready for TermParser.parseTerm),
     *         or {@code null} on EOF before any content (the caller maps it to end_of_file)
     */
    public static String readTermText(java.io.Reader r) throws java.io.IOException {
        StringBuilder buf = new StringBuilder();
        boolean seenContent = false;
        int c = r.read();
        while (true) {
            if (c == -1) {
                return seenContent ? buf.toString() : null;
            }
            char ch = (char) c;
            if (!seenContent && Character.isWhitespace(ch)) {     // leading layout
                c = r.read();
                continue;
            }
            if (ch == '%') {                                      // % line comment (layout)
                do { c = r.read(); } while (c != -1 && c != '\n');
                if (seenContent) buf.append(' ');
                if (c != -1) c = r.read();
                continue;
            }
            if (ch == '/') {
                int next = r.read();
                if (next == '*') {                                // block comment (layout)
                    int prev = -1;
                    int k = r.read();
                    while (k != -1 && !(prev == '*' && k == '/')) { prev = k; k = r.read(); }
                    if (seenContent) buf.append(' ');
                    c = (k == -1) ? -1 : r.read();
                    continue;
                }
                seenContent = true;
                buf.append('/');
                c = next;
                continue;
            }
            if (ch == '.' && !endsWithGraphicChar(buf)) {         // end-token candidate
                int next = r.read();
                if (next == -1) return buf.toString();            // '.' at EOF: end token
                if (next == '%') {                                // '.' + comment: end token;
                    int k;                                        // consume the comment as trailing layout
                    do { k = r.read(); } while (k != -1 && k != '\n');
                    return buf.toString();
                }
                if (Character.isWhitespace((char) next)) {        // '.' + layout: end token
                    return buf.toString();
                }
                seenContent = true;                               // not an end token (e.g. 3.14, .(a,b))
                buf.append('.');
                c = next;
                continue;
            }
            if (ch == '\'' && isCharCodeLiteralQuote(buf)) {      // 0'c character literal
                seenContent = true;
                buf.append('\'');
                int lit = r.read();
                if (lit == -1) return buf.toString();             // malformed; let the parser report
                buf.append((char) lit);
                if (lit == '\\') {                                // 0'\n, 0'\\, 0'\x41\ ...
                    int esc = r.read();
                    if (esc == -1) return buf.toString();
                    buf.append((char) esc);
                } else if (lit == '\'') {                         // 0''' is the quote character
                    int q2 = r.read();
                    if (q2 == '\'') {
                        buf.append('\'');
                    } else {
                        c = q2;                                   // reprocess at loop top
                        continue;
                    }
                }
                c = r.read();
                continue;
            }
            if (ch == '\'' || ch == '"' || ch == '`') {           // quoted token: copy verbatim
                seenContent = true;
                buf.append(ch);
                c = r.read();
                boolean closed = false;
                while (c != -1 && !closed) {
                    buf.append((char) c);
                    if (c == '\\') {                              // escape: next char is literal
                        int e = r.read();
                        if (e == -1) break;
                        buf.append((char) e);
                        c = r.read();
                    } else if (c == ch) {
                        int p = r.read();
                        if (p == ch) {                            // doubled quote stays inside
                            buf.append(ch);
                            c = r.read();
                        } else {
                            closed = true;
                            c = p;                                // reprocess at loop top
                        }
                    } else {
                        c = r.read();
                    }
                }
                continue;                                         // (unterminated quote: parser reports)
            }
            seenContent = true;
            buf.append(ch);
            c = r.read();
        }
    }

    /** Does the buffered text end with an ISO graphic char ('.' after one belongs to a graphic token like =..). */
    private static boolean endsWithGraphicChar(StringBuilder buf) {
        if (buf.length() == 0) return false;
        return "#$&*+-./:<=>?@^~\\".indexOf(buf.charAt(buf.length() - 1)) >= 0;
    }

    /** Is a quote at this position the start of a 0'c character-code literal (a '0' not preceded by an identifier char)? */
    private static boolean isCharCodeLiteralQuote(StringBuilder buf) {
        int n = buf.length();
        if (n == 0 || buf.charAt(n - 1) != '0') return false;
        if (n == 1) return true;
        char p = buf.charAt(n - 2);
        return !(Character.isLetterOrDigit(p) || p == '_');
    }
    // END_CHANGE: ISS-2025-0408

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("Context-dependent built-in 'read' must be invoked with context");
    }
}
