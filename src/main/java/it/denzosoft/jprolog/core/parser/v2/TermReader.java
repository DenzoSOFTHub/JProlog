package it.denzosoft.jprolog.core.parser.v2;

import it.denzosoft.jprolog.core.operator.Operator;
import it.denzosoft.jprolog.core.operator.OperatorTable;
import it.denzosoft.jprolog.core.system.PrologFlags;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Clean-room ISO operator-precedence (Pratt) parser for the v2 parser. Consumes the
 * {@link Lexer} token stream and produces the existing {@link Term} AST, using the
 * shared {@link OperatorTable} for prefix/infix/postfix operators.
 *
 * <p>Correctly handles (the things the legacy parser gets wrong):
 * <ul>
 *   <li>canonical functor notation for operator symbols: {@code -(1,2)} is {@code -/2};
 *   <li>operator-as-atom: {@code X = -}, {@code foo(-, +)}, {@code p(a, -, b)};
 *   <li>postfix operators (xf/yf);
 *   <li>negative number literals ({@code -1} adjacent) vs prefix minus ({@code - X});
 *   <li>the {@code 0'c}, radix, quoted-atom and doubled-quote tokens (via the Lexer);
 *   <li>quote-aware clause splitting (clauses are split on {@code END} tokens).
 * </ul>
 */
public final class TermReader {

    private final List<Lexer.Token> tokens;
    private final OperatorTable ops;
    private int idx = 0;

    /** Variables shared within the term being read (one scope per top-level term/clause). */
    private final Map<String, Variable> varScope = new HashMap<>();

    // START_CHANGE: ISS-2025-0473 - engine v4 wave W7 (design B.11): an explicit nesting limit.
    // The reader is recursive descent, so deeply nested input used to blow the Java stack; the
    // resulting StackOverflowError was converted to resource_error(stack_overflow) by whichever
    // frame happened to catch it (and, inside term_to_atom/2 or read_term/2,3, could surface as a
    // plain failure). ISO wants a resource error naming the real resource, and the design requires
    // it never to be a silent failure: raise resource_error(parser_nesting) at a fixed depth well
    // below the JVM's own limit, so the error is deterministic and identical on every path.
    // START_CHANGE: ISS-2025-0561 - P3.8/P3.12: operator chains (a body of 10 000 goals, a long
    // ';' chain, 1+2+...+n) are parsed ITERATIVELY in parseOperators and no longer count as
    // nesting. Only real nesting (brackets, arguments, prefix operators) recurses. Up to
    // INLINE_NESTING levels are read on the caller's stack; deeper input is re-read from its
    // first token on a helper thread with a large stack, where the limit is MAX_NESTING.
    private static final int INLINE_NESTING = 1000;
    private static final int MAX_NESTING = 200_000;
    private static final long DEEP_STACK_BYTES = 1L << 30;   // reserved, committed only as used
    private int maxNesting = INLINE_NESTING;
    private boolean deepThread;
    /** Signals "re-read this term on the deep-stack thread" (no stack trace: control flow). */
    private static final class NeedsDeepStack extends RuntimeException {
        NeedsDeepStack() { super(null, null, false, false); }
    }
    private static final NeedsDeepStack NEEDS_DEEP = new NeedsDeepStack();
    // END_CHANGE: ISS-2025-0561
    private int depth = 0;
    // END_CHANGE: ISS-2025-0473

    // START_CHANGE: ISS-2025-0566 - the flags the reader depends on are thread-local in the
    // engine, so they are sampled on the CALLING thread at the start of every top-level read
    // (a deep re-read runs on another thread). null = sample PrologFlags.
    private String dqMode;
    private String bqMode = "codes";
    private String fixedDqMode;
    /** Read runtime terms with FRESH unnamed variable cells (read/1, term_to_atom/2 ...). */
    private boolean freshVariables;
    /** Variable names in order of first appearance, and their occurrence counts (read_term). */
    private final java.util.LinkedHashMap<String, Variable> namedOrder = new java.util.LinkedHashMap<>();
    private final Map<String, Integer> occurrences = new HashMap<>();
    private final List<Variable> allVariables = new ArrayList<>();

    /** Fix the double_quotes mode instead of reading the flag (read_term option double_quotes). */
    public TermReader withDoubleQuotes(String mode) { this.fixedDqMode = mode; return this; }
    /** Give every variable of a read term a fresh, unnamed cell (the name stays in variableNames). */
    public TermReader withFreshVariables(boolean b) { this.freshVariables = b; this.track = b; return this; }
    /** Record variable names/occurrences (read_term's options); off for consult (it is not free). */
    private boolean track;
    /** Named variables of the last term, in order of first appearance. */
    public java.util.LinkedHashMap<String, Variable> variableNames() { return namedOrder; }
    /** How often each named variable occurred in the last term. */
    public Map<String, Integer> variableOccurrences() { return occurrences; }
    /** Every variable of the last term (anonymous ones included), left to right. */
    public List<Variable> variables() { return allVariables; }

    private void sampleFlags() {
        if (fixedDqMode != null) { dqMode = fixedDqMode; return; }
        Term flagValue = PrologFlags.getFlag("double_quotes");
        dqMode = (flagValue instanceof Atom) ? ((Atom) flagValue).getName() : "codes";
        Term bq = PrologFlags.getFlag("back_quotes");
        bqMode = (bq instanceof Atom) ? ((Atom) bq).getName() : "codes";
    }

    /**
     * Read one term at priority 1200 from the current token, with the deep-stack fallback.
     * Every public entry point goes through here.
     */
    private Term readTop() {
        dqMode = null;                        // sampled lazily, on the first string (stringTerm)
        int start = idx;
        java.util.HashMap<String, Variable> scopeBefore =
            varScope.isEmpty() ? null : new java.util.HashMap<>(varScope);
        if (track) { namedOrder.clear(); occurrences.clear(); allVariables.clear(); }
        try {
            depth = 0;
            pTop = 0;
            return readTerm(1200);
        } catch (NeedsDeepStack | StackOverflowError e) {
            pTop = 0;
            idx = start;
            varScope.clear();
            if (scopeBefore != null) varScope.putAll(scopeBefore);
            if (track) { namedOrder.clear(); occurrences.clear(); allVariables.clear(); }
            sampleFlags();                    // on THIS thread: the flags are thread-local
            return readDeep();
        }
    }

    private Term readDeep() {
        final Object[] result = new Object[1];
        final Throwable[] failure = new Throwable[1];
        Thread t = new Thread(null, () -> {
            deepThread = true;
            maxNesting = MAX_NESTING;
            depth = 0;
            pTop = 0;
            try {
                result[0] = readTerm(1200);
            } catch (StackOverflowError so) {
                failure[0] = new it.denzosoft.jprolog.core.exceptions.PrologException(
                    it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.resourceError("parser_nesting", "read"));
            } catch (Throwable x) {
                failure[0] = x;
            } finally {
                deepThread = false;
                maxNesting = INLINE_NESTING;
            }
        }, "jprolog-deep-reader", DEEP_STACK_BYTES);
        t.setDaemon(true);
        t.start();
        boolean interrupted = false;
        for (;;) {
            try { t.join(); break; } catch (InterruptedException ie) { interrupted = true; }
        }
        if (interrupted) Thread.currentThread().interrupt();
        if (failure[0] instanceof RuntimeException) throw (RuntimeException) failure[0];
        if (failure[0] instanceof Error) throw (Error) failure[0];
        return (Term) result[0];
    }
    // END_CHANGE: ISS-2025-0566

    public TermReader(List<Lexer.Token> tokens, OperatorTable ops) {
        this.tokens = tokens;
        this.ops = ops != null ? ops : OperatorTable.getDefault();
    }

    // ====================================================================== facade

    /** Parse a single term (no trailing END required), e.g. a query. */
    public static Term parseTerm(String src, OperatorTable ops) {
        TermReader r = new TermReader(Lexer.tokenize(src), ops);
        return r.readSingle();
    }

    // START_CHANGE: ISS-2025-0566 - one term from a text (term_to_atom/2, read_term from a string):
    // an optional END, then nothing else.
    public Term readSingle() {
        Term t = readTop();
        if (peek().kind == Lexer.Kind.END) next();
        expectEof();
        return t;
    }
    // END_CHANGE: ISS-2025-0566

    /**
     * Read the next clause term, or null at end of input. Lets a consult driver parse a
     * program clause-by-clause and EXECUTE directives (e.g. {@code :- op(...)}) between
     * clauses, so later clauses see operators defined earlier. Each clause gets a fresh
     * variable scope. The shared {@link OperatorTable} is consulted live, so operators
     * added between calls take effect immediately.
     */
    public Term nextClause() {
        if (peek().kind == Lexer.Kind.EOF) return null;
        varScope.clear();
        Term t = readTop();   // ISS-2025-0561: deep-stack fallback
        if (peek().kind != Lexer.Kind.END) {
            throw err("operator expected (or missing '.')");
        }
        next(); // consume END
        return t;
    }

    /** True when no more tokens remain. */
    public boolean atEof() { return peek().kind == Lexer.Kind.EOF; }

    /** Source line of the next token (for diagnostics). */
    public int peekLine() { return peek().line; }

    /** After a clause parse error, skip tokens up to and including the next END, so the consult
     *  driver can continue with the following clause (error recovery / resync). */
    public void recover() {
        while (peek().kind != Lexer.Kind.EOF) {
            if (next().kind == Lexer.Kind.END) return;
        }
    }

    /** Split a program into clause terms (END-token-delimited), quote-aware. */
    public static List<Term> parseProgram(String src, OperatorTable ops) {
        TermReader r = new TermReader(Lexer.tokenize(src), ops);
        List<Term> clauses = new ArrayList<>();
        while (r.peek().kind != Lexer.Kind.EOF) {
            r.varScope.clear();
            Term t = r.readTop();   // ISS-2025-0561
            if (r.peek().kind != Lexer.Kind.END) {
                throw r.err("operator expected (or missing '.')");
            }
            r.next(); // consume END
            clauses.add(t);
        }
        return clauses;
    }

    // ====================================================================== core

    private Lexer.Token peek() { return tokens.get(idx); }
    private Lexer.Token peekNext() { return tokens.get(Math.min(idx + 1, tokens.size() - 1)); }
    private Lexer.Token next() { return tokens.get(idx++); }

    private void expectEof() {
        if (peek().kind != Lexer.Kind.EOF) throw err("unexpected token after term");
    }

    /** A parsed term together with its priority (for precedence checks). */
    private static final class Parsed {
        final Term term;
        final int prec;
        Parsed(Term term, int prec) { this.term = term; this.prec = prec; }
    }

    /** Read a term whose priority is at most {@code maxPrec}. */
    Term readTerm(int maxPrec) {
        return parse(maxPrec).term;
    }

    private Parsed parse(int maxPrec) {
        // ISS-2025-0473: deterministic nesting limit (see MAX_NESTING)
        if (++depth > maxNesting) {
            depth = 0;
            if (!deepThread) throw NEEDS_DEEP;   // ISS-2025-0561: re-read on the deep-stack thread
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.resourceError("parser_nesting", "read"));
        }
        try {
            return parse0(maxPrec);
        } finally {
            depth--;
        }
    }

    private Parsed parse0(int maxPrec) {
        Parsed left = parsePrimary(maxPrec);
        return parseOperators(left, maxPrec);
    }

    /** Consume infix/postfix operators (left-to-right, respecting precedence/associativity). */
    // START_CHANGE: ISS-2025-0561 - iterative: the right operand of an infix operator is read in
    // the same loop, with the pending (left, operator, outer priority) frames on an explicit
    // stack. Popping a frame restores the outer priority exactly as returning from the old
    // recursive parse(rightPrecedence) did, so the terms built are identical.
    // The pending frames live in four parallel arrays shared by the nested parseOperators calls
    // (each call works above its own base index), so an operator costs no allocation.
    private Term[] pLeft = new Term[16];
    private String[] pName = new String[16];
    private int[] pPrec = new int[16];
    private int[] pOuter = new int[16];
    private int pTop;

    private void pushPending(Term left, String name, int prec, int outerMax) {
        if (pTop == pLeft.length) {
            int n = pTop * 2;
            pLeft = java.util.Arrays.copyOf(pLeft, n);
            pName = java.util.Arrays.copyOf(pName, n);
            pPrec = java.util.Arrays.copyOf(pPrec, n);
            pOuter = java.util.Arrays.copyOf(pOuter, n);
        }
        pLeft[pTop] = left; pName[pTop] = name; pPrec[pTop] = prec; pOuter[pTop] = outerMax;
        pTop++;
    }

    private Parsed parseOperators(Parsed left, int maxPrec) {
        final int base = pTop;
        for (;;) {
            Operator op = peekInfixOrPostfix();
            if (op != null && op.getPrecedence() <= maxPrec && left.prec <= op.getLeftPrecedence()) {
                String name = op.getName();
                next(); // consume the operator token
                if (op.isInfix()) {
                    pushPending(left.term, name, op.getPrecedence(), maxPrec);
                    maxPrec = op.getRightPrecedence();
                    left = parsePrimary(maxPrec);
                } else { // postfix
                    left = new Parsed(new CompoundTerm(new Atom(name),
                            java.util.Arrays.asList(left.term)), op.getPrecedence());
                }
                continue;
            }
            if (pTop == base) return left;
            pTop--;
            left = new Parsed(new CompoundTerm(new Atom(pName[pTop]),
                    java.util.Arrays.asList(pLeft[pTop], left.term)), pPrec[pTop]);
            pLeft[pTop] = null;
            maxPrec = pOuter[pTop];
        }
    }
    // END_CHANGE: ISS-2025-0561

    /**
     * Describe the current token as an infix or postfix operator, or null if it is not in
     * operator position. COMMA and BAR are treated as the synthetic operators {@code ','/2}
     * (1000 xfy) and {@code ;/2} (1100 xfy) respectively.
     */
    private static final Operator COMMA_OP = new Operator(1000, Operator.Type.XFY, ",");
    private static final Operator BAR_OP = new Operator(1100, Operator.Type.XFY, ";");

    private Operator peekInfixOrPostfix() {
        Lexer.Token t = peek();
        switch (t.kind) {
            case COMMA: return COMMA_OP;     // ISS-2025-0561: shared, not one per token
            case BAR:   return BAR_OP;
            case ATOM: {
                Operator infix = ops.getInfixOperator(t.text);
                if (infix != null) return infix;
                return ops.getPostfixOperator(t.text);
            }
            default: return null;
        }
    }

    private Parsed parsePrimary(int maxPrec) {
        Lexer.Token t = peek();
        switch (t.kind) {
            case NUMBER:
                next();
                return new Parsed(t.number, 0);
            case VAR:
                next();
                return new Parsed(variable(t.text), 0);
            case STRING:
                next();
                return new Parsed(stringTerm(t.text), 0);
            // START_CHANGE: ISS-2025-0579 - `text` is a code list (SWI default back_quotes=codes)
            case BACKQUOTE:
                next();
                if (dqMode == null) sampleFlags();
                return new Parsed(stringTerm(t.text, "codes".equals(bqMode) || bqMode == null ? "codes"
                        : ("symbol_char".equals(bqMode) ? "atom" : bqMode)), 0);
            // END_CHANGE: ISS-2025-0579
            case LPAREN: {
                next();
                Term inner = readTerm(1200);
                expect(Lexer.Kind.RPAREN, ")");
                return new Parsed(inner, 0); // parenthesised term has priority 0
            }
            case LBRACKET:
                return new Parsed(parseList(), 0);
            case LBRACE:
                return new Parsed(parseBrace(), 0);
            case ATOM:
                return parseAtomOrPrefix(t, maxPrec);
            default:
                throw err("unexpected token '" + describe(t) + "'");
        }
    }

    private Parsed parseAtomOrPrefix(Lexer.Token t, int maxPrec) {
        String name = t.text;
        Lexer.Token after = peekNext();

        // (1) Canonical functor:  name '(' with NO layout  ->  compound application.
        // This must win over prefix-operator interpretation, so -(1,2) is -/2 (not -(','(1,2))).
        if (after.kind == Lexer.Kind.LPAREN && !after.precededByLayout) {
            next();                       // atom
            next();                       // '('
            List<Term> args = parseArgList();
            return new Parsed(new CompoundTerm(new Atom(name), args), 0);
        }

        // (2) Negative number literal:  '-' immediately adjacent to a number (ISO 6.3.1.2).
        // Only '-' forms a numeric literal; '+1' is the compound +(1), not the integer 1.
        if (!t.quotedAtom && name.equals("-")
                && after.kind == Lexer.Kind.NUMBER && !after.precededByLayout) {
            next();                       // '-'
            Lexer.Token numT = next();    // number
            return new Parsed(applySign(name, (Number) numT.number), 0);
        }

        // (3) Prefix operator — but only if a term can actually follow (else: operator-as-atom).
        // Quoting is transparent for operators (consistent with infix/postfix); the canStartTerm
        // guard still falls back to a plain atom when no operand follows (e.g. X = '-').
        Operator prefix = ops.getPrefixOperator(name);
        if (prefix != null && prefix.getPrecedence() <= maxPrec && canStartTerm(after)
                && !isInfixOnlyAtomFollow(after)) {
            next();                       // operator atom
            Parsed arg = parse(prefix.getRightPrecedence());
            return new Parsed(new CompoundTerm(new Atom(name),
                    java.util.Arrays.asList(arg.term)), prefix.getPrecedence());
        }
        // START_CHANGE: ISS-2025-0566 - SWI leniency: a prefix operator whose priority exceeds
        // the context (`X = \+a`, `f(:- a)`, `[dynamic p]`) is still applied, with its operand
        // read at the context priority and the result given that priority. Text that parsed
        // before is unaffected: this only turns a syntax error into the reading SWI gives.
        if (prefix != null && prefix.getPrecedence() > maxPrec && maxPrec < 1200 && canStartTerm(after)
                && !isInfixOnlyAtomFollow(after) && !(after.kind == Lexer.Kind.ATOM && isOperatorAtomToken(after))) {
            next();                       // operator atom
            Parsed arg = parse(Math.min(prefix.getRightPrecedence(), maxPrec));
            return new Parsed(new CompoundTerm(new Atom(name),
                    java.util.Arrays.asList(arg.term)), maxPrec);
        }
        // END_CHANGE: ISS-2025-0566

        // (4) Plain atom (includes an operator used as an atom: X = -, foo(-, +), ...).
        next();
        return new Parsed(new Atom(name), 0);
    }

    /** Arguments of a compound: parse(999) items separated by ',', until ')'. */
    private List<Term> parseArgList() {
        List<Term> args = new ArrayList<>();
        args.add(parse(999).term);
        while (peek().kind == Lexer.Kind.COMMA) {
            next();
            args.add(parse(999).term);
        }
        expect(Lexer.Kind.RPAREN, ")");
        return args;
    }

    private Term parseList() {
        next(); // '['
        if (peek().kind == Lexer.Kind.RBRACKET) {
            next();
            return new Atom("[]");
        }
        List<Term> elems = new ArrayList<>();
        elems.add(parse(999).term);
        while (peek().kind == Lexer.Kind.COMMA) {
            next();
            elems.add(parse(999).term);
        }
        Term tail = new Atom("[]");
        if (peek().kind == Lexer.Kind.BAR) {
            next();
            tail = parse(999).term;
        }
        expect(Lexer.Kind.RBRACKET, "]");
        Term list = tail;
        for (int i = elems.size() - 1; i >= 0; i--) {
            list = new CompoundTerm(new Atom("."), java.util.Arrays.asList(elems.get(i), list));
        }
        return list;
    }

    private Term parseBrace() {
        next(); // '{'
        if (peek().kind == Lexer.Kind.RBRACE) {
            next();
            return new Atom("{}");
        }
        Term inner = readTerm(1200);
        expect(Lexer.Kind.RBRACE, "}");
        return new CompoundTerm(new Atom("{}"), java.util.Arrays.asList(inner));
    }

    // ====================================================================== helpers

    /** True if the token can begin a term (used for the operator-as-atom fallback). */
    private boolean canStartTerm(Lexer.Token t) {
        switch (t.kind) {
            case NUMBER: case VAR: case STRING: case ATOM: case BACKQUOTE:
            case LPAREN: case LBRACKET: case LBRACE:
                return true;
            default:
                return false; // ) ] } , | END EOF  -> the operator is a plain atom
        }
    }

    /**
     * A following ATOM that is exclusively an infix/postfix operator (and not also a prefix
     * operator or a normal atom-as-operand) should NOT be swallowed as a prefix operand;
     * e.g. {@code (- = 1)} reads '-' as an atom. We keep this conservative: only block when
     * the next atom is an infix/postfix operator that is not itself prefix-capable and not
     * followed by '(' (functor).
     */
    private boolean isInfixOnlyAtomFollow(Lexer.Token after) {
        if (after.kind != Lexer.Kind.ATOM || after.quotedAtom) return false;
        // ISS-2025-0566: `- mod(X)` — an infix-operator NAME followed by '(' is a compound
        Lexer.Token next2 = tokens.get(Math.min(idx + 2, tokens.size() - 1));
        if (next2.kind == Lexer.Kind.LPAREN && !next2.precededByLayout) return false;
        boolean infixOrPostfix = ops.getInfixOperator(after.text) != null
                || ops.getPostfixOperator(after.text) != null;
        boolean prefix = ops.getPrefixOperator(after.text) != null;
        return infixOrPostfix && !prefix;
    }

    /** ISS-2025-0566: an unquoted atom token that is an infix or postfix operator. */
    private boolean isOperatorAtomToken(Lexer.Token t) {
        return !t.quotedAtom && (ops.getInfixOperator(t.text) != null || ops.getPostfixOperator(t.text) != null);
    }

    private Variable variable(String name) {
        if ("_".equals(name)) {
            Variable anon = new Variable("_"); // anonymous: a fresh variable each occurrence
            if (track) allVariables.add(anon); // ISS-2025-0566
            return anon;
        }
        Variable v = varScope.get(name);
        if (v == null) {
            // ISS-2025-0566: a runtime read gets fresh cells (two reads never share a name)
            v = freshVariables ? new Variable() : new Variable(name);
            varScope.put(name, v);
            if (track) {
                namedOrder.put(name, v);
                allVariables.add(v);
                occurrences.put(name, 1);
            }
        } else if (track) {
            Integer n = occurrences.get(name);
            occurrences.put(name, n == null ? 1 : n + 1);
        }
        return v;
    }

    private Term applySign(String sign, Number n) {
        if (!"-".equals(sign)) return n;
        if (n.isInteger()) {
            return new Number(n.bigIntegerValue().negate());
        }
        return new Number(-n.doubleValue(), false);
    }

    /** Convert a double-quoted string per the double_quotes flag (codes|chars|atom|string). */
    private Term stringTerm(String s) {
        if (dqMode == null) sampleFlags();                       // ISS-2025-0566: lazily, once per term
        return stringTerm(s, dqMode);
    }

    private Term stringTerm(String s, String mode) {
        switch (mode) {
            case "atom":
                return new Atom(s);
            case "string":
                return new PrologString(s);
            case "chars": {
                List<Term> chars = new ArrayList<>();
                int i = 0;
                while (i < s.length()) {
                    int cp = s.codePointAt(i);
                    chars.add(new Atom(new String(Character.toChars(cp))));
                    i += Character.charCount(cp);
                }
                return buildList(chars);
            }
            case "codes":
            default: {
                List<Term> codes = new ArrayList<>();
                int i = 0;
                while (i < s.length()) {
                    int cp = s.codePointAt(i);
                    codes.add(new Number((long) cp));
                    i += Character.charCount(cp);
                }
                return buildList(codes);
            }
        }
    }

    private Term buildList(List<Term> elems) {
        Term list = new Atom("[]");
        for (int i = elems.size() - 1; i >= 0; i--) {
            list = new CompoundTerm(new Atom("."), java.util.Arrays.asList(elems.get(i), list));
        }
        return list;
    }

    private void expect(Lexer.Kind kind, String what) {
        if (peek().kind != kind) {
            throw err("expected '" + what + "' but found '" + describe(peek()) + "'");
        }
        next();
    }

    private static String describe(Lexer.Token t) {
        if (t.kind == Lexer.Kind.NUMBER) return String.valueOf(t.number);
        if (t.kind == Lexer.Kind.EOF) return "<end of input>";
        return t.text != null ? t.text : t.kind.toString();
    }

    private ParseException err(String msg) {
        Lexer.Token t = peek();
        return new ParseException(msg + " at line " + t.line);
    }

    /** Parse error (unchecked, mirrors the lexer's exception style). */
    public static final class ParseException extends RuntimeException {
        public ParseException(String message) { super(message); }
    }
}
