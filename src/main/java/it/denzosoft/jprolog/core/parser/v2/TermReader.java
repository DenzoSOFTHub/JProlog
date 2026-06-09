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

    public TermReader(List<Lexer.Token> tokens, OperatorTable ops) {
        this.tokens = tokens;
        this.ops = ops != null ? ops : OperatorTable.getDefault();
    }

    // ====================================================================== facade

    /** Parse a single term (no trailing END required), e.g. a query. */
    public static Term parseTerm(String src, OperatorTable ops) {
        TermReader r = new TermReader(Lexer.tokenize(src), ops);
        Term t = r.readTerm(1200);
        if (r.peek().kind == Lexer.Kind.END) r.next();
        r.expectEof();
        return t;
    }

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
        Term t = readTerm(1200);
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
            Term t = r.readTerm(1200);
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
        Parsed left = parsePrimary(maxPrec);
        return parseOperators(left, maxPrec);
    }

    /** Consume infix/postfix operators (left-to-right, respecting precedence/associativity). */
    private Parsed parseOperators(Parsed left, int maxPrec) {
        for (;;) {
            Operator op = peekInfixOrPostfix();
            if (op == null) break;
            if (op.getPrecedence() > maxPrec) break;
            if (left.prec > op.getLeftPrecedence()) break;

            String name = op.getName();
            next(); // consume the operator token
            if (op.isInfix()) {
                Parsed right = parse(op.getRightPrecedence());
                left = new Parsed(new CompoundTerm(new Atom(name),
                        java.util.Arrays.asList(left.term, right.term)), op.getPrecedence());
            } else { // postfix
                left = new Parsed(new CompoundTerm(new Atom(name),
                        java.util.Arrays.asList(left.term)), op.getPrecedence());
            }
        }
        return left;
    }

    /**
     * Describe the current token as an infix or postfix operator, or null if it is not in
     * operator position. COMMA and BAR are treated as the synthetic operators {@code ','/2}
     * (1000 xfy) and {@code ;/2} (1100 xfy) respectively.
     */
    private Operator peekInfixOrPostfix() {
        Lexer.Token t = peek();
        switch (t.kind) {
            case COMMA: return new Operator(1000, Operator.Type.XFY, ",");
            case BAR:   return new Operator(1100, Operator.Type.XFY, ";");
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
            case NUMBER: case VAR: case STRING: case ATOM:
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
        boolean infixOrPostfix = ops.getInfixOperator(after.text) != null
                || ops.getPostfixOperator(after.text) != null;
        boolean prefix = ops.getPrefixOperator(after.text) != null;
        return infixOrPostfix && !prefix;
    }

    private Variable variable(String name) {
        if ("_".equals(name)) {
            return new Variable("_"); // anonymous: a fresh variable each occurrence
        }
        Variable v = varScope.get(name);
        if (v == null) {
            v = new Variable(name);
            varScope.put(name, v);
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
        Term flagValue = PrologFlags.getFlag("double_quotes");
        String mode = (flagValue instanceof Atom) ? ((Atom) flagValue).getName() : "codes";
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
