package it.denzosoft.jprolog.core.parser.v2;

import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

/**
 * Clean-room ISO 13211-1 tokenizer for the v2 parser. Produces a flat token stream
 * from Prolog source text. Unlike the legacy {@code TermParser}, tokenization is a
 * single pass that fully handles quoted atoms / strings (including {@code ''} and
 * {@code ""} doubled-quote escapes and backslash escapes), character-code literals
 * ({@code 0'c}), radix integers ({@code 0x}/{@code 0o}/{@code 0b}), floats, graphic
 * (symbolic) atoms, and end-tokens — so clause splitting becomes quote-aware "for free".
 */
public final class Lexer {

    /** ISO "graphic char" set; a maximal run of these forms a symbolic atom. */
    private static final String GRAPHIC = "#$&*+-./:<=>?@^~\\";

    public enum Kind {
        ATOM, VAR, NUMBER, STRING,
        LPAREN, RPAREN, LBRACKET, RBRACKET, LBRACE, RBRACE,
        COMMA, BAR, END, EOF
    }

    /** A single token. {@code number} is set only for {@link Kind#NUMBER}. */
    public static final class Token {
        public final Kind kind;
        public final String text;          // atom/var name; for STRING, the decoded content
        public final Term number;          // for NUMBER
        public final boolean precededByLayout;
        public final boolean quotedAtom;   // ATOM that came from '...' (never an operator/end)
        public final int pos;
        public final int line;

        Token(Kind kind, String text, Term number, boolean precededByLayout,
              boolean quotedAtom, int pos, int line) {
            this.kind = kind;
            this.text = text;
            this.number = number;
            this.precededByLayout = precededByLayout;
            this.quotedAtom = quotedAtom;
            this.pos = pos;
            this.line = line;
        }

        @Override public String toString() {
            return kind + "(" + (number != null ? number : text) + ")";
        }
    }

    private final String src;
    private int pos = 0;
    private int line = 1;

    public Lexer(String src) {
        this.src = src;
    }

    public static List<Token> tokenize(String src) {
        Lexer lex = new Lexer(src);
        List<Token> tokens = new ArrayList<>();
        Token t;
        do {
            t = lex.next();
            tokens.add(t);
        } while (t.kind != Kind.EOF);
        return tokens;
    }

    private char peek() { return pos < src.length() ? src.charAt(pos) : '\0'; }
    private char peek(int k) { return pos + k < src.length() ? src.charAt(pos + k) : '\0'; }
    private boolean eof() { return pos >= src.length(); }

    private char advance() {
        char c = src.charAt(pos++);
        if (c == '\n') line++;
        return c;
    }

    /** Skip layout (whitespace + line/block comments). Returns true if anything was skipped. */
    private boolean skipLayout() {
        boolean skipped = false;
        while (!eof()) {
            char c = peek();
            if (c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\f') {
                advance(); skipped = true;
            } else if (c == '%') {                       // line comment
                while (!eof() && peek() != '\n') advance();
                skipped = true;
            } else if (c == '/' && peek(1) == '*') {     // block comment
                advance(); advance();
                while (!eof() && !(peek() == '*' && peek(1) == '/')) advance();
                if (!eof()) { advance(); advance(); }
                skipped = true;
            } else {
                break;
            }
        }
        return skipped;
    }

    public Token next() {
        boolean layout = skipLayout();
        int start = pos, startLine = line;
        if (eof()) return new Token(Kind.EOF, "", null, layout, false, start, startLine);

        char c = peek();

        // ---- punctuation / structure ----
        switch (c) {
            case '(': advance(); return tok(Kind.LPAREN, "(", layout, start, startLine);
            case ')': advance(); return tok(Kind.RPAREN, ")", layout, start, startLine);
            case '[': advance(); return tok(Kind.LBRACKET, "[", layout, start, startLine);
            case ']': advance(); return tok(Kind.RBRACKET, "]", layout, start, startLine);
            case '{': advance(); return tok(Kind.LBRACE, "{", layout, start, startLine);
            case '}': advance(); return tok(Kind.RBRACE, "}", layout, start, startLine);
            case ',': advance(); return tok(Kind.COMMA, ",", layout, start, startLine);
            case '|':
                // '||' is a graphic-ish atom in some dialects; ISO treats single '|' specially.
                advance(); return tok(Kind.BAR, "|", layout, start, startLine);
            case '!': advance(); return atom("!", layout, false, start, startLine);
            case ';': advance(); return atom(";", layout, false, start, startLine);
            default: break;
        }

        // ---- end token:  '.' followed by layout / EOF / '%'  ----
        if (c == '.') {
            char n = peek(1);
            if (n == '\0' || n == ' ' || n == '\t' || n == '\r' || n == '\n' || n == '\f' || n == '%') {
                advance();
                return tok(Kind.END, ".", layout, start, startLine);
            }
            // otherwise '.' is a graphic char (part of a symbolic atom, e.g. =.. or the cons '.')
        }

        // ---- numbers ----
        if (Character.isDigit(c)) {
            return number(layout, start, startLine);
        }

        // ---- variables (uppercase / underscore start) ----
        if (c == '_' || Character.isUpperCase(c)) {
            StringBuilder sb = new StringBuilder();
            while (!eof() && isIdentChar(peek())) sb.append(advance());
            return tok2(Kind.VAR, sb.toString(), layout, false, start, startLine);
        }

        // ---- unquoted alphanumeric atoms (lowercase letter start) ----
        if (Character.isLetter(c)) {
            StringBuilder sb = new StringBuilder();
            while (!eof() && isIdentChar(peek())) sb.append(advance());
            return atom(sb.toString(), layout, false, start, startLine);
        }

        // ---- quoted atom ----
        if (c == '\'') {
            String content = readQuoted('\'');
            return atom(content, layout, true, start, startLine);
        }

        // ---- string ----
        if (c == '"') {
            String content = readQuoted('"');
            return tok2(Kind.STRING, content, layout, false, start, startLine);
        }

        // ---- back-quoted (treat like a string) ----
        if (c == '`') {
            String content = readQuoted('`');
            return tok2(Kind.STRING, content, layout, false, start, startLine);
        }

        // ---- symbolic (graphic) atoms ----
        if (GRAPHIC.indexOf(c) >= 0) {
            StringBuilder sb = new StringBuilder();
            while (!eof() && GRAPHIC.indexOf(peek()) >= 0) sb.append(advance());
            return atom(sb.toString(), layout, false, start, startLine);
        }

        throw new LexException("Unexpected character '" + c + "'", line, pos);
    }

    // ------------------------------------------------------------------ helpers

    private static boolean isIdentChar(char c) {
        return c == '_' || Character.isLetterOrDigit(c);
    }

    private Token tok(Kind k, String text, boolean layout, int start, int startLine) {
        return new Token(k, text, null, layout, false, start, startLine);
    }
    private Token tok2(Kind k, String text, boolean layout, boolean quoted, int start, int startLine) {
        return new Token(k, text, null, layout, quoted, start, startLine);
    }
    private Token atom(String name, boolean layout, boolean quoted, int start, int startLine) {
        return new Token(Kind.ATOM, name, null, layout, quoted, start, startLine);
    }
    private Token numTok(Term n, boolean layout, int start, int startLine) {
        return new Token(Kind.NUMBER, null, n, layout, false, start, startLine);
    }

    private Token number(boolean layout, int start, int startLine) {
        // Special integer forms beginning with 0
        if (peek() == '0') {
            char n = peek(1);
            if (n == '\'') {                       // 0'c  character code
                advance(); advance();              // consume 0 and '
                int code = readCharCode();
                return numTok(new Number((long) code), layout, start, startLine);
            }
            if (n == 'x' || n == 'X') return radix(16, layout, start, startLine);
            if (n == 'o' || n == 'O') return radix(8, layout, start, startLine);
            if (n == 'b' || n == 'B') return radix(2, layout, start, startLine);
        }

        StringBuilder sb = new StringBuilder();
        while (!eof() && Character.isDigit(peek())) sb.append(advance());

        boolean isFloat = false;
        // fractional part: '.' must be followed by a digit
        if (peek() == '.' && Character.isDigit(peek(1))) {
            isFloat = true;
            sb.append(advance());                  // '.'
            while (!eof() && Character.isDigit(peek())) sb.append(advance());
        }
        // exponent
        if ((peek() == 'e' || peek() == 'E')
                && (Character.isDigit(peek(1))
                    || ((peek(1) == '+' || peek(1) == '-') && Character.isDigit(peek(2))))) {
            isFloat = true;
            sb.append(advance());                  // e/E
            if (peek() == '+' || peek() == '-') sb.append(advance());
            while (!eof() && Character.isDigit(peek())) sb.append(advance());
        }

        if (isFloat) {
            double d = Double.parseDouble(sb.toString());
            if (Double.isInfinite(d)) throw new LexException("floating point literal overflow", line, pos);
            return numTok(new Number(d, false), layout, start, startLine);
        }
        return numTok(new Number(new BigInteger(sb.toString())), layout, start, startLine);
    }

    private Token radix(int base, boolean layout, int start, int startLine) {
        advance(); advance(); // consume 0 and x/o/b
        StringBuilder sb = new StringBuilder();
        while (!eof() && Character.digit(peek(), base) >= 0) sb.append(advance());
        if (sb.length() == 0) {
            throw new LexException("Expected base-" + base + " digits", line, pos);
        }
        return numTok(new Number(new BigInteger(sb.toString(), base)), layout, start, startLine);
    }

    /** Read the character code after {@code 0'}: a plain char, an escape, or '' for a quote. */
    private int readCharCode() {
        if (eof()) throw new LexException("Incomplete 0' character literal", line, pos);
        char c = peek();
        if (c == '\\') {
            advance();
            return readEscape('\0');               // escape; no terminating quote context
        }
        if (c == '\'' && peek(1) == '\'') {        // 0'' -> the quote character
            advance(); advance();
            return '\'';
        }
        int cp = src.codePointAt(pos);
        pos += Character.charCount(cp);
        return cp;
    }

    /** Read a quoted token body (after recognising the opening quote), returning decoded content. */
    private String readQuoted(char q) {
        advance(); // opening quote
        StringBuilder sb = new StringBuilder();
        while (true) {
            if (eof()) throw new LexException("Unterminated quoted " + (q == '"' ? "string" : "atom"), line, pos);
            char c = peek();
            if (c == q) {
                if (peek(1) == q) {                // doubled quote -> single quote char
                    advance(); advance();
                    sb.append(q);
                    continue;
                }
                advance();                         // closing quote
                break;
            }
            if (c == '\\') {
                advance();
                if (peek() == '\n') { advance(); continue; }   // line continuation
                int cp = readEscape(q);
                if (cp >= 0) sb.appendCodePoint(cp);
                continue;
            }
            int cp = src.codePointAt(pos);
            pos += Character.charCount(cp);
            if (cp == '\n') line++;
            sb.appendCodePoint(cp);
        }
        return sb.toString();
    }

    /** Read a backslash escape (the backslash already consumed). Returns a code point, or -1 to skip. */
    private int readEscape(char quote) {
        if (eof()) throw new LexException("Incomplete escape", line, pos);
        char e = advance();
        switch (e) {
            case 'n': return '\n';
            case 't': return '\t';
            case 'r': return '\r';
            case 'a': return 7;
            case 'b': return '\b';
            case 'f': return '\f';
            case 'v': return 11;
            case '0': case '1': case '2': case '3':
            case '4': case '5': case '6': case '7': {
                // octal: \NNN\   (first octal digit already consumed)
                long val = e - '0';
                while (!eof() && peek() >= '0' && peek() <= '7') {
                    val = val * 8 + (advance() - '0');
                    if (val > Character.MAX_CODE_POINT) throw new LexException("octal character escape out of range", line, pos);
                }
                if (peek() == '\\') advance();
                return (int) val;
            }
            case 'x': {
                long val = 0; int digits = 0;
                while (!eof() && Character.digit(peek(), 16) >= 0) {
                    val = val * 16 + Character.digit(advance(), 16);
                    digits++;
                    if (val > Character.MAX_CODE_POINT) throw new LexException("\\x character escape out of range", line, pos);
                }
                if (digits == 0) throw new LexException("missing hex digits after \\x", line, pos);
                if (peek() == '\\') advance();
                return (int) val;
            }
            case '\\': return '\\';
            case '\'': return '\'';
            case '"': return '"';
            case '`': return '`';
            default:
                // Unknown escape: keep the character literally (lenient).
                return e;
        }
    }

    /** Lexing error (unchecked so it composes with the parser's exception flow). */
    public static final class LexException extends RuntimeException {
        public final int line;
        public final int pos;
        public LexException(String message, int line, int pos) {
            super(message + " at line " + line);
            this.line = line;
            this.pos = pos;
        }
    }
}
