// START_CHANGE: ISS-2025-0085 - Unified operator system via shared OperatorTable
package it.denzosoft.jprolog.core.parser;

import it.denzosoft.jprolog.core.exceptions.PrologParserException;
import it.denzosoft.jprolog.core.operator.Operator;
import it.denzosoft.jprolog.core.operator.OperatorTable;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.util.TermUtils;

import java.util.ArrayList;
import java.util.List;


public class TermParser {
    private java.lang.String input;
    private int position;
    private int line;
    private int column;

    private final OperatorTable operatorTable;

    public TermParser() {
        this(new OperatorTable());
    }

    public TermParser(OperatorTable operatorTable) {
        this.operatorTable = operatorTable;
    }

    public OperatorTable getOperatorTable() {
        return operatorTable;
    }

    public Term parseTerm(java.lang.String input) throws PrologParserException {
        this.input = input;
        this.position = 0;
        this.line = 1;
        this.column = 1;

        Term result = parseExpression(1200);
        skipWhitespace();
        if (position < input.length()) {
            throw new PrologParserException("Unexpected token at line " + line + ", column " + column + ": " + currentChar());
        }
        return result;
    }

    private void skipWhitespace() {
        while (position < input.length() && Character.isWhitespace(currentChar())) {
            if (currentChar() == '\n') {
                line++;
                column = 1;
            } else {
                column++;
            }
            position++;
        }
    }

    private char currentChar() {
        if (position >= input.length()) {
            return '\0';
        }
        return input.charAt(position);
    }

    private char nextChar() {
        position++;
        if (position < input.length() && input.charAt(position-1) == '\n') {
            line++;
            column = 1;
        } else {
            column++;
        }
        return currentChar();
    }

    private java.lang.String peekToken() throws PrologParserException {
        int savedPosition = position;
        int savedLine = line;
        int savedColumn = column;
        java.lang.String token = readToken();
        position = savedPosition;
        line = savedLine;
        column = savedColumn;
        return token;
    }

    private java.lang.String readToken() throws PrologParserException {
        skipWhitespace();
        if (position >= input.length()) {
            return null;
        }

        char c = currentChar();

        // Handle special characters
        if (c == '(' || c == ')' || c == '[' || c == ']' || c == '|' || c == '.') {
            nextChar();
            return java.lang.String.valueOf(c);
        }

        // Handle string literals (double quotes)
        if (c == '"') {
            StringBuilder token = new StringBuilder();
            token.append(c);
            nextChar();
            while (position < input.length() && currentChar() != '"') {
                if (currentChar() == '\\') {
                    token.append(currentChar());
                    nextChar();
                    if (position < input.length()) {
                        token.append(currentChar());
                        nextChar();
                    }
                } else {
                    token.append(currentChar());
                    nextChar();
                }
            }
            if (position >= input.length()) {
                throw new PrologParserException("Unterminated string literal at line " + line + ", column " + column);
            }
            token.append(currentChar()); // closing quote
            nextChar();
            return token.toString();
        }

        // Handle quoted atoms
        if (c == '\'') {
            StringBuilder token = new StringBuilder();
            token.append(c);
            nextChar();
            while (position < input.length() && currentChar() != '\'') {
                if (currentChar() == '\\') {
                    token.append(currentChar());
                    nextChar();
                }
                token.append(currentChar());
                nextChar();
            }
            if (position >= input.length()) {
                throw new PrologParserException("Unterminated quoted atom at line " + line + ", column " + column);
            }
            token.append(currentChar()); // closing quote
            nextChar();
            return token.toString();
        }

        // Handle identifiers (letters, digits, underscore) - read full word
        // Alphabetic operators (is, mod, rem, xor, etc.) are read as identifiers;
        // the expression parser decides if they are operators based on context.
        if (Character.isLetter(c) || c == '_') {
            StringBuilder token = new StringBuilder();
            while (position < input.length() &&
                   (Character.isLetterOrDigit(currentChar()) || currentChar() == '_')) {
                token.append(currentChar());
                nextChar();
            }
            return token.toString();
        }

        // Handle numbers starting with digit
        if (Character.isDigit(c)) {
            // Don't consume here - let caller handle via parseNumber
            // Just return the digit sequence as a token
            StringBuilder token = new StringBuilder();
            while (position < input.length() && Character.isDigit(currentChar())) {
                token.append(currentChar());
                nextChar();
            }
            return token.toString();
        }

        // Handle symbolic operators: greedy longest match against OperatorTable
        if (isSymbolicChar(c)) {
            // Read all consecutive symbolic characters, also including '.' for =..
            StringBuilder allSymbols = new StringBuilder();
            int tempPos = position;
            while (tempPos < input.length()) {
                char ch = input.charAt(tempPos);
                if (isSymbolicChar(ch) || (ch == '.' && allSymbols.length() > 0)) {
                    allSymbols.append(ch);
                    tempPos++;
                } else {
                    break;
                }
            }

            // Find the longest prefix that matches a known operator
            java.lang.String candidate = allSymbols.toString();
            java.lang.String bestMatch = null;

            for (java.lang.String knownOp : operatorTable.getAllOperatorNames()) {
                if (!isAlphabetic(knownOp) && candidate.startsWith(knownOp) &&
                    (bestMatch == null || knownOp.length() > bestMatch.length())) {
                    bestMatch = knownOp;
                }
            }

            if (bestMatch != null) {
                for (int i = 0; i < bestMatch.length(); i++) {
                    nextChar();
                }
                return bestMatch;
            } else {
                // Single character fallback
                nextChar();
                return java.lang.String.valueOf(c);
            }
        }

        // Fallback: single character
        nextChar();
        return java.lang.String.valueOf(c);
    }

    /**
     * Check if a character is a symbolic (non-alphanumeric) operator character.
     */
    private boolean isSymbolicChar(char c) {
        switch (c) {
            case '+': case '-': case '*': case '/': case '\\':
            case '^': case '<': case '>': case '=': case ':':
            case '?': case '@': case '#': case '~': case '!':
            case ';': case ',': case '&':
                return true;
            default:
                return false;
        }
    }

    /**
     * Check if an operator name is purely alphabetic (like 'is', 'mod', 'rem', 'xor').
     */
    private boolean isAlphabetic(java.lang.String name) {
        if (name == null || name.isEmpty()) return false;
        for (int i = 0; i < name.length(); i++) {
            if (!Character.isLetter(name.charAt(i)) && name.charAt(i) != '_') {
                return false;
            }
        }
        return true;
    }

    private Term parseExpression(int maxPrecedence) throws PrologParserException {
        // Check for prefix operators first
        int savedPosition = position;
        int savedLine = line;
        int savedColumn = column;
        skipWhitespace();

        java.lang.String token = peekToken();
        if (token != null) {
            Operator prefixOp = operatorTable.getPrefixOperator(token);
            if (prefixOp != null && prefixOp.getPrecedence() <= maxPrecedence) {
                // For '-' or '+': if immediately followed by digit, treat as number not prefix
                if (("-".equals(token) || "+".equals(token))) {
                    int sp = position;
                    int sl = line;
                    int sc = column;
                    skipWhitespace();
                    // Check if '-'/'+'  is immediately followed by a digit (no space)
                    if (position + 1 < input.length() && Character.isDigit(input.charAt(position + 1))) {
                        // It's a negative/positive number — fall through to parsePrimary
                        position = savedPosition;
                        line = savedLine;
                        column = savedColumn;
                        Term left = parsePrimary();
                        return parseInfixOperators(left, maxPrecedence, 0);
                    }
                    position = sp; line = sl; column = sc;
                }

                // If the prefix token is alphabetic and followed by '(', it's a functor not a prefix
                if (isAlphabetic(token)) {
                    int sp = position;
                    int sl = line;
                    int sc = column;
                    skipWhitespace();
                    readToken(); // consume the token
                    skipWhitespace();
                    boolean parenFollows = position < input.length() && currentChar() == '(';
                    position = sp; line = sl; column = sc;
                    if (parenFollows) {
                        position = savedPosition;
                        line = savedLine;
                        column = savedColumn;
                        Term left = parsePrimary();
                        return parseInfixOperators(left, maxPrecedence, 0);
                    }
                }

                position = savedPosition;
                line = savedLine;
                column = savedColumn;

                readToken(); // Consume the prefix operator
                Term operand = parseExpression(prefixOp.getRightPrecedence());

                List<Term> args = new ArrayList<>();
                args.add(operand);
                Term left = new CompoundTerm(new Atom(token), args);

                return parseInfixOperators(left, maxPrecedence, prefixOp.getPrecedence());
            }
        }

        // Restore position and parse primary
        position = savedPosition;
        line = savedLine;
        column = savedColumn;

        Term left = parsePrimary();
        return parseInfixOperators(left, maxPrecedence, 0);
    }

    /**
     * Parse infix operators with correct associativity handling.
     *
     * @param left The left operand already parsed
     * @param maxPrecedence The maximum allowed precedence for this context
     * @param leftPrec The effective precedence of the left operand (0 for atoms/primary terms)
     */
    private Term parseInfixOperators(Term left, int maxPrecedence, int leftPrec) throws PrologParserException {
        while (true) {
            int savedPosition = position;
            int savedLine = line;
            int savedColumn = column;
            skipWhitespace();

            java.lang.String token = peekToken();
            if (token == null) break;

            Operator infixOp = operatorTable.getInfixOperator(token);
            if (infixOp == null) break;

            // Check if this operator can appear in the current context
            if (infixOp.getPrecedence() > maxPrecedence) break;

            // Check left-side associativity constraint
            if (leftPrec > infixOp.getLeftPrecedence()) break;

            // For alphabetic operators in infix position, ensure word boundary
            // (the token after parsePrimary must be a full word match)

            // Consume the operator
            position = savedPosition;
            line = savedLine;
            column = savedColumn;
            readToken();

            // Parse right side with proper associativity
            Term right = parseExpression(infixOp.getRightPrecedence());

            List<Term> args = new ArrayList<>();
            args.add(left);
            args.add(right);
            left = new CompoundTerm(new Atom(infixOp.getName()), args);
            leftPrec = infixOp.getPrecedence();
        }

        return left;
    }

    private Term parsePrimary() throws PrologParserException {
        skipWhitespace();
        if (position >= input.length()) {
            throw new PrologParserException("Unexpected end of input at line " + line + ", column " + column);
        }

        char c = currentChar();

        if (c == '[') {
            return parseList();
        } else if (c == '{') {
            return parseBraces();
        } else if (c == '(') {
            nextChar(); // consume '('
            Term term = parseExpression(1200);
            skipWhitespace();
            if (currentChar() != ')') {
                throw new PrologParserException("Expected ')' at line " + line + ", column " + column);
            }
            nextChar(); // consume ')'
            return term;
        } else if (c == '\'') {
            // Quoted atom like 'hello'
            return parseQuotedAtomOrCompound();
        } else if (c == '"') {
            return parseString();
        } else if (Character.isDigit(c) || (c == '-' && position + 1 < input.length() && Character.isDigit(input.charAt(position + 1)))) {
            return parseNumber();
        } else if (Character.isUpperCase(c) || c == '_') {
            return parseVariable();
        } else if (Character.isLetter(c)) {
            // Read the full identifier
            int savedPosition = position;
            int savedLine = line;
            int savedColumn = column;

            java.lang.String name = readIdentifier();

            skipWhitespace();
            if (currentChar() == '(') {
                // It's a compound term (functor with arguments)
                nextChar(); // consume '('
                List<Term> arguments = new ArrayList<>();
                skipWhitespace();

                if (currentChar() != ')') {
                    do {
                        arguments.add(parseExpression(999));
                        skipWhitespace();
                        if (currentChar() == ',') {
                            nextChar(); // consume ','
                            skipWhitespace();
                        } else {
                            break;
                        }
                    } while (currentChar() != ')');
                }

                if (currentChar() != ')') {
                    throw new PrologParserException("Expected ')' at line " + line + ", column " + column);
                }
                nextChar(); // consume ')'

                return new CompoundTerm(new Atom(name), arguments);
            } else {
                // It's just an atom (the identifier was already consumed)
                return new Atom(name);
            }
        } else if (isSymbolicChar(c)) {
            // Symbolic atom or operator used as functor (e.g., =..(T, L))
            return parseSymbolicAtomOrCompound();
        } else {
            throw new PrologParserException("Unexpected character at line " + line + ", column " + column + ": " + c);
        }
    }

    /**
     * Read a full alphabetic identifier (letters, digits, underscore).
     */
    private java.lang.String readIdentifier() {
        StringBuilder name = new StringBuilder();
        while (position < input.length() &&
               (Character.isLetterOrDigit(currentChar()) || currentChar() == '_')) {
            name.append(currentChar());
            nextChar();
        }
        return name.toString();
    }

    /**
     * Parse a quoted atom or compound term (e.g., 'hello' or 'hello'(X)).
     */
    private Term parseQuotedAtomOrCompound() throws PrologParserException {
        java.lang.String name = parseAtomName(); // handles quoted atoms

        skipWhitespace();
        if (currentChar() == '(') {
            nextChar(); // consume '('
            List<Term> arguments = new ArrayList<>();
            skipWhitespace();

            if (currentChar() != ')') {
                do {
                    arguments.add(parseExpression(999));
                    skipWhitespace();
                    if (currentChar() == ',') {
                        nextChar();
                        skipWhitespace();
                    } else {
                        break;
                    }
                } while (currentChar() != ')');
            }

            if (currentChar() != ')') {
                throw new PrologParserException("Expected ')' at line " + line + ", column " + column);
            }
            nextChar(); // consume ')'

            return new CompoundTerm(new Atom(name), arguments);
        } else {
            return new Atom(name);
        }
    }

    /**
     * Parse a symbolic atom or compound term (e.g., =..(T, L) or just +).
     */
    private Term parseSymbolicAtomOrCompound() throws PrologParserException {
        // Read the symbolic token
        java.lang.String name = readSymbolicToken();

        skipWhitespace();
        if (currentChar() == '(') {
            nextChar(); // consume '('
            List<Term> arguments = new ArrayList<>();
            skipWhitespace();

            if (currentChar() != ')') {
                do {
                    arguments.add(parseExpression(999));
                    skipWhitespace();
                    if (currentChar() == ',') {
                        nextChar(); // consume ','
                        skipWhitespace();
                    } else {
                        break;
                    }
                } while (currentChar() != ')');
            }

            if (currentChar() != ')') {
                throw new PrologParserException("Expected ')' at line " + line + ", column " + column);
            }
            nextChar(); // consume ')'

            return new CompoundTerm(new Atom(name), arguments);
        } else {
            return new Atom(name);
        }
    }

    /**
     * Read a symbolic token (sequence of symbolic characters).
     * Tries to match the longest known operator; falls back to reading all symbolic chars.
     */
    private java.lang.String readSymbolicToken() {
        // Read all consecutive symbolic chars, also including '.' for operators like =..
        StringBuilder allSymbols = new StringBuilder();
        int tempPos = position;
        while (tempPos < input.length()) {
            char ch = input.charAt(tempPos);
            if (isSymbolicChar(ch) || (ch == '.' && allSymbols.length() > 0)) {
                allSymbols.append(ch);
                tempPos++;
            } else {
                break;
            }
        }

        // Find the longest operator match
        java.lang.String candidate = allSymbols.toString();
        java.lang.String bestMatch = null;

        for (java.lang.String knownOp : operatorTable.getAllOperatorNames()) {
            if (!isAlphabetic(knownOp) && candidate.startsWith(knownOp) &&
                (bestMatch == null || knownOp.length() > bestMatch.length())) {
                bestMatch = knownOp;
            }
        }

        if (bestMatch != null) {
            for (int i = 0; i < bestMatch.length(); i++) {
                nextChar();
            }
            return bestMatch;
        } else {
            // Fallback: just read the symbolic chars (not dots)
            StringBuilder fallback = new StringBuilder();
            while (position < input.length() && isSymbolicChar(currentChar())) {
                fallback.append(currentChar());
                nextChar();
            }
            return fallback.length() > 0 ? fallback.toString() : java.lang.String.valueOf(allSymbols.charAt(0));
        }
    }

    private java.lang.String parseAtomName() throws PrologParserException {
        skipWhitespace();
        StringBuilder name = new StringBuilder();

        if (currentChar() == '\'') {
            // START_CHANGE: ISS-2025-0059 - Fix quoted atom escape processing
            nextChar(); // consume opening quote
            while (position < input.length() && currentChar() != '\'') {
                if (currentChar() == '\\') {
                    nextChar(); // consume backslash
                    if (position >= input.length()) break;
                    name.append(processEscapeChar(currentChar()));
                } else {
                    name.append(currentChar());
                }
                nextChar();
            }
            // END_CHANGE: ISS-2025-0059
            if (position >= input.length()) {
                throw new PrologParserException("Unterminated quoted atom at line " + line + ", column " + column);
            }
            nextChar(); // consume closing quote
        } else if (Character.isLetter(currentChar())) {
            do {
                name.append(currentChar());
                nextChar();
            } while (position < input.length() &&
                   (Character.isLetterOrDigit(currentChar()) || currentChar() == '_'));
        } else if (isSymbolicChar(currentChar())) {
            // Read symbolic token
            return readSymbolicToken();
        } else {
            throw new PrologParserException("Expected atom name at line " + line + ", column " + column);
        }

        return name.toString();
    }

    private Term parseVariable() throws PrologParserException {
        skipWhitespace();
        StringBuilder name = new StringBuilder();

        if (Character.isUpperCase(currentChar()) || currentChar() == '_') {
            do {
                name.append(currentChar());
                nextChar();
            } while (position < input.length() &&
                     (Character.isLetterOrDigit(currentChar()) || currentChar() == '_'));
        } else {
            throw new PrologParserException("Expected variable name at line " + line + ", column " + column);
        }

        return new Variable(name.toString());
    }

    private Term parseNumber() throws PrologParserException {
        skipWhitespace();
        StringBuilder number = new StringBuilder();

        if (currentChar() == '-') {
            number.append(currentChar());
            nextChar();
        }

        // START_CHANGE: ISS-2025-0058 - Add hex, octal, binary literals and fix character codes
        if (currentChar() == '0' && position + 1 < input.length()) {
            char next = input.charAt(position + 1);

            // 0'c - character code literal
            if (next == '\'') {
                nextChar(); // consume '0'
                nextChar(); // consume '\''
                if (position >= input.length()) {
                    throw new PrologParserException("Incomplete character literal at line " + line + ", column " + column);
                }
                char literalChar;
                if (currentChar() == '\\') {
                    nextChar();
                    if (position >= input.length()) {
                        throw new PrologParserException("Incomplete escape in character literal at line " + line + ", column " + column);
                    }
                    literalChar = processEscapeChar(currentChar());
                } else {
                    literalChar = currentChar();
                }
                nextChar();
                return new Number((double) literalChar, true);
            }

            // 0xFF - hexadecimal literal
            if (next == 'x' || next == 'X') {
                nextChar(); // consume '0'
                nextChar(); // consume 'x'
                StringBuilder hex = new StringBuilder();
                while (position < input.length() && isHexDigit(currentChar())) {
                    hex.append(currentChar());
                    nextChar();
                }
                if (hex.length() == 0) {
                    throw new PrologParserException("Expected hex digits after 0x at line " + line + ", column " + column);
                }
                return new Number((double) Long.parseLong(hex.toString(), 16), true);
            }

            // 0o77 - octal literal
            if (next == 'o' || next == 'O') {
                nextChar(); // consume '0'
                nextChar(); // consume 'o'
                StringBuilder oct = new StringBuilder();
                while (position < input.length() && currentChar() >= '0' && currentChar() <= '7') {
                    oct.append(currentChar());
                    nextChar();
                }
                if (oct.length() == 0) {
                    throw new PrologParserException("Expected octal digits after 0o at line " + line + ", column " + column);
                }
                return new Number((double) Long.parseLong(oct.toString(), 8), true);
            }

            // 0b1010 - binary literal
            if (next == 'b' || next == 'B') {
                nextChar(); // consume '0'
                nextChar(); // consume 'b'
                StringBuilder bin = new StringBuilder();
                while (position < input.length() && (currentChar() == '0' || currentChar() == '1')) {
                    bin.append(currentChar());
                    nextChar();
                }
                if (bin.length() == 0) {
                    throw new PrologParserException("Expected binary digits after 0b at line " + line + ", column " + column);
                }
                return new Number((double) Long.parseLong(bin.toString(), 2), true);
            }
        }
        // END_CHANGE: ISS-2025-0058

        boolean hasDecimalPoint = false;
        while (position < input.length() &&
               (Character.isDigit(currentChar()) || currentChar() == '.')) {
            if (currentChar() == '.') {
                if (position + 1 < input.length() && Character.isDigit(input.charAt(position + 1))) {
                    hasDecimalPoint = true;
                } else {
                    break; // Sentence terminator, not decimal point
                }
            }
            number.append(currentChar());
            nextChar();
        }

        // START_CHANGE: ISS-2025-0058 - Support scientific notation
        if (position < input.length() && (currentChar() == 'e' || currentChar() == 'E')) {
            hasDecimalPoint = true;
            number.append(currentChar());
            nextChar();
            if (position < input.length() && (currentChar() == '+' || currentChar() == '-')) {
                number.append(currentChar());
                nextChar();
            }
            while (position < input.length() && Character.isDigit(currentChar())) {
                number.append(currentChar());
                nextChar();
            }
        }
        // END_CHANGE: ISS-2025-0058

        try {
            double val = Double.parseDouble(number.toString());
            return new Number(val, !hasDecimalPoint);
        } catch (NumberFormatException e) {
            throw new PrologParserException("Invalid number format: " + number.toString() +
                                          " at line " + line + ", column " + column);
        }
    }

    private Term parseList() throws PrologParserException {
        nextChar(); // consume '['
        skipWhitespace();

        if (currentChar() == ']') {
            nextChar(); // consume ']'
            return new Atom("[]");
        }

        List<Term> elements = new ArrayList<>();
        do {
            elements.add(parseListElement());
            skipWhitespace();

            if (currentChar() == '|') {
                nextChar(); // consume '|'
                Term tail = parseListElement();
                skipWhitespace();
                if (currentChar() != ']') {
                    throw new PrologParserException("Expected ']' at line " + line + ", column " + column);
                }
                nextChar(); // consume ']'
                return buildListWithTail(elements, tail);
            }

            if (currentChar() == ',') {
                nextChar(); // consume ','
                skipWhitespace();
            } else {
                break;
            }
        } while (currentChar() != ']');

        if (currentChar() != ']') {
            throw new PrologParserException("Expected ']' at line " + line + ", column " + column);
        }
        nextChar(); // consume ']'

        return buildList(elements);
    }

    private Term parseListElement() throws PrologParserException {
        return parseExpression(999);
    }

    private Term buildList(List<Term> elements) {
        if (elements.isEmpty()) {
            return new Atom("[]");
        }

        Term result = new Atom("[]");
        for (int i = elements.size() - 1; i >= 0; i--) {
            List<Term> args = new ArrayList<>();
            args.add(elements.get(i));
            args.add(result);
            result = new CompoundTerm(new Atom("."), args);
        }
        return result;
    }

    private Term buildListWithTail(List<Term> elements, Term tail) {
        Term result = tail;
        for (int i = elements.size() - 1; i >= 0; i--) {
            List<Term> args = new ArrayList<>();
            args.add(elements.get(i));
            args.add(result);
            result = new CompoundTerm(new Atom("."), args);
        }
        return result;
    }

    private Term parseBraces() throws PrologParserException {
        nextChar(); // consume '{'
        skipWhitespace();

        if (currentChar() == '}') {
            nextChar(); // consume '}'
            return TermUtils.createCompound("{}", new Atom("true"));
        }

        Term goal = parseExpression(1200);
        skipWhitespace();

        if (currentChar() != '}') {
            throw new PrologParserException("Expected '}' at line " + line + ", column " + column);
        }
        nextChar(); // consume '}'

        return TermUtils.createCompound("{}", goal);
    }

    private Term parseString() throws PrologParserException {
        skipWhitespace();
        if (currentChar() != '"') {
            throw new PrologParserException("Expected '\"' at line " + line + ", column " + column);
        }

        nextChar(); // consume opening quote
        StringBuilder value = new StringBuilder();

        while (position < input.length() && currentChar() != '"') {
            if (currentChar() == '\\') {
                nextChar();
                if (position >= input.length()) {
                    throw new PrologParserException("Unexpected end of input in string escape at line " + line + ", column " + column);
                }
                value.append(processEscapeChar(currentChar()));
                nextChar();
            } else {
                value.append(currentChar());
                nextChar();
            }
        }

        if (position >= input.length()) {
            throw new PrologParserException("Unterminated string literal at line " + line + ", column " + column);
        }

        nextChar(); // consume closing quote
        return new PrologString(value.toString());
    }

    private boolean isHexDigit(char c) {
        return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
    }

    private char processEscapeChar(char c) {
        switch (c) {
            case 'a': return '\u0007';
            case 'b': return '\b';
            case 'f': return '\f';
            case 'n': return '\n';
            case 'r': return '\r';
            case 't': return '\t';
            case 'v': return '\u000B';
            case '\\': return '\\';
            case '\'': return '\'';
            case '"': return '"';
            default: return c;
        }
    }
}
// END_CHANGE: ISS-2025-0085
