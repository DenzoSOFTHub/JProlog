package it.denzosoft.jprolog.core.parser;

import it.denzosoft.jprolog.core.exceptions.PrologParserException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.util.TermUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;




public class TermParser {
    private java.lang.String input;
    private int position;
    private int line;
    private int column;
    
    // Define operator precedences (lower number = higher precedence)
    private static final Map<java.lang.String, Integer> OPERATOR_PRECEDENCE = new HashMap<>();
    private static final Map<java.lang.String, java.lang.String> OPERATOR_FUNCTORS = new HashMap<>();

    static {
        // Arithmetic operators - using standard Prolog precedences
        OPERATOR_PRECEDENCE.put("is", 700);
        OPERATOR_PRECEDENCE.put("=", 700);
        OPERATOR_PRECEDENCE.put("=\\=", 700);
        OPERATOR_PRECEDENCE.put("=:=", 700);
        OPERATOR_PRECEDENCE.put("+", 500);
        OPERATOR_PRECEDENCE.put("-", 500);
        OPERATOR_PRECEDENCE.put("*", 400);
        OPERATOR_PRECEDENCE.put("/", 400);
        OPERATOR_PRECEDENCE.put("mod", 400);
        OPERATOR_PRECEDENCE.put("**", 200);
        
        // START_CHANGE: ISS-2025-0017 - Add missing arithmetic operators to parser
        OPERATOR_PRECEDENCE.put("rem", 400);    // Same precedence as mod
        OPERATOR_PRECEDENCE.put("/\\", 500);    // Bitwise AND
        OPERATOR_PRECEDENCE.put("\\/", 500);    // Bitwise OR  
        OPERATOR_PRECEDENCE.put("xor", 500);    // Bitwise XOR
        OPERATOR_PRECEDENCE.put("<<", 400);     // Left shift
        OPERATOR_PRECEDENCE.put(">>", 400);     // Right shift
        // END_CHANGE: ISS-2025-0017
        
        // Comparison operators
        OPERATOR_PRECEDENCE.put(">", 700);
        OPERATOR_PRECEDENCE.put("<", 700);
        OPERATOR_PRECEDENCE.put(">=", 700);
        OPERATOR_PRECEDENCE.put("=<", 700);
        
        // Term comparison operators
        OPERATOR_PRECEDENCE.put("@<", 700);
        OPERATOR_PRECEDENCE.put("@=<", 700);
        OPERATOR_PRECEDENCE.put("@>", 700);
        OPERATOR_PRECEDENCE.put("@>=", 700);
        OPERATOR_PRECEDENCE.put("==", 700);
        OPERATOR_PRECEDENCE.put("\\==", 700);
        OPERATOR_PRECEDENCE.put("\\=", 700);  // Unification failure (not equal)
        
        // Term construction
        OPERATOR_PRECEDENCE.put("=..", 700);
        
        // START_CHANGE: ISS-2025-0037 - Add existential quantification operator
        OPERATOR_PRECEDENCE.put("^", 200);    // Higher precedence than power (**)
        // END_CHANGE: ISS-2025-0037
        
        // Conditional operators (if-then-else)
        OPERATOR_PRECEDENCE.put("->", 1050);
        OPERATOR_PRECEDENCE.put(";", 1100);
        
        // DCG operator (lower precedence to be parsed correctly)
        OPERATOR_PRECEDENCE.put("-->", 1200);
        
        // Conjunction operator
        OPERATOR_PRECEDENCE.put(",", 1000);
        
        // Negation as failure
        OPERATOR_PRECEDENCE.put("\\+", 900);
        
        // Cut operator
        OPERATOR_PRECEDENCE.put("!", 0);  // Highest precedence (lowest number)
        
        // Functor mappings
        OPERATOR_FUNCTORS.put("is", "is");
        OPERATOR_FUNCTORS.put("=", "=");
        OPERATOR_FUNCTORS.put("=\\=", "=\\=");
        OPERATOR_FUNCTORS.put("=:=", "=:=");
        OPERATOR_FUNCTORS.put("+", "+");
        OPERATOR_FUNCTORS.put("-", "-");
        OPERATOR_FUNCTORS.put("*", "*");
        OPERATOR_FUNCTORS.put("/", "/");
        OPERATOR_FUNCTORS.put("mod", "mod");
        OPERATOR_FUNCTORS.put("**", "**");
        
        // START_CHANGE: ISS-2025-0017 - Add missing arithmetic operator functors
        OPERATOR_FUNCTORS.put("rem", "rem");
        OPERATOR_FUNCTORS.put("/\\", "/\\");
        OPERATOR_FUNCTORS.put("\\/", "\\/");
        OPERATOR_FUNCTORS.put("xor", "xor");
        OPERATOR_FUNCTORS.put("<<", "<<");
        OPERATOR_FUNCTORS.put(">>", ">>");
        // END_CHANGE: ISS-2025-0017
        OPERATOR_FUNCTORS.put(">", ">");
        OPERATOR_FUNCTORS.put("<", "<");
        OPERATOR_FUNCTORS.put(">=", ">=");
        OPERATOR_FUNCTORS.put("=<", "=<");
        OPERATOR_FUNCTORS.put("@<", "@<");
        OPERATOR_FUNCTORS.put("@=<", "@=<");
        OPERATOR_FUNCTORS.put("@>", "@>");
        OPERATOR_FUNCTORS.put("@>=", "@>=");
        OPERATOR_FUNCTORS.put("==", "==");
        OPERATOR_FUNCTORS.put("\\==", "\\==");
        OPERATOR_FUNCTORS.put("\\=", "\\=");
        OPERATOR_FUNCTORS.put("=..", "=..");
        // START_CHANGE: ISS-2025-0037 - Add existential quantification operator functor
        OPERATOR_FUNCTORS.put("^", "^");
        // END_CHANGE: ISS-2025-0037
        OPERATOR_FUNCTORS.put("->", "->");
        OPERATOR_FUNCTORS.put(";", ";");
        OPERATOR_FUNCTORS.put("-->", "-->");
        OPERATOR_FUNCTORS.put(",", ",");
        OPERATOR_FUNCTORS.put("\\+", "\\+");
        OPERATOR_FUNCTORS.put("!", "!");
    }

    public TermParser() {
        // Default constructor
    }

    public Term parseTerm(java.lang.String input) throws PrologParserException {
        this.input = input;
        this.position = 0;
        this.line = 1;
        this.column = 1;
        
        Term result = parseExpression(1200); // Start with highest precedence number (lowest binding)
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

        StringBuilder token = new StringBuilder();
        char c = currentChar();

        // Handle special characters
        if (c == '(' || c == ')' || c == '[' || c == ']' || c == '|' || c == '.') {
            token.append(c);
            nextChar();
            return token.toString();
        }
        
        // Handle string literals (double quotes)
        if (c == '"') {
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

        // Handle operators using same greedy approach as peekNextOperator
        if (isOperatorStart(java.lang.String.valueOf(c))) {
            StringBuilder op = new StringBuilder();
            op.append(c);
            int tempPos = position + 1;
            
            // Keep reading as long as it forms a valid operator prefix
            while (tempPos < input.length() && isOperatorChar(input.charAt(tempPos))) {
                java.lang.String extended = op.toString() + input.charAt(tempPos);
                boolean validPrefix = false;
                for (java.lang.String knownOp : OPERATOR_PRECEDENCE.keySet()) {
                    if (knownOp.startsWith(extended)) {
                        validPrefix = true;
                        break;
                    }
                }
                if (!validPrefix) break;
                op.append(input.charAt(tempPos));
                tempPos++;
            }
            
            // Find the longest operator that exactly matches
            java.lang.String candidate = op.toString();
            java.lang.String bestMatch = null;
            
            for (java.lang.String knownOp : OPERATOR_PRECEDENCE.keySet()) {
                if (candidate.startsWith(knownOp) && 
                    (bestMatch == null || knownOp.length() > bestMatch.length())) {
                    bestMatch = knownOp;
                }
            }
            
            if (bestMatch != null) {
                // Consume exactly the characters for the best match (skip first char as we already read it)
                for (int i = 1; i < bestMatch.length(); i++) {
                    nextChar();
                }
                nextChar(); // consume the first character we read
                return bestMatch;
            } else {
                // Fall back to single character if no operator match
                nextChar();
                return String.valueOf(c);
            }
        }

        // Handle quoted atoms
        if (c == '\'') {
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

        // Handle regular tokens (alphanumeric, underscore)
        while (position < input.length() &&
            (Character.isLetterOrDigit(c) || c == '_')) {
            token.append(c);
            nextChar();
            if (position < input.length()) {
                c = currentChar();
            }
        }

        return token.toString();
    }

    private boolean isOperatorStart(java.lang.String token) {
        // Check if token is start of any known operator
        for (java.lang.String op : OPERATOR_PRECEDENCE.keySet()) {
            if (op.startsWith(token)) {
                return true;
            }
        }
        return token.equals("=") || token.equals("\\") || token.equals(":") ||
            token.equals("+") || token.equals("-") || token.equals("*") ||
            token.equals("/") || token.equals("<") || token.equals(">") || token.equals(";") ||
            token.equals(",") || token.equals("!") || token.equals("m") || // 'm' for 'mod'
            token.equals("^"); // '^' for existential quantification
    }
    
    private boolean isOperatorChar(char c) {
        return c == '=' || c == '\\' || c == ':' || c == '+' ||
            c == '-' || c == '*' || c == '/' || c == '<' || c == '>' || c == '@' || c == '.' ||
            c == ';' || c == ',' || c == '!' || c == '^' || Character.isLetter(c);
    }

    private Term parseExpression(int minPrecedence) throws PrologParserException {
        // Check for prefix operators first
        java.lang.String prefixOp = peekNextOperator();
        if (prefixOp != null && isPrefixOperator(prefixOp) && 
            OPERATOR_PRECEDENCE.containsKey(prefixOp) && 
            OPERATOR_PRECEDENCE.get(prefixOp) < minPrecedence) {
            
            readToken(); // Consume the prefix operator
            int precedence = OPERATOR_PRECEDENCE.get(prefixOp);
            Term operand = parseExpression(precedence);
            
            // Create compound term for prefix operator
            java.lang.String functor = OPERATOR_FUNCTORS.getOrDefault(prefixOp, prefixOp);
            List<Term> args = new ArrayList<>();
            args.add(operand);
            Term left = new CompoundTerm(new Atom(functor), args);
            
            // Continue with infix operators if any
            return parseInfixOperators(left, minPrecedence);
        } else {
            // No prefix operator, parse primary and continue with infix
            Term left = parsePrimary();
            return parseInfixOperators(left, minPrecedence);
        }
    }
    
    private Term parseInfixOperators(Term left, int minPrecedence) throws PrologParserException {
        while (true) {
            java.lang.String op = peekNextOperator();
            if (op == null || !OPERATOR_PRECEDENCE.containsKey(op) || OPERATOR_PRECEDENCE.get(op) >= minPrecedence) {
                break;
            }
            
            readToken(); // Consume operator
            int precedence = OPERATOR_PRECEDENCE.get(op);
            // For left-associative operators, use precedence to handle right side
            Term right = parseExpression(precedence);
            
            // Create compound term for the operator
            java.lang.String functor = OPERATOR_FUNCTORS.getOrDefault(op, op);
            List<Term> args = new ArrayList<>();
            args.add(left);
            args.add(right);
            left = new CompoundTerm(new Atom(functor), args);
        }
        
        return left;
    }
    
    private boolean isPrefixOperator(java.lang.String op) {
        return "\\+".equals(op);
    }

    private java.lang.String peekNextOperator() throws PrologParserException {
        int savedPosition = position;
        int savedLine = line;
        int savedColumn = column;
        skipWhitespace();
        StringBuilder op = new StringBuilder();
        
        // Try to read a potential operator
        if (position < input.length() && isOperatorStart(String.valueOf(currentChar()))) {
            op.append(currentChar());
            int tempPos = position + 1;
            
            // Keep reading as long as it forms a valid operator prefix
            while (tempPos < input.length() && isOperatorChar(input.charAt(tempPos))) {
                java.lang.String extended = op.toString() + input.charAt(tempPos);
                boolean validPrefix = false;
                for (java.lang.String knownOp : OPERATOR_PRECEDENCE.keySet()) {
                    if (knownOp.startsWith(extended)) {
                        validPrefix = true;
                        break;
                    }
                }
                if (!validPrefix) break;
                op.append(input.charAt(tempPos));
                tempPos++;
            }
            
            // Find the longest operator that exactly matches the input starting at this position
            java.lang.String candidate = op.toString();
            java.lang.String bestMatch = null;
            
            for (java.lang.String knownOp : OPERATOR_PRECEDENCE.keySet()) {
                if (candidate.startsWith(knownOp) && 
                    (bestMatch == null || knownOp.length() > bestMatch.length())) {
                    bestMatch = knownOp;
                }
            }
            
            if (bestMatch != null) {
                position = savedPosition;
                line = savedLine;
                column = savedColumn;
                return bestMatch;
            }
        }
        
        position = savedPosition;
        line = savedLine;
        column = savedColumn;
        return null;
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
        } else if (c == '"') {
            return parseString();
        } else if (Character.isDigit(c) || (c == '-' && position + 1 < input.length() && Character.isDigit(input.charAt(position + 1)))) {
            return parseNumber();
        } else if (Character.isUpperCase(c) || c == '_') {
            return parseVariable();
        } else if (Character.isLetter(c)) {
            // This could be an atom or a function
            int savedPosition = position;
            int savedLine = line;
            int savedColumn = column;
            
            java.lang.String name = parseAtomName();
            
            skipWhitespace();
            if (currentChar() == '(') {
                // It's a compound term
                nextChar(); // consume '('
                List<Term> arguments = new ArrayList<>();
                skipWhitespace();
                
                if (currentChar() != ')') {
                    do {
                        arguments.add(parseExpression(0));
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
                // It's just an atom
                position = savedPosition;
                line = savedLine;
                column = savedColumn;
                return parseAtomOrCompound();
            }
        } else if (isOperatorStart(java.lang.String.valueOf(c))) {
            // Handle operators that start with special characters
            return parseAtomOrCompound();
        } else {
            return parseAtomOrCompound();
        }
    }

    private Term parseAtomOrCompound() throws PrologParserException {
        java.lang.String name = parseAtomName();
        
        skipWhitespace();
        if (currentChar() == '(') {
            nextChar(); // consume '('
            List<Term> arguments = new ArrayList<>();
            skipWhitespace();
            
            if (currentChar() != ')') {
                do {
                    arguments.add(parseExpression(0));
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

    private java.lang.String parseAtomName() throws PrologParserException {
        skipWhitespace();
        StringBuilder name = new StringBuilder();
        
        if (currentChar() == '\'') {
            // Quoted atom
            nextChar(); // consume opening quote
            while (position < input.length() && currentChar() != '\'') {
                if (currentChar() == '\\') {
                    name.append(currentChar());
                    nextChar();
                }
                name.append(currentChar());
                nextChar();
            }
            if (position >= input.length()) {
                throw new PrologParserException("Unterminated quoted atom at line " + line + ", column " + column);
            }
            nextChar(); // consume closing quote
        } else if (Character.isLetter(currentChar()) || isOperatorStart(java.lang.String.valueOf(currentChar()))) {
            // Regular atom or operator - can start with letter or operator character
            if (Character.isLetter(currentChar())) {
                // Normal atom name starting with letter (prioritize over operators)
                do {
                    name.append(currentChar());
                    nextChar();
                } while (position < input.length() && 
                       (Character.isLetterOrDigit(currentChar()) || currentChar() == '_'));
            } else if (isOperatorStart(java.lang.String.valueOf(currentChar()))) {
                // Handle operators as function names
                StringBuilder op = new StringBuilder();
                do {
                    op.append(currentChar());
                    nextChar();
                } while (position < input.length() && isOperatorChar(currentChar()));
                return op.toString();
            }
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
        
        // Check for character literal syntax: 0'c
        if (currentChar() == '0' && position + 1 < input.length() && input.charAt(position + 1) == '\'') {
            nextChar(); // consume '0'
            nextChar(); // consume '\''
            
            if (position >= input.length()) {
                throw new PrologParserException("Incomplete character literal at line " + line + ", column " + column);
            }
            
            char literalChar = currentChar();
            nextChar(); // consume the character
            
            // Return the ASCII code of the character
            return new Number((double) literalChar);
        }
        
        while (position < input.length() && 
               (Character.isDigit(currentChar()) || currentChar() == '.')) {
            number.append(currentChar());
            nextChar();
        }
        
        try {
            return new Number(Double.parseDouble(number.toString()));
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
            // Parse a single list element (not allowing commas as operators)
            elements.add(parseListElement());
            skipWhitespace();
            
            if (currentChar() == '|') {
                nextChar(); // consume '|'
                Term tail = parseListElement(); // Parse tail element
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
    
    /**
     * Parse a single list element. This method parses terms inside lists
     * without treating commas as operators, since commas are list separators.
     */
    private Term parseListElement() throws PrologParserException {
        // Parse using normal expression parsing but with restricted precedence
        // to prevent commas from being parsed as operators at this level
        return parseExpression(999); // Use precedence below comma (1000)
    }
    
    /**
     * Parse an expression that stops at any of the specified delimiter characters.
     */
    private Term parseExpressionUntil(char... delimiters) throws PrologParserException {
        // Save current position to handle backtracking if needed
        int savedPos = position;
        int savedLine = line;
        int savedColumn = column;
        
        // Parse primary term first
        Term left = parsePrimary();
        
        // Parse infix operators but stop at delimiters
        while (true) {
            skipWhitespace();
            
            // Check if we've hit a delimiter
            char currentChar = currentChar();
            for (char delimiter : delimiters) {
                if (currentChar == delimiter) {
                    return left;
                }
            }
            
            // Check for operators
            java.lang.String op = peekNextOperator();
            if (op == null || !OPERATOR_PRECEDENCE.containsKey(op)) {
                break;
            }
            
            // Don't parse comma as operator in list context
            if (",".equals(op)) {
                break;
            }
            
            readToken(); // Consume operator
            int precedence = OPERATOR_PRECEDENCE.get(op);
            Term right = parseExpressionUntil(delimiters);
            
            // Create compound term for the operator
            java.lang.String functor = OPERATOR_FUNCTORS.getOrDefault(op, op);
            List<Term> args = new ArrayList<>();
            args.add(left);
            args.add(right);
            left = new CompoundTerm(new Atom(functor), args);
        }
        
        return left;
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
    
    /**
     * Parse braces {Goal} for DCG support.
     */
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
                
                char escapeChar = currentChar();
                switch (escapeChar) {
                    case 'n':
                        value.append('\n');
                        break;
                    case 't':
                        value.append('\t');
                        break;
                    case 'r':
                        value.append('\r');
                        break;
                    case '\\':
                        value.append('\\');
                        break;
                    case '"':
                        value.append('"');
                        break;
                    default:
                        // For other characters, include them literally
                        value.append(escapeChar);
                        break;
                }
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
}
