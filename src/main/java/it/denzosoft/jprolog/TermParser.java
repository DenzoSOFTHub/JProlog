package it.denzosoft.jprolog;

import it.denzosoft.jprolog.terms.Atom;
import it.denzosoft.jprolog.terms.CompoundTerm;
import it.denzosoft.jprolog.terms.Number;
import it.denzosoft.jprolog.terms.Term;
import it.denzosoft.jprolog.terms.Variable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;






public class TermParser {
    private String input;
    private int position;
    private int line;
    private int column;
    
    // Define operator precedences (lower number = higher precedence)
    private static final Map<String, Integer> OPERATOR_PRECEDENCE = new HashMap<>();
    private static final Map<String, String> OPERATOR_FUNCTORS = new HashMap<>();

    static {
        // Arithmetic operators
        OPERATOR_PRECEDENCE.put("is", 700);
        OPERATOR_PRECEDENCE.put("=", 700);
        OPERATOR_PRECEDENCE.put("=\\=", 700);
        OPERATOR_PRECEDENCE.put("=:=", 700);
        OPERATOR_PRECEDENCE.put("+", 500);
        OPERATOR_PRECEDENCE.put("-", 500);
        OPERATOR_PRECEDENCE.put("*", 400);
        OPERATOR_PRECEDENCE.put("/", 400);
        
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
        
        // Term construction
        OPERATOR_PRECEDENCE.put("=..", 700);
        
        // Functor mappings
        OPERATOR_FUNCTORS.put("is", "is");
        OPERATOR_FUNCTORS.put("=", "=");
        OPERATOR_FUNCTORS.put("=\\=", "=\\=");
        OPERATOR_FUNCTORS.put("=:=", "=:=");
        OPERATOR_FUNCTORS.put("+", "+");
        OPERATOR_FUNCTORS.put("-", "-");
        OPERATOR_FUNCTORS.put("*", "*");
        OPERATOR_FUNCTORS.put("/", "/");
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
        OPERATOR_FUNCTORS.put("=..", "=..");
    }

    public TermParser() {
        // Default constructor
    }

    public Term parseTerm(String input) throws PrologParserException {
        this.input = input;
        this.position = 0;
        this.line = 1;
        this.column = 1;
        
        Term result = parseExpression(0); // Start with lowest precedence
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

    private String peekToken() throws PrologParserException {
        int savedPosition = position;
        int savedLine = line;
        int savedColumn = column;
        String token = readToken();
        position = savedPosition;
        line = savedLine;
        column = savedColumn;
        return token;
    }

   private String readToken() throws PrologParserException {
        skipWhitespace();
        if (position >= input.length()) {
            return null;
        }

        StringBuilder token = new StringBuilder();
        char c = currentChar();

        // Handle special characters
        if (c == '(' || c == ')' || c == ',' || c == '[' || c == ']' || c == '|' || c == '.') {
            token.append(c);
            nextChar();
            return token.toString();
        }

        // Handle operators
        if (isOperatorStart(String.valueOf(c))) {
            token.append(c);
            nextChar();
            while (position < input.length() && isOperatorChar(currentChar())) {
                token.append(currentChar());
                nextChar();
            }
            return token.toString();
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

    private boolean isOperatorStart(String token) {
        // Check if token is start of any known operator
        for (String op : OPERATOR_PRECEDENCE.keySet()) {
            if (op.startsWith(token)) {
                return true;
            }
        }
        return token.equals("=") || token.equals("\\") || token.equals(":") ||
            token.equals("+") || token.equals("-") || token.equals("*") ||
            token.equals("/") || token.equals("<") || token.equals(">");
    }
    
    private boolean isOperatorChar(char c) {
        return c == '=' || c == '\\' || c == ':' || c == '+' ||
            c == '-' || c == '*' || c == '/' || c == '<' || c == '>' || c == '@' || c == '.';
    }

    private Term parseExpression(int minPrecedence) throws PrologParserException {
        Term left = parsePrimary();
        
        while (true) {
            String op = peekNextOperator();
            if (op == null || !OPERATOR_PRECEDENCE.containsKey(op) || OPERATOR_PRECEDENCE.get(op) <= minPrecedence) {
                break;
            }
            
            readToken(); // Consume operator
            int precedence = OPERATOR_PRECEDENCE.get(op);
            Term right = parseExpression(precedence);
            
            // Create compound term for the operator
            String functor = OPERATOR_FUNCTORS.getOrDefault(op, op);
            List<Term> args = new ArrayList<>();
            args.add(left);
            args.add(right);
            left = new CompoundTerm(new Atom(functor), args);
        }
        
        return left;
    }

    private String peekNextOperator() throws PrologParserException {
        int savedPosition = position;
        int savedLine = line;
        int savedColumn = column;
        skipWhitespace();
        StringBuilder op = new StringBuilder();
        
        if (position < input.length() && isOperatorStart(String.valueOf(currentChar()))) {
            op.append(currentChar());
            int tempPos = position + 1;
            while (tempPos < input.length() && isOperatorChar(input.charAt(tempPos))) {
                op.append(input.charAt(tempPos));
                tempPos++;
            }
            // Check if this is a valid operator
            String candidate = op.toString();
            if (OPERATOR_PRECEDENCE.containsKey(candidate)) {
                position = savedPosition;
                line = savedLine;
                column = savedColumn;
                return candidate;
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
        } else if (c == '(') {
            nextChar(); // consume '('
            Term term = parseExpression(0);
            skipWhitespace();
            if (currentChar() != ')') {
                throw new PrologParserException("Expected ')' at line " + line + ", column " + column);
            }
            nextChar(); // consume ')'
            return term;
        } else if (Character.isDigit(c) || (c == '-' && position + 1 < input.length() && Character.isDigit(input.charAt(position + 1)))) {
            return parseNumber();
        } else if (Character.isUpperCase(c) || c == '_') {
            return parseVariable();
        } else {
            return parseAtomOrCompound();
        }
    }

    private Term parseAtomOrCompound() throws PrologParserException {
        String name = parseAtomName();
        
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

    private String parseAtomName() throws PrologParserException {
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
        } else {
            // Regular atom - fix to properly handle operators
            int startPos = position;
            while (position < input.length() && 
                   (Character.isLetterOrDigit(currentChar()) || currentChar() == '_')) {
                name.append(currentChar());
                nextChar();
            }
            
            // Check if this could be an operator
            String potentialName = name.toString();
            if (OPERATOR_PRECEDENCE.containsKey(potentialName)) {
                // It's an operator, return it as is
                return potentialName;
            }
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
            elements.add(parseExpression(0));
            skipWhitespace();
            
            if (currentChar() == '|') {
                nextChar(); // consume '|'
                Term tail = parseExpression(0);
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
}
