package it.denzosoft.jprolog;

import it.denzosoft.jprolog.terms.Atom;
import it.denzosoft.jprolog.terms.CompoundTerm;
import it.denzosoft.jprolog.terms.Number;
import it.denzosoft.jprolog.terms.Term;
import it.denzosoft.jprolog.terms.Variable;

import java.util.*;



public class PrologParser {

    private String input;
    private int currentTokenIndex;
    private List<String> tokens;

    private static final Map<String, Integer> OPERATOR_PRECEDENCE = new HashMap<>();

    static {
        // Define operator precedences (lower number = higher precedence)
        OPERATOR_PRECEDENCE.put(":-", 1200);  //Rule definition
        OPERATOR_PRECEDENCE.put(";", 1100);   //OR
        OPERATOR_PRECEDENCE.put("->", 1050);  //IF-THEN
        OPERATOR_PRECEDENCE.put(",", 1000);   //AND
        OPERATOR_PRECEDENCE.put("=", 700);    //Unification
        OPERATOR_PRECEDENCE.put("==", 700);   //Identity
        OPERATOR_PRECEDENCE.put("is", 700);   //is
        OPERATOR_PRECEDENCE.put("+", 500);
        OPERATOR_PRECEDENCE.put("-", 500);
        OPERATOR_PRECEDENCE.put("*", 400);
        OPERATOR_PRECEDENCE.put("/", 400);
    }

    public PrologParser(String input) {
        this.input = input;
        this.tokenize();
        this.currentTokenIndex = 0;
    }

    private void tokenize() {
        this.tokens = new ArrayList<>();
        StringTokenizer tokenizer = new StringTokenizer(input, " .,()[]:-+\\-*/;!?", true);  //Added operators
        while (tokenizer.hasMoreTokens()) {
            String token = tokenizer.nextToken().trim();
            if (!token.isEmpty()) {
                this.tokens.add(token);
            }
        }
    }

    private String peek() {
        if (currentTokenIndex < tokens.size()) {
            return tokens.get(currentTokenIndex);
        }
        return null;
    }

    private String nextToken() {
        if (currentTokenIndex < tokens.size()) {
            return tokens.get(currentTokenIndex++);
        }
        return null;
    }

    public Term parseTerm() throws PrologParseException {
        return parseExpression(0); // Start with precedence 0
    }

    private Term parseExpression(int minPrecedence) throws PrologParseException {
        Term left = parsePrimary();

        while (true) {
            String operator = peek();
            if (operator == null || !OPERATOR_PRECEDENCE.containsKey(operator) || OPERATOR_PRECEDENCE.get(operator) <= minPrecedence) {
                break;
            }
            nextToken(); // Consume operator
            int precedence = OPERATOR_PRECEDENCE.get(operator);
            Term right = parseExpression(precedence);
            left = new CompoundTerm(new Atom(operator), Arrays.asList(left, right));  //Create compound term for operator
        }
        return left;
    }

    private Term parsePrimary() throws PrologParseException {
        String token = peek();
        if (token == null) {
            return null;
        }

        if (token.matches("[a-z].*")) {
            return parseAtomOrCompoundTerm(); // Modified to handle compound terms
        } else if (token.matches("[A-Z_].*")) {
            return parseVariable();
        } else if (token.matches("-?\\d+(\\.\\d+)?")) {
            return parseNumber();
        } else if (token.equals("[")) {
            return parseList();
        } else if (token.equals("(")) {
            nextToken(); // Consume '('
            Term term = parseTerm();
            if (!peek().equals(")")) {
                throw new PrologParseException("Expected ')'");
            }
            nextToken(); // Consume ')'
            return term;
        } else {
            throw new PrologParseException("Unexpected token: " + token);
        }
    }

    private Term parseAtomOrCompoundTerm() throws PrologParseException {
        String name = nextToken();
        if (peek() != null && peek().equals("(")) {
            nextToken(); // Consume '('
            List<Term> arguments = new ArrayList<>();
            while (true) {
                Term argument = parseTerm();
                if (argument != null) {
                    arguments.add(argument);
                }

                String token = peek();
                if (token == null) {
                    throw new PrologParseException("Unclosed parenthesis in compound term");
                } else if (token.equals(")")) {
                    nextToken(); // Consume ')'
                    break;
                } else if (token.equals(",")) {
                    nextToken(); // Consume ','
                } else {
                    throw new PrologParseException("Unexpected token in compound term: " + token);
                }
            }
            return new CompoundTerm(new Atom(name), arguments);
        } else {
            return new Atom(name);
        }
    }


    private Variable parseVariable() {
        String name = nextToken();
        return new Variable(name);
    }

    private Number parseNumber() throws PrologParseException {
        String numberString = nextToken();
        try {
            double value = Double.parseDouble(numberString);
            return new Number(value);
        } catch (NumberFormatException e) {
            throw new PrologParseException("Invalid number format: " + numberString);
        }
    }

    private ListTerm parseList() throws PrologParseException {
        nextToken(); // Consume '['
        List<Term> elements = new ArrayList<>();
        while (true) {
            String token = peek();
            if (token == null) {
                throw new PrologParseException("Unclosed list");
            }
            if (token.equals("]")) {
                nextToken(); // Consume ']'
                break;
            }
            Term element = parseTerm();
            if (element != null) {
                elements.add(element);
            } else {
                throw new PrologParseException("Expected term in list");
            }

            if (peek() != null && peek().equals(",")) {
                nextToken(); // Consume ','
            }
        }
        return new ListTerm(elements);
    }


    public static void main(String[] args) {
        String input = "parent(X, Y) :- father(X,Y), mother(X,Y).";
        PrologParser parser = new PrologParser(input);
        try {
            Term term = parser.parseTerm();
            System.out.println("Parsed term: " + term);
        } catch (PrologParseException e) {
            System.err.println("Error parsing term: " + e.getMessage());
        }
    }


    public static class PrologParseException extends Exception {
        public PrologParseException(String message) {
            super(message);
        }
    }
}
