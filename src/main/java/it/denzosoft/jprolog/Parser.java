package it.denzosoft.jprolog;

import it.denzosoft.jprolog.terms.Term;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class Parser {
    private final TermParser termParser;

    /**
     * Create a parser with default term parser.
     */
    public Parser() {
        this(new TermParser());
    }

    /**
     * Create a parser with specified term parser.
     * 
     * @param termParser The term parser to use
     */
    public Parser(TermParser termParser) {
        this.termParser = Objects.requireNonNull(termParser, "Term parser cannot be null");
    }

    /**
     * Parse a Prolog program string into a list of rules.
     * 
     * @param program The program string to parse
     * @return List of parsed rules
     * @throws PrologParserException if parsing fails
     */
    public List<Rule> parse(String program) throws PrologParserException {
        if (program == null) {
            return new ArrayList<>();
        }
        
        List<Rule> rules = new ArrayList<>();
        // Split on '.' but be careful not to split inside quotes or comments
        String[] ruleStrings = program.split("\\.(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", -1);
        
        for (String ruleString : ruleStrings) {
            String trimmedRule = ruleString.trim();
            if (!trimmedRule.isEmpty()) {
                try {
                    rules.add(parseRule(trimmedRule));
                } catch (PrologParserException e) {
                    throw new PrologParserException("Error parsing rule: '" + trimmedRule + "' - " + e.getMessage(), e);
                }
            }
        }
        return rules;
    }

    private Rule parseRule(String ruleString) throws PrologParserException {
        if (ruleString.contains(":-")) {
            String[] parts = ruleString.split(":-", 2);
            if (parts.length != 2) {
                throw new PrologParserException("Invalid rule format: " + ruleString);
            }
            Term head = termParser.parseTerm(parts[0].trim());
            List<Term> body = parseBody(parts[1].trim());
            return new Rule(head, body);
        } else {
            return new Rule(termParser.parseTerm(ruleString.trim()), new ArrayList<>());
        }
    }

    private List<Term> parseBody(String bodyString) throws PrologParserException {
        List<Term> body = new ArrayList<>();
        // Split on ',' but be careful not to split inside parentheses
        List<String> termStrings = splitOnCommasOutsideParens(bodyString);
        
        for (String termString : termStrings) {
            String trimmedTerm = termString.trim();
            if (!trimmedTerm.isEmpty()) {
                body.add(termParser.parseTerm(trimmedTerm));
            }
        }
        return body;
    }

    // Split a string on commas that are not inside parentheses
    private List<String> splitOnCommasOutsideParens(String input) {
        List<String> result = new ArrayList<>();
        int parenCount = 0;
        int lastSplit = 0;
        
        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            if (c == '(') {
                parenCount++;
            } else if (c == ')') {
                parenCount--;
            } else if (c == ',' && parenCount == 0) {
                result.add(input.substring(lastSplit, i));
                lastSplit = i + 1;
            }
        }
        result.add(input.substring(lastSplit));
        return result;
    }

    /**
     * Parse a single term.
     * 
     * @param termString The term string to parse
     * @return The parsed term
     * @throws PrologParserException if parsing fails
     */
    public Term parseTerm(String termString) throws PrologParserException {
        return termParser.parseTerm(termString);
    }
}
