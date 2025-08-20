package it.denzosoft.jprolog.core.parser;

import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.exceptions.PrologParserException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

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
        List<String> clauses = extractClauses(program);
        
        for (String clause : clauses) {
            String trimmedClause = clause.trim();
            if (!trimmedClause.isEmpty()) {
                try {
                    rules.add(parseRule(trimmedClause));
                } catch (PrologParserException e) {
                    throw new PrologParserException("Error parsing rule: '" + trimmedClause + "' - " + e.getMessage(), e);
                }
            }
        }
        return rules;
    }
    
    /**
     * Estrae le clausole dal programma gestendo correttamente commenti e stringhe.
     */
    private List<String> extractClauses(String program) {
        List<String> clauses = new ArrayList<>();
        StringBuilder currentClause = new StringBuilder();
        
        boolean inMultilineComment = false;
        boolean inSingleLineComment = false;
        boolean inQuotedString = false;
        char quoteChar = '\0';
        
        for (int i = 0; i < program.length(); i++) {
            char c = program.charAt(i);
            char next = (i + 1 < program.length()) ? program.charAt(i + 1) : '\0';
            
            // Handle newlines - end single line comments
            if (c == '\n') {
                inSingleLineComment = false;
                if (!inMultilineComment && !inQuotedString) {
                    currentClause.append(' '); // Replace newline with space to preserve structure
                }
                continue;
            }
            
            // Skip content inside comments
            if (inSingleLineComment || inMultilineComment) {
                // Check for end of multiline comment
                if (inMultilineComment && c == '*' && next == '/') {
                    inMultilineComment = false;
                    i++; // Skip the '/'
                }
                continue;
            }
            
            // Handle quoted strings
            if (inQuotedString) {
                currentClause.append(c);
                if (c == quoteChar && (i == 0 || program.charAt(i - 1) != '\\')) {
                    inQuotedString = false;
                    quoteChar = '\0';
                }
                continue;
            }
            
            // Check for start of comments (only outside quotes)
            if (c == '/' && next == '*') {
                inMultilineComment = true;
                i++; // Skip the '*'
                continue;
            } else if (c == '%') {
                inSingleLineComment = true;
                continue;
            }
            
            // Check for start of quoted strings
            if (c == '\'' || c == '"') {
                inQuotedString = true;
                quoteChar = c;
                currentClause.append(c);
                continue;
            }
            
            // Check for end of clause
            if (c == '.') {
                currentClause.append(c);
                // Look ahead to see if this might be part of a number or operator
                if (Character.isDigit(next) || next == '.') {
                    // This '.' is part of a number, not end of clause
                    continue;
                }
                
                // Check if this period is part of a floating point number
                // Only consider it part of a number if there's a digit immediately before AND after
                if (i > 0 && Character.isDigit(program.charAt(i - 1)) && 
                    i + 1 < program.length() && Character.isDigit(program.charAt(i + 1))) {
                    // This '.' is part of a floating point number
                    continue;
                }
                
                // Check if we're at end of clause by looking for whitespace or end of input
                if (next == '\0' || Character.isWhitespace(next) || next == '%' || 
                    (next == '/' && i + 2 < program.length() && program.charAt(i + 2) == '*')) {
                    // End of clause
                    String clause = currentClause.toString().trim();
                    if (!clause.isEmpty()) {
                        clauses.add(clause);
                    }
                    currentClause = new StringBuilder();
                }
            } else {
                currentClause.append(c);
            }
        }
        
        // Handle any remaining content
        String remaining = currentClause.toString().trim();
        if (!remaining.isEmpty()) {
            clauses.add(remaining);
        }
        
        return clauses;
    }

    private Rule parseRule(String ruleString) throws PrologParserException {
        // Remove trailing '.' if present
        String cleanRule = ruleString.trim();
        if (cleanRule.endsWith(".")) {
            cleanRule = cleanRule.substring(0, cleanRule.length() - 1).trim();
        }
        
        if (cleanRule.startsWith(":-")) {
            // This is a directive like :- module(...) or :- use_module(...)
            String directiveBody = cleanRule.substring(2).trim();
            Term directive = termParser.parseTerm(directiveBody);
            // Create a rule with :- as head and directive as single body term
            Term head = termParser.parseTerm(":-(" + directiveBody + ")");
            return new Rule(head, new ArrayList<>());
        } else if (cleanRule.contains("-->")) {
            // This is a DCG rule - handle specially
            String[] parts = cleanRule.split("-->", 2);
            if (parts.length != 2) {
                throw new PrologParserException("Invalid DCG rule format: " + ruleString);
            }
            String headPart = parts[0].trim();
            String bodyPart = parts[1].trim();
            
            if (headPart.isEmpty()) {
                throw new PrologParserException("Missing head in DCG rule: " + ruleString);
            }
            
            Term head = termParser.parseTerm(headPart);
            
            // Parse the body as a single term (it may contain commas, so parse as sequence)
            Term body;
            if (bodyPart.contains(",")) {
                // Body contains comma-separated goals, parse them individually and create comma structure
                List<Term> bodyGoals = parseBody(bodyPart);
                body = createCommaSequence(bodyGoals);
            } else {
                body = termParser.parseTerm(bodyPart);
            }
            
            // Create a compound term for the DCG rule: head --> body
            // Use the actual parsed terms instead of string concatenation
            List<Term> dcgArgs = new ArrayList<>();
            dcgArgs.add(head);
            dcgArgs.add(body);
            Term dcgTerm = new CompoundTerm(new Atom("-->"), dcgArgs);
            return new Rule(dcgTerm, new ArrayList<>());
        } else if (cleanRule.contains(":-")) {
            String[] parts = cleanRule.split(":-", 2);
            if (parts.length != 2) {
                throw new PrologParserException("Invalid rule format: " + ruleString);
            }
            String headPart = parts[0].trim();
            if (headPart.isEmpty()) {
                throw new PrologParserException("Missing head in rule: " + ruleString);
            }
            Term head = termParser.parseTerm(headPart);
            List<Term> body = parseBody(parts[1].trim());
            return new Rule(head, body);
        } else {
            return new Rule(termParser.parseTerm(cleanRule), new ArrayList<>());
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

    // Split a string on commas that are not inside parentheses or braces
    private List<String> splitOnCommasOutsideParens(String input) {
        List<String> result = new ArrayList<>();
        int parenCount = 0;
        int braceCount = 0;
        int lastSplit = 0;
        
        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            if (c == '(') {
                parenCount++;
            } else if (c == ')') {
                parenCount--;
            } else if (c == '{') {
                braceCount++;
            } else if (c == '}') {
                braceCount--;
            } else if (c == ',' && parenCount == 0 && braceCount == 0) {
                result.add(input.substring(lastSplit, i));
                lastSplit = i + 1;
            }
        }
        result.add(input.substring(lastSplit));
        return result;
    }
    
    // Create a comma sequence from a list of terms: [a,b,c] -> ','(a, ','(b, c))
    private Term createCommaSequence(List<Term> terms) {
        if (terms.isEmpty()) {
            throw new IllegalArgumentException("Cannot create comma sequence from empty list");
        }
        if (terms.size() == 1) {
            return terms.get(0);
        }
        
        // Build right-associative comma structure: ','(first, ','(second, third))
        Term result = terms.get(terms.size() - 1);
        for (int i = terms.size() - 2; i >= 0; i--) {
            List<Term> commaArgs = new ArrayList<>();
            commaArgs.add(terms.get(i));
            commaArgs.add(result);
            result = new CompoundTerm(new Atom(","), commaArgs);
        }
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
    
    /**
     * Get the term parser.
     * 
     * @return The term parser
     */
    public TermParser getTermParser() {
        return termParser;
    }
}
