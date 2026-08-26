package it.denzosoft.jprolog.core.parser;

import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.exceptions.PrologParserException;
import it.denzosoft.jprolog.core.operator.OperatorTable;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.dcg.DCGTransformer;

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

    // START_CHANGE: ISS-2025-0085 - Accept shared OperatorTable
    /**
     * Create a parser with a shared operator table.
     */
    public Parser(OperatorTable operatorTable) {
        this(new TermParser(operatorTable));
    }
    // END_CHANGE: ISS-2025-0085

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
    
    // START_CHANGE: ISS-2025-0085 - Make extractClauses/parseRule accessible for incremental parsing
    /**
     * Extract clause strings from a Prolog program, handling comments and quotes.
     */
    public List<String> extractClauses(String program) {
        List<String> clauses = new ArrayList<>();
        StringBuilder currentClause = new StringBuilder();
        // START_CHANGE: ISS-2025-0447 - record each clause's 1-based start line alongside its text,
        // so Prolog.compile() can stamp Rule.sourceLine and the .jpc format (v0x03) can carry it:
        // without this a .jpc-loaded file had no line information and the IDE's line breakpoints
        // silently did nothing on compiled sources.
        lastClauseLines.clear();
        int line = 1;
        int clauseStartLine = -1;
        // END_CHANGE: ISS-2025-0447
        
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
                line++;                                            // ISS-2025-0447
                if (!inMultilineComment && !inQuotedString) {
                    currentClause.append(' '); // Replace newline with space to preserve structure
                }
                continue;
            }
            // ISS-2025-0447 - the clause starts at the first non-blank character after the last '.'
            if (clauseStartLine < 0 && !inSingleLineComment && !inMultilineComment
                    && !Character.isWhitespace(c) && c != '%') {
                clauseStartLine = line;
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
                        lastClauseLines.add(clauseStartLine < 0 ? -1 : clauseStartLine);   // ISS-2025-0447
                    }
                    currentClause = new StringBuilder();
                    clauseStartLine = -1;                                                  // ISS-2025-0447
                }
            } else {
                currentClause.append(c);
            }
        }
        
        // Handle any remaining content
        String remaining = currentClause.toString().trim();
        if (!remaining.isEmpty()) {
            clauses.add(remaining);
            lastClauseLines.add(clauseStartLine < 0 ? -1 : clauseStartLine);               // ISS-2025-0447
        }
        
        return clauses;
    }

    // START_CHANGE: ISS-2025-0447 - 1-based start line of each clause returned by the LAST
    // extractClauses() call on this Parser (same size and order as that list; -1 when unknown).
    private final List<Integer> lastClauseLines = new ArrayList<>();

    /** Start lines of the clauses returned by the most recent {@link #extractClauses} call. */
    public List<Integer> getLastClauseLines() {
        return java.util.Collections.unmodifiableList(new ArrayList<>(lastClauseLines));
    }
    // END_CHANGE: ISS-2025-0447

    /**
     * Parse a single clause string into a Rule.
     */
    public Rule parseRule(String ruleString) throws PrologParserException {
    // END_CHANGE: ISS-2025-0085
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
            // START_CHANGE: ISS-2025-0008 - Transform DCG rules properly
            // This is a DCG rule - transform it to proper Prolog rule
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
            if (containsTopLevelCommas(bodyPart)) {
                // Body contains comma-separated goals at top level, parse them individually and create comma structure
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
            
            // Transform the DCG rule using DCGTransformer
            try {
                DCGTransformer transformer = new DCGTransformer();
                Rule transformedRule = transformer.transformDCGRule((CompoundTerm) dcgTerm);
                return transformedRule;
            } catch (Exception e) {
                it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
                throw new PrologParserException("Error transforming DCG rule: " + e.getMessage(), e);
            }
            // END_CHANGE: ISS-2025-0008
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

    /**
     * Check if the string contains commas at the top level (not inside parentheses, brackets, or braces).
     * This helps distinguish between comma-separated goals and commas inside lists or other structures.
     */
    private boolean containsTopLevelCommas(String text) {
        int parenDepth = 0;
        int bracketDepth = 0;
        int braceDepth = 0;
        boolean inQuotes = false;
        char quoteChar = '\0';
        
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            
            // Handle quotes
            if (!inQuotes && (c == '"' || c == '\'')) {
                inQuotes = true;
                quoteChar = c;
                continue;
            } else if (inQuotes && c == quoteChar) {
                inQuotes = false;
                continue;
            }
            
            // Skip everything inside quotes
            if (inQuotes) {
                continue;
            }
            
            // Track nesting levels
            switch (c) {
                case '(':
                    parenDepth++;
                    break;
                case ')':
                    parenDepth--;
                    break;
                case '[':
                    bracketDepth++;
                    break;
                case ']':
                    bracketDepth--;
                    break;
                case '{':
                    braceDepth++;
                    break;
                case '}':
                    braceDepth--;
                    break;
                case ',':
                    // If we're at top level (no nesting), this is a top-level comma
                    if (parenDepth == 0 && bracketDepth == 0 && braceDepth == 0) {
                        return true;
                    }
                    break;
            }
        }
        
        return false;
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

    // Split a string on commas that are not inside parentheses, brackets, or braces
    private List<String> splitOnCommasOutsideParens(String input) {
        List<String> result = new ArrayList<>();
        int parenCount = 0;
        int braceCount = 0;
        int bracketCount = 0;
        boolean inQuotes = false;
        char quoteChar = '\0';
        int lastSplit = 0;
        
        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            
            // Handle quotes
            if (!inQuotes && (c == '"' || c == '\'')) {
                inQuotes = true;
                quoteChar = c;
            } else if (inQuotes && c == quoteChar) {
                inQuotes = false;
            }
            
            // Skip everything inside quotes
            if (inQuotes) {
                continue;
            }
            
            if (c == '(') {
                parenCount++;
            } else if (c == ')') {
                parenCount--;
            } else if (c == '{') {
                braceCount++;
            } else if (c == '}') {
                braceCount--;
            } else if (c == '[') {
                bracketCount++;
            } else if (c == ']') {
                bracketCount--;
            } else if (c == ',' && parenCount == 0 && braceCount == 0 && bracketCount == 0) {
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
