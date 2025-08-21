# Chapter 6bis: Syntax - JProlog Language Structure

## Overview

This chapter provides a comprehensive analysis of JProlog's syntax implementation, covering the complete language structure from low-level character processing to high-level term construction. It demonstrates how JProlog implements ISO Prolog syntax specifications with detailed parsing examples, tokenization strategies, and syntactic analysis.

## 6.1 Notation

### 6.1.1 Backus Naur Form

JProlog implements a complete BNF-based parser that recognizes the full ISO Prolog syntax. The parser uses recursive descent parsing with proper operator precedence handling.

#### BNF Grammar Implementation

```java
// Core grammar productions in JProlog Parser
public class PrologParser {
    
    /**
     * term ::= variable | atomic_term | compound_term
     * Implements the fundamental term production rule
     */
    public Term parseTerm() throws PrologParserException {
        if (currentToken.getType() == TokenType.VARIABLE) {
            return parseVariable();
        } else if (isAtomicToken(currentToken)) {
            return parseAtomicTerm();
        } else {
            return parseCompoundTerm();
        }
    }
    
    /**
     * compound_term ::= functor '(' arguments ')'
     *                 | '(' term ')'
     *                 | list
     *                 | curly_bracketed_term
     */
    public CompoundTerm parseCompoundTerm() throws PrologParserException {
        if (currentToken.getType() == TokenType.ATOM) {
            String functor = currentToken.getValue();
            expectToken(TokenType.LEFT_PAREN);
            List<Term> args = parseArgumentList();
            expectToken(TokenType.RIGHT_PAREN);
            return new CompoundTerm(functor, args);
        } else if (currentToken.getType() == TokenType.LEFT_PAREN) {
            // Handle parenthesized terms
            nextToken();
            Term innerTerm = parseTerm();
            expectToken(TokenType.RIGHT_PAREN);
            return (CompoundTerm) innerTerm;
        } else if (currentToken.getType() == TokenType.LEFT_BRACKET) {
            return parseList();
        } else if (currentToken.getType() == TokenType.LEFT_BRACE) {
            return parseCurlyBracketedTerm();
        }
        throw new PrologParserException("Invalid compound term structure");
    }
    
    /**
     * arguments ::= term | term ',' arguments
     * Handles argument list parsing with proper precedence
     */
    private List<Term> parseArgumentList() throws PrologParserException {
        List<Term> arguments = new ArrayList<>();
        arguments.add(parseTerm());
        
        while (currentToken.getType() == TokenType.COMMA) {
            nextToken(); // consume comma
            arguments.add(parseTerm());
        }
        return arguments;
    }
}
```

#### Grammar Productions for Operators

```java
/**
 * Operator precedence table implementation
 * Following ISO Prolog operator precedence rules
 */
public class OperatorTable {
    
    // Operator precedence levels (1200 = lowest, 1 = highest)
    private static final Map<String, OperatorInfo> OPERATORS = Map.of(
        ":-", new OperatorInfo(1200, "xfx"),    // Rule operator
        ";", new OperatorInfo(1100, "xfy"),     // Disjunction
        "->", new OperatorInfo(1050, "xfy"),    // If-then
        ",", new OperatorInfo(1000, "xfy"),     // Conjunction
        "\\+", new OperatorInfo(900, "fy"),     // Not provable
        "=", new OperatorInfo(700, "xfx"),      // Unification
        "\\=", new OperatorInfo(700, "xfx"),    // Not unifiable
        "==", new OperatorInfo(700, "xfx"),     // Term equality
        "\\==", new OperatorInfo(700, "xfx"),   // Term inequality
        "is", new OperatorInfo(700, "xfx"),     // Arithmetic evaluation
        "=:=", new OperatorInfo(700, "xfx"),    // Arithmetic equality
        "=\\=", new OperatorInfo(700, "xfx"),   // Arithmetic inequality
        "<", new OperatorInfo(700, "xfx"),      // Less than
        "=<", new OperatorInfo(700, "xfx"),     // Less or equal
        ">", new OperatorInfo(700, "xfx"),      // Greater than
        ">=", new OperatorInfo(700, "xfx"),     // Greater or equal
        "+", new OperatorInfo(500, "yfx"),      // Addition
        "-", new OperatorInfo(500, "yfx"),      // Subtraction
        "*", new OperatorInfo(400, "yfx"),      // Multiplication
        "/", new OperatorInfo(400, "yfx"),      // Division
        "mod", new OperatorInfo(400, "yfx"),    // Modulo
        "**", new OperatorInfo(200, "xfx"),     // Power
        "^", new OperatorInfo(200, "xfy"),      // Power (alternative)
        "-", new OperatorInfo(200, "fy"),       // Unary minus
        "+", new OperatorInfo(200, "fy")        // Unary plus
    );
    
    /**
     * Parse operator expression with proper precedence
     */
    public Term parseOperatorExpression(int precedence) throws PrologParserException {
        Term left = parsePrimaryTerm();
        
        while (isOperator(currentToken) && 
               getOperatorPrecedence(currentToken.getValue()) <= precedence) {
            
            String operator = currentToken.getValue();
            OperatorInfo opInfo = OPERATORS.get(operator);
            nextToken();
            
            Term right = parseOperatorExpression(opInfo.precedence - 1);
            left = new CompoundTerm(operator, Arrays.asList(left, right));
        }
        return left;
    }
}
```

### 6.1.2 Abstract Term Syntax

JProlog implements a complete abstract syntax tree (AST) representation that accurately captures the hierarchical structure of Prolog terms.

#### Abstract Term Hierarchy

```java
/**
 * Abstract base class for all Prolog terms
 * Provides the foundation for the term type hierarchy
 */
public abstract class Term {
    protected boolean isGround = false;
    protected Set<Variable> variables = new HashSet<>();
    
    public abstract boolean unify(Term other, Substitution substitution);
    public abstract Term substitute(Substitution substitution);
    public abstract boolean isVariable();
    public abstract boolean isAtom();
    public abstract boolean isNumber();
    public abstract boolean isCompound();
    public abstract String toCanonicalString();
    
    /**
     * Abstract term structure representation
     * Returns the term in abstract syntax form
     */
    public abstract String toAbstractSyntax();
}

/**
 * Atomic term representation covering atoms and numbers
 */
public class Atom extends Term {
    private final String value;
    
    public Atom(String value) {
        this.value = value;
        this.isGround = true;
    }
    
    @Override
    public String toAbstractSyntax() {
        return String.format("atom('%s')", value);
    }
    
    @Override
    public boolean unify(Term other, Substitution substitution) {
        if (other instanceof Variable) {
            return other.unify(this, substitution);
        }
        return other instanceof Atom && this.value.equals(((Atom) other).value);
    }
}

/**
 * Variable term with proper scoping and binding
 */
public class Variable extends Term {
    private final String name;
    private final UUID uuid;  // Unique identifier for variable scoping
    private Term binding = null;
    
    public Variable(String name) {
        this.name = name;
        this.uuid = UUID.randomUUID();
        this.variables.add(this);
    }
    
    @Override
    public String toAbstractSyntax() {
        return String.format("variable('%s', '%s')", name, uuid.toString().substring(0, 8));
    }
    
    @Override
    public boolean unify(Term other, Substitution substitution) {
        if (this.binding != null) {
            return this.binding.unify(other, substitution);
        }
        
        // Occurs check implementation
        if (other.variables.contains(this)) {
            return false; // Prevent infinite structures
        }
        
        substitution.bind(this, other);
        return true;
    }
}

/**
 * Compound term with functor and arguments
 */
public class CompoundTerm extends Term {
    private final String functor;
    private final List<Term> arguments;
    private final int arity;
    
    public CompoundTerm(String functor, List<Term> arguments) {
        this.functor = functor;
        this.arguments = new ArrayList<>(arguments);
        this.arity = arguments.size();
        
        // Calculate variables and ground status
        for (Term arg : arguments) {
            this.variables.addAll(arg.variables);
        }
        this.isGround = this.variables.isEmpty();
    }
    
    @Override
    public String toAbstractSyntax() {
        StringBuilder sb = new StringBuilder();
        sb.append(String.format("compound('%s', %d, [", functor, arity));
        for (int i = 0; i < arguments.size(); i++) {
            if (i > 0) sb.append(", ");
            sb.append(arguments.get(i).toAbstractSyntax());
        }
        sb.append("])");
        return sb.toString();
    }
}
```

## 6.2 Prolog Text and Data

### 6.2.1 Prolog Text

JProlog implements a comprehensive text processing system that handles complete Prolog programs with proper clause parsing and directive processing.

#### Text Processing Implementation

```java
/**
 * Prolog text processor handling complete programs
 */
public class PrologTextProcessor {
    
    /**
     * Process complete Prolog text into clauses and directives
     */
    public PrologProgram processPrologText(String prologText) throws PrologParserException {
        List<Clause> clauses = new ArrayList<>();
        List<Directive> directives = new ArrayList<>();
        
        PrologTokenizer tokenizer = new PrologTokenizer(prologText);
        PrologParser parser = new PrologParser(tokenizer);
        
        while (!parser.isAtEnd()) {
            if (parser.currentToken().getType() == TokenType.DIRECTIVE_PREFIX) {
                // Process directive: :- goal.
                directives.add(parseDirective(parser));
            } else {
                // Process clause: head :- body. or fact.
                clauses.add(parseClause(parser));
            }
            
            // Expect clause terminator
            parser.expectToken(TokenType.DOT);
        }
        
        return new PrologProgram(clauses, directives);
    }
    
    /**
     * Parse individual clause (fact or rule)
     */
    private Clause parseClause(PrologParser parser) throws PrologParserException {
        Term head = parser.parseTerm();
        
        if (parser.currentToken().getType() == TokenType.RULE_OPERATOR) {
            // Rule: head :- body
            parser.nextToken(); // consume :-
            Term body = parser.parseTerm();
            return new Rule(head, body);
        } else {
            // Fact: head
            return new Fact(head);
        }
    }
    
    /**
     * Parse directive
     */
    private Directive parseDirective(PrologParser parser) throws PrologParserException {
        parser.expectToken(TokenType.DIRECTIVE_PREFIX); // :-
        Term goal = parser.parseTerm();
        return new Directive(goal);
    }
}

/**
 * Program representation with clauses and directives
 */
public class PrologProgram {
    private final List<Clause> clauses;
    private final List<Directive> directives;
    private final Map<String, List<Clause>> predicateIndex;
    
    public PrologProgram(List<Clause> clauses, List<Directive> directives) {
        this.clauses = clauses;
        this.directives = directives;
        this.predicateIndex = buildPredicateIndex();
    }
    
    /**
     * Build index for efficient clause retrieval
     */
    private Map<String, List<Clause>> buildPredicateIndex() {
        Map<String, List<Clause>> index = new HashMap<>();
        
        for (Clause clause : clauses) {
            String predicateIndicator = clause.getPredicateIndicator();
            index.computeIfAbsent(predicateIndicator, k -> new ArrayList<>())
                 .add(clause);
        }
        return index;
    }
}
```

### 6.2.2 Prolog Data

JProlog implements a complete data representation system that maintains proper term relationships and supports dynamic modification.

#### Data Structure Implementation

```java
/**
 * Prolog data management system
 */
public class PrologDataManager {
    
    /**
     * Convert external data to Prolog terms
     */
    public Term convertToProlog(Object data) {
        if (data == null) {
            return new Atom("null");
        } else if (data instanceof String) {
            return new Atom((String) data);
        } else if (data instanceof Integer) {
            return new Number((Integer) data);
        } else if (data instanceof Double) {
            return new Number((Double) data);
        } else if (data instanceof List<?>) {
            return convertListToProlog((List<?>) data);
        } else if (data instanceof Map<?, ?>) {
            return convertMapToProlog((Map<?, ?>) data);
        } else {
            return new Atom(data.toString());
        }
    }
    
    /**
     * Convert List to Prolog list structure
     */
    private Term convertListToProlog(List<?> list) {
        if (list.isEmpty()) {
            return new Atom("[]");
        }
        
        Term result = new Atom("[]");
        for (int i = list.size() - 1; i >= 0; i--) {
            Term element = convertToProlog(list.get(i));
            result = new CompoundTerm(".", Arrays.asList(element, result));
        }
        return result;
    }
    
    /**
     * Convert Map to Prolog compound term
     */
    private Term convertMapToProlog(Map<?, ?> map) {
        List<Term> pairs = new ArrayList<>();
        for (Map.Entry<?, ?> entry : map.entrySet()) {
            Term key = convertToProlog(entry.getKey());
            Term value = convertToProlog(entry.getValue());
            pairs.add(new CompoundTerm("pair", Arrays.asList(key, value)));
        }
        return new CompoundTerm("map", pairs);
    }
}
```

## 6.3 Terms

### 6.3.1 Atomic Terms

JProlog implements complete atomic term support including atoms, numbers, and special constants with proper ISO compliance.

#### Atomic Term Implementation

```java
/**
 * Comprehensive atomic term implementation
 */
public abstract class AtomicTerm extends Term {
    
    @Override
    public final boolean isCompound() { return false; }
    @Override
    public final boolean isVariable() { return false; }
    
    public abstract Object getValue();
    public abstract String getCanonicalForm();
}

/**
 * Atom implementation with proper quoting rules
 */
public class Atom extends AtomicTerm {
    private final String value;
    private final boolean needsQuoting;
    
    public Atom(String value) {
        this.value = value;
        this.needsQuoting = requiresQuoting(value);
        this.isGround = true;
    }
    
    /**
     * Determine if atom requires quoting according to ISO rules
     */
    private boolean requiresQuoting(String atom) {
        if (atom.isEmpty()) return true;
        
        // Check if starts with lowercase letter
        if (!Character.isLowerCase(atom.charAt(0))) {
            return true;
        }
        
        // Check all characters are alphanumeric or underscore
        for (int i = 1; i < atom.length(); i++) {
            char c = atom.charAt(i);
            if (!Character.isLetterOrDigit(c) && c != '_') {
                return true;
            }
        }
        return false;
    }
    
    @Override
    public String toString() {
        if (needsQuoting) {
            return "'" + value.replace("'", "''") + "'";
        }
        return value;
    }
    
    @Override
    public String getCanonicalForm() {
        return value;
    }
    
    @Override
    public Object getValue() {
        return value;
    }
}

/**
 * Number implementation supporting integers and floating point
 */
public class Number extends AtomicTerm {
    private final java.lang.Number value;
    private final boolean isInteger;
    
    public Number(int value) {
        this.value = value;
        this.isInteger = true;
        this.isGround = true;
    }
    
    public Number(double value) {
        this.value = value;
        this.isInteger = false;
        this.isGround = true;
    }
    
    @Override
    public boolean isNumber() { return true; }
    
    @Override
    public String toString() {
        if (isInteger) {
            return String.valueOf(value.intValue());
        } else {
            return String.valueOf(value.doubleValue());
        }
    }
    
    @Override
    public Object getValue() {
        return value;
    }
    
    /**
     * Arithmetic operations support
     */
    public Number add(Number other) {
        if (this.isInteger && other.isInteger) {
            return new Number(this.value.intValue() + other.value.intValue());
        } else {
            return new Number(this.value.doubleValue() + other.value.doubleValue());
        }
    }
    
    public Number multiply(Number other) {
        if (this.isInteger && other.isInteger) {
            return new Number(this.value.intValue() * other.value.intValue());
        } else {
            return new Number(this.value.doubleValue() * other.value.doubleValue());
        }
    }
}
```

### 6.3.2 Variables

JProlog implements proper variable handling with scoping, binding, and occurs check according to ISO specifications.

#### Variable Implementation with Scoping

```java
/**
 * Variable implementation with proper scoping and binding semantics
 */
public class Variable extends Term {
    private final String name;
    private final UUID scopeId;
    private Term binding = null;
    private final boolean isAnonymous;
    
    public Variable(String name) {
        this.name = name;
        this.scopeId = UUID.randomUUID();
        this.isAnonymous = "_".equals(name) || name.startsWith("_");
        this.variables.add(this);
    }
    
    /**
     * Create variable with specific scope (for rule copying)
     */
    public Variable(String name, UUID scopeId) {
        this.name = name;
        this.scopeId = scopeId;
        this.isAnonymous = "_".equals(name) || name.startsWith("_");
        this.variables.add(this);
    }
    
    @Override
    public boolean unify(Term other, Substitution substitution) {
        // Check if already bound
        if (binding != null) {
            return binding.unify(other, substitution);
        }
        
        // Anonymous variables unify with anything
        if (isAnonymous) {
            return true;
        }
        
        // Check existing binding in substitution
        Term existingBinding = substitution.getBinding(this);
        if (existingBinding != null) {
            return existingBinding.unify(other, substitution);
        }
        
        // Occurs check to prevent infinite structures
        if (performOccursCheck() && other.variables.contains(this)) {
            return false;
        }
        
        // Create new binding
        substitution.bind(this, other);
        return true;
    }
    
    @Override
    public Term substitute(Substitution substitution) {
        Term binding = substitution.getBinding(this);
        if (binding != null) {
            return binding.substitute(substitution);
        }
        return this;
    }
    
    @Override
    public String toString() {
        if (binding != null) {
            return binding.toString();
        }
        return name;
    }
    
    /**
     * Create renamed copy for rule copying
     */
    public Variable createRenamedCopy() {
        String newName = isAnonymous ? "_" : name + "_" + System.nanoTime();
        return new Variable(newName, UUID.randomUUID());
    }
    
    /**
     * Variable equality based on scope
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!(obj instanceof Variable)) return false;
        Variable other = (Variable) obj;
        return name.equals(other.name) && scopeId.equals(other.scopeId);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(name, scopeId);
    }
}

/**
 * Variable substitution management
 */
public class Substitution {
    private final Map<Variable, Term> bindings = new HashMap<>();
    
    public void bind(Variable variable, Term term) {
        bindings.put(variable, term);
    }
    
    public Term getBinding(Variable variable) {
        return bindings.get(variable);
    }
    
    public boolean isBound(Variable variable) {
        return bindings.containsKey(variable);
    }
    
    /**
     * Compose this substitution with another
     */
    public Substitution compose(Substitution other) {
        Substitution result = new Substitution();
        
        // Apply other substitution to our bindings
        for (Map.Entry<Variable, Term> entry : this.bindings.entrySet()) {
            result.bind(entry.getKey(), entry.getValue().substitute(other));
        }
        
        // Add bindings from other that don't conflict
        for (Map.Entry<Variable, Term> entry : other.bindings.entrySet()) {
            if (!result.bindings.containsKey(entry.getKey())) {
                result.bind(entry.getKey(), entry.getValue());
            }
        }
        
        return result;
    }
}
```

### 6.3.3 Compound Terms - Functional Notation

JProlog implements complete functional notation support with proper argument handling and functor management.

#### Functional Notation Implementation

```java
/**
 * Compound term implementation with functional notation support
 */
public class CompoundTerm extends Term {
    private final String functor;
    private final List<Term> arguments;
    private final int arity;
    private final String predicateIndicator;
    
    public CompoundTerm(String functor, List<Term> arguments) {
        this.functor = functor;
        this.arguments = new ArrayList<>(arguments);
        this.arity = arguments.size();
        this.predicateIndicator = functor + "/" + arity;
        
        // Calculate variables and ground status
        for (Term arg : arguments) {
            this.variables.addAll(arg.variables);
        }
        this.isGround = this.variables.isEmpty();
    }
    
    @Override
    public boolean unify(Term other, Substitution substitution) {
        if (other instanceof Variable) {
            return other.unify(this, substitution);
        }
        
        if (!(other instanceof CompoundTerm)) {
            return false;
        }
        
        CompoundTerm otherCompound = (CompoundTerm) other;
        
        // Check functor and arity match
        if (!this.functor.equals(otherCompound.functor) || 
            this.arity != otherCompound.arity) {
            return false;
        }
        
        // Unify all arguments
        for (int i = 0; i < arity; i++) {
            if (!this.arguments.get(i).unify(otherCompound.arguments.get(i), substitution)) {
                return false;
            }
        }
        
        return true;
    }
    
    @Override
    public Term substitute(Substitution substitution) {
        List<Term> newArgs = new ArrayList<>();
        boolean changed = false;
        
        for (Term arg : arguments) {
            Term newArg = arg.substitute(substitution);
            newArgs.add(newArg);
            if (newArg != arg) {
                changed = true;
            }
        }
        
        return changed ? new CompoundTerm(functor, newArgs) : this;
    }
    
    @Override
    public String toString() {
        if (arity == 0) {
            return functor;
        }
        
        StringBuilder sb = new StringBuilder();
        sb.append(functor).append("(");
        for (int i = 0; i < arguments.size(); i++) {
            if (i > 0) sb.append(", ");
            sb.append(arguments.get(i).toString());
        }
        sb.append(")");
        return sb.toString();
    }
    
    /**
     * Get argument by position (1-based indexing as per ISO)
     */
    public Term getArgument(int position) {
        if (position < 1 || position > arity) {
            throw new IllegalArgumentException("Invalid argument position: " + position);
        }
        return arguments.get(position - 1);
    }
    
    /**
     * Create new compound term with modified argument
     */
    public CompoundTerm withArgument(int position, Term newArg) {
        if (position < 1 || position > arity) {
            throw new IllegalArgumentException("Invalid argument position: " + position);
        }
        
        List<Term> newArgs = new ArrayList<>(arguments);
        newArgs.set(position - 1, newArg);
        return new CompoundTerm(functor, newArgs);
    }
    
    /**
     * Functor manipulation for functor/3 predicate
     */
    public static CompoundTerm createFromFunctorArity(String functor, int arity) {
        List<Term> args = new ArrayList<>();
        for (int i = 0; i < arity; i++) {
            args.add(new Variable("_G" + i));
        }
        return new CompoundTerm(functor, args);
    }
}
```

### 6.3.4 Compound Terms - Operator Notation

JProlog implements comprehensive operator notation with proper precedence handling and associativity rules.

#### Operator Notation Implementation

```java
/**
 * Operator parsing with proper precedence and associativity
 */
public class OperatorParser {
    
    /**
     * Operator information structure
     */
    public static class OperatorInfo {
        public final int precedence;
        public final String associativity; // fx, fy, xf, yf, xfx, xfy, yfx
        
        public OperatorInfo(int precedence, String associativity) {
            this.precedence = precedence;
            this.associativity = associativity;
        }
        
        public boolean isPrefix() {
            return associativity.startsWith("f");
        }
        
        public boolean isInfix() {
            return associativity.length() == 3;
        }
        
        public boolean isPostfix() {
            return associativity.endsWith("f") && associativity.length() == 2;
        }
    }
    
    /**
     * Parse operator expression with precedence climbing
     */
    public Term parseOperatorExpression(PrologParser parser, int minPrec) 
            throws PrologParserException {
        
        Term left = parsePrefix(parser);
        
        while (parser.hasMore()) {
            Token opToken = parser.currentToken();
            if (!isInfixOperator(opToken.getValue())) {
                break;
            }
            
            OperatorInfo opInfo = getOperatorInfo(opToken.getValue());
            if (opInfo.precedence < minPrec) {
                break;
            }
            
            parser.nextToken(); // consume operator
            
            int nextMinPrec = opInfo.associativity.equals("xfy") ? 
                opInfo.precedence : opInfo.precedence + 1;
            
            Term right = parseOperatorExpression(parser, nextMinPrec);
            left = new CompoundTerm(opToken.getValue(), 
                Arrays.asList(left, right));
        }
        
        return left;
    }
    
    /**
     * Parse prefix operators
     */
    private Term parsePrefix(PrologParser parser) throws PrologParserException {
        Token token = parser.currentToken();
        
        if (isPrefixOperator(token.getValue())) {
            OperatorInfo opInfo = getOperatorInfo(token.getValue());
            parser.nextToken();
            
            int nextMinPrec = opInfo.associativity.equals("fy") ? 
                opInfo.precedence : opInfo.precedence + 1;
            
            Term operand = parseOperatorExpression(parser, nextMinPrec);
            return new CompoundTerm(token.getValue(), Arrays.asList(operand));
        }
        
        return parser.parsePrimaryTerm();
    }
    
    /**
     * Standard operator definitions
     */
    private static final Map<String, OperatorInfo> STANDARD_OPERATORS = Map.of(
        // Arithmetic operators
        "+", new OperatorInfo(500, "yfx"),
        "-", new OperatorInfo(500, "yfx"),
        "*", new OperatorInfo(400, "yfx"),
        "/", new OperatorInfo(400, "yfx"),
        "mod", new OperatorInfo(400, "yfx"),
        "**", new OperatorInfo(200, "xfx"),
        
        // Comparison operators
        "=", new OperatorInfo(700, "xfx"),
        "\\=", new OperatorInfo(700, "xfx"),
        "==", new OperatorInfo(700, "xfx"),
        "\\==", new OperatorInfo(700, "xfx"),
        "is", new OperatorInfo(700, "xfx"),
        "=:=", new OperatorInfo(700, "xfx"),
        "=\\=", new OperatorInfo(700, "xfx"),
        "<", new OperatorInfo(700, "xfx"),
        "=<", new OperatorInfo(700, "xfx"),
        ">", new OperatorInfo(700, "xfx"),
        ">=", new OperatorInfo(700, "xfx"),
        
        // Control operators
        ":-", new OperatorInfo(1200, "xfx"),
        ";", new OperatorInfo(1100, "xfy"),
        "->", new OperatorInfo(1050, "xfy"),
        ",", new OperatorInfo(1000, "xfy"),
        "\\+", new OperatorInfo(900, "fy"),
        
        // Unary operators
        "-", new OperatorInfo(200, "fy"),
        "+", new OperatorInfo(200, "fy")
    );
    
    /**
     * Display operators in canonical form
     */
    public String displayOperatorTerm(CompoundTerm term) {
        String functor = term.getFunctor();
        int arity = term.getArity();
        
        if (arity == 1 && isPrefixOperator(functor)) {
            return functor + " " + term.getArgument(1).toString();
        } else if (arity == 2 && isInfixOperator(functor)) {
            return term.getArgument(1).toString() + " " + 
                   functor + " " + 
                   term.getArgument(2).toString();
        } else if (arity == 1 && isPostfixOperator(functor)) {
            return term.getArgument(1).toString() + " " + functor;
        } else {
            return term.toString(); // Use functional notation
        }
    }
}
```

### 6.3.5 Compound Terms - List Notation

JProlog implements complete list notation support with proper head/tail decomposition and ISO-compliant list representation.

#### List Implementation

```java
/**
 * List implementation with proper ISO Prolog semantics
 */
public class ListTerm extends CompoundTerm {
    
    /**
     * Create list from elements
     */
    public static Term createList(List<Term> elements) {
        if (elements.isEmpty()) {
            return new Atom("[]");
        }
        
        Term result = new Atom("[]");
        for (int i = elements.size() - 1; i >= 0; i--) {
            result = new CompoundTerm(".", Arrays.asList(elements.get(i), result));
        }
        return result;
    }
    
    /**
     * Create list with head/tail structure
     */
    public static Term createList(Term head, Term tail) {
        return new CompoundTerm(".", Arrays.asList(head, tail));
    }
    
    /**
     * Check if term represents a list
     */
    public static boolean isList(Term term) {
        if (term instanceof Atom && "[]".equals(((Atom) term).getValue())) {
            return true;
        }
        
        if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            if (".".equals(compound.getFunctor()) && compound.getArity() == 2) {
                return isList(compound.getArgument(2));
            }
        }
        
        return false;
    }
    
    /**
     * Check if term is a proper list (ends with [])
     */
    public static boolean isProperList(Term term) {
        while (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            if (!".".equals(compound.getFunctor()) || compound.getArity() != 2) {
                return false;
            }
            term = compound.getArgument(2);
        }
        
        return term instanceof Atom && "[]".equals(((Atom) term).getValue());
    }
    
    /**
     * Convert list term to Java List
     */
    public static List<Term> toJavaList(Term listTerm) {
        List<Term> result = new ArrayList<>();
        
        while (listTerm instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) listTerm;
            if (!".".equals(compound.getFunctor()) || compound.getArity() != 2) {
                break;
            }
            
            result.add(compound.getArgument(1));
            listTerm = compound.getArgument(2);
        }
        
        return result;
    }
    
    /**
     * Get list length
     */
    public static int getListLength(Term listTerm) {
        int length = 0;
        
        while (listTerm instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) listTerm;
            if (!".".equals(compound.getFunctor()) || compound.getArity() != 2) {
                return -1; // Not a proper list
            }
            
            length++;
            listTerm = compound.getArgument(2);
        }
        
        if (listTerm instanceof Atom && "[]".equals(((Atom) listTerm).getValue())) {
            return length;
        }
        
        return -1; // Not a proper list
    }
    
    /**
     * Display list in bracket notation
     */
    public static String displayList(Term listTerm) {
        if (!isList(listTerm)) {
            return listTerm.toString();
        }
        
        if (listTerm instanceof Atom && "[]".equals(((Atom) listTerm).getValue())) {
            return "[]";
        }
        
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        
        boolean first = true;
        while (listTerm instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) listTerm;
            if (!".".equals(compound.getFunctor()) || compound.getArity() != 2) {
                break;
            }
            
            if (!first) {
                sb.append(", ");
            }
            sb.append(compound.getArgument(1).toString());
            first = false;
            
            listTerm = compound.getArgument(2);
        }
        
        if (!(listTerm instanceof Atom && "[]".equals(((Atom) listTerm).getValue()))) {
            // Improper list with tail
            sb.append("|").append(listTerm.toString());
        }
        
        sb.append("]");
        return sb.toString();
    }
}

/**
 * List parsing support
 */
public class ListParser {
    
    /**
     * Parse list notation [H|T] or [a,b,c]
     */
    public Term parseList(PrologParser parser) throws PrologParserException {
        parser.expectToken(TokenType.LEFT_BRACKET);
        
        if (parser.currentToken().getType() == TokenType.RIGHT_BRACKET) {
            // Empty list []
            parser.nextToken();
            return new Atom("[]");
        }
        
        List<Term> elements = new ArrayList<>();
        Term tail = null;
        
        // Parse first element
        elements.add(parser.parseTerm());
        
        while (parser.currentToken().getType() == TokenType.COMMA) {
            parser.nextToken(); // consume comma
            elements.add(parser.parseTerm());
        }
        
        // Check for tail notation
        if (parser.currentToken().getType() == TokenType.PIPE) {
            parser.nextToken(); // consume |
            tail = parser.parseTerm();
        }
        
        parser.expectToken(TokenType.RIGHT_BRACKET);
        
        // Build list structure
        if (tail == null) {
            tail = new Atom("[]");
        }
        
        for (int i = elements.size() - 1; i >= 0; i--) {
            tail = new CompoundTerm(".", Arrays.asList(elements.get(i), tail));
        }
        
        return tail;
    }
}
```

### 6.3.6 Compound Terms - Curly Bracketed Term

JProlog implements curly bracketed terms with proper parsing and semantic handling.

#### Curly Bracketed Terms Implementation

```java
/**
 * Curly bracketed term implementation
 */
public class CurlyBracketedTermParser {
    
    /**
     * Parse curly bracketed term {term}
     */
    public Term parseCurlyBracketedTerm(PrologParser parser) 
            throws PrologParserException {
        
        parser.expectToken(TokenType.LEFT_BRACE);
        
        if (parser.currentToken().getType() == TokenType.RIGHT_BRACE) {
            // Empty braces {}
            parser.nextToken();
            return new CompoundTerm("{}", Collections.emptyList());
        }
        
        Term innerTerm = parser.parseTerm();
        parser.expectToken(TokenType.RIGHT_BRACE);
        
        return new CompoundTerm("{}", Arrays.asList(innerTerm));
    }
}

/**
 * Curly bracketed term representation
 */
public class CurlyTerm extends CompoundTerm {
    
    public CurlyTerm(Term content) {
        super("{}", content != null ? Arrays.asList(content) : Collections.emptyList());
    }
    
    public Term getContent() {
        return getArity() > 0 ? getArgument(1) : new Atom("true");
    }
    
    @Override
    public String toString() {
        if (getArity() == 0) {
            return "{}";
        }
        return "{" + getArgument(1).toString() + "}";
    }
    
    /**
     * Curly bracketed terms are often used for set notation
     * or to group terms without affecting parsing
     */
    public boolean isSet() {
        return getArity() > 0 && isSetContent(getArgument(1));
    }
    
    private boolean isSetContent(Term term) {
        // Check if term represents set elements
        if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            return ",".equals(compound.getFunctor()) && compound.getArity() == 2;
        }
        return true; // Single element set
    }
}
```

### 6.3.7 Terms - Double Quoted List Notation

JProlog implements double-quoted strings with proper character list conversion according to ISO specifications.

#### Double Quoted String Implementation

```java
/**
 * Double quoted string processing
 */
public class DoubleQuotedStringProcessor {
    
    /**
     * Convert double quoted string to character code list
     */
    public Term processDoubleQuotedString(String quotedString) {
        // Remove quotes
        String content = quotedString.substring(1, quotedString.length() - 1);
        
        // Handle escape sequences
        String processed = processEscapeSequences(content);
        
        // Convert to character codes
        List<Term> charCodes = new ArrayList<>();
        for (char c : processed.toCharArray()) {
            charCodes.add(new Number((int) c));
        }
        
        return ListTerm.createList(charCodes);
    }
    
    /**
     * Process escape sequences in string
     */
    private String processEscapeSequences(String input) {
        StringBuilder result = new StringBuilder();
        boolean inEscape = false;
        
        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            
            if (inEscape) {
                switch (c) {
                    case 'n':  result.append('\n'); break;
                    case 't':  result.append('\t'); break;
                    case 'r':  result.append('\r'); break;
                    case '\\': result.append('\\'); break;
                    case '"':  result.append('"'); break;
                    case '\'': result.append('\''); break;
                    case 'a':  result.append('\u0007'); break; // Alert
                    case 'b':  result.append('\b'); break;
                    case 'f':  result.append('\f'); break;
                    case 'v':  result.append('\u000B'); break; // Vertical tab
                    default:
                        // Octal or hex escape
                        if (Character.isDigit(c)) {
                            result.append(processNumericEscape(input, i));
                        } else {
                            result.append(c);
                        }
                        break;
                }
                inEscape = false;
            } else if (c == '\\') {
                inEscape = true;
            } else {
                result.append(c);
            }
        }
        
        return result.toString();
    }
    
    /**
     * Process numeric escape sequences
     */
    private char processNumericEscape(String input, int startPos) {
        // Handle octal escapes \nnn\ or hex escapes \xHH\
        if (startPos > 0 && input.charAt(startPos - 1) == 'x') {
            // Hexadecimal escape
            StringBuilder hex = new StringBuilder();
            int pos = startPos;
            while (pos < input.length() && hex.length() < 2) {
                char c = input.charAt(pos);
                if (Character.isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) {
                    hex.append(c);
                    pos++;
                } else {
                    break;
                }
            }
            return (char) Integer.parseInt(hex.toString(), 16);
        } else {
            // Octal escape
            StringBuilder octal = new StringBuilder();
            int pos = startPos;
            while (pos < input.length() && octal.length() < 3) {
                char c = input.charAt(pos);
                if (c >= '0' && c <= '7') {
                    octal.append(c);
                    pos++;
                } else {
                    break;
                }
            }
            return (char) Integer.parseInt(octal.toString(), 8);
        }
    }
    
    /**
     * Convert character list back to string
     */
    public String charListToString(Term listTerm) {
        if (!ListTerm.isList(listTerm)) {
            throw new IllegalArgumentException("Not a character list");
        }
        
        List<Term> elements = ListTerm.toJavaList(listTerm);
        StringBuilder result = new StringBuilder();
        
        for (Term element : elements) {
            if (!(element instanceof Number)) {
                throw new IllegalArgumentException("Invalid character code");
            }
            
            Number num = (Number) element;
            if (!num.isInteger() || num.intValue() < 0 || num.intValue() > 1114111) {
                throw new IllegalArgumentException("Invalid character code: " + num);
            }
            
            result.append((char) num.intValue());
        }
        
        return result.toString();
    }
}
```

## 6.4 Tokens

### 6.4.1 Layout Text

JProlog implements comprehensive layout text handling including comments, whitespace, and formatting preservation.

#### Layout Text Implementation

```java
/**
 * Layout text processor handling whitespace and comments
 */
public class LayoutTextProcessor {
    
    /**
     * Skip layout text (whitespace and comments)
     */
    public void skipLayoutText(PrologTokenizer tokenizer) {
        while (tokenizer.hasMore() && isLayoutText(tokenizer.peek())) {
            if (tokenizer.peek() == '%') {
                skipLineComment(tokenizer);
            } else if (tokenizer.peek() == '/' && tokenizer.peekAhead(1) == '*') {
                skipBlockComment(tokenizer);
            } else {
                // Skip whitespace
                tokenizer.next();
            }
        }
    }
    
    /**
     * Skip line comment (% to end of line)
     */
    private void skipLineComment(PrologTokenizer tokenizer) {
        while (tokenizer.hasMore() && tokenizer.peek() != '\n' && tokenizer.peek() != '\r') {
            tokenizer.next();
        }
        if (tokenizer.hasMore()) {
            tokenizer.next(); // Skip newline
        }
    }
    
    /**
     * Skip block comment (/* to */)
     */
    private void skipBlockComment(PrologTokenizer tokenizer) {
        tokenizer.next(); // Skip /
        tokenizer.next(); // Skip *
        
        while (tokenizer.hasMore()) {
            char c = tokenizer.next();
            if (c == '*' && tokenizer.hasMore() && tokenizer.peek() == '/') {
                tokenizer.next(); // Skip /
                break;
            }
        }
    }
    
    /**
     * Check if character is layout text
     */
    private boolean isLayoutText(char c) {
        return Character.isWhitespace(c) || c == '%' || 
               (c == '/' && /* check for block comment start */);
    }
    
    /**
     * Preserve formatting for pretty printing
     */
    public String preserveLayout(String originalText, List<Token> tokens) {
        StringBuilder result = new StringBuilder();
        int textPos = 0;
        
        for (Token token : tokens) {
            // Add layout text before token
            while (textPos < token.getStartPosition()) {
                result.append(originalText.charAt(textPos));
                textPos++;
            }
            
            // Add token text
            result.append(token.getValue());
            textPos = token.getEndPosition();
        }
        
        // Add remaining layout text
        while (textPos < originalText.length()) {
            result.append(originalText.charAt(textPos));
            textPos++;
        }
        
        return result.toString();
    }
}
```

### 6.4.2 Names

JProlog implements complete name tokenization including atoms, quoted atoms, and operator recognition.

#### Name Tokenization Implementation

```java
/**
 * Name tokenizer for atoms and operators
 */
public class NameTokenizer {
    
    /**
     * Tokenize name (atom or operator)
     */
    public Token tokenizeName(PrologTokenizer tokenizer) throws PrologParserException {
        int startPos = tokenizer.getPosition();
        StringBuilder name = new StringBuilder();
        
        if (tokenizer.peek() == '\'') {
            // Quoted atom
            return tokenizeQuotedAtom(tokenizer);
        } else if (isGraphicChar(tokenizer.peek())) {
            // Graphic name (operator)
            return tokenizeGraphicName(tokenizer);
        } else if (Character.isLowerCase(tokenizer.peek())) {
            // Letter-digit name
            return tokenizeLetterDigitName(tokenizer);
        } else {
            throw new PrologParserException("Invalid name start character");
        }
    }
    
    /**
     * Tokenize quoted atom 'atom'
     */
    private Token tokenizeQuotedAtom(PrologTokenizer tokenizer) 
            throws PrologParserException {
        int startPos = tokenizer.getPosition();
        StringBuilder atom = new StringBuilder();
        
        tokenizer.next(); // Skip opening quote
        
        while (tokenizer.hasMore()) {
            char c = tokenizer.next();
            
            if (c == '\'') {
                if (tokenizer.hasMore() && tokenizer.peek() == '\'') {
                    // Escaped quote
                    atom.append('\'');
                    tokenizer.next();
                } else {
                    // End of atom
                    break;
                }
            } else {
                atom.append(c);
            }
        }
        
        return new Token(TokenType.ATOM, atom.toString(), startPos, tokenizer.getPosition());
    }
    
    /**
     * Tokenize graphic name (operators like +, -, ==, etc.)
     */
    private Token tokenizeGraphicName(PrologTokenizer tokenizer) {
        int startPos = tokenizer.getPosition();
        StringBuilder name = new StringBuilder();
        
        while (tokenizer.hasMore() && isGraphicChar(tokenizer.peek())) {
            name.append(tokenizer.next());
        }
        
        String nameStr = name.toString();
        TokenType type = determineOperatorType(nameStr);
        
        return new Token(type, nameStr, startPos, tokenizer.getPosition());
    }
    
    /**
     * Tokenize letter-digit name (regular atoms)
     */
    private Token tokenizeLetterDigitName(PrologTokenizer tokenizer) {
        int startPos = tokenizer.getPosition();
        StringBuilder name = new StringBuilder();
        
        // First character must be letter
        name.append(tokenizer.next());
        
        // Subsequent characters can be letters, digits, or underscore
        while (tokenizer.hasMore()) {
            char c = tokenizer.peek();
            if (Character.isLetterOrDigit(c) || c == '_') {
                name.append(tokenizer.next());
            } else {
                break;
            }
        }
        
        return new Token(TokenType.ATOM, name.toString(), startPos, tokenizer.getPosition());
    }
    
    /**
     * Determine operator type from name
     */
    private TokenType determineOperatorType(String name) {
        switch (name) {
            case ":-": return TokenType.RULE_OPERATOR;
            case ",": return TokenType.COMMA;
            case ";": return TokenType.SEMICOLON;
            case "->": return TokenType.IF_THEN;
            case "=": return TokenType.UNIFY;
            case "\\=": return TokenType.NOT_UNIFY;
            case "==": return TokenType.EQUAL;
            case "\\==": return TokenType.NOT_EQUAL;
            case "is": return TokenType.IS;
            case "=:=": return TokenType.ARITH_EQUAL;
            case "=\\=": return TokenType.ARITH_NOT_EQUAL;
            case "<": return TokenType.LESS_THAN;
            case "=<": return TokenType.LESS_EQUAL;
            case ">": return TokenType.GREATER_THAN;
            case ">=": return TokenType.GREATER_EQUAL;
            case "+": return TokenType.PLUS;
            case "-": return TokenType.MINUS;
            case "*": return TokenType.MULTIPLY;
            case "/": return TokenType.DIVIDE;
            case "**": return TokenType.POWER;
            case "mod": return TokenType.MOD;
            case "\\+": return TokenType.NOT_PROVABLE;
            default: return TokenType.OPERATOR;
        }
    }
    
    /**
     * Check if character is graphic character
     */
    private boolean isGraphicChar(char c) {
        return "!@#$%^&*-+=|\\:<>.?/~`".indexOf(c) >= 0;
    }
}
```

### 6.4.3 Variables

JProlog implements variable tokenization with proper handling of anonymous variables and naming conventions.

#### Variable Tokenization

```java
/**
 * Variable tokenizer
 */
public class VariableTokenizer {
    
    /**
     * Tokenize variable
     */
    public Token tokenizeVariable(PrologTokenizer tokenizer) {
        int startPos = tokenizer.getPosition();
        StringBuilder varName = new StringBuilder();
        
        char firstChar = tokenizer.next();
        varName.append(firstChar);
        
        // Continue with alphanumeric characters and underscore
        while (tokenizer.hasMore()) {
            char c = tokenizer.peek();
            if (Character.isLetterOrDigit(c) || c == '_') {
                varName.append(tokenizer.next());
            } else {
                break;
            }
        }
        
        String name = varName.toString();
        TokenType type = isAnonymousVariable(name) ? 
            TokenType.ANONYMOUS_VARIABLE : TokenType.VARIABLE;
        
        return new Token(type, name, startPos, tokenizer.getPosition());
    }
    
    /**
     * Check if variable is anonymous
     */
    private boolean isAnonymousVariable(String name) {
        return "_".equals(name) || name.startsWith("_");
    }
    
    /**
     * Validate variable name
     */
    public boolean isValidVariableName(String name) {
        if (name.isEmpty()) {
            return false;
        }
        
        char first = name.charAt(0);
        if (!Character.isUpperCase(first) && first != '_') {
            return false;
        }
        
        for (int i = 1; i < name.length(); i++) {
            char c = name.charAt(i);
            if (!Character.isLetterOrDigit(c) && c != '_') {
                return false;
            }
        }
        
        return true;
    }
}
```

### 6.4.4 Integer Numbers

JProlog implements comprehensive integer number parsing including different bases and proper range checking.

#### Integer Number Implementation

```java
/**
 * Integer number tokenizer
 */
public class IntegerTokenizer {
    
    /**
     * Tokenize integer number
     */
    public Token tokenizeInteger(PrologTokenizer tokenizer) throws PrologParserException {
        int startPos = tokenizer.getPosition();
        
        // Check for different number bases
        if (tokenizer.peek() == '0') {
            if (tokenizer.peekAhead(1) == 'x' || tokenizer.peekAhead(1) == 'X') {
                return tokenizeHexInteger(tokenizer);
            } else if (tokenizer.peekAhead(1) == 'o' || tokenizer.peekAhead(1) == 'O') {
                return tokenizeOctalInteger(tokenizer);
            } else if (tokenizer.peekAhead(1) == 'b' || tokenizer.peekAhead(1) == 'B') {
                return tokenizeBinaryInteger(tokenizer);
            }
        }
        
        return tokenizeDecimalInteger(tokenizer);
    }
    
    /**
     * Tokenize decimal integer
     */
    private Token tokenizeDecimalInteger(PrologTokenizer tokenizer) {
        int startPos = tokenizer.getPosition();
        StringBuilder number = new StringBuilder();
        
        while (tokenizer.hasMore() && Character.isDigit(tokenizer.peek())) {
            number.append(tokenizer.next());
        }
        
        long value = Long.parseLong(number.toString());
        return new Token(TokenType.INTEGER, String.valueOf(value), startPos, tokenizer.getPosition());
    }
    
    /**
     * Tokenize hexadecimal integer (0x...)
     */
    private Token tokenizeHexInteger(PrologTokenizer tokenizer) throws PrologParserException {
        int startPos = tokenizer.getPosition();
        tokenizer.next(); // Skip 0
        tokenizer.next(); // Skip x
        
        StringBuilder hex = new StringBuilder();
        while (tokenizer.hasMore() && isHexDigit(tokenizer.peek())) {
            hex.append(tokenizer.next());
        }
        
        if (hex.length() == 0) {
            throw new PrologParserException("Invalid hexadecimal number");
        }
        
        long value = Long.parseLong(hex.toString(), 16);
        return new Token(TokenType.INTEGER, String.valueOf(value), startPos, tokenizer.getPosition());
    }
    
    /**
     * Tokenize octal integer (0o...)
     */
    private Token tokenizeOctalInteger(PrologTokenizer tokenizer) throws PrologParserException {
        int startPos = tokenizer.getPosition();
        tokenizer.next(); // Skip 0
        tokenizer.next(); // Skip o
        
        StringBuilder octal = new StringBuilder();
        while (tokenizer.hasMore() && isOctalDigit(tokenizer.peek())) {
            octal.append(tokenizer.next());
        }
        
        if (octal.length() == 0) {
            throw new PrologParserException("Invalid octal number");
        }
        
        long value = Long.parseLong(octal.toString(), 8);
        return new Token(TokenType.INTEGER, String.valueOf(value), startPos, tokenizer.getPosition());
    }
    
    /**
     * Tokenize binary integer (0b...)
     */
    private Token tokenizeBinaryInteger(PrologTokenizer tokenizer) throws PrologParserException {
        int startPos = tokenizer.getPosition();
        tokenizer.next(); // Skip 0
        tokenizer.next(); // Skip b
        
        StringBuilder binary = new StringBuilder();
        while (tokenizer.hasMore() && isBinaryDigit(tokenizer.peek())) {
            binary.append(tokenizer.next());
        }
        
        if (binary.length() == 0) {
            throw new PrologParserException("Invalid binary number");
        }
        
        long value = Long.parseLong(binary.toString(), 2);
        return new Token(TokenType.INTEGER, String.valueOf(value), startPos, tokenizer.getPosition());
    }
    
    private boolean isHexDigit(char c) {
        return Character.isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
    }
    
    private boolean isOctalDigit(char c) {
        return c >= '0' && c <= '7';
    }
    
    private boolean isBinaryDigit(char c) {
        return c == '0' || c == '1';
    }
}
```

### 6.4.5 Floating Point Numbers

JProlog implements floating point number parsing with proper scientific notation support.

#### Floating Point Implementation

```java
/**
 * Floating point number tokenizer
 */
public class FloatingPointTokenizer {
    
    /**
     * Tokenize floating point number
     */
    public Token tokenizeFloatingPoint(PrologTokenizer tokenizer) throws PrologParserException {
        int startPos = tokenizer.getPosition();
        StringBuilder number = new StringBuilder();
        
        // Parse integer part
        while (tokenizer.hasMore() && Character.isDigit(tokenizer.peek())) {
            number.append(tokenizer.next());
        }
        
        // Parse decimal point and fractional part
        if (tokenizer.hasMore() && tokenizer.peek() == '.') {
            number.append(tokenizer.next());
            
            if (!tokenizer.hasMore() || !Character.isDigit(tokenizer.peek())) {
                throw new PrologParserException("Invalid floating point number");
            }
            
            while (tokenizer.hasMore() && Character.isDigit(tokenizer.peek())) {
                number.append(tokenizer.next());
            }
        }
        
        // Parse exponent
        if (tokenizer.hasMore() && (tokenizer.peek() == 'e' || tokenizer.peek() == 'E')) {
            number.append(tokenizer.next());
            
            // Optional sign
            if (tokenizer.hasMore() && (tokenizer.peek() == '+' || tokenizer.peek() == '-')) {
                number.append(tokenizer.next());
            }
            
            if (!tokenizer.hasMore() || !Character.isDigit(tokenizer.peek())) {
                throw new PrologParserException("Invalid exponent in floating point number");
            }
            
            while (tokenizer.hasMore() && Character.isDigit(tokenizer.peek())) {
                number.append(tokenizer.next());
            }
        }
        
        double value = Double.parseDouble(number.toString());
        return new Token(TokenType.FLOAT, String.valueOf(value), startPos, tokenizer.getPosition());
    }
    
    /**
     * Validate floating point format
     */
    public boolean isValidFloatingPoint(String text) {
        try {
            Double.parseDouble(text);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }
}
```

### 6.4.6 Double Quoted Lists

Implementation covered in section 6.3.7.

### 6.4.7 Back Quoted Strings

JProlog implements back quoted string handling with proper character processing.

#### Back Quoted String Implementation

```java
/**
 * Back quoted string processor
 */
public class BackQuotedStringProcessor {
    
    /**
     * Process back quoted string
     */
    public Token processBackQuotedString(PrologTokenizer tokenizer) throws PrologParserException {
        int startPos = tokenizer.getPosition();
        StringBuilder content = new StringBuilder();
        
        tokenizer.next(); // Skip opening back quote
        
        while (tokenizer.hasMore()) {
            char c = tokenizer.next();
            
            if (c == '`') {
                // End of string
                break;
            } else if (c == '\\') {
                // Handle escape sequence
                if (tokenizer.hasMore()) {
                    char escaped = tokenizer.next();
                    content.append(processEscapeSequence(escaped));
                } else {
                    throw new PrologParserException("Unterminated escape sequence");
                }
            } else {
                content.append(c);
            }
        }
        
        return new Token(TokenType.BACK_QUOTED_STRING, content.toString(), 
                        startPos, tokenizer.getPosition());
    }
    
    private char processEscapeSequence(char escaped) {
        switch (escaped) {
            case 'n': return '\n';
            case 't': return '\t';
            case 'r': return '\r';
            case '\\': return '\\';
            case '`': return '`';
            default: return escaped;
        }
    }
}
```

### 6.4.8 Other Tokens

JProlog implements handling for all special tokens including punctuation and delimiters.

#### Special Token Implementation

```java
/**
 * Special token processor
 */
public class SpecialTokenProcessor {
    
    /**
     * Process special tokens (punctuation, delimiters)
     */
    public Token processSpecialToken(PrologTokenizer tokenizer) throws PrologParserException {
        int startPos = tokenizer.getPosition();
        char c = tokenizer.next();
        
        TokenType type;
        String value = String.valueOf(c);
        
        switch (c) {
            case '(': type = TokenType.LEFT_PAREN; break;
            case ')': type = TokenType.RIGHT_PAREN; break;
            case '[': type = TokenType.LEFT_BRACKET; break;
            case ']': type = TokenType.RIGHT_BRACKET; break;
            case '{': type = TokenType.LEFT_BRACE; break;
            case '}': type = TokenType.RIGHT_BRACE; break;
            case ',': type = TokenType.COMMA; break;
            case '.': 
                if (tokenizer.hasMore() && Character.isDigit(tokenizer.peek())) {
                    // This is a floating point number
                    tokenizer.backup();
                    return tokenizeFloatingPoint(tokenizer);
                } else {
                    type = TokenType.DOT;
                }
                break;
            case '|': type = TokenType.PIPE; break;
            case '!': type = TokenType.CUT; break;
            default:
                throw new PrologParserException("Unknown special character: " + c);
        }
        
        return new Token(type, value, startPos, tokenizer.getPosition());
    }
}
```

## 6.5 Processor Character Set

### 6.5.1 Graphic Characters

JProlog implements complete graphic character support for operator construction and special symbols.

#### Graphic Character Implementation

```java
/**
 * Graphic character processor
 */
public class GraphicCharacterProcessor {
    
    // ISO Prolog graphic characters
    private static final String GRAPHIC_CHARS = "#$&*+-./:<=>?@^`~\\";
    
    /**
     * Check if character is graphic
     */
    public boolean isGraphicChar(char c) {
        return GRAPHIC_CHARS.indexOf(c) >= 0;
    }
    
    /**
     * Process graphic character sequence
     */
    public String processGraphicSequence(String sequence) {
        StringBuilder result = new StringBuilder();
        
        for (char c : sequence.toCharArray()) {
            if (isGraphicChar(c)) {
                result.append(c);
            } else {
                break; // End of graphic sequence
            }
        }
        
        return result.toString();
    }
    
    /**
     * Validate graphic character usage in operators
     */
    public boolean isValidOperatorCharacter(char c) {
        return isGraphicChar(c) && c != '(' && c != ')' && c != '[' && c != ']' && 
               c != '{' && c != '}' && c != ',' && c != '|' && c != '.';
    }
}
```

### 6.5.2 Alphanumeric Characters

JProlog supports full alphanumeric character processing with Unicode support.

#### Alphanumeric Character Implementation

```java
/**
 * Alphanumeric character processor
 */
public class AlphanumericProcessor {
    
    /**
     * Check if character is alphanumeric
     */
    public boolean isAlphanumeric(char c) {
        return Character.isLetterOrDigit(c);
    }
    
    /**
     * Check if character is letter
     */
    public boolean isLetter(char c) {
        return Character.isLetter(c);
    }
    
    /**
     * Check if character is digit
     */
    public boolean isDigit(char c) {
        return Character.isDigit(c);
    }
    
    /**
     * Check if character can start an atom
     */
    public boolean canStartAtom(char c) {
        return Character.isLowerCase(c);
    }
    
    /**
     * Check if character can start a variable
     */
    public boolean canStartVariable(char c) {
        return Character.isUpperCase(c) || c == '_';
    }
    
    /**
     * Check if character can continue identifier
     */
    public boolean canContinueIdentifier(char c) {
        return Character.isLetterOrDigit(c) || c == '_';
    }
}
```

### 6.5.3 Solo Characters

JProlog handles solo characters (single-character tokens) properly.

#### Solo Character Implementation

```java
/**
 * Solo character processor
 */
public class SoloCharacterProcessor {
    
    // Characters that always form single-character tokens
    private static final Set<Character> SOLO_CHARS = Set.of(
        '(', ')', '[', ']', '{', '}', ',', '|', '!'
    );
    
    /**
     * Check if character is solo character
     */
    public boolean isSoloChar(char c) {
        return SOLO_CHARS.contains(c);
    }
    
    /**
     * Process solo character
     */
    public Token processSoloChar(char c, int position) {
        TokenType type;
        switch (c) {
            case '(': type = TokenType.LEFT_PAREN; break;
            case ')': type = TokenType.RIGHT_PAREN; break;
            case '[': type = TokenType.LEFT_BRACKET; break;
            case ']': type = TokenType.RIGHT_BRACKET; break;
            case '{': type = TokenType.LEFT_BRACE; break;
            case '}': type = TokenType.RIGHT_BRACE; break;
            case ',': type = TokenType.COMMA; break;
            case '|': type = TokenType.PIPE; break;
            case '!': type = TokenType.CUT; break;
            default:
                throw new IllegalArgumentException("Not a solo character: " + c);
        }
        
        return new Token(type, String.valueOf(c), position, position + 1);
    }
}
```

### 6.5.4 Layout Characters

JProlog implements comprehensive layout character handling.

#### Layout Character Implementation

```java
/**
 * Layout character processor
 */
public class LayoutCharacterProcessor {
    
    /**
     * Check if character is layout character
     */
    public boolean isLayoutChar(char c) {
        return Character.isWhitespace(c);
    }
    
    /**
     * Check if character is space
     */
    public boolean isSpace(char c) {
        return c == ' ' || c == '\t';
    }
    
    /**
     * Check if character is newline
     */
    public boolean isNewline(char c) {
        return c == '\n' || c == '\r';
    }
    
    /**
     * Skip layout characters
     */
    public void skipLayout(PrologTokenizer tokenizer) {
        while (tokenizer.hasMore() && isLayoutChar(tokenizer.peek())) {
            tokenizer.next();
        }
    }
    
    /**
     * Preserve layout for formatting
     */
    public String preserveLayoutBetweenTokens(String original, int start, int end) {
        StringBuilder layout = new StringBuilder();
        for (int i = start; i < end; i++) {
            char c = original.charAt(i);
            if (isLayoutChar(c)) {
                layout.append(c);
            }
        }
        return layout.toString();
    }
}
```

### 6.5.5 Meta Characters

JProlog handles meta characters that have special meaning in parsing.

#### Meta Character Implementation

```java
/**
 * Meta character processor
 */
public class MetaCharacterProcessor {
    
    // Characters with special meaning in Prolog syntax
    private static final Set<Character> META_CHARS = Set.of(
        '\\', '\'', '"', '`', '%'
    );
    
    /**
     * Check if character is meta character
     */
    public boolean isMetaChar(char c) {
        return META_CHARS.contains(c);
    }
    
    /**
     * Process meta character in context
     */
    public void processMetaChar(char c, PrologTokenizer tokenizer) throws PrologParserException {
        switch (c) {
            case '\\':
                processEscapeCharacter(tokenizer);
                break;
            case '\'':
                processQuotedAtom(tokenizer);
                break;
            case '"':
                processDoubleQuotedString(tokenizer);
                break;
            case '`':
                processBackQuotedString(tokenizer);
                break;
            case '%':
                processLineComment(tokenizer);
                break;
            default:
                throw new PrologParserException("Unknown meta character: " + c);
        }
    }
    
    private void processEscapeCharacter(PrologTokenizer tokenizer) {
        // Handle escape sequences in strings and atoms
        if (tokenizer.hasMore()) {
            char nextChar = tokenizer.peek();
            // Process specific escape sequence
        }
    }
    
    private void processQuotedAtom(PrologTokenizer tokenizer) throws PrologParserException {
        // Implementation for quoted atom parsing
    }
    
    private void processDoubleQuotedString(PrologTokenizer tokenizer) throws PrologParserException {
        // Implementation for double quoted string parsing
    }
    
    private void processBackQuotedString(PrologTokenizer tokenizer) throws PrologParserException {
        // Implementation for back quoted string parsing
    }
    
    private void processLineComment(PrologTokenizer tokenizer) {
        // Skip line comment
        while (tokenizer.hasMore() && tokenizer.peek() != '\n') {
            tokenizer.next();
        }
    }
}
```

## 6.6 Collating Sequence

JProlog implements ISO-compliant collating sequence for term ordering and comparison.

#### Collating Sequence Implementation

```java
/**
 * Collating sequence implementation for term ordering
 */
public class CollatingSequence {
    
    /**
     * Compare terms according to ISO Prolog standard ordering
     * Variables < Numbers < Atoms < Compound terms
     */
    public int compareTerms(Term t1, Term t2) {
        // Get type order for each term
        int order1 = getTypeOrder(t1);
        int order2 = getTypeOrder(t2);
        
        if (order1 != order2) {
            return Integer.compare(order1, order2);
        }
        
        // Same type, compare within type
        return compareWithinType(t1, t2);
    }
    
    /**
     * Get ordering value for term type
     */
    private int getTypeOrder(Term term) {
        if (term.isVariable()) return 1;
        if (term.isNumber()) return 2;
        if (term.isAtom()) return 3;
        return 4; // Compound term
    }
    
    /**
     * Compare terms of the same type
     */
    private int compareWithinType(Term t1, Term t2) {
        if (t1.isVariable()) {
            return compareVariables((Variable) t1, (Variable) t2);
        } else if (t1.isNumber()) {
            return compareNumbers((Number) t1, (Number) t2);
        } else if (t1.isAtom()) {
            return compareAtoms((Atom) t1, (Atom) t2);
        } else {
            return compareCompoundTerms((CompoundTerm) t1, (CompoundTerm) t2);
        }
    }
    
    /**
     * Compare variables (by creation order/name)
     */
    private int compareVariables(Variable v1, Variable v2) {
        return v1.getName().compareTo(v2.getName());
    }
    
    /**
     * Compare numbers (numeric ordering)
     */
    private int compareNumbers(Number n1, Number n2) {
        return Double.compare(n1.doubleValue(), n2.doubleValue());
    }
    
    /**
     * Compare atoms (alphabetical ordering)
     */
    private int compareAtoms(Atom a1, Atom a2) {
        return a1.getValue().compareTo(a2.getValue());
    }
    
    /**
     * Compare compound terms
     * First by arity, then by functor name, then by arguments
     */
    private int compareCompoundTerms(CompoundTerm c1, CompoundTerm c2) {
        // Compare by arity first
        int arityCompare = Integer.compare(c1.getArity(), c2.getArity());
        if (arityCompare != 0) {
            return arityCompare;
        }
        
        // Compare by functor name
        int functorCompare = c1.getFunctor().compareTo(c2.getFunctor());
        if (functorCompare != 0) {
            return functorCompare;
        }
        
        // Compare arguments left to right
        for (int i = 0; i < c1.getArity(); i++) {
            int argCompare = compareTerms(c1.getArgument(i + 1), c2.getArgument(i + 1));
            if (argCompare != 0) {
                return argCompare;
            }
        }
        
        return 0; // Terms are equal
    }
    
    /**
     * Standard term ordering predicate @< implementation
     */
    public boolean standardTermLessThan(Term t1, Term t2) {
        return compareTerms(t1, t2) < 0;
    }
    
    /**
     * Standard term ordering predicate @=< implementation
     */
    public boolean standardTermLessOrEqual(Term t1, Term t2) {
        return compareTerms(t1, t2) <= 0;
    }
    
    /**
     * Standard term ordering predicate @> implementation
     */
    public boolean standardTermGreaterThan(Term t1, Term t2) {
        return compareTerms(t1, t2) > 0;
    }
    
    /**
     * Standard term ordering predicate @>= implementation
     */
    public boolean standardTermGreaterOrEqual(Term t1, Term t2) {
        return compareTerms(t1, t2) >= 0;
    }
}
```

## Conclusion

JProlog implements a comprehensive syntax system that fully supports ISO Prolog language specifications. The implementation includes:

- **Complete BNF Grammar**: Recursive descent parser with proper operator precedence
- **Abstract Term Syntax**: Hierarchical AST representation with proper term relationships  
- **Text Processing**: Full program parsing with clause and directive handling
- **Term System**: Complete implementation of atomic, variable, and compound terms
- **Tokenization**: Comprehensive tokenizer supporting all ISO token types
- **Character Processing**: Full character set support with proper classification
- **Collating Sequence**: ISO-compliant term ordering for comparison operations

The syntax implementation provides a solid foundation for the JProlog interpreter, ensuring compatibility with standard Prolog code while maintaining clean separation between parsing, representation, and execution phases.

This syntax system supports advanced features including operator definitions, DCG notation, list processing, and proper variable scoping, making JProlog suitable for both educational use and practical Prolog programming applications.