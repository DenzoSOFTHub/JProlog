# Chapter 4: Working with Operators

## Overview

This chapter provides comprehensive documentation of JProlog's implementation of the Prolog operator system. Operators in Prolog allow for more natural mathematical and logical expressions by providing infix, prefix, and postfix notation alternatives to traditional functor notation. JProlog implements the ISO Prolog operator system with full support for operator precedence, associativity, and dynamic operator definition.

## 4.1 Precedence and Associativity

### 4.1.1 Operator Precedence System

JProlog implements a precedence-based parsing system where operators with lower precedence values bind more tightly than those with higher values. The precedence range is 1-1200, following ISO Prolog standards.

#### Core Precedence Implementation

```java
// OperatorTable.java - Standard operator precedences
private void initializeStandardOperators() {
    // Highest precedence (1200) - clause and goal structure
    defineOperator(1200, Operator.Type.XFX, ":-");      // Rule definition
    defineOperator(1200, Operator.Type.XFX, "-->");     // DCG rules
    defineOperator(1200, Operator.Type.FX, ":-");       // Directives
    defineOperator(1200, Operator.Type.FX, "?-");       // Queries
    
    // Control structures
    defineOperator(1100, Operator.Type.XFY, ";");       // Disjunction
    defineOperator(1050, Operator.Type.XFY, "->");      // If-then
    defineOperator(1000, Operator.Type.XFY, ",");       // Conjunction
    
    // Negation
    defineOperator(900, Operator.Type.FY, "\\+");       // Negation as failure
    
    // Comparison and unification
    defineOperator(700, Operator.Type.XFX, "=");        // Unification
    defineOperator(700, Operator.Type.XFX, "\\=");      // Not unifiable
    defineOperator(700, Operator.Type.XFX, "==");       // Term identity
    defineOperator(700, Operator.Type.XFX, "\\==");     // Term non-identity
    defineOperator(700, Operator.Type.XFX, "is");       // Arithmetic evaluation
    defineOperator(700, Operator.Type.XFX, "=:=");      // Arithmetic equality
    defineOperator(700, Operator.Type.XFX, "=\\=");     // Arithmetic inequality
    
    // Arithmetic operations
    defineOperator(500, Operator.Type.YFX, "+");        // Addition
    defineOperator(500, Operator.Type.YFX, "-");        // Subtraction
    defineOperator(400, Operator.Type.YFX, "*");        // Multiplication
    defineOperator(400, Operator.Type.YFX, "/");        // Division
    defineOperator(200, Operator.Type.XFX, "**");       // Exponentiation
}
```

#### Precedence-Driven Parsing

The parser uses precedence to resolve ambiguous expressions:

```prolog
% Example: Expression parsing with precedence
X is 2 + 3 * 4.
% Parsed as: X is 2 + (3 * 4)
% Because * has precedence 400 (higher binding) than + precedence 500

X is (2 + 3) * 4.
% Explicitly grouped to override precedence

% Complex precedence example:
Goal = (P ; Q), (R -> S ; T).
% Parsed as: Goal = ((P ; Q), ((R -> S) ; T))
% Precedences: , (1000), ; (1100), -> (1050)
```

#### Precedence Validation in Parsing

```java
// TermParser.java - Precedence-based expression parsing
private static final Map<String, Integer> OPERATOR_PRECEDENCE = new HashMap<>();

static {
    // Standard ISO Prolog precedences
    OPERATOR_PRECEDENCE.put("is", 700);
    OPERATOR_PRECEDENCE.put("=", 700);
    OPERATOR_PRECEDENCE.put("=:=", 700);
    OPERATOR_PRECEDENCE.put("+", 500);
    OPERATOR_PRECEDENCE.put("-", 500);
    OPERATOR_PRECEDENCE.put("*", 400);
    OPERATOR_PRECEDENCE.put("/", 400);
    OPERATOR_PRECEDENCE.put("**", 200);
    OPERATOR_PRECEDENCE.put("->", 1050);
    OPERATOR_PRECEDENCE.put(";", 1100);
    OPERATOR_PRECEDENCE.put(",", 1000);
}

public Term parseExpression(int minPrecedence) {
    Term left = parseAtom();
    
    while (!isAtEnd() && isOperator(current()) && 
           getPrecedence(current()) >= minPrecedence) {
        
        String operator = consume();
        int precedence = getPrecedence(operator);
        
        // Right-associative operators need special handling
        int nextMinPrec = isRightAssociative(operator) ? 
                          precedence : precedence + 1;
        
        Term right = parseExpression(nextMinPrec);
        left = new CompoundTerm(new Atom(operator), Arrays.asList(left, right));
    }
    
    return left;
}
```

### 4.1.2 Associativity Rules

JProlog supports three types of associativity following ISO Prolog standards:

#### Left Associativity (YFX, YF)

Left-associative operators group from left to right:

```prolog
% Example: Left-associative operators
A + B + C + D
% Parsed as: ((A + B) + C) + D
% Because + is defined as YFX (left-associative infix)

A - B - C
% Parsed as: (A - B) - C
% Maintains mathematical convention
```

#### Right Associativity (XFY, FY)

Right-associative operators group from right to left:

```prolog
% Example: Right-associative operators
P , Q , R
% Parsed as: P , (Q , R)  
% Because , is defined as XFY (right-associative)

\+ \+ P
% Parsed as: \+ (\+ P)
% Because \+ is FY (right-associative prefix)
```

#### Non-Associativity (XFX, FX, XF)

Non-associative operators cannot be chained without parentheses:

```prolog
% Example: Non-associative operators
X = Y = Z          % SYNTAX ERROR
X = (Y = Z)        % Legal - explicitly parenthesized
(X = Y) = Z        % Legal - explicitly parenthesized

% Because = is XFX (non-associative infix)
```

#### Implementation of Associativity

```java
// Operator.java - Associativity implementation
public enum Type {
    FX,   // Prefix, non-associative:     fx(arg)
    FY,   // Prefix, right-associative:   fy(fy(arg))
    XF,   // Postfix, non-associative:    (arg)xf  
    YF,   // Postfix, left-associative:   ((arg)yf)yf
    XFX,  // Infix, non-associative:      arg1 xfx arg2
    XFY,  // Infix, right-associative:    arg1 xfy (arg2 xfy arg3)
    YFX   // Infix, left-associative:     (arg1 yfx arg2) yfx arg3
}

public boolean isLeftAssociative() {
    return type == Type.YFX || type == Type.YF;
}

public boolean isRightAssociative() {
    return type == Type.XFY || type == Type.FY;
}

public boolean isNonAssociative() {
    return type == Type.XFX || type == Type.FX || type == Type.XF;
}

// Precedence calculation for argument positions
public int getLeftPrecedence() {
    switch (type) {
        case YFX:
        case YF:
            return precedence;        // Can accept same precedence
        case XFX:
        case XFY:
            return precedence - 1;    // Must have strictly higher precedence
        default:
            return -1;                // No left argument
    }
}

public int getRightPrecedence() {
    switch (type) {
        case XFY:
        case FY:
            return precedence;        // Can accept same precedence
        case XFX:
        case YFX:
        case FX:
            return precedence - 1;    // Must have strictly higher precedence
        default:
            return -1;                // No right argument
    }
}
```

### 4.1.3 Precedence Conflicts and Resolution

When multiple operators have the same precedence, associativity determines parsing:

```prolog
% Example: Same precedence, different associativity
% + and - both have precedence 500, both YFX (left-associative)
A + B - C + D
% Parsed as: ((A + B) - C) + D

% Mixed operators with same precedence
A + B * C
% Parsed as: A + (B * C)  
% Because * has precedence 400 (higher binding) than + precedence 500
```

#### Precedence Table Organization

```java
// OperatorTable.java - Organized precedence levels
public class OperatorTable {
    
    private void initializeStandardOperators() {
        // Level 1200: Clause structure
        defineOperator(1200, Operator.Type.XFX, ":-");
        defineOperator(1200, Operator.Type.XFX, "-->");
        
        // Level 1100: Disjunction
        defineOperator(1100, Operator.Type.XFY, ";");
        
        // Level 1050: If-then
        defineOperator(1050, Operator.Type.XFY, "->");
        
        // Level 1000: Conjunction  
        defineOperator(1000, Operator.Type.XFY, ",");
        
        // Level 900: Negation
        defineOperator(900, Operator.Type.FY, "\\+");
        
        // Level 700: Comparisons and unification
        defineOperator(700, Operator.Type.XFX, "=");
        defineOperator(700, Operator.Type.XFX, "is");
        defineOperator(700, Operator.Type.XFX, "=:=");
        
        // Level 500: Addition/subtraction
        defineOperator(500, Operator.Type.YFX, "+");
        defineOperator(500, Operator.Type.YFX, "-");
        
        // Level 400: Multiplication/division
        defineOperator(400, Operator.Type.YFX, "*");
        defineOperator(400, Operator.Type.YFX, "/");
        
        // Level 200: Power/exponentiation
        defineOperator(200, Operator.Type.XFX, "**");
        defineOperator(200, Operator.Type.XFY, "^");
    }
}
```

## 4.2 Declaring Operators with op/3

### 4.2.1 The op/3 Predicate

The `op/3` predicate allows dynamic operator definition and modification during program execution:

```prolog
% Syntax: op(+Precedence, +Type, +Name)
% Define a new operator or modify existing one

% Examples:
op(500, yfx, likes).           % Define infix operator 'likes'
op(300, fx, very).             % Define prefix operator 'very'
op(250, xf, indeed).           % Define postfix operator 'indeed'

% Usage after definition:
john likes mary.               % Equivalent to: likes(john, mary)
very important.                % Equivalent to: very(important)  
true indeed.                   % Equivalent to: indeed(true)
```

#### op/3 Implementation

```java
// Op.java - Implementation of op/3 predicate
public class Op extends AbstractBuiltInWithContext {
    
    private final OperatorTable operatorTable;
    
    @Override
    public boolean solve(QuerySolver solver, Map<String, Term> bindings) {
        Term[] args = getArguments();
        if (args.length != 3) {
            return false;
        }
        
        try {
            // Extract precedence
            if (!(args[0] instanceof it.denzosoft.jprolog.core.terms.Number)) {
                return false;
            }
            int precedence = ((it.denzosoft.jprolog.core.terms.Number) args[0])
                           .getValue().intValue();
            
            // Extract type
            if (!(args[1] instanceof Atom)) {
                return false;
            }
            String typeStr = ((Atom) args[1]).getName();
            Operator.Type type = Operator.parseType(typeStr);
            
            // Extract name(s)
            if (args[2] instanceof Atom) {
                String name = ((Atom) args[2]).getName();
                return defineOrRemoveOperator(precedence, type, name);
            } else if (args[2] instanceof CompoundTerm && 
                      ".".equals(((CompoundTerm) args[2]).getFunctor().getName())) {
                // List of operator names
                return defineOrRemoveOperatorList(precedence, type, args[2]);
            }
            
            return false;
            
        } catch (Exception e) {
            return false;
        }
    }
    
    private boolean defineOrRemoveOperator(int precedence, Operator.Type type, String name) {
        try {
            if (precedence == 0) {
                // Remove operator
                return operatorTable.removeOperator(0, type, name) || 
                       removeAllOperators(name, type);
            } else {
                // Define operator
                if (precedence < 1 || precedence > 1200) {
                    return false; // Invalid precedence range
                }
                
                // Check for conflicts with existing operators
                if (!isValidOperatorDefinition(precedence, type, name)) {
                    return false;
                }
                
                operatorTable.defineOperator(precedence, type, name);
                return true;
            }
        } catch (Exception e) {
            return false;
        }
    }
}
```

### 4.2.2 Operator Types and Syntax

JProlog supports all seven ISO Prolog operator types:

#### Prefix Operators (FX, FY)

```prolog
% FX - Non-associative prefix
op(900, fx, not).
not true.                      % Legal: not(true)
not not true.                  % ILLEGAL: Cannot chain FX operators

% FY - Right-associative prefix  
op(900, fy, really).
really very important.         % Legal: really(very(important))
really really important.       % Legal: really(really(important))
```

#### Postfix Operators (XF, YF)

```prolog
% XF - Non-associative postfix
op(100, xf, '!').              % Factorial operator
5 !.                           % Legal: !(5) 
5 ! !.                         % ILLEGAL: Cannot chain XF operators

% YF - Left-associative postfix
op(150, yf, percent).
tax 15 percent percent.        % Legal: percent(percent(tax(15)))
```

#### Infix Operators (XFX, XFY, YFX)

```prolog
% XFX - Non-associative infix
op(700, xfx, equals).
A equals B equals C.           % ILLEGAL: Cannot chain XFX operators
A equals (B equals C).         % Legal: explicitly parenthesized

% XFY - Right-associative infix
op(400, xfy, then).
A then B then C.               % Legal: A then (B then C)

% YFX - Left-associative infix
op(400, yfx, plus).
A plus B plus C.               % Legal: (A plus B) plus C
```

### 4.2.3 Operator Definition Validation

JProlog performs comprehensive validation when defining operators:

#### Precedence Range Validation

```java
// Op.java - Precedence validation
private boolean isValidOperatorDefinition(int precedence, Operator.Type type, String name) {
    // Check precedence limits (ISO standard)
    if (precedence < 1 || precedence > 1200) {
        return false;
    }
    
    // Check for reserved operators
    if (isReservedOperator(name)) {
        return false;
    }
    
    // Validate operator name format
    if (!isValidOperatorName(name)) {
        return false;
    }
    
    // Check for parsing conflicts
    return !hasConflictingDefinition(precedence, type, name);
}

private boolean isReservedOperator(String name) {
    // Some fundamental operators cannot be redefined
    switch (name) {
        case ",":  // Conjunction is fundamental to Prolog syntax
        case ";":  // Disjunction structure
        case "!":  // Cut cannot be redefined
            return true;
        default:
            return false;
    }
}
```

#### Name Validation

```java
// Op.java - Operator name validation
private boolean isValidOperatorName(String name) {
    if (name == null || name.isEmpty()) {
        return false;
    }
    
    // Operator names must be valid atoms
    // Cannot start with uppercase (would be variable)
    if (Character.isUpperCase(name.charAt(0)) || name.charAt(0) == '_') {
        return false;
    }
    
    // Additional ISO validation rules could be added here
    return true;
}
```

### 4.2.4 Multiple Operator Definitions

The same name can have multiple operator definitions with different types:

```prolog
% Multiple definitions for same name
op(500, yfx, +).               % Infix addition  
op(200, fy, +).                % Prefix plus (unary)

% Usage examples:
X is + 5.                      % Unary plus: +(5)
X is 3 + 5.                    % Binary plus: +(3,5)
X is + 3 + 5.                  % Mixed: +(+(3),5)
```

#### Implementation of Multiple Definitions

```java
// OperatorTable.java - Multiple operator support
public class OperatorTable {
    
    private final Map<String, Set<Operator>> operators;
    private final Map<String, Operator> prefixOperators;
    private final Map<String, Operator> postfixOperators;
    private final Map<String, Operator> infixOperators;
    
    public void defineOperator(int precedence, Operator.Type type, String name) {
        Operator operator = new Operator(precedence, type, name);
        
        // Add to main operators map (supports multiple definitions)
        operators.computeIfAbsent(name, k -> new HashSet<>()).add(operator);
        
        // Update specialized maps for parsing efficiency
        if (operator.isPrefix()) {
            prefixOperators.put(name, operator);
        }
        if (operator.isPostfix()) {
            postfixOperators.put(name, operator);
        }
        if (operator.isInfix()) {
            infixOperators.put(name, operator);
        }
    }
    
    public Set<Operator> getOperators(String name) {
        return new HashSet<>(operators.getOrDefault(name, Collections.emptySet()));
    }
}
```

### 4.2.5 Operator Removal

Operators can be removed by defining them with precedence 0:

```prolog
% Remove specific operator definition
op(0, yfx, +).                 % Remove infix + operator
op(0, fy, +).                  % Remove prefix + operator

% Remove all definitions of an operator
op(0, fx, myop).               % Remove all myop operators
```

#### Implementation of Operator Removal

```java
// Op.java - Operator removal
private boolean defineOrRemoveOperator(int precedence, Operator.Type type, String name) {
    if (precedence == 0) {
        // Remove operator with specified type, or all if type matches
        return operatorTable.removeOperator(0, type, name) || 
               removeAllOperators(name, type);
    } else {
        // Define new operator
        if (!isValidOperatorDefinition(precedence, type, name)) {
            return false;
        }
        
        operatorTable.defineOperator(precedence, type, name);
        return true;
    }
}

private boolean removeAllOperators(String name, Operator.Type type) {
    boolean removed = false;
    
    // Find and remove all operators with matching name and type
    for (Operator op : operatorTable.getOperators(name)) {
        if (op.getType() == type) {
            operatorTable.removeOperator(op.getPrecedence(), op.getType(), op.getName());
            removed = true;
        }
    }
    
    return removed;
}
```

## 4.3 Advanced Operator Features

### 4.3.1 Current Operator Introspection

JProlog provides built-in predicates to examine current operator definitions:

```prolog
% current_op(?Precedence, ?Type, ?Name)
% Find current operator definitions

?- current_op(P, T, +).        % Find all + operators
P = 500, T = yfx ;             % Infix addition
P = 200, T = fy.               % Prefix plus

?- current_op(700, T, N).      % Find all precedence 700 operators
T = xfx, N = '=' ;
T = xfx, N = 'is' ;
T = xfx, N = '=:=' ;
% ... etc
```

#### Implementation of current_op/3

```java
// CurrentOp.java - Operator introspection
public class CurrentOp implements BuiltIn {
    
    private final OperatorTable operatorTable;
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, 
                          List<Map<String, Term>> solutions) {
        
        if (query.getArguments().size() != 3) {
            return false;
        }
        
        Term precTerm = query.getArguments().get(0);
        Term typeTerm = query.getArguments().get(1);
        Term nameTerm = query.getArguments().get(2);
        
        // Get all current operators
        List<Operator> currentOps = operatorTable.getCurrentOperators();
        
        for (Operator op : currentOps) {
            Map<String, Term> newBindings = new HashMap<>(bindings);
            
            // Try to unify with precedence
            Term precedence = new it.denzosoft.jprolog.core.terms.Number(op.getPrecedence());
            if (!precTerm.unify(precedence, newBindings)) {
                continue;
            }
            
            // Try to unify with type
            Term type = new Atom(op.getType().name().toLowerCase());
            if (!typeTerm.unify(type, newBindings)) {
                continue;
            }
            
            // Try to unify with name
            Term name = new Atom(op.getName());
            if (!nameTerm.unify(name, newBindings)) {
                continue;
            }
            
            solutions.add(newBindings);
        }
        
        return !solutions.isEmpty();
    }
}
```

### 4.3.2 Operator-Based Expression Evaluation

Operators integrate seamlessly with JProlog's expression evaluation:

```prolog
% Custom operators for domain-specific languages
op(500, xfx, contains).
op(400, yfx, and).
op(300, fx, not).

% Natural language-like expressions
list([1,2,3]) contains 2.               % True
not list([1,2,3]) contains 4.           % True  
X contains 1 and X contains 2.          % X = list containing both 1 and 2
```

#### Integration with Arithmetic Evaluator

```java
// ArithmeticEvaluator.java - Operator integration
public class ArithmeticEvaluator {
    
    private final OperatorTable operatorTable;
    
    public Number evaluateExpression(Term expression, Map<String, Term> bindings) {
        if (expression instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) expression;
            String functor = compound.getFunctor().getName();
            
            // Check if functor is a defined arithmetic operator
            if (operatorTable.isOperator(functor)) {
                return evaluateOperatorExpression(compound, bindings);
            }
        }
        
        return evaluateStandardExpression(expression, bindings);
    }
    
    private Number evaluateOperatorExpression(CompoundTerm compound, 
                                             Map<String, Term> bindings) {
        String operator = compound.getFunctor().getName();
        List<Term> args = compound.getArguments();
        
        // Handle custom arithmetic operators
        switch (operator) {
            case "+":
                return evaluateAddition(args, bindings);
            case "*":
                return evaluateMultiplication(args, bindings);
            case "**":
                return evaluateExponentiation(args, bindings);
            default:
                throw new ArithmeticException("Unknown operator: " + operator);
        }
    }
}
```

### 4.3.3 Parsing with Custom Operators

The parser dynamically adapts to newly defined operators:

```prolog
% Define domain-specific operators
op(400, xfx, loves).
op(300, fx, definitely).
op(200, xf, '!!!').

% Write natural expressions
john loves mary.                        % loves(john, mary)
definitely john loves mary.             % definitely(loves(john, mary))
john loves mary !!!.                    % !!!(loves(john, mary))
```

#### Dynamic Parser Adaptation

```java
// TermParser.java - Dynamic operator parsing
public class TermParser {
    
    private final OperatorTable operatorTable;
    
    public Term parseExpression(int minPrecedence) {
        Term left = parsePrimary();
        
        while (!isAtEnd() && isCurrentAnOperator()) {
            String operatorName = getCurrentToken();
            Operator op = operatorTable.getInfixOperator(operatorName);
            
            if (op == null || op.getPrecedence() < minPrecedence) {
                break;
            }
            
            consume(); // consume operator
            
            int nextMinPrec = op.isRightAssociative() ? 
                             op.getPrecedence() : op.getPrecedence() + 1;
            
            Term right = parseExpression(nextMinPrec);
            left = new CompoundTerm(new Atom(operatorName), 
                                   Arrays.asList(left, right));
        }
        
        return left;
    }
    
    private boolean isCurrentAnOperator() {
        String token = getCurrentToken();
        return operatorTable.isOperator(token);
    }
}
```

## 4.4 Examples and Applications

### 4.4.1 Mathematical Domain-Specific Language

```prolog
% Define mathematical notation operators
op(400, xfx, times).
op(350, xfx, divided_by).
op(300, xfx, plus).
op(300, xfx, minus).

% Natural mathematical expressions
area_calculation(Length, Width, Area) :-
    Area is Length times Width.

tax_calculation(Gross, Rate, Net) :-
    Tax is Gross times Rate divided_by 100,
    Net is Gross minus Tax.

% Usage:
?- area_calculation(5, 3, A).          % A = 15
?- tax_calculation(1000, 20, N).       % N = 800
```

### 4.4.2 Logic Programming DSL

```prolog
% Define logical operators
op(300, xfx, implies).
op(250, xfx, iff).                     % If and only if
op(200, fx, necessarily).
op(150, fx, possibly).

% Modal logic expressions
logical_rule(P, Q) :-
    P implies Q.

modal_logic(Statement) :-
    necessarily Statement.

biconditional(P, Q) :-
    P iff Q.

% Usage:
?- logical_rule(socrates_is_man, socrates_is_mortal).
?- modal_logic(possibly raining).
```

### 4.4.3 Custom Comparison Operators

```prolog
% Define custom comparison operators
op(700, xfx, approximately).
op(700, xfx, much_greater_than).
op(700, xfx, close_to).

% Implementation of custom comparisons
X approximately Y :-
    Diff is abs(X - Y),
    Diff < 0.1.

X much_greater_than Y :-
    X > Y * 2.

X close_to Y :-
    X approximately Y.

% Usage in applications:
temperature_check(Actual, Expected) :-
    Actual close_to Expected.

performance_comparison(New, Old) :-
    New much_greater_than Old.
```

### 4.4.4 String and List Processing Operators

```prolog
% Define string/list operators
op(500, xfx, concat).
op(450, xfx, contains).
op(400, xfx, starts_with).
op(400, xfx, ends_with).

% String operations (assuming string predicates exist)
String1 concat String2 :-
    atom_concat(String1, String2, Result).

List contains Element :-
    member(Element, List).

String starts_with Prefix :-
    atom_codes(String, Codes),
    atom_codes(Prefix, PrefixCodes),
    append(PrefixCodes, _, Codes).

% Usage:
?- "hello" concat " world".            % String concatenation
?- [1,2,3] contains 2.                 % List membership
?- "filename.txt" ends_with ".txt".    % String suffix check
```

## 4.5 Performance Considerations

### 4.5.1 Operator Table Efficiency

JProlog optimizes operator lookup through specialized data structures:

```java
// OperatorTable.java - Performance optimizations
public class OperatorTable {
    
    // Separate maps for different operator types for O(1) lookup
    private final Map<String, Operator> prefixOperators;
    private final Map<String, Operator> postfixOperators;
    private final Map<String, Operator> infixOperators;
    
    // Concurrent data structures for thread safety
    private final Map<String, Set<Operator>> operators = new ConcurrentHashMap<>();
    
    public Operator getInfixOperator(String name) {
        // Direct O(1) lookup instead of searching through all operators
        return infixOperators.get(name);
    }
    
    public boolean isOperator(String name) {
        // Fast membership test
        return operators.containsKey(name);
    }
}
```

### 4.5.2 Parsing Performance

The parser caches operator information to avoid repeated lookups:

```java
// TermParser.java - Parsing optimizations
public class TermParser {
    
    // Cache operator precedences for frequently used operators
    private static final Map<String, Integer> PRECEDENCE_CACHE = new HashMap<>();
    
    private int getOperatorPrecedence(String operator) {
        return PRECEDENCE_CACHE.computeIfAbsent(operator, op -> {
            Operator opDef = operatorTable.getInfixOperator(op);
            return opDef != null ? opDef.getPrecedence() : Integer.MAX_VALUE;
        });
    }
}
```

## Conclusion

This chapter covered JProlog's comprehensive operator system:

1. **Precedence and Associativity**: Complete implementation of ISO Prolog precedence rules with proper associativity handling for all operator types
2. **Dynamic Operator Definition**: Full support for `op/3` predicate with validation, multiple definitions, and operator removal
3. **Operator Types**: Support for all seven ISO operator types (FX, FY, XF, YF, XFX, XFY, YFX) with correct parsing behavior
4. **Integration**: Seamless integration with arithmetic evaluation, parsing, and expression handling
5. **Performance**: Optimized data structures and caching for efficient operator processing

The operator system enables natural mathematical notation, domain-specific languages, and expressive programming constructs while maintaining full ISO Prolog compatibility.

---

*This documentation reflects JProlog's implementation of the ISO Prolog operator system with Java-specific optimizations and thread-safety considerations.*