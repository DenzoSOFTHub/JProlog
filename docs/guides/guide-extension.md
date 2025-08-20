# JProlog Extension Guide

## Table of Contents
1. [Introduction](#introduction)
2. [Extending with Prolog Predicates](#extending-with-prolog-predicates)
3. [Extending with Java Plugins](#extending-with-java-plugins)
4. [Adding Mathematical Functions](#adding-mathematical-functions)
5. [Creating Custom Operators](#creating-custom-operators)
6. [Advanced Extension Patterns](#advanced-extension-patterns)
7. [Best Practices](#best-practices)
8. [Complete Examples](#complete-examples)

## Introduction

JProlog can be extended in two primary ways without modifying the core library:

1. **Prolog-based Extensions**: Define new predicates using existing JProlog functionality
2. **Java-based Extensions**: Create Java classes that integrate with JProlog's architecture

This guide covers both approaches with practical examples and best practices.

## Extending with Prolog Predicates

### Basic Predicate Definition

Create `.pl` files containing predicate definitions using JProlog's existing built-ins:

**math_predicates.pl**
```prolog
% Mathematical predicates
factorial(0, 1).
factorial(N, F) :- 
    N > 0, 
    N1 is N - 1, 
    factorial(N1, F1), 
    F is N * F1.

% Power function using recursion
power(_, 0, 1).
power(Base, Exp, Result) :- 
    Exp > 0,
    Exp1 is Exp - 1,
    power(Base, Exp1, Result1),
    Result is Base * Result1.

% Greatest Common Divisor
gcd(A, 0, A) :- A > 0.
gcd(A, B, G) :- 
    B > 0, 
    R is A mod B, 
    gcd(B, R, G).

% Least Common Multiple
lcm(A, B, L) :- 
    gcd(A, B, G), 
    L is (A * B) / G.
```

**Usage:**
```prolog
?- :consult math_predicates.pl
?- factorial(5, F).
F = 120.0.

?- power(2, 10, P).
P = 1024.0.

?- gcd(48, 18, G).
G = 6.0.
```

### Advanced List Operations

**list_extensions.pl**
```prolog
% Advanced list predicates
sum_list([], 0).
sum_list([H|T], Sum) :- 
    sum_list(T, TSum), 
    Sum is H + TSum.

average_list(List, Avg) :- 
    sum_list(List, Sum), 
    length(List, Len), 
    Avg is Sum / Len.

% Find maximum in list
max_list([X], X).
max_list([H|T], Max) :- 
    max_list(T, TMax), 
    (H > TMax -> Max = H ; Max = TMax).

% Find minimum in list
min_list([X], X).
min_list([H|T], Min) :- 
    min_list(T, TMin), 
    (H < TMin -> Min = H ; Min = TMin).

% Remove duplicates
unique_list([], []).
unique_list([H|T], [H|Result]) :- 
    \+ member(H, T), 
    unique_list(T, Result).
unique_list([H|T], Result) :- 
    member(H, T), 
    unique_list(T, Result).

% Flatten nested lists
flatten_list([], []).
flatten_list([H|T], Flat) :- 
    is_list(H), 
    flatten_list(H, FlatH), 
    flatten_list(T, FlatT), 
    append(FlatH, FlatT, Flat).
flatten_list([H|T], [H|FlatT]) :- 
    \+ is_list(H), 
    flatten_list(T, FlatT).
```

### String and Atom Utilities

**string_utils.pl**
```prolog
% String utility predicates
string_empty('').

string_trim_spaces(Input, Output) :-
    atom_string(Input, InputStr),
    % Simple implementation - would need proper whitespace handling
    Output = InputStr.

% Check if atom/string contains substring
contains_substring(String, Substring) :-
    atom_string(String, Str),
    atom_string(Substring, Sub),
    sub_string(Str, _, _, _, Sub).

% Convert to uppercase (simplified)
to_uppercase(Input, Output) :-
    atom_string(Input, Str),
    % Would need actual case conversion logic
    Output = Str.

% Repeat string N times
repeat_string(_, 0, '').
repeat_string(Str, N, Result) :-
    N > 0,
    N1 is N - 1,
    repeat_string(Str, N1, Rest),
    string_concat(Str, Rest, Result).
```

### Validation Predicates

**validation.pl**
```prolog
% Data validation predicates
valid_range(X, Min, Max) :- 
    number(X), 
    X >= Min, 
    X =< Max.

valid_positive(X) :- 
    number(X), 
    X > 0.

valid_non_negative(X) :- 
    number(X), 
    X >= 0.

valid_email_format(Email) :-
    atom_string(Email, Str),
    string_length(Str, Len),
    Len > 5,
    contains_substring(Email, '@'),
    contains_substring(Email, '.').

valid_date(Year, Month, Day) :-
    valid_range(Year, 1900, 3000),
    valid_range(Month, 1, 12),
    valid_range(Day, 1, 31).

% Credit card number validation (Luhn algorithm simplified)
valid_credit_card_length(Number) :-
    atom_string(Number, Str),
    string_length(Str, Len),
    valid_range(Len, 13, 19).
```

## Extending with Java Plugins

### Creating Custom Built-in Predicates

Create Java classes that implement JProlog's built-in interface pattern:

**CustomMathPredicates.java**
```java
package extensions.math;

import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.*;
import it.denzosoft.jprolog.builtin.BuiltInWithContext;

import java.util.HashMap;
import java.util.Map;

public class CustomMathPredicates {
    
    // Fibonacci built-in predicate
    public static class FibonacciPredicate extends BuiltInWithContext {
        
        @Override
        public boolean solve(QuerySolver solver, Map<String, Term> bindings) {
            Term[] args = getArguments();
            if (args.length != 2) return false;
            
            try {
                // fibonacci(N, F) - calculate Fibonacci number
                Term nTerm = args[0];
                Term fTerm = args[1];
                
                if (nTerm instanceof Number) {
                    int n = ((Number) nTerm).getValue().intValue();
                    if (n < 0) return false;
                    
                    long fib = calculateFibonacci(n);
                    Term result = new Number((double) fib);
                    
                    Map<String, Term> newBindings = new HashMap<>(bindings);
                    return unify(fTerm, result, newBindings);
                }
                
                return false;
            } catch (Exception e) {
                return false;
            }
        }
        
        private long calculateFibonacci(int n) {
            if (n <= 1) return n;
            
            long a = 0, b = 1;
            for (int i = 2; i <= n; i++) {
                long temp = a + b;
                a = b;
                b = temp;
            }
            return b;
        }
    }
    
    // Prime checking predicate
    public static class IsPrimePredicate extends BuiltInWithContext {
        
        @Override
        public boolean solve(QuerySolver solver, Map<String, Term> bindings) {
            Term[] args = getArguments();
            if (args.length != 1) return false;
            
            try {
                Term nTerm = args[0];
                if (nTerm instanceof Number) {
                    int n = ((Number) nTerm).getValue().intValue();
                    return isPrime(n);
                }
                return false;
            } catch (Exception e) {
                return false;
            }
        }
        
        private boolean isPrime(int n) {
            if (n < 2) return false;
            if (n == 2) return true;
            if (n % 2 == 0) return false;
            
            for (int i = 3; i * i <= n; i += 2) {
                if (n % i == 0) return false;
            }
            return true;
        }
    }
}
```

### Extension Manager Class

**ExtensionManager.java**
```java
package extensions;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.BuiltInFactory;
import extensions.math.CustomMathPredicates;
import extensions.string.CustomStringPredicates;

public class ExtensionManager {
    
    public static void registerAllExtensions(Prolog prolog) {
        registerMathExtensions(prolog);
        registerStringExtensions(prolog);
        registerValidationExtensions(prolog);
        loadPrologExtensions(prolog);
    }
    
    private static void registerMathExtensions(Prolog prolog) {
        try {
            // Register custom built-in predicates
            BuiltInFactory.registerBuiltIn("fibonacci/2", 
                CustomMathPredicates.FibonacciPredicate.class);
            BuiltInFactory.registerBuiltIn("is_prime/1", 
                CustomMathPredicates.IsPrimePredicate.class);
            
            System.out.println("Math extensions registered: fibonacci/2, is_prime/1");
        } catch (Exception e) {
            System.err.println("Error registering math extensions: " + e.getMessage());
        }
    }
    
    private static void registerStringExtensions(Prolog prolog) {
        // Register string predicates using direct Prolog rules
        try {
            prolog.asserta("string_reverse(Str, Rev) :- " +
                          "string_chars(Str, Chars), " +
                          "reverse(Chars, RevChars), " +
                          "string_chars(Rev, RevChars).");
            
            prolog.asserta("string_palindrome(Str) :- " +
                          "string_reverse(Str, Rev), " +
                          "Str = Rev.");
            
            System.out.println("String extensions loaded: string_reverse/2, string_palindrome/1");
        } catch (Exception e) {
            System.err.println("Error loading string extensions: " + e.getMessage());
        }
    }
    
    private static void registerValidationExtensions(Prolog prolog) {
        try {
            // Load validation predicates
            prolog.asserta("valid_ip_octet(N) :- integer(N), N >= 0, N =< 255.");
            
            prolog.asserta("valid_port(P) :- integer(P), P > 0, P =< 65535.");
            
            prolog.asserta("valid_percentage(P) :- number(P), P >= 0, P =< 100.");
            
            System.out.println("Validation extensions loaded");
        } catch (Exception e) {
            System.err.println("Error loading validation extensions: " + e.getMessage());
        }
    }
    
    private static void loadPrologExtensions(Prolog prolog) {
        // Auto-load Prolog extension files
        String[] extensionFiles = {
            "extensions/math_predicates.pl",
            "extensions/list_extensions.pl",
            "extensions/string_utils.pl",
            "extensions/validation.pl"
        };
        
        for (String file : extensionFiles) {
            try {
                consultFile(prolog, file);
            } catch (Exception e) {
                System.err.println("Could not load extension file: " + file);
            }
        }
    }
    
    private static void consultFile(Prolog prolog, String filename) {
        try {
            java.nio.file.Path path = java.nio.file.Paths.get(filename);
            if (java.nio.file.Files.exists(path)) {
                String content = new String(java.nio.file.Files.readAllBytes(path));
                String[] lines = content.split("\\r?\\n");
                
                for (String line : lines) {
                    line = line.trim();
                    if (line.isEmpty() || line.startsWith("%") || !line.endsWith(".")) {
                        continue;
                    }
                    prolog.asserta(line);
                }
                System.out.println("Loaded extension file: " + filename);
            }
        } catch (Exception e) {
            System.err.println("Error loading " + filename + ": " + e.getMessage());
        }
    }
}
```

## Adding Mathematical Functions

### Custom Arithmetic Functions

**CustomArithmeticFunctions.java**
```java
package extensions.arithmetic;

import it.denzosoft.jprolog.builtin.arithmetic.ArithmeticOperation;

public class CustomArithmeticFunctions {
    
    // Factorial function
    public static class FactorialFunction implements ArithmeticOperation {
        @Override
        public Double evaluate(Double... args) {
            if (args.length != 1) return null;
            
            int n = args[0].intValue();
            if (n < 0) return null;
            if (n == 0) return 1.0;
            
            double result = 1.0;
            for (int i = 1; i <= n; i++) {
                result *= i;
            }
            return result;
        }
    }
    
    // Fibonacci function  
    public static class FibonacciFunction implements ArithmeticOperation {
        @Override
        public Double evaluate(Double... args) {
            if (args.length != 1) return null;
            
            int n = args[0].intValue();
            if (n < 0) return null;
            if (n <= 1) return (double) n;
            
            double a = 0, b = 1;
            for (int i = 2; i <= n; i++) {
                double temp = a + b;
                a = b;
                b = temp;
            }
            return b;
        }
    }
    
    // Combination function: C(n,r) = n! / (r! * (n-r)!)
    public static class CombinationFunction implements ArithmeticOperation {
        @Override
        public Double evaluate(Double... args) {
            if (args.length != 2) return null;
            
            int n = args[0].intValue();
            int r = args[1].intValue();
            
            if (n < 0 || r < 0 || r > n) return null;
            
            return factorial(n) / (factorial(r) * factorial(n - r));
        }
        
        private double factorial(int n) {
            if (n <= 1) return 1;
            double result = 1;
            for (int i = 2; i <= n; i++) {
                result *= i;
            }
            return result;
        }
    }
    
    // Logarithm base N
    public static class LogBaseFunction implements ArithmeticOperation {
        @Override
        public Double evaluate(Double... args) {
            if (args.length != 2) return null;
            
            double value = args[0];
            double base = args[1];
            
            if (value <= 0 || base <= 0 || base == 1) return null;
            
            return Math.log(value) / Math.log(base);
        }
    }
}
```

### Registering Arithmetic Functions

**ArithmeticExtensionManager.java**
```java
package extensions.arithmetic;

import it.denzosoft.jprolog.core.engine.ArithmeticEvaluator;
import extensions.arithmetic.CustomArithmeticFunctions;

public class ArithmeticExtensionManager {
    
    public static void registerCustomFunctions(ArithmeticEvaluator evaluator) {
        try {
            // Register custom arithmetic functions
            evaluator.registerFunction("fact", new CustomArithmeticFunctions.FactorialFunction());
            evaluator.registerFunction("fib", new CustomArithmeticFunctions.FibonacciFunction());
            evaluator.registerFunction("comb", new CustomArithmeticFunctions.CombinationFunction());
            evaluator.registerFunction("logbase", new CustomArithmeticFunctions.LogBaseFunction());
            
            System.out.println("Custom arithmetic functions registered:");
            System.out.println("  fact/1   - factorial");
            System.out.println("  fib/1    - fibonacci");
            System.out.println("  comb/2   - combination C(n,r)");
            System.out.println("  logbase/2 - logarithm base N");
            
        } catch (Exception e) {
            System.err.println("Error registering arithmetic functions: " + e.getMessage());
        }
    }
}
```

**Usage:**
```prolog
?- X is fact(5).
X = 120.0.

?- Y is fib(10).  
Y = 55.0.

?- Z is comb(10, 3).
Z = 120.0.

?- W is logbase(1000, 10).
W = 3.0.
```

## Creating Custom Operators

### Operator Definition in Prolog

**custom_operators.pl**
```prolog
% Define custom operators using existing functionality

% Power operator using ** 
% (Note: actual operator precedence would need Java implementation)
power_op(Base, Exp, Result) :- 
    Result is Base ^ Exp.

% Custom comparison operators
between_inclusive(X, Low, High) :- 
    X >= Low, 
    X =< High.

% List membership with custom syntax
in_list(Element, List) :- 
    member(Element, List).

% Set operations
set_union(Set1, Set2, Union) :- 
    append(Set1, Set2, Combined), 
    sort(Combined, Union).

set_intersection([], _, []).
set_intersection([H|T], Set2, [H|Result]) :- 
    member(H, Set2), 
    set_intersection(T, Set2, Result).
set_intersection([H|T], Set2, Result) :- 
    \+ member(H, Set2), 
    set_intersection(T, Set2, Result).

% Pattern matching predicates
matches_pattern(atom, Term) :- atom(Term).
matches_pattern(number, Term) :- number(Term).
matches_pattern(list, Term) :- is_list(Term).
matches_pattern(compound, Term) :- compound(Term).
```

### Advanced Operator Implementation

**CustomOperators.java**
```java
package extensions.operators;

import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.*;
import it.denzosoft.jprolog.builtin.BuiltInWithContext;

import java.util.HashMap;
import java.util.Map;

public class CustomOperators {
    
    // Implementation of custom 'between' operator: X between Low and High
    public static class BetweenOperator extends BuiltInWithContext {
        
        @Override
        public boolean solve(QuerySolver solver, Map<String, Term> bindings) {
            Term[] args = getArguments();
            if (args.length != 3) return false;
            
            try {
                Term xTerm = args[0];
                Term lowTerm = args[1]; 
                Term highTerm = args[2];
                
                if (xTerm instanceof Number && 
                    lowTerm instanceof Number && 
                    highTerm instanceof Number) {
                    
                    double x = ((Number) xTerm).getValue();
                    double low = ((Number) lowTerm).getValue();
                    double high = ((Number) highTerm).getValue();
                    
                    return x >= low && x <= high;
                }
                
                return false;
            } catch (Exception e) {
                return false;
            }
        }
    }
    
    // Custom 'matches' operator for pattern matching
    public static class MatchesOperator extends BuiltInWithContext {
        
        @Override
        public boolean solve(QuerySolver solver, Map<String, Term> bindings) {
            Term[] args = getArguments();
            if (args.length != 2) return false;
            
            try {
                Term termToCheck = args[0];
                Term pattern = args[1];
                
                if (pattern instanceof Atom) {
                    String patternName = ((Atom) pattern).getName();
                    
                    switch (patternName) {
                        case "atom":
                            return termToCheck instanceof Atom;
                        case "number":
                            return termToCheck instanceof Number;
                        case "integer":
                            return termToCheck instanceof Number && 
                                   ((Number) termToCheck).getValue() % 1 == 0;
                        case "float":
                            return termToCheck instanceof Number && 
                                   ((Number) termToCheck).getValue() % 1 != 0;
                        case "variable":
                            return termToCheck instanceof Variable;
                        case "compound":
                            return termToCheck instanceof CompoundTerm;
                        default:
                            return false;
                    }
                }
                
                return false;
            } catch (Exception e) {
                return false;
            }
        }
    }
}
```

## Advanced Extension Patterns

### Extension Configuration System

**ExtensionConfig.java**
```java
package extensions.config;

import java.util.*;
import java.io.*;
import java.nio.file.*;

public class ExtensionConfig {
    
    private Map<String, String> extensionPaths;
    private List<String> autoloadFiles;
    private Map<String, Integer> customOperators;
    
    public ExtensionConfig() {
        this.extensionPaths = new HashMap<>();
        this.autoloadFiles = new ArrayList<>();
        this.customOperators = new HashMap<>();
        loadDefaultConfig();
    }
    
    private void loadDefaultConfig() {
        // Default extension paths
        extensionPaths.put("math", "extensions/math/");
        extensionPaths.put("string", "extensions/string/");
        extensionPaths.put("validation", "extensions/validation/");
        extensionPaths.put("io", "extensions/io/");
        
        // Default autoload files
        autoloadFiles.add("extensions/math/arithmetic.pl");
        autoloadFiles.add("extensions/string/string_utils.pl");
        autoloadFiles.add("extensions/validation/validation.pl");
    }
    
    public void loadFromFile(String configPath) {
        try {
            if (Files.exists(Paths.get(configPath))) {
                // Load configuration from JSON or properties file
                // Implementation would parse the config file
                System.out.println("Loaded extension config from: " + configPath);
            }
        } catch (Exception e) {
            System.err.println("Error loading config: " + e.getMessage());
        }
    }
    
    public List<String> getAutoloadFiles() {
        return new ArrayList<>(autoloadFiles);
    }
    
    public String getExtensionPath(String category) {
        return extensionPaths.get(category);
    }
    
    public void addAutoloadFile(String path) {
        if (!autoloadFiles.contains(path)) {
            autoloadFiles.add(path);
        }
    }
}
```

### Extension Registry

**ExtensionRegistry.java**
```java
package extensions.registry;

import it.denzosoft.jprolog.core.engine.Prolog;
import extensions.config.ExtensionConfig;
import extensions.ExtensionManager;

import java.util.*;

public class ExtensionRegistry {
    
    private static final ExtensionRegistry instance = new ExtensionRegistry();
    private final Map<String, Class<?>> registeredPredicates;
    private final Map<String, Class<?>> registeredFunctions;
    private final ExtensionConfig config;
    
    private ExtensionRegistry() {
        this.registeredPredicates = new HashMap<>();
        this.registeredFunctions = new HashMap<>();
        this.config = new ExtensionConfig();
    }
    
    public static ExtensionRegistry getInstance() {
        return instance;
    }
    
    public void initializeExtensions(Prolog prolog) {
        System.out.println("Initializing JProlog extensions...");
        
        // Load configuration
        config.loadFromFile("jprolog-extensions.json");
        
        // Register all extensions
        ExtensionManager.registerAllExtensions(prolog);
        
        // Auto-load configured files
        for (String file : config.getAutoloadFiles()) {
            try {
                consultFile(prolog, file);
            } catch (Exception e) {
                System.err.println("Failed to autoload: " + file);
            }
        }
        
        System.out.println("Extension initialization complete.");
        printRegisteredExtensions();
    }
    
    private void consultFile(Prolog prolog, String filename) throws Exception {
        java.nio.file.Path path = java.nio.file.Paths.get(filename);
        if (java.nio.file.Files.exists(path)) {
            String content = new String(java.nio.file.Files.readAllBytes(path));
            String[] lines = content.split("\\r?\\n");
            
            int loaded = 0;
            for (String line : lines) {
                line = line.trim();
                if (!line.isEmpty() && !line.startsWith("%") && line.endsWith(".")) {
                    prolog.asserta(line);
                    loaded++;
                }
            }
            System.out.println("Loaded " + loaded + " clauses from " + filename);
        }
    }
    
    public void registerPredicate(String name, Class<?> implementation) {
        registeredPredicates.put(name, implementation);
    }
    
    public void registerFunction(String name, Class<?> implementation) {
        registeredFunctions.put(name, implementation);
    }
    
    private void printRegisteredExtensions() {
        System.out.println("\n=== Registered Extensions ===");
        
        if (!registeredPredicates.isEmpty()) {
            System.out.println("Predicates:");
            registeredPredicates.keySet().forEach(name -> 
                System.out.println("  " + name));
        }
        
        if (!registeredFunctions.isEmpty()) {
            System.out.println("Functions:");
            registeredFunctions.keySet().forEach(name -> 
                System.out.println("  " + name));
        }
        
        System.out.println("================================\n");
    }
}
```

## Best Practices

### 1. Naming Conventions

```prolog
% Use descriptive names with proper arity notation
% Good:
valid_email_address/1
calculate_compound_interest/4
find_shortest_path/3

% Avoid:
val/1
calc/4  
find/3
```

### 2. Error Handling

```java
// Always include proper error handling in Java extensions
public class SafePredicate extends BuiltInWithContext {
    
    @Override
    public boolean solve(QuerySolver solver, Map<String, Term> bindings) {
        try {
            Term[] args = getArguments();
            
            // Validate arguments
            if (args.length != expectedArity()) {
                return false;
            }
            
            // Type checking
            for (Term arg : args) {
                if (!isValidArgument(arg)) {
                    return false;
                }
            }
            
            // Main logic here
            return performOperation(args, bindings);
            
        } catch (Exception e) {
            // Log error if needed, but don't throw
            System.err.println("Error in predicate: " + e.getMessage());
            return false;
        }
    }
    
    private boolean isValidArgument(Term term) {
        // Implement argument validation
        return term != null;
    }
    
    private int expectedArity() {
        return 2; // Override in subclasses
    }
}
```

### 3. Performance Considerations

```prolog
% Use tail recursion for better performance
sum_list_optimized(List, Sum) :- 
    sum_list_acc(List, 0, Sum).

sum_list_acc([], Acc, Acc).
sum_list_acc([H|T], Acc, Sum) :- 
    NewAcc is Acc + H, 
    sum_list_acc(T, NewAcc, Sum).

% Avoid deep recursion for large datasets
% Use iterative approaches in Java extensions when possible
```

### 4. Documentation

```prolog
% Always document your predicates thoroughly
% 
% factorial(+N, -F) is det
%   Calculates factorial of N
%   @param N: non-negative integer
%   @param F: factorial result
%   @example factorial(5, F) -> F = 120
%
factorial(0, 1).
factorial(N, F) :- 
    N > 0, 
    N1 is N - 1, 
    factorial(N1, F1), 
    F is N * F1.
```

## Complete Examples

### Mathematics Extension Package

**File structure:**
```
extensions/
└── math/
    ├── arithmetic.pl
    ├── statistics.pl
    ├── geometry.pl
    └── MathPredicates.java
```

**arithmetic.pl**
```prolog
% Extended arithmetic predicates
factorial(0, 1).
factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1.

fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(N, F) :- N > 1, N1 is N - 1, N2 is N - 2, 
    fibonacci(N1, F1), fibonacci(N2, F2), F is F1 + F2.

gcd(A, 0, A) :- A > 0.
gcd(A, B, G) :- B > 0, R is A mod B, gcd(B, R, G).

lcm(A, B, L) :- gcd(A, B, G), L is (A * B) / G.

prime(2).
prime(N) :- N > 2, odd(N), not_divisible(N, 3).

odd(N) :- 1 is N mod 2.

not_divisible(N, D) :- D * D > N.
not_divisible(N, D) :- D * D =< N, R is N mod D, R > 0, D2 is D + 2, not_divisible(N, D2).
```

**statistics.pl**
```prolog
% Statistical functions
mean(List, Mean) :- sum_list(List, Sum), length(List, Len), Mean is Sum / Len.

median([X], X).
median(List, Median) :- 
    sort(List, Sorted), 
    length(Sorted, Len), 
    Mid is Len // 2, 
    (0 is Len mod 2 -> 
        nth0(Mid, Sorted, Median) 
    ; 
        Mid1 is Mid - 1, 
        nth0(Mid1, Sorted, A), 
        nth0(Mid, Sorted, B), 
        Median is (A + B) / 2
    ).

variance(List, Var) :- 
    mean(List, M), 
    maplist(square_diff(M), List, Diffs), 
    mean(Diffs, Var).

square_diff(Mean, X, Diff) :- Diff is (X - Mean) * (X - Mean).

standard_deviation(List, StdDev) :- 
    variance(List, Var), 
    StdDev is sqrt(Var).
```

### Complete Extension Loader

**ExtendedPrologCLI.java**
```java
package examples;

import it.denzosoft.jprolog.PrologCLI;
import it.denzosoft.jprolog.core.engine.Prolog;
import extensions.registry.ExtensionRegistry;

public class ExtendedPrologCLI extends PrologCLI {
    
    @Override
    public void start() {
        // Initialize extensions before starting CLI
        ExtensionRegistry.getInstance().initializeExtensions(prolog);
        
        // Call parent start method
        super.start();
    }
    
    @Override
    protected void loadExampleFacts() {
        super.loadExampleFacts();
        
        System.out.println("Extended JProlog CLI ready with custom extensions!");
        System.out.println("Try these extended predicates:");
        System.out.println("  factorial(10, F).");
        System.out.println("  fibonacci(15, F).");
        System.out.println("  prime(17).");
        System.out.println("  mean([1,2,3,4,5], M).");
        System.out.println("  standard_deviation([1,2,3,4,5], S).");
        System.out.println();
    }
    
    public static void main(String[] args) {
        new ExtendedPrologCLI().start();
    }
}
```

This comprehensive guide provides multiple approaches for extending JProlog without modifying the core library, from simple Prolog predicates to advanced Java integration patterns.

## Important Notes for v2.0.6

- **Updated Package Paths**: All core classes are now in `it.denzosoft.jprolog.core.*` packages
- **AbstractBuiltInWithContext**: Use `AbstractBuiltInWithContext` instead of `BuiltInWithContext` for custom predicates  
- **Internal Access**: Some classes like `BuiltInFactory` and `ArithmeticEvaluator` require internal access for extensions
- **Consult vs Asserta**: Use `prolog.consult()` instead of `prolog.asserta()` for complex rules to ensure proper parsing
- **Extension Registration**: For external extensions, focus on Prolog-based predicates rather than Java built-in registration

This guide is current as of JProlog v2.0.6. For the most up-to-date class paths and API, refer to the JavaDoc documentation or browse the source code in `src/main/java/it/denzosoft/jprolog/`.