# JProlog Java Integration Guide

## Table of Contents

1. [Overview](#overview)
2. [Setting Up the Prolog Engine](#setting-up-the-prolog-engine)
3. [Loading Prolog Programs](#loading-prolog-programs)
4. [Executing Queries](#executing-queries)
5. [Working with Results](#working-with-results)
6. [Creating Custom Predicates](#creating-custom-predicates)
7. [Implementing Custom Operators](#implementing-custom-operators)
8. [Adding Mathematical Functions](#adding-mathematical-functions)
9. [Advanced Integration Patterns](#advanced-integration-patterns)
10. [Error Handling](#error-handling)
11. [Performance Considerations](#performance-considerations)
12. [Complete Examples](#complete-examples)

---

## Overview

JProlog provides a comprehensive Java API for integrating Prolog reasoning capabilities into Java applications. This guide demonstrates how to embed the Prolog engine, load knowledge bases, execute queries, and extend the system with custom predicates and functions.

### Key Features

- **Embedded Engine**: Full Prolog interpreter as a Java library
- **Type-Safe API**: Strongly typed interfaces for terms and results
- **Extensibility**: Easy addition of custom predicates and operators
- **Backtracking Support**: Access to all solutions through Java iterators
- **Thread Safety**: Engine instances can be used safely in multi-threaded environments
- **✅ Exception Handling**: Full ISO Prolog exception system (catch/3, throw/1, halt/0-1)
- **✅ Meta-Predicates**: Higher-order programming with call/1-8, once/1, ignore/1, forall/2
- **✅ Dynamic Database**: Runtime modification with assert/retract family and abolish/1

### Maven Dependency

```xml
<dependency>
    <groupId>it.denzosoft</groupId>
    <artifactId>jprolog</artifactId>
    <version>1.0-SNAPSHOT</version>
</dependency>
```

---

## Setting Up the Prolog Engine

### Basic Engine Creation

```java
import it.denzosoft.jprolog.Prolog;
import it.denzosoft.jprolog.terms.Term;
import java.util.List;
import java.util.Map;

public class BasicPrologExample {
    public static void main(String[] args) {
        // Create a new Prolog engine instance
        Prolog prolog = new Prolog();
        
        // The engine is now ready to load programs and execute queries
        System.out.println("Prolog engine created successfully");
    }
}
```

### Engine with Custom Configuration

```java
public class ConfiguredPrologExample {
    private Prolog prolog;
    
    public ConfiguredPrologExample() {
        // Create engine with custom settings
        prolog = new Prolog();
        
        // Add custom built-ins during initialization
        registerCustomPredicates();
    }
    
    private void registerCustomPredicates() {
        // Custom predicates will be registered here
        // (see sections below for implementation details)
    }
}
```

---

## Loading Prolog Programs

### Loading from String

```java
public class ProgramLoadingExample {
    public static void main(String[] args) {
        Prolog prolog = new Prolog();
        
        // Load facts and rules as strings
        prolog.consult("parent(tom, bob).");
        prolog.consult("parent(tom, liz).");
        prolog.consult("parent(bob, ann).");
        prolog.consult("parent(bob, pat).");
        
        // Load rules
        prolog.consult("father(X, Y) :- parent(X, Y), male(X).");
        prolog.consult("mother(X, Y) :- parent(X, Y), female(X).");
        
        // Load gender facts
        prolog.consult("male(tom).");
        prolog.consult("male(bob).");
        prolog.consult("female(liz).");
        prolog.consult("female(ann).");
        prolog.consult("female(pat).");
        
        System.out.println("Program loaded successfully");
    }
}
```

### Loading from File

```java
import java.io.*;
import java.nio.file.*;

public class FileLoadingExample {
    public static void loadPrologFile(Prolog prolog, String filename) {
        try {
            // Read file content
            String content = Files.readString(Paths.get(filename));
            
            // Split into clauses and load each one
            String[] clauses = content.split("\\.");
            
            for (String clause : clauses) {
                clause = clause.trim();
                if (!clause.isEmpty()) {
                    prolog.consult(clause + ".");
                }
            }
            
            System.out.println("File " + filename + " loaded successfully");
            
        } catch (IOException e) {
            System.err.println("Error loading file: " + e.getMessage());
        }
    }
    
    public static void main(String[] args) {
        Prolog prolog = new Prolog();
        loadPrologFile(prolog, "family.pl");
    }
}
```

### Loading Multiple Programs

```java
public class MultiProgramExample {
    private Prolog prolog;
    
    public MultiProgramExample() {
        prolog = new Prolog();
        loadKnowledgeBases();
    }
    
    private void loadKnowledgeBases() {
        // Load different knowledge domains
        loadFamilyKnowledge();
        loadGeographyKnowledge();
        loadMathematicalRules();
    }
    
    private void loadFamilyKnowledge() {
        String[] familyFacts = {
            "parent(john, mary).",
            "parent(john, tom).",
            "parent(mary, ann).",
            "male(john).",
            "male(tom).",
            "female(mary).",
            "female(ann).",
            "father(X, Y) :- parent(X, Y), male(X).",
            "mother(X, Y) :- parent(X, Y), female(X)."
        };
        
        for (String fact : familyFacts) {
            prolog.consult(fact);
        }
    }
    
    private void loadGeographyKnowledge() {
        String[] geoFacts = {
            "capital(italy, rome).",
            "capital(france, paris).",
            "capital(germany, berlin).",
            "in_europe(italy).",
            "in_europe(france).",
            "in_europe(germany).",
            "european_capital(City) :- capital(Country, City), in_europe(Country)."
        };
        
        for (String fact : geoFacts) {
            prolog.consult(fact);
        }
    }
    
    private void loadMathematicalRules() {
        String[] mathRules = {
            "even(X) :- 0 =:= X mod 2.",
            "odd(X) :- 1 =:= X mod 2.",
            "positive(X) :- X > 0.",
            "negative(X) :- X < 0."
        };
        
        for (String rule : mathRules) {
            prolog.consult(rule);
        }
    }
}
```

---

## Executing Queries

### Simple Queries

```java
import it.denzosoft.jprolog.terms.Atom;
import it.denzosoft.jprolog.terms.Number;

public class BasicQueryExample {
    public static void main(String[] args) {
        Prolog prolog = new Prolog();
        
        // Load some facts
        prolog.consult("likes(mary, food).");
        prolog.consult("likes(mary, wine).");
        prolog.consult("likes(john, wine).");
        prolog.consult("likes(john, mary).");
        
        // Execute queries and check results
        List<Map<String, Term>> results;
        
        // Query: Does mary like food?
        results = prolog.solve("likes(mary, food).");
        if (!results.isEmpty()) {
            System.out.println("Mary likes food: true");
        }
        
        // Query: Does john like pizza?
        results = prolog.solve("likes(john, pizza).");
        if (results.isEmpty()) {
            System.out.println("John likes pizza: false");
        }
        
        // Query: What does mary like?
        results = prolog.solve("likes(mary, X).");
        System.out.println("Mary likes:");
        for (Map<String, Term> solution : results) {
            Term item = solution.get("X");
            System.out.println("  - " + item.toString());
        }
    }
}
```

### Queries with Variables

```java
public class VariableQueryExample {
    public static void main(String[] args) {
        Prolog prolog = new Prolog();
        
        // Load family relationships
        prolog.consult("parent(tom, bob).");
        prolog.consult("parent(tom, liz).");
        prolog.consult("parent(bob, ann).");
        prolog.consult("male(tom).");
        prolog.consult("male(bob).");
        prolog.consult("female(liz).");
        prolog.consult("female(ann).");
        prolog.consult("father(X, Y) :- parent(X, Y), male(X).");
        
        // Find all parent-child relationships
        List<Map<String, Term>> results = prolog.solve("parent(X, Y).");
        System.out.println("Parent-child relationships:");
        for (Map<String, Term> solution : results) {
            String parent = solution.get("X").toString();
            String child = solution.get("Y").toString();
            System.out.println("  " + parent + " -> " + child);
        }
        
        // Find all fathers
        results = prolog.solve("father(X, Y).");
        System.out.println("\nFather-child relationships:");
        for (Map<String, Term> solution : results) {
            String father = solution.get("X").toString();
            String child = solution.get("Y").toString();
            System.out.println("  " + father + " -> " + child);
        }
    }
}
```

### Complex Queries

```java
public class ComplexQueryExample {
    public static void main(String[] args) {
        Prolog prolog = new Prolog();
        
        // Load knowledge base
        loadEmployeeDatabase(prolog);
        
        // Complex query: Find all managers who earn more than 50000
        String query = "manager(X), salary(X, S), S > 50000.";
        List<Map<String, Term>> results = prolog.solve(query);
        
        System.out.println("High-earning managers:");
        for (Map<String, Term> solution : results) {
            String name = solution.get("X").toString();
            double salary = ((Number) solution.get("S")).getValue();
            System.out.println("  " + name + ": $" + salary);
        }
        
        // Query with conditional logic
        query = "(salary(X, S), S > 60000 -> status(X, senior) ; status(X, junior)).";
        results = prolog.solve(query);
        
        System.out.println("\nEmployee status:");
        for (Map<String, Term> solution : results) {
            String name = solution.get("X").toString();
            String status = solution.get("status").toString();
            System.out.println("  " + name + ": " + status);
        }
    }
    
    private static void loadEmployeeDatabase(Prolog prolog) {
        String[] facts = {
            "employee(john).",
            "employee(mary).",
            "employee(bob).",
            "employee(alice).",
            "manager(john).",
            "manager(mary).",
            "salary(john, 75000).",
            "salary(mary, 65000).",
            "salary(bob, 45000).",
            "salary(alice, 55000).",
            "department(john, engineering).",
            "department(mary, marketing).",
            "department(bob, engineering).",
            "department(alice, hr)."
        };
        
        for (String fact : facts) {
            prolog.consult(fact);
        }
    }
}
```

---

## Working with Results

### Processing Solution Sets

```java
import it.denzosoft.jprolog.terms.*;

public class ResultProcessingExample {
    public static void main(String[] args) {
        Prolog prolog = new Prolog();
        setupDatabase(prolog);
        
        // Query for all students and their grades
        List<Map<String, Term>> results = prolog.solve("student(Name, Grade).");
        
        // Process each solution
        for (Map<String, Term> solution : results) {
            processStudentRecord(solution);
        }
        
        // Aggregate results
        double averageGrade = calculateAverageGrade(results);
        System.out.println("Average grade: " + averageGrade);
    }
    
    private static void processStudentRecord(Map<String, Term> solution) {
        // Extract terms and convert to Java types
        String name = ((Atom) solution.get("Name")).getName();
        double grade = ((Number) solution.get("Grade")).getValue();
        
        // Determine grade category
        String category = categorizeGrade(grade);
        
        System.out.println(name + ": " + grade + " (" + category + ")");
    }
    
    private static String categorizeGrade(double grade) {
        if (grade >= 90) return "Excellent";
        if (grade >= 80) return "Good";
        if (grade >= 70) return "Satisfactory";
        return "Needs Improvement";
    }
    
    private static double calculateAverageGrade(List<Map<String, Term>> results) {
        double sum = 0;
        int count = 0;
        
        for (Map<String, Term> solution : results) {
            double grade = ((Number) solution.get("Grade")).getValue();
            sum += grade;
            count++;
        }
        
        return count > 0 ? sum / count : 0;
    }
    
    private static void setupDatabase(Prolog prolog) {
        String[] studentData = {
            "student(alice, 95).",
            "student(bob, 87).",
            "student(charlie, 72).",
            "student(diana, 91).",
            "student(eve, 68)."
        };
        
        for (String data : studentData) {
            prolog.consult(data);
        }
    }
}
```

### Handling Different Term Types

```java
public class TermTypeExample {
    public static void main(String[] args) {
        Prolog prolog = new Prolog();
        setupComplexDatabase(prolog);
        
        List<Map<String, Term>> results = prolog.solve("person(Name, Age, Address).");
        
        for (Map<String, Term> solution : results) {
            processPerson(solution);
        }
    }
    
    private static void processPerson(Map<String, Term> solution) {
        // Handle atom
        Term nameTerm = solution.get("Name");
        String name = "";
        if (nameTerm instanceof Atom) {
            name = ((Atom) nameTerm).getName();
        }
        
        // Handle number
        Term ageTerm = solution.get("Age");
        int age = 0;
        if (ageTerm instanceof Number) {
            age = (int) ((Number) ageTerm).getValue();
        }
        
        // Handle compound term
        Term addressTerm = solution.get("Address");
        String addressString = "";
        if (addressTerm instanceof CompoundTerm) {
            addressString = formatAddress((CompoundTerm) addressTerm);
        }
        
        System.out.println(name + " (age " + age + "): " + addressString);
    }
    
    private static String formatAddress(CompoundTerm address) {
        if (address.getFunctor().getName().equals("address") && 
            address.getArguments().size() == 3) {
            
            String street = ((Atom) address.getArguments().get(0)).getName();
            String city = ((Atom) address.getArguments().get(1)).getName();
            String country = ((Atom) address.getArguments().get(2)).getName();
            
            return street + ", " + city + ", " + country;
        }
        return address.toString();
    }
    
    private static void setupComplexDatabase(Prolog prolog) {
        String[] facts = {
            "person(john, 30, address('123 Main St', london, uk)).",
            "person(mary, 25, address('456 Oak Ave', paris, france)).",
            "person(bob, 35, address('789 Pine Rd', berlin, germany))."
        };
        
        for (String fact : facts) {
            prolog.consult(fact);
        }
    }
}
```

---

## Creating Custom Predicates

### Simple Custom Predicate

```java
import it.denzosoft.jprolog.BuiltIn;
import it.denzosoft.jprolog.BuiltInFactory;
import it.denzosoft.jprolog.terms.Term;
import java.util.List;
import java.util.Map;

// Custom predicate to check if a number is prime
public class IsPrimePredicate implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, 
                          List<Map<String, Term>> solutions) {
        
        // Get the argument from the query
        if (query.getArguments().size() != 1) {
            return false; // Wrong arity
        }
        
        Term arg = query.getArguments().get(0);
        
        // Resolve the argument with current bindings
        Term resolved = arg.resolveBindings(bindings);
        
        // Check if it's a number
        if (!(resolved instanceof Number)) {
            return false;
        }
        
        int number = (int) ((Number) resolved).getValue();
        
        // Check if prime
        if (isPrime(number)) {
            solutions.add(new HashMap<>(bindings));
            return true;
        }
        
        return false;
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

// Usage example
public class CustomPredicateExample {
    public static void main(String[] args) {
        Prolog prolog = new Prolog();
        
        // Register the custom predicate
        registerCustomPredicate(prolog);
        
        // Test the predicate
        List<Map<String, Term>> results;
        
        results = prolog.solve("is_prime(7).");
        System.out.println("7 is prime: " + !results.isEmpty());
        
        results = prolog.solve("is_prime(8).");
        System.out.println("8 is prime: " + !results.isEmpty());
        
        // Use in rules
        prolog.consult("small_prime(X) :- X < 10, is_prime(X).");
        results = prolog.solve("small_prime(X).");
        
        System.out.println("Small primes:");
        for (Map<String, Term> solution : results) {
            System.out.println("  " + solution.get("X"));
        }
    }
    
    private static void registerCustomPredicate(Prolog prolog) {
        // Note: This is a simplified example. In practice, you would need to
        // modify the BuiltInFactory to register the predicate properly.
        // See the actual implementation for the correct registration method.
    }
}
```

### Context-Dependent Custom Predicate

```java
import it.denzosoft.jprolog.BuiltInWithContext;
import it.denzosoft.jprolog.QuerySolver;
import it.denzosoft.jprolog.CutStatus;

// Custom predicate that can access the query solver
public class ForAllPredicate implements BuiltInWithContext {
    
    private QuerySolver querySolver;
    
    public ForAllPredicate(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, 
                                    Map<String, Term> bindings, 
                                    List<Map<String, Term>> solutions) {
        
        // forall(Condition, Action) - succeeds if Action succeeds for all Condition solutions
        if (query.getArguments().size() != 2) {
            return false;
        }
        
        Term condition = query.getArguments().get(0);
        Term action = query.getArguments().get(1);
        
        // Find all solutions to the condition
        List<Map<String, Term>> conditionSolutions = new ArrayList<>();
        boolean conditionHasSolutions = solver.solve(condition, new HashMap<>(bindings), 
                                                   conditionSolutions, CutStatus.notOccurred());
        
        if (!conditionHasSolutions || conditionSolutions.isEmpty()) {
            // If condition has no solutions, forall succeeds trivially
            solutions.add(new HashMap<>(bindings));
            return true;
        }
        
        // Check that action succeeds for each condition solution
        for (Map<String, Term> conditionBinding : conditionSolutions) {
            List<Map<String, Term>> actionSolutions = new ArrayList<>();
            boolean actionSucceeds = solver.solve(action, new HashMap<>(conditionBinding), 
                                                actionSolutions, CutStatus.notOccurred());
            
            if (!actionSucceeds || actionSolutions.isEmpty()) {
                return false; // Action failed for this condition
            }
        }
        
        // All actions succeeded
        solutions.add(new HashMap<>(bindings));
        return true;
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, 
                          List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("forall/2 requires context");
    }
}
```

### Database Access Predicate

```java
import java.sql.*;

// Custom predicate to query external database
public class DatabaseQueryPredicate implements BuiltIn {
    
    private Connection dbConnection;
    
    public DatabaseQueryPredicate(Connection connection) {
        this.dbConnection = connection;
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, 
                          List<Map<String, Term>> solutions) {
        
        // db_query(Table, Column, Value)
        if (query.getArguments().size() != 3) {
            return false;
        }
        
        Term tableTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term columnTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term valueTerm = query.getArguments().get(2);
        
        if (!(tableTerm instanceof Atom) || !(columnTerm instanceof Atom)) {
            return false;
        }
        
        String table = ((Atom) tableTerm).getName();
        String column = ((Atom) columnTerm).getName();
        
        try {
            String sql = "SELECT * FROM " + table + " WHERE " + column + " = ?";
            PreparedStatement stmt = dbConnection.prepareStatement(sql);
            
            // Handle different value types
            Term resolvedValue = valueTerm.resolveBindings(bindings);
            if (resolvedValue instanceof Atom) {
                stmt.setString(1, ((Atom) resolvedValue).getName());
            } else if (resolvedValue instanceof Number) {
                stmt.setDouble(1, ((Number) resolvedValue).getValue());
            } else if (resolvedValue instanceof Variable) {
                // Variable case - return all rows
                sql = "SELECT * FROM " + table;
                stmt = dbConnection.prepareStatement(sql);
            } else {
                return false;
            }
            
            ResultSet rs = stmt.executeQuery();
            boolean found = false;
            
            while (rs.next()) {
                Map<String, Term> solution = new HashMap<>(bindings);
                
                // If value was a variable, bind it
                if (valueTerm instanceof Variable) {
                    String varName = ((Variable) valueTerm).getName();
                    Object dbValue = rs.getObject(column);
                    Term termValue = convertToTerm(dbValue);
                    solution.put(varName, termValue);
                }
                
                solutions.add(solution);
                found = true;
            }
            
            return found;
            
        } catch (SQLException e) {
            System.err.println("Database error: " + e.getMessage());
            return false;
        }
    }
    
    private Term convertToTerm(Object value) {
        if (value instanceof String) {
            return new Atom((String) value);
        } else if (value instanceof Number) {
            return new Number(((java.lang.Number) value).doubleValue());
        }
        return new Atom(value.toString());
    }
}
```

---

## Implementing Custom Operators

### Arithmetic Operator

```java
import it.denzosoft.jprolog.builtin.arithmetic.ArithmeticOperation;

// Custom operator for power function
public class PowerOperation implements ArithmeticOperation {
    
    @Override
    public double evaluate(double left, double right) {
        return Math.pow(left, right);
    }
    
    @Override
    public String getOperatorSymbol() {
        return "**";
    }
    
    @Override
    public int getPrecedence() {
        return 200; // Higher precedence than +, -
    }
}

// Registration and usage
public class CustomOperatorExample {
    public static void main(String[] args) {
        Prolog prolog = new Prolog();
        
        // Register the power operator
        registerPowerOperator();
        
        // Test arithmetic with power
        List<Map<String, Term>> results = prolog.solve("X is 2 ** 3.");
        if (!results.isEmpty()) {
            double result = ((Number) results.get(0).get("X")).getValue();
            System.out.println("2 ** 3 = " + result); // Should print 8.0
        }
        
        // Use in more complex expressions
        results = prolog.solve("Y is 3 + 2 ** 4 - 1.");
        if (!results.isEmpty()) {
            double result = ((Number) results.get(0).get("Y")).getValue();
            System.out.println("3 + 2 ** 4 - 1 = " + result); // Should print 18.0
        }
    }
    
    private static void registerPowerOperator() {
        // Register with the arithmetic evaluator
        // Note: In practice, this would require modifying StandardArithmeticOperations
        // or providing a registration mechanism in ArithmeticEvaluator
    }
}
```

### Logical Operator

```java
// Custom logical operator for exclusive OR (XOR)
public class XorOperator implements BuiltInWithContext {
    
    private QuerySolver querySolver;
    
    public XorOperator(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, 
                                    Map<String, Term> bindings, 
                                    List<Map<String, Term>> solutions) {
        
        // xor(Goal1, Goal2) - succeeds if exactly one goal succeeds
        if (query.getArguments().size() != 2) {
            return false;
        }
        
        Term goal1 = query.getArguments().get(0);
        Term goal2 = query.getArguments().get(1);
        
        // Test both goals
        List<Map<String, Term>> solutions1 = new ArrayList<>();
        List<Map<String, Term>> solutions2 = new ArrayList<>();
        
        boolean success1 = solver.solve(goal1, new HashMap<>(bindings), 
                                      solutions1, CutStatus.notOccurred());
        boolean success2 = solver.solve(goal2, new HashMap<>(bindings), 
                                      solutions2, CutStatus.notOccurred());
        
        // XOR logic: exactly one should succeed
        if (success1 && !success2) {
            solutions.addAll(solutions1);
            return true;
        } else if (!success1 && success2) {
            solutions.addAll(solutions2);
            return true;
        }
        
        return false; // Both succeeded or both failed
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, 
                          List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("xor/2 requires context");
    }
}
```

---

## Adding Mathematical Functions

### Custom Math Function

```java
// Custom math function for factorial
public class FactorialFunction implements ArithmeticOperation {
    
    @Override
    public double evaluate(double value) {
        int n = (int) value;
        if (n < 0) {
            throw new ArithmeticException("Factorial undefined for negative numbers");
        }
        
        double result = 1;
        for (int i = 2; i <= n; i++) {
            result *= i;
        }
        return result;
    }
    
    @Override
    public String getFunctionName() {
        return "factorial";
    }
    
    // Note: This is a unary function, so we override the single-argument evaluate
    public double evaluate(double left, double right) {
        throw new UnsupportedOperationException("Factorial is a unary function");
    }
    
    @Override
    public String getOperatorSymbol() {
        return "!"; // Postfix notation
    }
    
    @Override
    public int getPrecedence() {
        return 100; // Very high precedence
    }
}

// Statistical functions
public class StatisticalFunctions {
    
    // Mean function for lists
    public static class MeanFunction implements BuiltIn {
        
        @Override
        public boolean execute(Term query, Map<String, Term> bindings, 
                              List<Map<String, Term>> solutions) {
            
            // mean(List, Result)
            if (query.getArguments().size() != 2) {
                return false;
            }
            
            Term listTerm = query.getArguments().get(0).resolveBindings(bindings);
            Term resultTerm = query.getArguments().get(1);
            
            // Extract numbers from list
            List<Double> numbers = extractNumbers(listTerm);
            if (numbers.isEmpty()) {
                return false;
            }
            
            // Calculate mean
            double sum = numbers.stream().mapToDouble(Double::doubleValue).sum();
            double mean = sum / numbers.size();
            
            // Unify with result
            Term meanTerm = new Number(mean);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            
            if (resultTerm.unify(meanTerm, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            
            return false;
        }
        
        private List<Double> extractNumbers(Term listTerm) {
            List<Double> numbers = new ArrayList<>();
            
            // Handle Prolog list structure [H|T]
            while (listTerm instanceof CompoundTerm) {
                CompoundTerm compound = (CompoundTerm) listTerm;
                
                if (!compound.getFunctor().getName().equals(".") || 
                    compound.getArguments().size() != 2) {
                    break;
                }
                
                Term head = compound.getArguments().get(0);
                if (head instanceof Number) {
                    numbers.add(((Number) head).getValue());
                }
                
                listTerm = compound.getArguments().get(1);
            }
            
            return numbers;
        }
    }
    
    // Standard deviation function
    public static class StdDevFunction implements BuiltIn {
        
        @Override
        public boolean execute(Term query, Map<String, Term> bindings, 
                              List<Map<String, Term>> solutions) {
            
            // stddev(List, Result)
            if (query.getArguments().size() != 2) {
                return false;
            }
            
            Term listTerm = query.getArguments().get(0).resolveBindings(bindings);
            Term resultTerm = query.getArguments().get(1);
            
            List<Double> numbers = extractNumbers(listTerm);
            if (numbers.size() < 2) {
                return false; // Need at least 2 values for std dev
            }
            
            // Calculate standard deviation
            double mean = numbers.stream().mapToDouble(Double::doubleValue).average().orElse(0.0);
            double variance = numbers.stream()
                .mapToDouble(x -> Math.pow(x - mean, 2))
                .average()
                .orElse(0.0);
            double stddev = Math.sqrt(variance);
            
            // Unify with result
            Term stddevTerm = new Number(stddev);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            
            if (resultTerm.unify(stddevTerm, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            
            return false;
        }
        
        private List<Double> extractNumbers(Term listTerm) {
            // Same implementation as in MeanFunction
            List<Double> numbers = new ArrayList<>();
            
            while (listTerm instanceof CompoundTerm) {
                CompoundTerm compound = (CompoundTerm) listTerm;
                
                if (!compound.getFunctor().getName().equals(".") || 
                    compound.getArguments().size() != 2) {
                    break;
                }
                
                Term head = compound.getArguments().get(0);
                if (head instanceof Number) {
                    numbers.add(((Number) head).getValue());
                }
                
                listTerm = compound.getArguments().get(1);
            }
            
            return numbers;
        }
    }
}
```

---

## ISO Prolog Features

### Exception Handling

JProlog now supports the full ISO Prolog exception handling mechanism:

```java
import it.denzosoft.jprolog.PrologException;

public class ExceptionHandlingExample {
    public static void main(String[] args) {
        Prolog prolog = new Prolog();
        
        // Define a rule that can throw exceptions
        prolog.consult(
            "safe_divide(X, Y, Result) :- " +
            "  (Y =:= 0 -> throw(division_by_zero) ; Result is X / Y)."
        );
        
        try {
            // This will succeed
            List<Map<String, Term>> solutions = prolog.solve("safe_divide(10, 2, R)");
            System.out.println("Result: " + solutions.get(0).get("R"));
            
            // This will throw an exception
            prolog.solve("safe_divide(10, 0, R)");
        } catch (PrologException e) {
            System.out.println("Caught Prolog exception: " + e.getMessage());
        }
        
        // Using catch/3 from Prolog
        prolog.consult(
            "handle_division(X, Y, Result) :- " +
            "  catch(safe_divide(X, Y, Result), Error, (Result = error(Error)))."
        );
        
        List<Map<String, Term>> solutions = prolog.solve("handle_division(10, 0, R)");
        System.out.println("Handled: " + solutions.get(0).get("R"));
    }
}
```

### Meta-Predicates

JProlog supports higher-order programming with meta-predicates:

```java
public class MetaPredicatesExample {
    public static void main(String[] args) {
        Prolog prolog = new Prolog();
        
        // Define some predicates
        prolog.consult(
            "process(X) :- write('Processing: '), write(X), nl."
        );
        
        // Using call/1 to invoke goals dynamically
        List<Map<String, Term>> solutions = prolog.solve("call(process(hello))");
        
        // Using once/1 for deterministic execution
        prolog.consult("choice(a). choice(b). choice(c).");
        solutions = prolog.solve("once(choice(X))");
        System.out.println("Once result: " + solutions.get(0).get("X"));
        
        // Using forall/2 for universal quantification
        prolog.consult("number(1). number(2). number(3).");
        solutions = prolog.solve("forall(number(X), (X > 0))");
        System.out.println("All positive: " + !solutions.isEmpty());
        
        // Using ignore/1 to handle potential failures
        solutions = prolog.solve("ignore(fail)");
        System.out.println("Ignore always succeeds: " + !solutions.isEmpty());
    }
}
```

### Dynamic Database Operations

Runtime modification of the knowledge base:

```java
public class DynamicDatabaseExample {
    public static void main(String[] args) {
        Prolog prolog = new Prolog();
        
        // Initial facts
        prolog.consult("initial_fact(a). initial_fact(b).");
        
        // Add facts dynamically
        prolog.solve("assertz(dynamic_fact(1))");
        prolog.solve("asserta(dynamic_fact(0))");  // Added at beginning
        prolog.solve("assertz(dynamic_fact(2))");
        
        // Query dynamic facts
        List<Map<String, Term>> solutions = prolog.solve("dynamic_fact(X)");
        System.out.println("Dynamic facts found: " + solutions.size());
        
        // Remove specific fact
        prolog.solve("retract(dynamic_fact(1))");
        
        // Remove all matching facts
        prolog.solve("retractall(dynamic_fact(_))");
        
        // Check what's left
        solutions = prolog.solve("dynamic_fact(X)");
        System.out.println("After retractall: " + solutions.size());
        
        // Add and then abolish entire predicate
        prolog.solve("assertz(temp_pred(x))");
        prolog.solve("assertz(temp_pred(y))");
        prolog.solve("abolish(temp_pred/1)");
        
        solutions = prolog.solve("temp_pred(X)");
        System.out.println("After abolish: " + solutions.size());
    }
}
```

### Working with Exceptions in Java

```java
public class ExceptionIntegrationExample {
    public static void main(String[] args) {
        Prolog prolog = new Prolog();
        
        // Custom predicate that throws exceptions
        BuiltIn riskyPredicate = new BuiltIn() {
            @Override
            public boolean execute(Term query, Map<String, Term> bindings, 
                                 List<Map<String, Term>> solutions) {
                // Simulate a risky operation
                if (Math.random() > 0.5) {
                    throw new PrologException(new Atom("random_failure"));
                }
                solutions.add(bindings);
                return true;
            }
        };
        
        prolog.registerBuiltInPredicate("risky_operation", riskyPredicate);
        
        // Use in Prolog with exception handling
        String program =
            "safe_operation :- " +
            "  catch(risky_operation, Error, " +
            "        (write('Caught: '), write(Error), nl)).";
        
        prolog.consult(program);
        prolog.solve("safe_operation");
    }
}
```

---

## Advanced Integration Patterns

### Prolog Service Class

```java
// Comprehensive service class for Prolog operations
public class PrologService {
    
    private final Prolog engine;
    private final Map<String, Object> cache;
    
    public PrologService() {
        this.engine = new Prolog();
        this.cache = new HashMap<>();
        initializeService();
    }
    
    private void initializeService() {
        // Load standard knowledge bases
        loadStandardKnowledge();
        
        // Register custom predicates
        registerCustomPredicates();
        
        // Set up caching
        setupCaching();
    }
    
    // Knowledge base management
    public void loadKnowledgeBase(String name, String... clauses) {
        for (String clause : clauses) {
            engine.consult(clause);
        }
        cache.put("kb_" + name, clauses);
    }
    
    public void loadKnowledgeBaseFromFile(String filename) throws IOException {
        String content = Files.readString(Paths.get(filename));
        String[] clauses = content.split("\\.");
        
        for (String clause : clauses) {
            clause = clause.trim();
            if (!clause.isEmpty()) {
                engine.consult(clause + ".");
            }
        }
    }
    
    // Query execution with different result formats
    public List<Map<String, Object>> queryForObjects(String query) {
        List<Map<String, Term>> results = engine.solve(query);
        return results.stream()
            .map(this::convertTermMapToObjectMap)
            .collect(Collectors.toList());
    }
    
    public <T> List<T> queryForType(String query, Class<T> resultType) {
        List<Map<String, Term>> results = engine.solve(query);
        return results.stream()
            .map(solution -> convertToType(solution, resultType))
            .filter(Objects::nonNull)
            .collect(Collectors.toList());
    }
    
    public boolean askQuery(String query) {
        List<Map<String, Term>> results = engine.solve(query);
        return !results.isEmpty();
    }
    
    public Optional<Map<String, Object>> querySingle(String query) {
        List<Map<String, Object>> results = queryForObjects(query);
        return results.isEmpty() ? Optional.empty() : Optional.of(results.get(0));
    }
    
    // Utility methods
    private Map<String, Object> convertTermMapToObjectMap(Map<String, Term> termMap) {
        Map<String, Object> objectMap = new HashMap<>();
        
        for (Map.Entry<String, Term> entry : termMap.entrySet()) {
            objectMap.put(entry.getKey(), convertTermToObject(entry.getValue()));
        }
        
        return objectMap;
    }
    
    private Object convertTermToObject(Term term) {
        if (term instanceof Atom) {
            return ((Atom) term).getName();
        } else if (term instanceof Number) {
            return ((Number) term).getValue();
        } else if (term instanceof CompoundTerm) {
            return term.toString(); // Could be enhanced for specific types
        } else {
            return term.toString();
        }
    }
    
    private <T> T convertToType(Map<String, Term> solution, Class<T> resultType) {
        // Implementation would depend on specific conversion needs
        // This is a simplified example
        try {
            Constructor<T> constructor = resultType.getDeclaredConstructor(Map.class);
            return constructor.newInstance(solution);
        } catch (Exception e) {
            return null;
        }
    }
    
    // Caching support
    private void setupCaching() {
        // Implement query result caching if needed
    }
    
    // Transaction support
    public void executeTransaction(Runnable operations) {
        // Save current state
        Prolog backup = new Prolog();
        // Copy state to backup...
        
        try {
            operations.run();
        } catch (Exception e) {
            // Restore from backup
            this.engine = backup;
            throw e;
        }
    }
}
```

### Integration with Spring Framework

```java
@Component
@Service
public class SpringPrologService {
    
    private final Prolog prologEngine;
    
    @Autowired
    public SpringPrologService(@Value("${prolog.knowledge.base.path}") String kbPath) {
        this.prologEngine = new Prolog();
        loadKnowledgeBase(kbPath);
    }
    
    @PostConstruct
    public void initialize() {
        registerCustomPredicates();
    }
    
    @Cacheable("prolog-queries")
    public List<Map<String, Object>> executeQuery(String query) {
        List<Map<String, Term>> results = prologEngine.solve(query);
        return convertResults(results);
    }
    
    @Async
    public CompletableFuture<List<Map<String, Object>>> executeQueryAsync(String query) {
        return CompletableFuture.supplyAsync(() -> executeQuery(query));
    }
    
    @EventListener
    public void handleKnowledgeUpdateEvent(KnowledgeUpdateEvent event) {
        // Reload knowledge base when notified
        loadKnowledgeBase(event.getKnowledgeBasePath());
    }
    
    private void loadKnowledgeBase(String path) {
        try {
            String content = Files.readString(Paths.get(path));
            String[] clauses = content.split("\\.");
            
            for (String clause : clauses) {
                clause = clause.trim();
                if (!clause.isEmpty()) {
                    prologEngine.consult(clause + ".");
                }
            }
        } catch (IOException e) {
            throw new RuntimeException("Failed to load knowledge base", e);
        }
    }
    
    private List<Map<String, Object>> convertResults(List<Map<String, Term>> results) {
        return results.stream()
            .map(this::convertTermMap)
            .collect(Collectors.toList());
    }
    
    private Map<String, Object> convertTermMap(Map<String, Term> termMap) {
        Map<String, Object> result = new HashMap<>();
        termMap.forEach((key, term) -> {
            if (term instanceof Atom) {
                result.put(key, ((Atom) term).getName());
            } else if (term instanceof Number) {
                result.put(key, ((Number) term).getValue());
            } else {
                result.put(key, term.toString());
            }
        });
        return result;
    }
}
```

---

## Error Handling

### Exception Management

```java
import it.denzosoft.jprolog.PrologEvaluationException;

public class ErrorHandlingExample {
    
    public static void main(String[] args) {
        Prolog prolog = new Prolog();
        
        // Example of handling different types of errors
        handleSyntaxErrors(prolog);
        handleRuntimeErrors(prolog);
        handleCustomErrors(prolog);
    }
    
    private static void handleSyntaxErrors(Prolog prolog) {
        try {
            // This will cause a syntax error
            prolog.consult("parent(tom bob).");  // Missing comma
        } catch (PrologEvaluationException e) {
            System.err.println("Syntax error: " + e.getMessage());
            // Log error, show user-friendly message, etc.
        }
    }
    
    private static void handleRuntimeErrors(Prolog prolog) {
        try {
            // This might cause runtime errors
            List<Map<String, Term>> results = prolog.solve("X is 1 / 0.");
        } catch (ArithmeticException e) {
            System.err.println("Arithmetic error: " + e.getMessage());
        } catch (PrologEvaluationException e) {
            System.err.println("Prolog evaluation error: " + e.getMessage());
        }
    }
    
    private static void handleCustomErrors(Prolog prolog) {
        try {
            // Custom error handling in predicates
            prolog.consult("safe_divide(X, Y, Z) :- (Y =:= 0 -> throw(division_by_zero) ; Z is X / Y).");
            
            List<Map<String, Term>> results = prolog.solve("safe_divide(10, 0, Z).");
            
        } catch (Exception e) {
            System.err.println("Custom error: " + e.getMessage());
        }
    }
}

// Custom exception types
public class PrologIntegrationException extends Exception {
    
    private final String query;
    private final String context;
    
    public PrologIntegrationException(String message, String query, String context) {
        super(message);
        this.query = query;
        this.context = context;
    }
    
    public String getQuery() { return query; }
    public String getContext() { return context; }
}
```

---

## Performance Considerations

### Optimization Techniques

```java
public class PerformanceOptimizationExample {
    
    private final Prolog prolog;
    private final ExecutorService executorService;
    private final Map<String, List<Map<String, Term>>> queryCache;
    
    public PerformanceOptimizationExample() {
        this.prolog = new Prolog();
        this.executorService = Executors.newFixedThreadPool(4);
        this.queryCache = new ConcurrentHashMap<>();
    }
    
    // Query caching
    public List<Map<String, Term>> cachedQuery(String query) {
        return queryCache.computeIfAbsent(query, k -> prolog.solve(k));
    }
    
    // Batch query execution
    public Map<String, List<Map<String, Term>>> executeBatch(List<String> queries) {
        Map<String, List<Map<String, Term>>> results = new ConcurrentHashMap<>();
        
        List<CompletableFuture<Void>> futures = queries.stream()
            .map(query -> CompletableFuture.runAsync(() -> {
                results.put(query, prolog.solve(query));
            }, executorService))
            .collect(Collectors.toList());
        
        // Wait for all queries to complete
        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
        
        return results;
    }
    
    // Optimized knowledge base loading
    public void loadOptimizedKnowledgeBase(List<String> clauses) {
        // Sort clauses to optimize indexing
        List<String> facts = clauses.stream()
            .filter(clause -> !clause.contains(":-"))
            .collect(Collectors.toList());
        
        List<String> rules = clauses.stream()
            .filter(clause -> clause.contains(":-"))
            .collect(Collectors.toList());
        
        // Load facts first (better for indexing)
        facts.forEach(prolog::consult);
        rules.forEach(prolog::consult);
    }
    
    // Memory management
    public void clearCache() {
        queryCache.clear();
    }
    
    // Resource cleanup
    public void shutdown() {
        executorService.shutdown();
        clearCache();
    }
}
```

---

## Complete Examples

### Expert System Integration

```java
// Complete example: Medical diagnosis expert system
public class MedicalDiagnosisSystem {
    
    private final Prolog prolog;
    private final Map<String, Double> confidenceScores;
    
    public MedicalDiagnosisSystem() {
        this.prolog = new Prolog();
        this.confidenceScores = new HashMap<>();
        initializeMedicalKnowledge();
    }
    
    private void initializeMedicalKnowledge() {
        // Load medical knowledge base
        String[] medicalRules = {
            // Symptoms
            "symptom(fever).",
            "symptom(cough).",
            "symptom(headache).",
            "symptom(sore_throat).",
            "symptom(fatigue).",
            
            // Diseases and their symptoms
            "has_symptom(flu, fever).",
            "has_symptom(flu, cough).",
            "has_symptom(flu, headache).",
            "has_symptom(flu, fatigue).",
            
            "has_symptom(cold, cough).",
            "has_symptom(cold, sore_throat).",
            "has_symptom(cold, fatigue).",
            
            "has_symptom(migraine, headache).",
            "has_symptom(migraine, fatigue).",
            
            // Diagnostic rules
            "possible_disease(Disease, Symptoms) :- " +
            "    findall(S, (member(S, Symptoms), has_symptom(Disease, S)), MatchedSymptoms), " +
            "    length(MatchedSymptoms, Count), " +
            "    Count > 0.",
            
            // Confidence calculation
            "confidence(Disease, Symptoms, Confidence) :- " +
            "    findall(S, has_symptom(Disease, S), AllSymptoms), " +
            "    findall(S, (member(S, Symptoms), has_symptom(Disease, S)), MatchedSymptoms), " +
            "    length(AllSymptoms, Total), " +
            "    length(MatchedSymptoms, Matched), " +
            "    Confidence is (Matched / Total) * 100."
        };
        
        for (String rule : medicalRules) {
            prolog.consult(rule);
        }
    }
    
    public List<DiagnosisResult> diagnose(List<String> symptoms) {
        // Convert symptoms to Prolog list format
        String symptomList = "[" + symptoms.stream()
            .collect(Collectors.joining(", ")) + "]";
        
        // Find possible diseases
        String query = "possible_disease(Disease, " + symptomList + ").";
        List<Map<String, Term>> results = prolog.solve(query);
        
        List<DiagnosisResult> diagnoses = new ArrayList<>();
        
        for (Map<String, Term> result : results) {
            String disease = ((Atom) result.get("Disease")).getName();
            
            // Calculate confidence
            String confidenceQuery = "confidence(" + disease + ", " + symptomList + ", C).";
            List<Map<String, Term>> confidenceResults = prolog.solve(confidenceQuery);
            
            double confidence = 0.0;
            if (!confidenceResults.isEmpty()) {
                confidence = ((Number) confidenceResults.get(0).get("C")).getValue();
            }
            
            diagnoses.add(new DiagnosisResult(disease, confidence, symptoms));
        }
        
        // Sort by confidence (highest first)
        diagnoses.sort((a, b) -> Double.compare(b.getConfidence(), a.getConfidence()));
        
        return diagnoses;
    }
    
    // Add new medical knowledge
    public void addMedicalFact(String disease, String symptom) {
        prolog.consult("has_symptom(" + disease + ", " + symptom + ").");
    }
    
    // Diagnosis result class
    public static class DiagnosisResult {
        private final String disease;
        private final double confidence;
        private final List<String> symptoms;
        
        public DiagnosisResult(String disease, double confidence, List<String> symptoms) {
            this.disease = disease;
            this.confidence = confidence;
            this.symptoms = new ArrayList<>(symptoms);
        }
        
        // Getters
        public String getDisease() { return disease; }
        public double getConfidence() { return confidence; }
        public List<String> getSymptoms() { return symptoms; }
        
        @Override
        public String toString() {
            return String.format("%s (%.1f%% confidence)", disease, confidence);
        }
    }
    
    // Usage example
    public static void main(String[] args) {
        MedicalDiagnosisSystem system = new MedicalDiagnosisSystem();
        
        List<String> patientSymptoms = Arrays.asList("fever", "cough", "headache");
        List<DiagnosisResult> diagnoses = system.diagnose(patientSymptoms);
        
        System.out.println("Possible diagnoses for symptoms: " + patientSymptoms);
        for (DiagnosisResult diagnosis : diagnoses) {
            System.out.println("  " + diagnosis);
        }
    }
}
```

This comprehensive guide covers all aspects of integrating JProlog with Java applications. The examples demonstrate practical patterns for embedding Prolog reasoning in Java systems, from basic query execution to complex expert systems. The modular design allows developers to choose the integration level that best fits their application needs.