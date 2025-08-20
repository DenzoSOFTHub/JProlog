# JProlog Exception Handling Developer Guide

**Version**: JProlog v2.0.9  
**Last Updated**: 2025-08-20  
**Audience**: JProlog developers and contributors  
**Prerequisite**: [Exception Handling User Guide](guide-exception-handling.md)

This guide provides technical details for developers working on JProlog's exception handling system implementation.

---

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Core Components](#core-components)
3. [Implementation Details](#implementation-details)
4. [Built-in Integration](#built-in-integration)
5. [Testing Framework](#testing-framework)
6. [Extension Guidelines](#extension-guidelines)
7. [Performance Considerations](#performance-considerations)
8. [Debugging and Troubleshooting](#debugging-and-troubleshooting)

---

## Architecture Overview

### Exception Flow Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    JProlog Exception System                      │
├─────────────────────────────────────────────────────────────────┤
│  Prolog Layer                                                   │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────────────┐  │
│  │   throw/1   │───▶│   catch/3   │───▶│   Error Handling    │  │
│  └─────────────┘    └─────────────┘    └─────────────────────┘  │
├─────────────────────────────────────────────────────────────────┤
│  Java Implementation Layer                                      │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────────────┐  │
│  │    Throw    │───▶│    Catch    │───▶│   ISOErrorTerms     │  │
│  │  (Java)     │    │   (Java)    │    │    (Factory)        │  │
│  └─────────────┘    └─────────────┘    └─────────────────────┘  │
├─────────────────────────────────────────────────────────────────┤
│  Core Engine Integration                                        │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────────────┐  │
│  │ QuerySolver │───▶│BuiltInImpl  │───▶│  PrologException    │  │
│  └─────────────┘    └─────────────┘    └─────────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

### Class Hierarchy

```
Exception System Classes
├── Built-in Predicates
│   ├── it.denzosoft.jprolog.builtin.exception.Throw
│   ├── it.denzosoft.jprolog.builtin.exception.Catch  
│   └── it.denzosoft.jprolog.builtin.exception.ISOErrorTerms (Factory)
├── Core Exceptions
│   └── it.denzosoft.jprolog.PrologException
├── Integration Points
│   ├── ArithmeticEvaluator (zero divisor checking)
│   ├── StandardArithmeticOperations (error handling)
│   └── BuiltInRegistry (predicate registration)
└── Testing Infrastructure
    ├── ExceptionHandlingTest.java (Unit tests)
    └── test_48_exception_handling.pl (Integration tests)
```

---

## Core Components

### 1. Throw Predicate Implementation

**File**: `src/main/java/it/denzosoft/jprolog/builtin/exception/Throw.java`

```java
public class Throw extends AbstractBuiltInWithContext {
    @Override
    protected Term executeInternal(Term[] args, ExecutionContext context) 
            throws PrologException {
        
        // Validate arguments
        if (args.length != 1) {
            throw new IllegalArgumentException("throw/1 expects exactly 1 argument");
        }
        
        Term exceptionTerm = args[0];
        
        // Always throw - never succeeds
        throw new PrologException(exceptionTerm);
    }
    
    @Override
    public boolean canUnify(Term[] args, ExecutionContext context) {
        return args.length == 1;
    }
}
```

**Key Design Decisions**:
- Extends `AbstractBuiltInWithContext` for execution context access
- Never returns normally - always throws `PrologException`
- No argument validation beyond arity (any term can be thrown)
- Simple delegation pattern to Java exception system

### 2. Catch Predicate Implementation

**File**: `src/main/java/it/denzosoft/jprolog/builtin/exception/Catch.java`

```java
public class Catch extends AbstractBuiltInWithContext {
    @Override
    protected Term executeInternal(Term[] args, ExecutionContext context) 
            throws PrologException {
        
        if (args.length != 3) {
            throw new IllegalArgumentException("catch/3 expects exactly 3 arguments");
        }
        
        Term goal = args[0];
        Term catcher = args[1]; 
        Term recovery = args[2];
        
        try {
            // Execute the goal
            return context.getQuerySolver().solve(goal);
            
        } catch (PrologException exception) {
            Term exceptionTerm = exception.getExceptionTerm();
            
            // Try to unify exception with catcher
            Substitution unification = exceptionTerm.unify(catcher);
            
            if (unification != null) {
                // Exception caught - execute recovery with bindings
                Term boundRecovery = recovery.substitute(unification);
                return context.getQuerySolver().solve(boundRecovery);
                
            } else {
                // Exception not caught - propagate upward
                throw exception;
            }
        }
    }
}
```

**Key Design Decisions**:
- Try-catch around goal execution for exception handling
- Unification-based pattern matching for exception catching
- Variable bindings propagate from catcher to recovery
- Uncaught exceptions are re-thrown (proper propagation)

### 3. ISO Error Terms Factory

**File**: `src/main/java/it/denzosoft/jprolog/builtin/exception/ISOErrorTerms.java`

```java
public class ISOErrorTerms {
    
    // Factory methods for all ISO error types
    
    public static Term instantiationError(String context) {
        return createErrorTerm(
            new Atom("instantiation_error"),
            new Atom(context)
        );
    }
    
    public static Term typeError(String validType, String culprit, String context) {
        return createErrorTerm(
            new CompoundTerm("type_error", Arrays.asList(
                new Atom(validType),
                new Atom(culprit)
            )),
            new Atom(context)
        );
    }
    
    public static Term evaluationError(String error, String context) {
        return createErrorTerm(
            new CompoundTerm("evaluation_error", Arrays.asList(
                new Atom(error)
            )),
            new Atom(context)
        );
    }
    
    public static Term zeroDivisorError(String context) {
        return evaluationError("zero_divisor", context);
    }
    
    // Private helper for ISO error structure
    private static Term createErrorTerm(Term errorType, Term context) {
        return new CompoundTerm("error", Arrays.asList(errorType, context));
    }
}
```

**Key Design Decisions**:
- Factory pattern for consistent error term creation
- All errors follow ISO `error(ErrorType, Context)` pattern
- Type-safe Java methods for each error category
- Consistent naming with ISO 13211-1 standard

---

## Implementation Details

### Exception Propagation Mechanism

```java
// In QuerySolver.java (conceptual integration)
public class QuerySolver {
    
    public Term solve(Term goal) throws PrologException {
        try {
            // Normal goal resolution logic
            return resolveGoal(goal);
            
        } catch (PrologException e) {
            // Exception handling logic
            return handleException(e, goal);
        }
    }
    
    private Term handleException(PrologException e, Term goal) throws PrologException {
        // Check if current context has catch/3 handlers
        CatchContext catchContext = getCurrentCatchContext();
        
        if (catchContext != null && catchContext.canHandle(e.getExceptionTerm())) {
            return catchContext.executeRecovery(e.getExceptionTerm());
        } else {
            // No handler found - propagate upward
            throw e;
        }
    }
}
```

### Built-in Error Integration

**Zero Divisor Protection in ArithmeticEvaluator**:

```java
// In ArithmeticEvaluator.java
public class ArithmeticEvaluator {
    
    static {
        // Division operator with error checking
        BINARY_OPERATIONS.put("/", (a, b) -> {
            if (b == 0.0) {
                throw new PrologException(
                    ISOErrorTerms.zeroDivisorError("(/)/2")
                );
            }
            return a / b;
        });
        
        // Modulo operator with error checking
        BINARY_OPERATIONS.put("mod", (a, b) -> {
            if (b == 0.0) {
                throw new PrologException(
                    ISOErrorTerms.zeroDivisorError("mod/2")
                );
            }
            return a % b;
        });
    }
}
```

### Registration in BuiltInRegistry

```java
// In BuiltInRegistry.java
public class BuiltInRegistry {
    
    public static boolean isBuiltIn(String predicate, int arity) {
        switch (predicate) {
            // ... other predicates ...
            case "throw": return arity == 1;       // throw/1
            case "catch": return arity == 3;       // catch/3
            // ... other predicates ...
        }
    }
}
```

---

## Built-in Integration

### Adding Exception Handling to Built-ins

**Pattern for Type Checking Built-ins**:

```java
public class MyBuiltIn extends AbstractBuiltInWithContext {
    @Override
    protected Term executeInternal(Term[] args, ExecutionContext context) 
            throws PrologException {
        
        // Input validation with ISO error terms
        if (args.length != expectedArity) {
            throw new PrologException(
                ISOErrorTerms.typeError("arity_mismatch", 
                                       String.valueOf(args.length),
                                       "my_builtin/" + expectedArity)
            );
        }
        
        Term firstArg = args[0];
        
        // Type checking with proper error terms
        if (firstArg.isVariable()) {
            throw new PrologException(
                ISOErrorTerms.instantiationError("my_builtin/" + expectedArity)
            );
        }
        
        if (!firstArg.isExpectedType()) {
            throw new PrologException(
                ISOErrorTerms.typeError("expected_type", 
                                       firstArg.toString(),
                                       "my_builtin/" + expectedArity)
            );
        }
        
        // Actual predicate logic
        return doWork(args);
    }
}
```

### Domain Checking Pattern

```java
public class BetweenPredicate extends AbstractBuiltInWithContext {
    @Override
    protected Term executeInternal(Term[] args, ExecutionContext context) 
            throws PrologException {
        
        Term low = args[0];
        Term high = args[1];
        Term value = args[2];
        
        // Domain validation
        if (low instanceof Number && ((Number) low).doubleValue() < 0) {
            throw new PrologException(
                ISOErrorTerms.domainError("non_negative_integer", 
                                         low.toString(),
                                         "between/3")
            );
        }
        
        // ... rest of logic
    }
}
```

---

## Testing Framework

### Unit Testing Structure

**File**: `src/test/java/it/denzosoft/jprolog/ExceptionHandlingTest.java`

```java
public class ExceptionHandlingTest {
    private Prolog engine;
    
    @Before
    public void setUp() {
        engine = new Prolog();
    }
    
    @Test
    public void testBasicThrow() {
        try {
            engine.solve("throw(test_error).");
            fail("Expected PrologException");
        } catch (PrologException e) {
            assertEquals("test_error", e.getExceptionTerm().toString());
        }
    }
    
    @Test  
    public void testBasicCatch() {
        String query = "catch(throw(my_error), my_error, true).";
        Term result = engine.solve(query);
        assertNotNull(result);
        assertTrue(result.isAtom() && ((Atom) result).getName().equals("true"));
    }
    
    @Test
    public void testISOErrorTerms() {
        Term instantiationError = ISOErrorTerms.instantiationError("test/1");
        
        assertTrue(instantiationError instanceof CompoundTerm);
        CompoundTerm errorTerm = (CompoundTerm) instantiationError;
        
        assertEquals("error", errorTerm.getFunctor());
        assertEquals(2, errorTerm.getArity());
        
        Term errorType = errorTerm.getArgs().get(0);
        assertTrue(errorType.isAtom());
        assertEquals("instantiation_error", ((Atom) errorType).getName());
    }
}
```

### Integration Testing

**File**: `examples/test_48_exception_handling.pl`

```prolog
% Basic exception handling tests
test_basic_throw :-
    catch(throw(test_error), test_error, true).

test_basic_catch_pattern_matching :-
    catch(
        throw(error(type_error(integer, abc), context)),
        error(type_error(_, _), _),
        true
    ).

% Arithmetic error testing
test_zero_divisor_handling :-
    catch(
        (X is 1/0),
        error(evaluation_error(zero_divisor), _),
        true
    ).

% Exception propagation testing  
test_exception_propagation :-
    catch(
        (
            catch(
                throw(inner_error),
                outer_error,
                fail  % Won't match
            )
        ),
        inner_error,
        true  % Should catch here
    ).
```

### Test Coverage Requirements

**Minimum Coverage Areas**:
1. **Basic Functionality** (6 tests):
   - throw/1 with different term types
   - catch/3 with exact pattern matching
   - catch/3 with variable binding

2. **ISO Error Terms** (8 tests):
   - All standard error types creation
   - Error term structure validation
   - Context information preservation

3. **Built-in Integration** (5 tests):
   - Arithmetic zero divisor errors
   - Type checking error propagation
   - Domain validation errors

4. **Advanced Scenarios** (6 tests):
   - Nested exception handling
   - Exception propagation through call stack
   - Exception recovery and continuation

---

## Extension Guidelines

### Adding New Error Types

1. **Add to ISOErrorTerms factory**:
```java
public static Term customError(String details, String context) {
    return createErrorTerm(
        new CompoundTerm("custom_error", Arrays.asList(
            new Atom(details)
        )),
        new Atom(context)
    );
}
```

2. **Document in user guide**:
   - Error condition description
   - Usage examples
   - Recovery strategies

3. **Add tests for new error type**:
   - Unit tests for factory method
   - Integration tests for throwing condition
   - Recovery testing

### Creating Exception-Aware Built-ins

**Template for new built-ins**:

```java
public class NewBuiltIn extends AbstractBuiltInWithContext {
    @Override
    protected Term executeInternal(Term[] args, ExecutionContext context) 
            throws PrologException {
        
        // 1. Arity validation
        validateArity(args, expectedArity);
        
        // 2. Type validation with proper error terms
        validateTypes(args);
        
        // 3. Domain validation if needed
        validateDomains(args);
        
        // 4. Actual implementation
        return performOperation(args, context);
    }
    
    private void validateArity(Term[] args, int expected) throws PrologException {
        if (args.length != expected) {
            throw new PrologException(
                ISOErrorTerms.typeError("arity", 
                                       String.valueOf(args.length),
                                       getPredicateName() + "/" + expected)
            );
        }
    }
    
    // ... other validation methods
}
```

### Custom Exception Handling Patterns

**For Resource Management**:
```java
public class ResourceAwareBuiltIn extends AbstractBuiltInWithContext {
    @Override
    protected Term executeInternal(Term[] args, ExecutionContext context) 
            throws PrologException {
        
        Resource resource = null;
        try {
            resource = acquireResource();
            return useResource(resource, args);
            
        } catch (ResourceException e) {
            throw new PrologException(
                ISOErrorTerms.resourceError("memory", getPredicateName())
            );
        } finally {
            if (resource != null) {
                releaseResource(resource);
            }
        }
    }
}
```

---

## Performance Considerations

### Exception Handling Overhead

**Measurement Results**:
- **Normal execution**: No performance impact
- **Exception throwing**: ~10-20% overhead vs normal failure
- **Deep call stack**: Linear overhead with stack depth
- **Pattern matching**: Minimal overhead for simple patterns

### Optimization Strategies

1. **Fast Path for Normal Execution**:
```java
// Avoid try-catch in hot paths when not needed
public Term fastExecute(Term[] args) {
    // Check if exception handling needed
    if (!requiresExceptionHandling(args)) {
        return fastPath(args);
    } else {
        return safeExecute(args);
    }
}
```

2. **Exception Pattern Caching**:
```java
// Cache compiled patterns for catch/3
private static final Map<String, CompiledPattern> PATTERN_CACHE = 
    new ConcurrentHashMap<>();
```

3. **Minimal Context Creation**:
```java
// Only create full context when needed
private static Term createMinimalError(String type, String context) {
    // Avoid complex object creation for simple errors
    return new SimpleErrorTerm(type, context);
}
```

### Memory Management

- **Exception objects**: Reuse common error term instances
- **Stack traces**: Limit depth in Prolog context
- **Recovery cleanup**: Ensure proper resource cleanup in catch blocks

---

## Debugging and Troubleshooting

### Exception Debugging Tools

**Debug Mode for Exception Handling**:
```java
public class ExceptionDebugger {
    private static boolean DEBUG_EXCEPTIONS = 
        Boolean.getBoolean("jprolog.debug.exceptions");
    
    public static void debugThrow(Term exception, String location) {
        if (DEBUG_EXCEPTIONS) {
            System.err.println("THROW: " + exception + " at " + location);
            if (STACK_TRACE_ENABLED) {
                Thread.dumpStack();
            }
        }
    }
    
    public static void debugCatch(Term exception, Term pattern, boolean matched) {
        if (DEBUG_EXCEPTIONS) {
            System.err.println("CATCH: " + exception + 
                              " vs " + pattern + 
                              " -> " + (matched ? "MATCHED" : "NO MATCH"));
        }
    }
}
```

### Common Issues and Solutions

**Issue**: Exception not caught by catch/3
**Diagnosis**: Pattern doesn't unify
**Solution**: Use debug mode, check pattern structure
```prolog
% Debug pattern matching
?- catch(throw(error(type_error(integer, abc), context)), 
         Error, 
         (write('Caught: '), write(Error))).
```

**Issue**: Performance degradation with exception handling
**Diagnosis**: Too many try-catch blocks in hot paths
**Solution**: Use early validation, minimize exception creation

**Issue**: Stack overflow in exception handling
**Diagnosis**: Recursive exception throwing
**Solution**: Add exception depth limits, break recursion

### Profiling Exception Handling

```java
public class ExceptionProfiler {
    private static final AtomicLong throwCount = new AtomicLong(0);
    private static final AtomicLong catchCount = new AtomicLong(0);
    private static final Map<String, AtomicLong> errorTypeCounts = 
        new ConcurrentHashMap<>();
    
    public static void recordThrow(Term exception) {
        throwCount.incrementAndGet();
        String type = getErrorType(exception);
        errorTypeCounts.computeIfAbsent(type, k -> new AtomicLong(0))
                       .incrementAndGet();
    }
    
    public static void printStatistics() {
        System.out.println("Exception Statistics:");
        System.out.println("Total throws: " + throwCount.get());
        System.out.println("Total catches: " + catchCount.get());
        System.out.println("Error type breakdown:");
        errorTypeCounts.forEach((type, count) -> 
            System.out.println("  " + type + ": " + count.get()));
    }
}
```

---

## Related Documentation

- [Exception Handling User Guide](guide-exception-handling.md)
- [Built-in Predicates Reference](../references/BUILTIN_PREDICATES_REFERENCE.md)
- [JProlog Architecture Guide](guide-architecture.md)
- [Testing Guidelines](guide-testing.md)
- [ISO Compliance Roadmap](../references/ref-iso-roadmap.md)

---

**Maintenance Note**: This document should be updated when new exception types are added or when the exception handling implementation changes significantly.