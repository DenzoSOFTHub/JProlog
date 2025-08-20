# JProlog Exception Handling Guide

**Version**: JProlog v2.0.9  
**Last Updated**: 2025-08-20  
**ISO Compliance**: Full ISO 13211-1 standard support

This guide provides comprehensive documentation for exception handling in JProlog, covering the complete ISO-compliant exception system implemented in Phase 2.

---

## Table of Contents

1. [Overview](#overview)
2. [Basic Exception Handling](#basic-exception-handling)
3. [ISO Standard Error Terms](#iso-standard-error-terms)
4. [Built-in Predicates](#built-in-predicates)
5. [Common Usage Patterns](#common-usage-patterns)
6. [Error Recovery Strategies](#error-recovery-strategies)
7. [Advanced Topics](#advanced-topics)
8. [Examples](#examples)
9. [Troubleshooting](#troubleshooting)

---

## Overview

JProlog implements the complete ISO 13211-1 standard for Prolog exception handling. The system provides:

- **throw/1**: Throws exceptions with any term
- **catch/3**: Catches and handles exceptions 
- **ISO error terms**: Standard error term hierarchy
- **Built-in error checking**: Automatic error detection in arithmetic operations
- **Exception propagation**: Proper exception bubbling through call stack

### Key Concepts

**Exception**: Any Prolog term that represents an error or exceptional condition
**Throwing**: The act of raising an exception using throw/1
**Catching**: The act of handling an exception using catch/3
**Recovery**: The alternative computation executed when an exception is caught

---

## Basic Exception Handling

### The catch/3 Predicate

The fundamental mechanism for exception handling in JProlog:

```prolog
catch(+Goal, +Catcher, +Recovery)
```

**How it works**:
1. Execute `Goal`
2. If `Goal` succeeds without throwing, catch/3 succeeds
3. If `Goal` throws an exception that unifies with `Catcher`, execute `Recovery`
4. If the exception doesn't unify with `Catcher`, it propagates upward

**Basic Example**:
```prolog
% Simple exception handling
?- catch(throw(my_error), my_error, writeln('Error handled')).
Error handled
true.
```

### The throw/1 Predicate

Throws an exception with any Prolog term:

```prolog
throw(+Exception)
```

**Basic Example**:
```prolog
% Throw a simple atom
?- throw(error_occurred).
ERROR: Uncaught exception: error_occurred

% Throw a compound term
?- throw(error(type_error(integer, abc), context)).
ERROR: Uncaught exception: error(type_error(integer, abc), context)
```

---

## ISO Standard Error Terms

JProlog supports the complete ISO 13211-1 error term hierarchy. All error terms follow the pattern:

```prolog
error(ErrorType, Context)
```

### Standard Error Types

#### instantiation_error
**Used when**: An argument is a variable when it should be instantiated
```prolog
% Example: calling atom_length with uninstantiated atom
?- catch(atom_length(X, _), Error, writeln(Error)).
error(instantiation_error, atom_length/2)
true.
```

#### type_error(ValidType, Culprit)
**Used when**: An argument has wrong type
```prolog
% Example: calling atom_length with number instead of atom
?- catch(atom_length(123, _), Error, writeln(Error)).
error(type_error(atom, 123), atom_length/2)
true.
```

#### domain_error(ValidDomain, Culprit)
**Used when**: An argument is correct type but outside valid domain
```prolog
% Example: negative number where positive expected
?- catch(succ(-1, _), Error, writeln(Error)).
error(domain_error(not_less_than_zero, -1), succ/2)
true.
```

#### evaluation_error(Error)
**Used for**: Arithmetic evaluation errors
```prolog
% Zero divisor error
?- catch(X is 1/0, Error, writeln(Error)).
error(evaluation_error(zero_divisor), (/)/2)
true.
```

#### existence_error(ObjectType, Culprit)
**Used when**: Referenced object doesn't exist
```prolog
% File not found
?- catch(open('nonexistent.txt', read, _), Error, writeln(Error)).
error(existence_error(source_sink, 'nonexistent.txt'), open/3)
true.
```

#### permission_error(Operation, PermissionType, Culprit)
**Used when**: Operation not permitted on object
```prolog
% Attempting to modify built-in predicate
?- catch(retract(member(_, _)), Error, writeln(Error)).
error(permission_error(modify, static_procedure, member/2), retract/1)
true.
```

#### representation_error(Flag)
**Used when**: Implementation limit exceeded
```prolog
% Number too large to represent
?- catch(X is 10^1000, Error, writeln(Error)).
error(representation_error(max_integer), is/2)
true.
```

#### resource_error(Resource)
**Used when**: System resources exhausted
```prolog
% Out of memory
?- catch(create_huge_structure, Error, writeln(Error)).
error(resource_error(memory), create_huge_structure/0)
true.
```

#### syntax_error(Message)
**Used for**: Parsing errors
```prolog
% Invalid syntax in term
?- catch(read_term_from_string("f(", Term, []), Error, writeln(Error)).
error(syntax_error('missing closing parenthesis'), read_term_from_string/3)
true.
```

### Factory Class: ISOErrorTerms

JProlog provides a factory class for creating standard error terms:

```java
// Java code - creates ISO-compliant error terms
Term instantiationError = ISOErrorTerms.instantiationError("predicate/arity");
Term typeError = ISOErrorTerms.typeError("integer", "abc", "predicate/arity");
Term evaluationError = ISOErrorTerms.zeroDivisorError("(/)/2");
```

---

## Built-in Predicates

### throw/1
```prolog
throw(+Exception)
```

**Purpose**: Throw an exception
**Always**: Never succeeds normally - always throws
**Exception**: Can be any Prolog term

**Examples**:
```prolog
% Simple atom
?- throw(error).

% ISO error term
?- throw(error(type_error(integer, abc), my_predicate/1)).

% Complex structure
?- throw(custom_error(code(123), message("Something went wrong"))).
```

### catch/3
```prolog
catch(+Goal, +Catcher, +Recovery)
```

**Purpose**: Execute Goal with exception handling
**Success**: Succeeds if Goal succeeds or if Recovery succeeds after catching
**Exception**: Unhandled exceptions propagate upward

**Examples**:
```prolog
% Basic pattern matching
?- catch(throw(error), error, write('caught')).

% Partial pattern matching
?- catch(
    throw(error(type_error(integer, abc), context)),
    error(type_error(_, _), _),
    write('Type error caught')
).

% Multiple exception types
?- catch(
    risky_operation,
    Error,
    handle_error(Error)
).
```

---

## Common Usage Patterns

### 1. Input Validation

```prolog
validate_positive(X) :-
    (var(X) ->
        throw(error(instantiation_error, validate_positive/1))
    ; \+ number(X) ->
        throw(error(type_error(number, X), validate_positive/1))
    ; X =< 0 ->
        throw(error(domain_error(positive_number, X), validate_positive/1))
    ; true
    ).

safe_sqrt(X, Result) :-
    catch(
        (validate_positive(X), Result is sqrt(X)),
        Error,
        (writeln('Error in sqrt calculation:'), writeln(Error), fail)
    ).
```

### 2. Resource Management

```prolog
safe_file_operation(FileName, Result) :-
    catch(
        (
            open(FileName, read, Stream),
            read_file_contents(Stream, Contents),
            close(Stream),
            process_contents(Contents, Result)
        ),
        Error,
        (
            writeln('File operation failed:'),
            writeln(Error),
            fail
        )
    ).
```

### 3. Graceful Degradation

```prolog
robust_calculation(Input, Result) :-
    catch(
        complex_calculation(Input, Result),
        Error,
        (
            writeln('Complex calculation failed, using simple method'),
            simple_calculation(Input, Result)
        )
    ).
```

### 4. Custom Error Types

```prolog
validate_email(Email) :-
    (atom(Email) ->
        (atom_codes(Email, Codes),
         (member(64, Codes) ->  % '@' character
            true
         ;  throw(invalid_email(missing_at_symbol, Email))
         ))
    ; throw(invalid_email(not_atom, Email))
    ).

process_user_data(UserData) :-
    catch(
        (
            validate_email(UserData.email),
            process_validated_user(UserData)
        ),
        invalid_email(Reason, Email),
        (
            format('Email validation failed: ~w for ~w~n', [Reason, Email]),
            use_default_email(UserData)
        )
    ).
```

---

## Error Recovery Strategies

### 1. Retry with Different Parameters

```prolog
connect_with_retry(Host, Port, Connection) :-
    catch(
        connect(Host, Port, Connection),
        connection_error,
        (
            writeln('Primary connection failed, trying backup'),
            catch(
                connect('backup-server.com', Port, Connection),
                connection_error,
                fail
            )
        )
    ).
```

### 2. Logging and Continuation

```prolog
safe_process_item(Item) :-
    catch(
        process_item(Item),
        Error,
        (
            log_error('Item processing failed', Item, Error),
            % Continue with next item instead of failing
            true
        )
    ).

safe_process_list([]).
safe_process_list([H|T]) :-
    safe_process_item(H),
    safe_process_list(T).
```

### 3. Default Values

```prolog
get_config_value(Key, Value) :-
    catch(
        read_config_file(Key, Value),
        Error,
        (
            writeln('Using default value for config key'),
            default_config_value(Key, Value)
        )
    ).
```

---

## Advanced Topics

### Exception Propagation

Exceptions propagate up the call stack until caught:

```prolog
level3 :- throw(deep_error).
level2 :- level3.
level1 :- level2.

test_propagation :-
    catch(
        level1,
        deep_error,
        writeln('Caught exception from level 3')
    ).
```

### Nested Exception Handling

```prolog
complex_operation :-
    catch(
        (
            catch(
                risky_operation_1,
                type_error(_, _),
                writeln('Type error in operation 1')
            ),
            catch(
                risky_operation_2,
                domain_error(_, _),
                writeln('Domain error in operation 2')
            )
        ),
        Error,
        (
            writeln('Unexpected error:'),
            writeln(Error)
        )
    ).
```

### Exception Cleanup

```prolog
with_resource(ResourceId, Goal) :-
    acquire_resource(ResourceId),
    catch(
        call(Goal),
        Error,
        (
            cleanup_resource(ResourceId),
            throw(Error)  % Re-throw after cleanup
        )
    ),
    cleanup_resource(ResourceId).
```

---

## Examples

### Example 1: Calculator with Error Handling

```prolog
safe_divide(X, Y, Result) :-
    catch(
        (
            (var(X) -> throw(error(instantiation_error, safe_divide/3)); true),
            (var(Y) -> throw(error(instantiation_error, safe_divide/3)); true),
            (\+ number(X) -> throw(error(type_error(number, X), safe_divide/3)); true),
            (\+ number(Y) -> throw(error(type_error(number, Y), safe_divide/3)); true),
            (Y =:= 0 -> throw(error(evaluation_error(zero_divisor), safe_divide/3)); true),
            Result is X / Y
        ),
        Error,
        (
            writeln('Division error:'),
            writeln(Error),
            fail
        )
    ).

% Usage examples:
% ?- safe_divide(10, 2, R).     % R = 5.0
% ?- safe_divide(10, 0, R).     % Error: zero divisor
% ?- safe_divide(X, 2, R).      % Error: instantiation_error
% ?- safe_divide(abc, 2, R).    % Error: type_error(number, abc)
```

### Example 2: File Processing Pipeline

```prolog
process_file_safely(FileName) :-
    catch(
        (
            open(FileName, read, Stream),
            catch(
                (
                    read_and_process_data(Stream, ProcessedData),
                    write_results(ProcessedData)
                ),
                processing_error(Reason),
                (
                    format('Data processing failed: ~w~n', [Reason]),
                    close(Stream)
                )
            ),
            close(Stream)
        ),
        existence_error(source_sink, _),
        format('File not found: ~w~n', [FileName])
    ).

read_and_process_data(Stream, ProcessedData) :-
    catch(
        read_term(Stream, Term),
        syntax_error(Message),
        throw(processing_error(invalid_syntax(Message)))
    ),
    (Term == end_of_file ->
        ProcessedData = []
    ; 
        process_term(Term, ProcessedTerm),
        read_and_process_data(Stream, RestData),
        ProcessedData = [ProcessedTerm|RestData]
    ).
```

### Example 3: Robust Web Service Client

```prolog
call_web_service(Endpoint, Data, Result) :-
    catch(
        (
            validate_endpoint(Endpoint),
            validate_data(Data),
            make_http_request(Endpoint, Data, RawResult),
            parse_response(RawResult, Result)
        ),
        Error,
        handle_service_error(Error, Result)
    ).

handle_service_error(error(type_error(atom, BadEndpoint), _), error_result) :-
    format('Invalid endpoint: ~w~n', [BadEndpoint]).

handle_service_error(network_error(timeout), error_result) :-
    writeln('Network timeout - service unavailable').

handle_service_error(http_error(404), not_found) :-
    writeln('Service endpoint not found').

handle_service_error(parsing_error(malformed_json), error_result) :-
    writeln('Received malformed response from service').

handle_service_error(Error, error_result) :-
    format('Unexpected service error: ~w~n', [Error]).
```

---

## Troubleshooting

### Common Issues

**1. Exception Not Caught**
```prolog
% Problem: Pattern doesn't match
?- catch(throw(error(type_error(integer, abc), context)), type_error(integer, _), handle).

% Solution: Include full error structure
?- catch(throw(error(type_error(integer, abc), context)), error(type_error(integer, _), _), handle).
```

**2. Variable Binding in Catch Patterns**
```prolog
% Problem: Variable not accessible in recovery
?- catch(throw(error(Info, Context)), error(Info, Context), writeln(Info)).

% Solution: Use compound matching
?- catch(throw(error(type_error(integer, abc), context)), Error, (Error = error(Info, _), writeln(Info))).
```

**3. Exception Propagation Issues**
```prolog
% Problem: Exception caught too early
outer_goal :-
    catch(
        inner_goal,
        _,  % Catches all exceptions - too broad
        writeln('Something went wrong')
    ).

% Solution: Catch specific exceptions
outer_goal :-
    catch(
        inner_goal,
        error(type_error(_, _), _),  % Only catch type errors
        writeln('Type error occurred')
    ).
```

### Debugging Exception Handling

**Enable trace mode for exception tracking**:
```prolog
?- trace.
?- catch(problematic_goal, Error, writeln(Error)).
```

**Use debugging predicates**:
```prolog
debug_catch(Goal, Catcher, Recovery) :-
    writeln('Executing goal with exception handling'),
    catch(
        (writeln('Executing goal'), call(Goal)),
        Error,
        (
            format('Caught exception: ~w~n', [Error]),
            (Error = Catcher ->
                (writeln('Pattern matched, executing recovery'), call(Recovery))
            ;
                (writeln('Pattern did not match, re-throwing'), throw(Error))
            )
        )
    ).
```

### Performance Considerations

- **Exception handling has overhead**: Use only when necessary
- **Avoid exceptions in normal control flow**: Use deterministic predicates when possible
- **Catch specific exceptions**: Broad catch patterns can hide bugs
- **Clean up resources**: Always ensure proper cleanup in exception paths

---

**Related Documentation**:
- [Built-in Predicates Reference](../references/BUILTIN_PREDICATES_REFERENCE.md)
- [ISO Compliance Analysis](../reports/report-iso-compliance.md)  
- [Developer Guide](guide-developer-exception-handling.md)
- [JProlog User Manual](../../USER_MANUAL.md)