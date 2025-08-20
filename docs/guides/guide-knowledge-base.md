# JProlog Knowledge Base Predicates Guide

**Version**: JProlog v2.0.6  
**Last Updated**: August 2025  
**Compatibility**: All JProlog versions 2.0+

---

## Table of Contents

1. [Introduction](#introduction)
2. [Dynamic Database Operations](#dynamic-database-operations)
3. [Assertion Predicates](#assertion-predicates)
4. [Retraction Predicates](#retraction-predicates)
5. [Database Inspection](#database-inspection)
6. [Predicate Information](#predicate-information)
7. [Advanced Database Operations](#advanced-database-operations)
8. [Working Examples](#working-examples)
9. [Best Practices](#best-practices)
10. [Troubleshooting](#troubleshooting)

---

## Introduction

JProlog provides comprehensive support for dynamic knowledge base manipulation, allowing you to modify facts and rules at runtime. This guide covers all built-in predicates for database operations, from basic assert/retract to advanced inspection capabilities.

### Core Concepts

- **Dynamic Predicates**: Facts and rules that can be modified at runtime
- **Static Predicates**: Built-in predicates that cannot be modified
- **Clause**: A fact or rule in the knowledge base
- **Database**: The collection of all clauses in the knowledge base

---

## Dynamic Database Operations

### Overview

JProlog treats the knowledge base as a dynamic database that can be modified during program execution. This is essential for:
- Learning systems that acquire new knowledge
- Temporary fact storage during computation
- State management in complex programs
- Interactive applications

---

## Assertion Predicates

### `assert/1` - Add Clause to Database

Adds a clause (fact or rule) to the knowledge base.

**Syntax:**
```prolog
assert(+Clause)
```

**Arguments:**
- `+Clause`: The clause to add (fact or rule)

**Examples:**
```prolog
% Add simple facts
?- assert(likes(mary, wine)).
true.

?- assert(parent(bob, alice)).
true.

% Add rules  
?- assert((grandparent(X, Z) :- parent(X, Y), parent(Y, Z))).
true.

% Add complex terms
?- assert(person(john, age(25), city(rome))).
true.

% Verify additions
?- likes(mary, wine).
true.

?- grandparent(bob, X).
X = alice.
```

### `asserta/1` - Add Clause at Beginning

Adds a clause at the beginning of the database (first to be found).

**Syntax:**
```prolog
asserta(+Clause)
```

**Examples:**
```prolog
% Add facts in order
?- assert(color(red)).
true.

?- assert(color(blue)).
true.

?- asserta(color(green)).
true.

% Query will find green first (added with asserta)
?- color(X).
X = green ;
X = red ;
X = blue.
```

### `assertz/1` - Add Clause at End

Adds a clause at the end of the database (last to be found).

**Syntax:**
```prolog
assertz(+Clause)
```

**Examples:**
```prolog
% Build ordered facts
?- asserta(priority(high)).
true.

?- assert(priority(medium)).
true.

?- assertz(priority(low)).
true.

% Query finds in insertion order
?- priority(X).
X = high ;
X = medium ;
X = low.
```

---

## Retraction Predicates

### `retract/1` - Remove Single Clause

Removes the first clause that matches the given pattern.

**Syntax:**
```prolog
retract(+Clause)
```

**Examples:**
```prolog
% Add some facts
?- assert(temp(data1)).
true.

?- assert(temp(data2)).
true.

?- assert(temp(data1)).
true.

% Remove first matching clause
?- retract(temp(data1)).
true.

% Verify: one temp(data1) remains
?- temp(X).
X = data2 ;
X = data1.

% Remove specific variable bindings
?- assert(process(task1, running)).
true.

?- assert(process(task2, stopped)).
true.

?- retract(process(task1, Status)).
Status = running.
```

### `retractall/1` - Remove All Matching Clauses

Removes all clauses that match the given pattern.

**Syntax:**
```prolog
retractall(+Head)
```

**Examples:**
```prolog
% Add multiple facts
?- assert(session(user1, active)).
true.

?- assert(session(user2, active)).
true.

?- assert(session(user1, expired)).
true.

% Remove all sessions for user1
?- retractall(session(user1, _)).
true.

% Verify: only user2 session remains
?- session(X, Y).
X = user2,
Y = active.

% Clean up all temporary data
?- retractall(temp(_)).
true.

?- retractall(cache(_, _)).
true.
```

### `abolish/1` - Remove All Clauses for Predicate

Removes all clauses for a specific predicate name/arity.

**Syntax:**
```prolog
abolish(+PredicateIndicator)
```

**Arguments:**
- `+PredicateIndicator`: Predicate in form `name/arity`

**Examples:**
```prolog
% Add various facts
?- assert(temp(a)).
true.

?- assert(temp(b)).
true.

?- assert(temp(c)).
true.

?- assert(cache(key1, value1)).
true.

% Remove all temp/1 clauses
?- abolish(temp/1).
true.

% Verify temp/1 is gone
?- temp(X).
false.

% But cache/2 remains
?- cache(X, Y).
X = key1,
Y = value1.
```

---

## Database Inspection

### `listing/0` - List All Clauses

Displays all clauses in the knowledge base.

**Syntax:**
```prolog
listing
```

**Examples:**
```prolog
% Add some facts and rules
?- assert(parent(alice, bob)).
true.

?- assert((grandparent(X, Z) :- parent(X, Y), parent(Y, Z))).
true.

% List everything
?- listing.
parent(alice, bob).
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
father(bob, liz).
mother(ann, bob).
% ... (shows all loaded clauses)
```

### `listing/1` - List Specific Predicate

Displays all clauses for a specific predicate.

**Syntax:**
```prolog
listing(+PredicateIndicator)
```

**Examples:**
```prolog
% List only parent/2 clauses
?- listing(parent/2).
parent(alice, bob).
parent(bob, charlie).
parent(ann, bob).

% List specific predicate
?- listing(likes/2).
likes(mary, wine).
likes(john, beer).
likes(mary, food).
```

---

## Predicate Information

### `current_predicate/1` - Check Predicate Existence

Tests whether a predicate is currently defined.

**Syntax:**
```prolog
current_predicate(?PredicateIndicator)
```

**Examples:**
```prolog
% Check if predicate exists
?- current_predicate(parent/2).
true.

?- current_predicate(undefined/1).
false.

% Find all defined predicates with arity 2
?- current_predicate(Name/2).
Name = parent ;
Name = likes ;
Name = father ;
% ... (all predicates with arity 2)

% Check built-in predicates
?- current_predicate(append/3).
true.

?- current_predicate(is/2).
true.
```

---

## Advanced Database Operations

### Transaction-Like Operations

JProlog supports pseudo-transactional operations for complex database modifications:

```prolog
% Safe database update pattern
update_user_status(User, NewStatus) :-
    % Save old status
    retract(user(User, OldStatus)),
    % Try to update
    (   validate_status(NewStatus) ->
        assert(user(User, NewStatus))
    ;   % Rollback on failure
        assert(user(User, OldStatus)),
        fail
    ).

% Batch operations
clear_expired_sessions :-
    get_time(Now),
    retractall(session(_, Time, _)),
    Time < Now - 3600.  % Remove sessions older than 1 hour
```

### Dynamic Rule Generation

Create rules programmatically:

```prolog
% Generate rules based on data
create_access_rules :-
    % For each user-role pair, create access rule
    user_role(User, Role),
    role_permission(Role, Permission),
    \+ access(User, Permission),  % Don't duplicate
    assert(access(User, Permission)),
    fail.
create_access_rules.  % Succeed after all are created

% Example data
?- assert(user_role(alice, admin)).
true.

?- assert(user_role(bob, user)).
true.

?- assert(role_permission(admin, read_all)).
true.

?- assert(role_permission(user, read_own)).
true.

% Generate access rules
?- create_access_rules.
true.

% Check generated rules
?- access(alice, X).
X = read_all.

?- access(bob, X).
X = read_own.
```

---

## Working Examples

### Example 1: Simple Cache Implementation

```prolog
% Cache management system
:- dynamic(cache/2).

% Store value in cache
cache_put(Key, Value) :-
    retractall(cache(Key, _)),  % Remove old value
    assert(cache(Key, Value)).

% Retrieve value from cache
cache_get(Key, Value) :-
    cache(Key, Value).

% Check if key exists in cache
cache_exists(Key) :-
    current_predicate(cache/2),
    cache(Key, _).

% Clear entire cache
cache_clear :-
    retractall(cache(_, _)).

% Usage example
?- cache_put(user1, john_doe).
true.

?- cache_put(session_id, abc123).
true.

?- cache_get(user1, Name).
Name = john_doe.

?- cache_exists(user1).
true.

?- cache_clear.
true.
```

### Example 2: Learning System

```prolog
% Dynamic learning system
:- dynamic(learned/2, confidence/2).

% Learn new fact with confidence
learn(Fact, Confidence) :-
    assert(learned(Fact, Confidence)),
    assert(confidence(Fact, Confidence)).

% Update confidence for existing fact
update_confidence(Fact, NewConfidence) :-
    retract(confidence(Fact, _)),
    assert(confidence(Fact, NewConfidence)).

% Forget facts below threshold
forget_uncertain(Threshold) :-
    confidence(Fact, Conf),
    Conf < Threshold,
    retract(learned(Fact, _)),
    retract(confidence(Fact, _)),
    fail.
forget_uncertain(_).

% Query with confidence
reliable_fact(Fact) :-
    learned(Fact, Conf),
    Conf >= 0.8.

% Example usage
?- learn(weather(sunny), 0.9).
true.

?- learn(temperature(25), 0.7).
true.

?- learn(will_rain, 0.3).
true.

?- reliable_fact(X).
X = weather(sunny).

?- forget_uncertain(0.5).
true.

% will_rain is now forgotten (confidence was 0.3 < 0.5)
?- learned(will_rain, _).
false.
```

### Example 3: State Machine Implementation

```prolog
% Simple state machine using dynamic predicates
:- dynamic(current_state/1, transition/3).

% Initialize state machine
init_state_machine(InitialState) :-
    retractall(current_state(_)),
    assert(current_state(InitialState)).

% Define transitions
add_transition(From, Event, To) :-
    assert(transition(From, Event, To)).

% Execute transition
execute_event(Event) :-
    current_state(CurrentState),
    transition(CurrentState, Event, NewState),
    retract(current_state(CurrentState)),
    assert(current_state(NewState)),
    format('Transitioned from ~w to ~w on event ~w~n', 
           [CurrentState, NewState, Event]).

% Get current state
get_state(State) :-
    current_state(State).

% Example: Traffic light state machine
init_traffic_light :-
    init_state_machine(red),
    add_transition(red, timer, green),
    add_transition(green, timer, yellow),
    add_transition(yellow, timer, red),
    add_transition(green, emergency, red).

% Usage
?- init_traffic_light.
true.

?- get_state(State).
State = red.

?- execute_event(timer).
Transitioned from red to green on event timer
true.

?- execute_event(emergency).
Transitioned from green to red on event emergency
true.
```

---

## Best Practices

### 1. Always Use Dynamic Declarations

```prolog
% Good: Declare dynamic predicates
:- dynamic(user_session/2, temp_data/1).

user_session(alice, session_123).
temp_data(processing).
```

### 2. Safe Retraction Pattern

```prolog
% Good: Check before retracting
safe_retract(Clause) :-
    (   retract(Clause) -> true
    ;   true  % Don't fail if clause doesn't exist
    ).

% Or use retractall for certainty
cleanup_user(User) :-
    retractall(session(User, _)),
    retractall(temp_data(User, _)).
```

### 3. Transactional Updates

```prolog
% Good: Atomic update pattern
update_counter(Name, NewValue) :-
    (   retract(counter(Name, OldValue)) ->
        assert(counter(Name, NewValue))
    ;   assert(counter(Name, NewValue))
    ).
```

### 4. Use Meaningful Predicate Names

```prolog
% Good: Clear, descriptive names
add_user_session(User, SessionId) :-
    assert(user_session(User, SessionId)).

remove_expired_sessions(Cutoff) :-
    retractall(user_session(_, Time)),
    Time < Cutoff.

% Avoid: Generic names
% add_data(X, Y) :- assert(data(X, Y)).
```

---

## Troubleshooting

### Common Issues

1. **Cannot retract built-in predicates**
   ```prolog
   ?- retract(append([], L, L)).
   ERROR: Cannot retract built-in predicate append/3
   ```

2. **Predicate not found**
   ```prolog
   ?- retract(undefined_pred(X)).
   false.  % Silently fails if predicate doesn't exist
   ```

3. **Incorrect arity in abolish**
   ```prolog
   ?- abolish(parent).  % Wrong: needs arity
   ERROR: Expected predicate_indicator

   ?- abolish(parent/2).  % Correct
   true.
   ```

### Debugging Tips

1. **Use listing to verify changes**
   ```prolog
   ?- assert(test(data)).
   true.
   
   ?- listing(test/1).
   test(data).
   ```

2. **Check predicate existence**
   ```prolog
   ?- current_predicate(my_pred/2).
   false.  % Predicate not defined
   ```

3. **Use findall to see all instances**
   ```prolog
   ?- findall(X, my_pred(X), All).
   All = [value1, value2, value3].
   ```

---

## Launch Instructions

To test knowledge base predicates in JProlog:

```bash
# Start JProlog CLI
java -cp target/classes it.denzosoft.jprolog.PrologCLI

# Or launch IDE
java -cp target/classes it.denzosoft.jprolog.editor.PrologIDE
```

---

**Next Steps**: Explore [Meta-Predicates Guide](guide-meta-predicates.md) for advanced querying capabilities, or [I/O Predicates Guide](guide-io-predicates.md) for file and stream operations.

---

*This guide is part of the JProlog documentation series. For more information, see the [User Manual](../USER_MANUAL.md) or [Quick Start Guide](guide-quick-start.md).*