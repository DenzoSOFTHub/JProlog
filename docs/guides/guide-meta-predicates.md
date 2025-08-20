# JProlog Meta-Predicates Guide

**Version**: JProlog v2.0.6  
**Last Updated**: August 2025  
**Compatibility**: All JProlog versions 2.0+

---

## Table of Contents

1. [Introduction](#introduction)
2. [Collection Predicates](#collection-predicates)
3. [Solution Finding](#solution-finding)
4. [Higher-Order Predicates](#higher-order-predicates)
5. [Control Meta-Predicates](#control-meta-predicates)
6. [Goal Manipulation](#goal-manipulation)
7. [Advanced Patterns](#advanced-patterns)
8. [Working Examples](#working-examples)
9. [Best Practices](#best-practices)
10. [Troubleshooting](#troubleshooting)

---

## Introduction

Meta-predicates are powerful constructs that operate on other predicates (goals). They enable advanced programming patterns like collecting all solutions, testing goal properties, and implementing higher-order logic. JProlog provides comprehensive support for ISO standard meta-predicates.

### Core Concepts

- **Goal**: A Prolog query or predicate call
- **Solution**: A set of variable bindings that satisfies a goal
- **Backtracking**: The process of finding alternative solutions
- **Higher-Order**: Predicates that take other predicates as arguments

---

## Collection Predicates

### `findall/3` - Find All Solutions

Collects all solutions to a goal into a list, including duplicates.

**Syntax:**
```prolog
findall(?Template, +Goal, -List)
```

**Arguments:**
- `?Template`: Pattern for collecting solutions
- `+Goal`: Goal to solve
- `-List`: Resulting list of solutions

**Examples:**
```prolog
% Collect all colors
?- findall(X, color(X), Colors).
Colors = [red, green, blue].

% Collect with template transformation
?- findall(person(Name), likes(Name, wine), WineLovers).
WineLovers = [person(mary), person(john)].

% Collect complex structures
?- findall([User, Item], likes(User, Item), Preferences).
Preferences = [[mary, wine], [mary, food], [john, wine]].

% Empty result when no solutions
?- findall(X, impossible(X), Result).
Result = [].

% Duplicates are preserved
?- findall(X, (member(X, [1,1,2,2,3]), X < 3), Small).
Small = [1, 1, 2, 2].
```

### `bagof/3` - Find Solutions by Group

Collects solutions grouped by free variables, preserving duplicates.

**Syntax:**
```prolog
bagof(?Template, +Goal, -List)
```

**Examples:**
```prolog
% Group solutions by free variables
?- bagof(Item, likes(mary, Item), MaryLikes).
MaryLikes = [wine, food].

?- bagof(Item, likes(john, Item), JohnLikes).
JohnLikes = [wine, beer].

% Multiple solutions for different bindings
?- bagof(Item, Person^likes(Person, Item), Items).
Items = [wine, food] ;
Items = [wine, beer].

% With existential quantification (^)
?- bagof(X, Person^Age^person(Person, Age, X), Cities).
Cities = [rome, milan, florence].

% Fails if no solutions
?- bagof(X, impossible(X), Result).
false.
```

### `setof/3` - Find Unique Solutions

Like bagof/3 but removes duplicates and sorts results.

**Syntax:**
```prolog
setof(?Template, +Goal, -List)
```

**Examples:**
```prolog
% Unique sorted results
?- setof(X, color(X), UniqueColors).
UniqueColors = [blue, green, red].

% Remove duplicates from complex data
?- setof(City, Person^Age^person(Person, Age, City), Cities).
Cities = [florence, milan, rome].

% Sorting with custom structures
?- setof(person(Name, Age), person(Name, Age, _), People).
People = [person(alice, 25), person(bob, 30), person(charlie, 28)].

% Different groups for different variable bindings
?- setof(Item, likes(Person, Item), Items).
Person = john,
Items = [beer, wine] ;
Person = mary,
Items = [food, wine].
```

---

## Solution Finding

### `once/1` - Find First Solution Only

Succeeds at most once, cutting off backtracking after first solution.

**Syntax:**
```prolog
once(+Goal)
```

**Examples:**
```prolog
% Get only first solution
?- once(color(X)).
X = red.

% Prevent backtracking in complex goals
?- once((member(X, [1,2,3,4,5]), X > 3)).
X = 4.

% Useful in deterministic contexts
first_parent(Child, Parent) :-
    once(parent(Parent, Child)).

?- first_parent(bob, P).
P = alice.  % Only first parent found
```

### `forall/2` - Test All Solutions

Succeeds if Condition implies Action for all solutions.

**Syntax:**
```prolog
forall(+Condition, +Action)
```

**Examples:**
```prolog
% Verify property for all instances
?- forall(person(Name, Age, _), Age >= 18).
true.  % All persons are adults

?- forall(color(C), atom(C)).
true.  % All colors are atoms

% Check constraints
?- forall(employee(Name, Salary), Salary >= 30000).
false.  % Some employee earns less

% Combined with side effects
validate_all_users :-
    forall(user(Name), (
        validate_user(Name),
        format('User ~w validated~n', [Name])
    )).
```

---

## Higher-Order Predicates

### `call/1` - Execute Goal Dynamically

Executes a goal constructed at runtime.

**Syntax:**
```prolog
call(+Goal)
```

**Examples:**
```prolog
% Simple goal execution
?- Goal = likes(mary, wine), call(Goal).
Goal = likes(mary, wine).

% Dynamic predicate construction
?- Predicate = color, Arg = red, call(Predicate(Arg)).
Predicate = color,
Arg = red.

% Conditional execution
execute_if_valid(Goal) :-
    (   valid_goal(Goal) ->
        call(Goal)
    ;   format('Invalid goal: ~w~n', [Goal])
    ).
```

### `call/2, call/3, call/N` - Call with Additional Arguments

Executes a goal with additional arguments appended.

**Syntax:**
```prolog
call(+Goal, +Arg1)
call(+Goal, +Arg1, +Arg2)
% ... up to call/N
```

**Examples:**
```prolog
% Call with one extra argument
?- call(likes(mary), wine).
true.  % Equivalent to likes(mary, wine)

% Call with multiple arguments
?- call(between, 1, 10, 5).
true.  % Equivalent to between(1, 10, 5)

% Higher-order predicate implementation
map_predicate(_, [], []).
map_predicate(Pred, [H|T], [H2|T2]) :-
    call(Pred, H, H2),
    map_predicate(Pred, T, T2).

% Usage: double all numbers
double(X, Y) :- Y is X * 2.

?- map_predicate(double, [1,2,3], Result).
Result = [2, 4, 6].
```

---

## Control Meta-Predicates

### `not/1` and `\+/1` - Negation as Failure

Succeeds if Goal fails (negation by failure).

**Syntax:**
```prolog
not(+Goal)
\+(+Goal)  % ISO standard form
```

**Examples:**
```prolog
% Simple negation
?- \+ color(purple).
true.

?- not(likes(bob, vegetables)).
true.

% Complex negation
?- \+ (color(X), X = purple).
true.

% Guard conditions
safe_divide(X, Y, Result) :-
    \+ Y =:= 0,  % Y is not zero
    Result is X / Y.

?- safe_divide(10, 2, R).
R = 5.0.

?- safe_divide(10, 0, R).
false.
```

### `if_then_else/3` - Conditional Execution

Executes different goals based on condition.

**Syntax:**
```prolog
(+If -> +Then ; +Else)
```

**Examples:**
```prolog
% Simple conditional
?- (5 > 3 -> write(greater) ; write(less_or_equal)).
greater
true.

% Variable binding conditional
check_age(Person, Status) :-
    person(Person, Age),
    (   Age >= 18 ->
        Status = adult
    ;   Status = minor
    ).

% Nested conditionals
categorize_number(N, Category) :-
    (   N > 0 ->
        (   N > 100 ->
            Category = large_positive
        ;   Category = small_positive
        )
    ;   N < 0 ->
        Category = negative
    ;   Category = zero
    ).

?- categorize_number(150, Cat).
Cat = large_positive.
```

---

## Goal Manipulation

### `copy_term/2` - Copy Term with Fresh Variables

Creates a copy of a term with fresh variables.

**Syntax:**
```prolog
copy_term(+Term, -Copy)
```

**Examples:**
```prolog
% Copy simple terms
?- copy_term(f(X, X), Copy).
Copy = f(_G123, _G123).  % Fresh variables, but still shared

% Copy complex structures
?- copy_term([X, Y, f(X, Y)], Copy).
Copy = [_G123, _G124, f(_G123, _G124)].

% Use in rule templates
rule_template(likes(Person, Item), person(Person), item(Item)).

create_rule(Person, Item, Rule) :-
    copy_term(rule_template(likes(P, I), person(P), item(I)), 
              rule_template(likes(Person, Item), person(Person), item(Item))).
```

### `functor/3` - Term Structure Analysis

Relates a term to its functor name and arity.

**Syntax:**
```prolog
functor(?Term, ?Name, ?Arity)
```

**Examples:**
```prolog
% Decompose compound term
?- functor(person(john, 25, rome), Name, Arity).
Name = person,
Arity = 3.

% Create term from functor
?- functor(Term, likes, 2).
Term = likes(_G123, _G124).

% Check term structure
is_binary_compound(Term) :-
    functor(Term, _, 2).

?- is_binary_compound(f(a, b)).
true.

?- is_binary_compound(atom).
false.
```

### `arg/3` - Access Term Arguments

Extracts the Nth argument from a compound term.

**Syntax:**
```prolog
arg(+N, +Term, ?Arg)
```

**Examples:**
```prolog
% Extract specific arguments
?- arg(2, person(john, 25, rome), Age).
Age = 25.

?- arg(1, f(a, b, c), First).
First = a.

% Process all arguments
process_args(Term) :-
    functor(Term, _, Arity),
    between(1, Arity, N),
    arg(N, Term, Arg),
    format('Arg ~w: ~w~n', [N, Arg]),
    fail.
process_args(_).

?- process_args(person(alice, 30, milan)).
Arg 1: alice
Arg 2: 30
Arg 3: milan
true.
```

---

## Advanced Patterns

### Meta-Predicate Implementation Pattern

```prolog
% Generic filter implementation
filter(_, [], []).
filter(Condition, [H|T], [H|Filtered]) :-
    call(Condition, H), !,
    filter(Condition, T, Filtered).
filter(Condition, [_|T], Filtered) :-
    filter(Condition, T, Filtered).

% Usage predicates
is_positive(X) :- X > 0.
is_atom(X) :- atom(X).

% Examples
?- filter(is_positive, [-1, 2, -3, 4], Positive).
Positive = [2, 4].

?- filter(is_atom, [hello, 123, world, 456], Atoms).
Atoms = [hello, world].
```

### Solution Counting Pattern

```prolog
% Count solutions to a goal
count_solutions(Goal, Count) :-
    findall(1, Goal, Ones),
    length(Ones, Count).

% Specific counters
count_likes(Person, Count) :-
    count_solutions(likes(Person, _), Count).

?- count_likes(mary, Count).
Count = 2.

% Alternative using aggregate (if available)
count_colors(Count) :-
    aggregate_all(count, color(_), Count).
```

### Dynamic Goal Construction

```prolog
% Build complex queries dynamically
build_person_query(Name, Age, City, Query) :-
    Query = (person(Name, Age, City), Age >= 18, atom(City)).

find_adult_in_city(City, People) :-
    findall(person(N, A), 
           (build_person_query(N, A, City, Query), call(Query)),
           People).

% Parametric queries
create_filter(Property, Value, Filter) :-
    Filter =.. [Property, Value].

?- create_filter(color, red, Filter), call(Filter).
Filter = color(red).
```

---

## Working Examples

### Example 1: Data Analysis System

```prolog
% Comprehensive data analysis using meta-predicates
:- dynamic(sale/4).  % sale(Date, Product, Amount, Customer)

% Sample data
sale(20250101, laptop, 1200, alice).
sale(20250102, mouse, 25, bob).
sale(20250102, laptop, 1200, charlie).
sale(20250103, keyboard, 80, alice).

% Analysis predicates
total_sales(Total) :-
    findall(Amount, sale(_, _, Amount, _), Amounts),
    sum_list(Amounts, Total).

sales_by_customer(Customer, Sales) :-
    findall(Amount, sale(_, _, Amount, Customer), Amounts),
    sum_list(Amounts, Sales).

top_customers(N, Customers) :-
    findall(Total-Customer, 
           (sale(_, _, _, Customer), sales_by_customer(Customer, Total)),
           CustomerTotals),
    setof(Total-Customer, member(Total-Customer, CustomerTotals), Sorted),
    reverse(Sorted, Desc),
    take_n(N, Desc, Customers).

% Helper: take first N elements
take_n(0, _, []) :- !.
take_n(_, [], []) :- !.
take_n(N, [H|T], [H|Rest]) :-
    N > 0,
    N1 is N - 1,
    take_n(N1, T, Rest).

% Usage
?- total_sales(Total).
Total = 2505.

?- sales_by_customer(alice, AliceSales).
AliceSales = 1280.

?- top_customers(2, Top).
Top = [1280-alice, 1200-charlie].
```

### Example 2: Rule Validation System

```prolog
% Meta-predicate based rule validation
:- dynamic(business_rule/3).  % business_rule(Name, Condition, Action)

% Define business rules
business_rule(age_check, 
             person(_, Age, _), 
             (Age >= 18 -> true ; (write('Age violation'), fail))).

business_rule(salary_minimum,
             employee(_, Salary),
             (Salary >= 30000 -> true ; (write('Salary too low'), fail))).

% Validation engine
validate_all_rules :-
    forall(business_rule(Name, Condition, Action),
           validate_rule(Name, Condition, Action)).

validate_rule(Name, Condition, Action) :-
    format('Validating rule: ~w~n', [Name]),
    forall(call(Condition), 
           (call(Action) -> 
               format('  ✓ Passed for ~w~n', [Condition])
           ;   format('  ✗ Failed for ~w~n', [Condition])
           )).

% Test data
person(alice, 25, rome).
person(bob, 17, milan).
employee(alice, 45000).
employee(bob, 25000).

% Usage
?- validate_all_rules.
Validating rule: age_check
  ✓ Passed for person(alice, 25, rome)
  Age violation✗ Failed for person(bob, 17, milan)
Validating rule: salary_minimum
  ✓ Passed for employee(alice, 45000)
  Salary too low✗ Failed for employee(bob, 25000)
true.
```

### Example 3: Query Builder System

```prolog
% Dynamic query construction system
build_query(Entity, Conditions, Query) :-
    build_base_query(Entity, BaseQuery),
    add_conditions(BaseQuery, Conditions, Query).

build_base_query(person, person(Name, Age, City)).
build_base_query(employee, employee(Name, Salary)).

add_conditions(Query, [], Query).
add_conditions(Query, [Condition|Rest], FinalQuery) :-
    combine_conditions(Query, Condition, NewQuery),
    add_conditions(NewQuery, Rest, FinalQuery).

combine_conditions(Query, Condition, (Query, Condition)).

% Query execution with results
execute_query(QuerySpec, Results) :-
    build_query(QuerySpec, [], Query),
    findall(Query, call(Query), Results).

execute_query(Entity, Conditions, Results) :-
    build_query(Entity, Conditions, Query),
    findall(Query, call(Query), Results).

% Usage examples
?- execute_query(person, [Age >= 18], Adults).
Adults = [person(alice, 25, rome), person(charlie, 30, milan)].

?- execute_query(employee, [Salary > 40000], HighEarners).
HighEarners = [employee(alice, 45000)].
```

---

## Best Practices

### 1. Choose Right Collection Predicate

```prolog
% Use findall/3 for all solutions including duplicates
?- findall(X, member(X, [1,2,2,3]), All).
All = [1, 2, 2, 3].

% Use setof/3 for unique sorted solutions
?- setof(X, member(X, [1,2,2,3]), Unique).
Unique = [1, 2, 3].

% Use bagof/3 when you need failure for no solutions
?- bagof(X, impossible(X), _).
false.  % Fails, unlike findall which returns []
```

### 2. Use Existential Quantification Properly

```prolog
% Wrong: Person will be bound
?- bagof(Item, likes(Person, Item), Items).
Person = mary, Items = [wine, food] ;
Person = john, Items = [wine, beer].

% Right: Use ^ to ignore Person binding
?- bagof(Item, Person^likes(Person, Item), Items).
Items = [wine, food, wine, beer].
```

### 3. Safe Goal Execution

```prolog
% Good: Check goal validity before calling
safe_call(Goal) :-
    (   callable(Goal) ->
        call(Goal)
    ;   format('Error: ~w is not callable~n', [Goal]),
        fail
    ).
```

### 4. Efficient Solution Collection

```prolog
% Efficient: Use specific templates
collect_names(Names) :-
    findall(Name, person(Name, _, _), Names).

% Inefficient: Collect full terms then extract
% collect_names(Names) :-
%     findall(person(Name, Age, City), person(Name, Age, City), People),
%     extract_names(People, Names).
```

---

## Troubleshooting

### Common Issues

1. **Variable scoping in meta-predicates**
   ```prolog
   % Wrong: X is not properly scoped
   ?- findall(X, (color(Y), X = Y), Colors).
   
   % Right: Use Y directly in template
   ?- findall(Y, color(Y), Colors).
   ```

2. **bagof/3 vs findall/3 behavior**
   ```prolog
   % bagof fails if no solutions
   ?- bagof(X, impossible(X), List).
   false.
   
   % findall returns empty list
   ?- findall(X, impossible(X), List).
   List = [].
   ```

3. **Existential quantification confusion**
   ```prolog
   % This creates separate solution groups
   ?- setof(Item, likes(Person, Item), Items).
   
   % This collects all items regardless of person
   ?- setof(Item, Person^likes(Person, Item), Items).
   ```

### Debugging Tips

1. **Test goals separately**
   ```prolog
   % First verify the goal works
   ?- likes(mary, wine).
   true.
   
   % Then use in meta-predicate
   ?- findall(X, likes(mary, X), Items).
   ```

2. **Use trace for complex meta-predicates**
   ```prolog
   ?- trace, forall(person(Name, Age, _), Age >= 18).
   ```

3. **Check template variables**
   ```prolog
   % Make sure template variables appear in goal
   ?- findall(X, likes(mary, Y), Items).  % Wrong: X not in goal
   Items = [_, _].  % Gets unbound variables
   
   ?- findall(Y, likes(mary, Y), Items).  % Right
   Items = [wine, food].
   ```

---

## Launch Instructions

To test meta-predicates in JProlog:

```bash
# Start JProlog CLI
java -cp target/classes it.denzosoft.jprolog.PrologCLI

# Or launch IDE
java -cp target/classes it.denzosoft.jprolog.editor.PrologIDE
```

---

**Next Steps**: Explore [I/O Predicates Guide](guide-io-predicates.md) for file operations, or [Knowledge Base Guide](guide-knowledge-base.md) for database manipulation.

---

*This guide is part of the JProlog documentation series. For more information, see the [User Manual](../USER_MANUAL.md) or [Quick Start Guide](guide-quick-start.md).*