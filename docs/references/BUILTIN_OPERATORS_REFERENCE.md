# JProlog Built-in Operators Reference

**Version**: JProlog v2.0.15  
**Last Updated**: 2025-08-20  
**Total Operators**: 25+ operators with comprehensive precedence rules

This reference guide organizes JProlog's built-in operators by their function and precedence. Each section includes explanations suitable for users new to Prolog, with detailed examples showing practical applications.

---

## Table of Contents

1. [Understanding Operator Precedence](#1-understanding-operator-precedence)
2. [Unification Operators](#2-unification-operators)
3. [Arithmetic Comparison Operators](#3-arithmetic-comparison-operators)
4. [Term Comparison Operators](#4-term-comparison-operators)
5. [Arithmetic Operators](#5-arithmetic-operators)
6. [Logical Control Operators](#6-logical-control-operators)
7. [List Operators](#7-list-operators)
8. [Special Purpose Operators](#8-special-purpose-operators)

---

## 1. Understanding Operator Precedence

In Prolog, operators have precedence levels (1-1200) that determine evaluation order. Lower numbers bind more tightly.

### Precedence Levels Overview
```
1200: ;  (semicolon - disjunction/or)
1100: -> (if-then)
1000: , (comma - conjunction/and)
 700: =, \=, ==, \==, @<, @=<, @>, @>=, =:=, =\=, <, =<, >, >=
 500: +, - (arithmetic)
 400: *, /, //, rem, mod, **
 200: ^ (exponentiation)
   1: Atoms and numbers
```

### Associativity
- **Left-associative**: `a op b op c` = `(a op b) op c`
- **Right-associative**: `a op b op c` = `a op (b op c)`
- **Non-associative**: `a op b op c` is an error

**Why This Matters**: Understanding precedence helps you write correct expressions and avoid unexpected behavior.

```prolog
% Without parentheses:
?- X is 2 + 3 * 4.
X = 14.  % Evaluated as: 2 + (3 * 4)

% With explicit parentheses:
?- X is (2 + 3) * 4.
X = 20.  % Evaluated as: (2 + 3) * 4

% Comparison precedence:
?- 2 + 3 > 4.
true.  % Evaluated as: (2 + 3) > 4, then 5 > 4

% Logical precedence:
?- member(X, [1, 2, 3]), X > 1.
X = 2 ;
X = 3.  % Conjunction has lower precedence than comparison
```

---

## 2. Unification Operators

Unification is the core mechanism in Prolog for matching and binding variables.

### =/2 (Unification)
**Purpose**: Unifies two terms (makes them identical).

**Precedence**: 700 (yfx - left-associative)

**When to use**: Use for pattern matching, variable binding, or testing term equality.

```prolog
% Basic unification
?- X = 5.
X = 5.

?- X = Y, Y = hello.
X = Y, Y = hello.

% Unifying structures
?- person(Name, Age) = person(john, 25).
Name = john, Age = 25.

% Unification with complex terms
?- [H|T] = [first, second, third].
H = first, T = [second, third].

% Practical example: Extract data from structures
process_command(Command) :-
    Command = cmd(Action, Args),
    write('Action: '), write(Action), nl,
    write('Arguments: '), write(Args), nl.

?- process_command(cmd(save, [file1, file2])).
Action: save
Arguments: [file1, file2]
true.

% Pattern matching in rules
handle_result(success(Data)) :-
    write('Operation succeeded with: '), write(Data), nl.
handle_result(error(Code, Message)) :-
    write('Error '), write(Code), write(': '), write(Message), nl.
handle_result(warning(Message)) :-
    write('Warning: '), write(Message), nl.

?- handle_result(error(404, 'Not found')).
Error 404: Not found
true.

% Important: Unification can fail
?- 5 = 10.
false.

?- person(john, X) = person(mary, 25).
false.  % john doesn't unify with mary

% Occurs check (detecting infinite structures)
?- X = f(X).
X = f(f(f(f(f(f(f(f(f(f(...)))))))))))).
% Creates infinite structure - some Prolog systems detect this
```

### \=/2 (Non-unification)
**Purpose**: Succeeds if two terms cannot be unified.

**Precedence**: 700 (yfx - left-associative)

**When to use**: Use to test that terms are different or to add constraints.

```prolog
% Basic non-unification
?- 5 \= 10.
true.

?- X \= Y.
true.  % Different unbound variables

?- john \= mary.
true.

% Using in rules for constraints
different_person(person(N1, A1), person(N2, A2)) :-
    N1 \= N2.  % Names must be different

?- different_person(person(john, 25), person(mary, 30)).
true.

?- different_person(person(john, 25), person(john, 30)).
false.  % Same name

% Practical example: Unique elements constraint
all_different([]).
all_different([H|T]) :-
    all_different_from(H, T),
    all_different(T).

all_different_from(_, []).
all_different_from(X, [H|T]) :-
    X \= H,
    all_different_from(X, T).

?- all_different([a, b, c]).
true.

?- all_different([a, b, a]).
false.

% Warning: \=/2 doesn't instantiate variables
?- X \= 5, X = 5.
false.  % The constraint X \= 5 is remembered

?- X = 5, X \= 5.
false.  % Direct contradiction
```

### ==/2 (Term Identity)
**Purpose**: Tests if two terms are identical without unification.

**Precedence**: 700 (yfx - left-associative)

**When to use**: Use to test if terms are exactly the same without variable binding.

```prolog
% Basic identity test
?- 5 == 5.
true.

?- john == john.
true.

% Variables must be the same variable
?- X == X.
true.

?- X == Y.
false.  % Different variables

% After unification
?- X = 5, Y = 5, X == Y.
X = 5, Y = 5.
true.  % Both bound to same value

% Structures must be identical
?- f(a, b) == f(a, b).
true.

?- f(X, Y) == f(X, Y).
true.  % Same term with same variables

?- f(X, Y) == f(A, B).
false.  % Different variable names

% Practical example: Check if processing needed
process_if_different(Old, New, Result) :-
    (   Old == New
    ->  Result = no_change
    ;   expensive_process(Old, New, Result)
    ).

% Optimization: avoid recomputation
cached_result(Input, Result) :-
    (   cache(Input, Cached),
        Cached == Input  % Exact same input
    ->  Result = Cached
    ;   compute_result(Input, Result),
        assertz(cache(Input, Result))
    ).
```

### \\==/2 (Term Non-identity)
**Purpose**: Tests if two terms are not identical.

**Precedence**: 700 (yfx - left-associative)

**When to use**: Use to test that terms are different without attempting unification.

```prolog
% Basic non-identity
?- 5 \== 10.
true.

?- X \== Y.
true.  % Different variables

?- john \== mary.
true.

% After partial binding
?- X = 5, X \== 10.
X = 5.
true.

% Practical example: Detect changes
has_changed(Old, New) :-
    Old \== New.

monitor_value(Key, NewValue) :-
    (   current_value(Key, OldValue),
        has_changed(OldValue, NewValue)
    ->  write('Value changed for '), write(Key), nl,
        retract(current_value(Key, OldValue)),
        assertz(current_value(Key, NewValue))
    ;   write('No change for '), write(Key), nl
    ).

% Difference from \=/2:
?- X \== 5.
true.   % Different terms (unbound variable vs number)

?- X \= 5.
true.   % Cannot unify (but constraint is remembered)

?- X \== 5, X = 5.
X = 5.  % \== doesn't prevent later unification

?- X \= 5, X = 5.
false.  % \= creates constraint that prevents this
```

---

## 3. Arithmetic Comparison Operators

These operators compare numeric values after evaluating arithmetic expressions.

### =:=/2 (Arithmetic Equality)
**Purpose**: Tests if two arithmetic expressions evaluate to the same number.

**Precedence**: 700 (yfx - left-associative)

**When to use**: Use to compare numeric values or check arithmetic equations.

```prolog
% Basic arithmetic comparison
?- 2 + 3 =:= 5.
true.

?- 10 - 3 =:= 7.
true.

?- 2 * 3 =:= 6.
true.

% Variables must be bound to numbers
?- X = 5, Y = 2 + 3, X =:= Y.
X = 5, Y = 2+3.
true.

% Different from unification
?- 2 + 3 = 5.
false.  % Term 2+3 doesn't unify with 5

?- 2 + 3 =:= 5.
true.   % Expressions evaluate to same number

% Practical example: Validate calculations
check_calculation(Expression, Expected) :-
    (   Expression =:= Expected
    ->  write('Calculation correct: '), write(Expression), 
        write(' = '), write(Expected), nl
    ;   write('Calculation error: '), write(Expression),
        write(' should equal '), write(Expected), nl
    ).

?- check_calculation(2 * 3 + 4, 10).
Calculation correct: 2*3+4 = 10
true.

% Mathematical identities
test_identity(sin(X)**2 + cos(X)**2) :-
    X = 1.5,  % Example value
    Result is sin(X)**2 + cos(X)**2,
    (   Result =:= 1.0
    ->  writeln('Trigonometric identity verified')
    ;   writeln('Identity failed (floating point precision?)')
    ).

% Floating point comparison (be careful!)
?- 0.1 + 0.2 =:= 0.3.
false.  % Floating point precision issue!

% Better floating point comparison
approximately_equal(X, Y, Tolerance) :-
    Diff is abs(X - Y),
    Diff =< Tolerance.

?- approximately_equal(0.1 + 0.2, 0.3, 0.0001).
true.
```

### =\\=/2 (Arithmetic Inequality)
**Purpose**: Tests if two arithmetic expressions evaluate to different numbers.

**Precedence**: 700 (yfx - left-associative)

**When to use**: Use to verify that numbers are different.

```prolog
% Basic inequality
?- 2 + 3 =\= 6.
true.

?- 5 =\= 10.
true.

?- 4 + 1 =\= 5.
false.  % Both sides equal 5

% Practical example: Input validation
validate_different_values(X, Y) :-
    (   X =\= Y
    ->  true
    ;   write('Error: Values must be different'), nl,
        fail
    ).

% Check non-zero values
is_nonzero(X) :-
    X =\= 0.

safe_divide(X, Y, Result) :-
    is_nonzero(Y),
    Result is X / Y.

?- safe_divide(10, 0, R).
false.

?- safe_divide(10, 2, R).
R = 5.0.

% Mathematical constraints
different_roots(A, B, C, Root1, Root2) :-
    % Quadratic formula: (-B ± sqrt(B² - 4AC)) / 2A
    Discriminant is B * B - 4 * A * C,
    Discriminant > 0,  % Real roots exist
    SqrtDisc is sqrt(Discriminant),
    Root1 is (-B + SqrtDisc) / (2 * A),
    Root2 is (-B - SqrtDisc) / (2 * A),
    Root1 =\= Root2.  % Ensure roots are different
```

### </2, =</2, >/2, >=/2 (Arithmetic Ordering)
**Purpose**: Compare numeric values for ordering.

**Precedence**: 700 (yfx - left-associative)

**When to use**: Use for range checking, sorting, or conditional logic.

```prolog
% Basic comparisons
?- 5 < 10.
true.

?- 3 + 2 =< 5.
true.

?- 10 > 7.
true.

?- 4 + 1 >= 5.
true.

% Range checking
in_range(Value, Min, Max) :-
    Value >= Min,
    Value =< Max.

?- in_range(5, 1, 10).
true.

?- in_range(15, 1, 10).
false.

% Practical example: Grade boundaries
assign_grade(Score, Grade) :-
    (   Score >= 90 -> Grade = 'A'
    ;   Score >= 80 -> Grade = 'B'
    ;   Score >= 70 -> Grade = 'C'
    ;   Score >= 60 -> Grade = 'D'
    ;   Grade = 'F'
    ).

?- assign_grade(85, G).
G = 'B'.

% Find minimum and maximum
min_max(X, Y, Min, Max) :-
    (   X < Y
    ->  Min = X, Max = Y
    ;   Min = Y, Max = X
    ).

?- min_max(7, 3, Min, Max).
Min = 3, Max = 7.

% Sort three numbers
sort_three(A, B, C, Sorted) :-
    (   A =< B, B =< C -> Sorted = [A, B, C]
    ;   A =< C, C =< B -> Sorted = [A, C, B]
    ;   B =< A, A =< C -> Sorted = [B, A, C]
    ;   B =< C, C =< A -> Sorted = [B, C, A]
    ;   C =< A, A =< B -> Sorted = [C, A, B]
    ;   Sorted = [C, B, A]
    ).

?- sort_three(5, 2, 8, S).
S = [2, 5, 8].

% Age categories
age_category(Age, Category) :-
    (   Age < 13 -> Category = child
    ;   Age < 20 -> Category = teenager
    ;   Age < 65 -> Category = adult
    ;   Category = senior
    ).

% Temperature warnings
temperature_warning(Temp, Warning) :-
    (   Temp < 0
    ->  Warning = freezing
    ;   Temp > 35
    ->  Warning = hot
    ;   Warning = normal
    ).
```

---

## 4. Term Comparison Operators

These operators compare terms using the standard Prolog term ordering without arithmetic evaluation.

### Standard Term Ordering
Variables < Numbers < Atoms < Compound Terms

Within each category:
- **Variables**: By age (older variables are "larger")
- **Numbers**: By numeric value
- **Atoms**: Alphabetically
- **Compound Terms**: By arity, then functor name, then arguments

### @</2, @=</2, @>/2, @>=/2 (Term Ordering)
**Purpose**: Compare terms using standard term ordering.

**Precedence**: 700 (yfx - left-associative)

**When to use**: Use for generic sorting, term comparison without arithmetic evaluation.

```prolog
% Variables vs other terms
?- X @< 5.
true.  % Variables come first in ordering

?- 5 @< atom.
true.  % Numbers before atoms

?- atom @< compound(term).
true.  % Atoms before compound terms

% Numeric comparison (different from arithmetic)
?- 2 + 3 @< 10.
true.  % Term "2+3" comes before term "10" alphabetically

?- 2 + 3 < 10.
true.  % Arithmetic: 5 < 10

% Atom comparison (alphabetical)
?- apple @< banana.
true.

?- zebra @< apple.
false.

% Compound term comparison
?- f(1) @< f(2).
true.  % Same functor, compare arguments

?- f(1) @< g(1).
true.  % Different functors: f < g alphabetically

?- f(a, b) @< f(a, c).
true.  % First args same, second: b < c

% Practical example: Generic sorting
generic_sort(List, Sorted) :-
    predsort(compare_standard, List, Sorted).

compare_standard(Order, Term1, Term2) :-
    (   Term1 @< Term2 -> Order = (<)
    ;   Term1 @> Term2 -> Order = (>)
    ;   Order = (=)
    ).

?- generic_sort([zebra, apple, 123, f(2), f(1), X], S).
S = [X, 123, apple, zebra, f(1), f(2)].

% Database key sorting
sort_records(Records, SortedRecords) :-
    keysort(Records, SortedRecords).

% Create sortable key-value pairs
create_sortable(Data, Key-Data) :-
    extract_sort_key(Data, Key).

extract_sort_key(person(Name, Age, _), Name-Age).
extract_sort_key(product(Name, Price), Name-Price).

% Mixed data type handling
classify_and_sort(Items, Classified) :-
    partition_by_type(Items, Vars, Numbers, Atoms, Compounds),
    append([Vars, Numbers, Atoms, Compounds], Classified).

partition_by_type([], [], [], [], []).
partition_by_type([H|T], [H|Vars], Nums, Atoms, Comps) :-
    var(H), !,
    partition_by_type(T, Vars, Nums, Atoms, Comps).
partition_by_type([H|T], Vars, [H|Nums], Atoms, Comps) :-
    number(H), !,
    partition_by_type(T, Vars, Nums, Atoms, Comps).
partition_by_type([H|T], Vars, Nums, [H|Atoms], Comps) :-
    atom(H), !,
    partition_by_type(T, Vars, Nums, Atoms, Comps).
partition_by_type([H|T], Vars, Nums, Atoms, [H|Comps]) :-
    partition_by_type(T, Vars, Nums, Atoms, Comps).
```

---

## 5. Arithmetic Operators

Arithmetic operators perform mathematical calculations.

### +/2, -/2 (Addition, Subtraction)
**Purpose**: Basic arithmetic operations.

**Precedence**: 500 (yfx - left-associative)

**When to use**: Use for mathematical calculations within `is/2` or arithmetic comparisons.

```prolog
% Basic arithmetic (within is/2)
?- X is 5 + 3.
X = 8.

?- Y is 10 - 4.
Y = 6.

% Complex expressions
?- Result is 2 + 3 * 4 - 1.
Result = 13.  % Evaluated as: 2 + (3 * 4) - 1 = 2 + 12 - 1

% With variables
?- A = 5, B = 3, Sum is A + B.
A = 5, B = 3, Sum = 8.

% Practical example: Calculate total cost
calculate_total(Items, Tax, Total) :-
    sum_prices(Items, Subtotal),
    Total is Subtotal + Subtotal * Tax.

sum_prices([], 0).
sum_prices([Price|Rest], Total) :-
    sum_prices(Rest, RestTotal),
    Total is Price + RestTotal.

?- calculate_total([10, 20, 30], 0.1, Total).
Total = 66.0.  % (10+20+30) + (10+20+30)*0.1

% Distance calculation
distance_2d(X1, Y1, X2, Y2, Distance) :-
    DX is X2 - X1,
    DY is Y2 - Y1,
    Distance is sqrt(DX * DX + DY * DY).

?- distance_2d(0, 0, 3, 4, D).
D = 5.0.

% Compound interest
compound_interest(Principal, Rate, Years, Amount) :-
    Amount is Principal * (1 + Rate) ** Years.

% Important: Operators don't evaluate outside arithmetic contexts
?- X = 2 + 3.
X = 2+3.  % Creates term structure, doesn't calculate

?- X = 2 + 3, Y is X.
X = 2+3, Y = 5.  % is/2 evaluates the expression
```

### */2, //2, /2 (Multiplication, Integer Division, Float Division)
**Purpose**: Multiplication and division operations.

**Precedence**: 400 (yfx - left-associative)

**When to use**: Use for calculations involving products and quotients.

```prolog
% Multiplication
?- X is 6 * 7.
X = 42.

% Float division
?- X is 7 / 2.
X = 3.5.

% Integer division
?- X is 7 // 2.
X = 3.

% Comparison of division types
?- A is 7 / 2, B is 7 // 2.
A = 3.5, B = 3.

% Practical example: Unit conversion
celsius_to_fahrenheit(C, F) :-
    F is C * 9 / 5 + 32.

fahrenheit_to_celsius(F, C) :-
    C is (F - 32) * 5 / 9.

?- celsius_to_fahrenheit(100, F).
F = 212.0.

% Area calculations
circle_area(Radius, Area) :-
    Area is pi * Radius * Radius.

rectangle_area(Width, Height, Area) :-
    Area is Width * Height.

triangle_area(Base, Height, Area) :-
    Area is Base * Height / 2.

% Percentage calculations
percentage(Part, Whole, Percent) :-
    Percent is Part / Whole * 100.

?- percentage(25, 200, P).
P = 12.5.

% Average calculation
average(Numbers, Avg) :-
    sum_list(Numbers, Total),
    length(Numbers, Count),
    Avg is Total / Count.

?- average([10, 20, 30, 40], A).
A = 25.0.

% Integer division for grouping
divide_into_groups(Total, GroupSize, FullGroups, Remainder) :-
    FullGroups is Total // GroupSize,
    Remainder is Total mod GroupSize.

?- divide_into_groups(23, 5, Groups, Rest).
Groups = 4, Rest = 3.  % 4 groups of 5, plus 3 remaining
```

### mod/2, rem/2 (Modulo Operations)
**Purpose**: Remainder operations.

**Precedence**: 400 (yfx - left-associative)

**When to use**: Use for cyclic operations, divisibility tests, or extracting digits.

```prolog
% Basic modulo
?- X is 17 mod 5.
X = 2.

?- X is 17 rem 5.
X = 2.

% Difference between mod and rem (with negative numbers)
?- A is -17 mod 5, B is -17 rem 5.
A = 3, B = -2.  % mod result has sign of divisor, rem has sign of dividend

% Test if even or odd
is_even(N) :- 0 is N mod 2.
is_odd(N) :- 1 is N mod 2.

?- is_even(42).
true.

?- is_odd(17).
true.

% Practical example: Cyclic operations
day_of_week(DayNumber, DayName) :-
    Index is DayNumber mod 7,
    nth0(Index, [sunday, monday, tuesday, wednesday, thursday, friday, saturday], DayName).

?- day_of_week(10, Day).
Day = wednesday.  % Day 10 mod 7 = 3 -> wednesday

% Extract digits
extract_digits(Number, Digits) :-
    extract_digits(Number, [], Digits).

extract_digits(0, Acc, Acc) :- !.
extract_digits(N, Acc, Digits) :-
    Digit is N mod 10,
    N1 is N // 10,
    extract_digits(N1, [Digit|Acc], Digits).

?- extract_digits(12345, D).
D = [1, 2, 3, 4, 5].

% Check digit (like credit card validation)
luhn_check_digit(Number, CheckDigit) :-
    CheckDigit is (10 - (Number mod 10)) mod 10.

% Wrap-around indexing
circular_index(Index, ListSize, WrappedIndex) :-
    WrappedIndex is Index mod ListSize.

% Hash function (simple)
simple_hash(Key, TableSize, Hash) :-
    atom_codes(Key, Codes),
    sum_list(Codes, Sum),
    Hash is Sum mod TableSize.

?- simple_hash(hello, 10, H).
H = 2.  % Sum of ASCII codes mod 10
```

### **/2 (Exponentiation)
**Purpose**: Raise a number to a power.

**Precedence**: 200 (xfx - non-associative)

**When to use**: Use for power calculations, exponential growth, or mathematical formulas.

```prolog
% Basic exponentiation
?- X is 2 ** 3.
X = 8.

?- Y is 10 ** 2.
Y = 100.

% Floating point powers
?- Z is 2 ** 0.5.
Z = 1.4142135623730951.  % Square root of 2

% Negative powers
?- A is 2 ** -3.
A = 0.125.  % 1/8

% Practical example: Compound growth
compound_growth(Initial, Rate, Time, Final) :-
    Final is Initial * (1 + Rate) ** Time.

?- compound_growth(1000, 0.05, 10, Amount).
Amount = 1628.8946267774416.  % $1000 at 5% for 10 years

% Powers of 2 (common in computing)
power_of_two(N, Result) :-
    Result is 2 ** N.

?- power_of_two(8, Bits).
Bits = 256.  % 2^8 = 256

% Scientific notation
scientific_notation(Mantissa, Exponent, Number) :-
    Number is Mantissa * (10 ** Exponent).

?- scientific_notation(1.5, 3, N).
N = 1500.0.  % 1.5 × 10³

% Geometric series sum
geometric_sum(FirstTerm, Ratio, NumTerms, Sum) :-
    Sum is FirstTerm * (1 - Ratio ** NumTerms) / (1 - Ratio).

% Distance formula (3D)
distance_3d(X1, Y1, Z1, X2, Y2, Z2, Distance) :-
    DX is X2 - X1,
    DY is Y2 - Y1,
    DZ is Z2 - Z1,
    Distance is (DX**2 + DY**2 + DZ**2) ** 0.5.

?- distance_3d(0, 0, 0, 1, 1, 1, D).
D = 1.7320508075688772.  % sqrt(3)
```

---

## 6. Logical Control Operators

These operators control the flow of execution and logical operations.

### ,/2 (Conjunction - AND)
**Purpose**: Logical AND - both goals must succeed.

**Precedence**: 1000 (xfy - right-associative)

**When to use**: Use to combine multiple conditions or sequential operations.

```prolog
% Basic conjunction
?- 5 > 3, 10 < 20.
true.  % Both conditions true

?- member(X, [1, 2, 3]), X > 2.
X = 3.  % X must be member AND greater than 2

% Sequential operations
process_file(Filename) :-
    exists_file(Filename),       % Check file exists
    open(Filename, read, Stream), % Open file
    read_data(Stream, Data),     % Read data
    close(Stream),               % Close file
    process_data(Data).          % Process the data

% Multiple conditions
valid_user(Username, Password) :-
    atom(Username),              % Username is atom
    atom_length(Username, Len),  % Get length
    Len >= 3,                    % Minimum 3 characters
    Len =< 20,                   % Maximum 20 characters
    check_password(Password).    % Password is valid

% Practical example: Filtering with multiple criteria
expensive_electronics(Item) :-
    item(Item, Category, Price),
    Category = electronics,
    Price > 500.

% Search with multiple constraints
find_person(Name, MinAge, MaxAge, City, Person) :-
    person(Name, Age, City, Person),
    Age >= MinAge,
    Age =< MaxAge.

% Validation chain
validate_input(Data) :-
    nonvar(Data),               % Must be bound
    Data = input(Value, Type),  % Must have correct structure
    validate_type(Type),        % Type must be valid
    validate_value(Value, Type). % Value must match type

% Important: Conjunction is right-associative
% A, B, C is parsed as A, (B, C)
% This affects cut behavior and performance
```

### ;/2 (Disjunction - OR)
**Purpose**: Logical OR - at least one goal must succeed.

**Precedence**: 1200 (xfy - right-associative)

**When to use**: Use for alternative solutions or fallback options.

```prolog
% Basic disjunction
?- 5 > 10 ; 3 < 5.
true.  % Second condition is true

% Multiple alternatives
animal(X) :-
    mammal(X) ; bird(X) ; fish(X).

% Alternative processing
process_data(Data, Result) :-
    (   quick_process(Data, Result)
    ;   slow_process(Data, Result)
    ;   Result = error
    ).

% Practical example: Multiple file formats
read_config(Filename, Config) :-
    (   atom_concat(Filename, '.json', JSONFile),
        exists_file(JSONFile),
        read_json(JSONFile, Config)
    ;   atom_concat(Filename, '.xml', XMLFile),
        exists_file(XMLFile),
        read_xml(XMLFile, Config)
    ;   atom_concat(Filename, '.ini', INIFile),
        exists_file(INIFile),
        read_ini(INIFile, Config)
    ;   write('No config file found'), nl,
        fail
    ).

% Default values
get_setting(Key, Value) :-
    (   stored_setting(Key, Value)  % Try stored value
    ;   default_setting(Key, Value) % Fall back to default
    ;   Value = unknown             % Final fallback
    ).

% Search multiple databases
find_user_info(Username, Info) :-
    (   local_db(Username, Info)
    ;   remote_db(Username, Info)
    ;   backup_db(Username, Info)
    ).

% Type checking with alternatives
numeric_value(X) :-
    integer(X) ; float(X).

atomic_value(X) :-
    atom(X) ; number(X).

% Error handling with alternatives
safe_operation(Input, Output) :-
    (   validate_input(Input),
        main_operation(Input, Output)
    ;   write('Invalid input, using default'), nl,
        Output = default
    ).
```

### ->/2 (If-Then)
**Purpose**: If-then construct - execute consequent only if condition succeeds.

**Precedence**: 1100 (xfy - right-associative)

**When to use**: Use for conditional execution without explicit else clause.

```prolog
% Basic if-then
test_positive(X) :-
    (X > 0 -> write('Positive'); true).

?- test_positive(5).
Positive
true.

?- test_positive(-3).
true.  % Condition failed, no output

% If-then-else using semicolon
classify_number(X, Type) :-
    (   X > 0 -> Type = positive
    ;   X < 0 -> Type = negative
    ;   Type = zero
    ).

?- classify_number(-5, T).
T = negative.

% Practical example: Conditional processing
process_user(User) :-
    (   admin(User)
    ->  grant_admin_access(User)
    ;   regular_user(User)
    ->  grant_user_access(User)
    ;   write('Unknown user type'), nl
    ).

% Guard conditions
safe_divide(X, Y, Result) :-
    (   Y =:= 0
    ->  write('Cannot divide by zero'), nl, fail
    ;   Result is X / Y
    ).

% Optimization with early exit
expensive_check(Data) :-
    (   quick_invalid_check(Data)
    ->  fail  % Early exit if obviously invalid
    ;   expensive_validation(Data)
    ).

% Nested conditionals
grade_comment(Grade, Comment) :-
    (   Grade >= 90
    ->  Comment = 'Excellent work!'
    ;   Grade >= 80
    ->  Comment = 'Good job!'
    ;   Grade >= 70
    ->  Comment = 'Satisfactory'
    ;   Grade >= 60
    ->  Comment = 'Needs improvement'
    ;   Comment = 'Please see instructor'
    ).

% Conditional debugging
debug_print(Message) :-
    (   debug_mode
    ->  write('[DEBUG] '), write(Message), nl
    ;   true  % Do nothing if not in debug mode
    ).
```

### \\+/1 (Negation as Failure)
**Purpose**: Succeeds if the goal fails (logical NOT).

**Precedence**: 900 (fy - prefix)

**When to use**: Use to test that something is NOT true.

```prolog
% Basic negation
?- \+ member(x, [a, b, c]).
true.  % x is not a member

?- \+ (5 > 10).
true.  % 5 is not greater than 10

% Practical example: Exclusion testing
not_in_blacklist(User) :-
    \+ blacklisted(User).

access_allowed(User) :-
    authenticated(User),
    not_in_blacklist(User).

% Find elements not satisfying condition
non_negative_numbers(List, Result) :-
    findall(X, (member(X, List), \+ (X < 0)), Result).

?- non_negative_numbers([1, -2, 3, -4, 5], R).
R = [1, 3, 5].

% Constraint checking
all_different(List) :-
    \+ has_duplicates(List).

has_duplicates(List) :-
    append(_, [X|Rest], List),
    member(X, Rest).

% Validation with negation
valid_password(Password) :-
    atom_length(Password, Len),
    Len >= 8,                    % At least 8 characters
    \+ all_lowercase(Password),  % Not all lowercase
    \+ all_uppercase(Password),  % Not all uppercase
    \+ all_digits(Password).     % Not all digits

% Important limitations of negation as failure:
% 1. Only works with ground terms
?- \+ X = 5.
false.  % Can't prove X is not 5 when X is unbound

% 2. Not true logical negation
?- \+ \+ X = 5.
true.   % Double negation doesn't give X = 5

% 3. Use with care in recursive predicates
no_cycles(Graph) :-
    \+ has_cycle(Graph).

% Safe use: check after instantiation
process_valid_data(Data) :-
    fully_instantiated(Data),
    \+ invalid_data(Data),
    process(Data).
```

---

## 7. List Operators

Special operators for list construction and manipulation.

### |/2 (List Constructor)
**Purpose**: Constructs lists with head and tail separation.

**Precedence**: Listed as special syntax (not a regular operator)

**When to use**: Use for list pattern matching, construction, and deconstruction.

```prolog
% Basic list construction
?- List = [1, 2, 3, 4].
List = [1, 2, 3, 4].

?- List = [head|tail].
List = [head|tail].

% Head-tail decomposition
?- [H|T] = [1, 2, 3, 4].
H = 1, T = [2, 3, 4].

% Multiple head elements
?- [H1, H2|T] = [a, b, c, d, e].
H1 = a, H2 = b, T = [c, d, e].

% List construction
build_list(H, T, [H|T]).

?- build_list(first, [second, third], L).
L = [first, second, third].

% Practical example: List processing patterns
process_list([]).
process_list([H|T]) :-
    process_element(H),
    process_list(T).

% Add element to front
add_front(Element, List, [Element|List]).

?- add_front(0, [1, 2, 3], Result).
Result = [0, 1, 2, 3].

% Split list at position
split_at(0, List, [], List) :- !.
split_at(N, [H|T], [H|Front], Back) :-
    N > 0,
    N1 is N - 1,
    split_at(N1, T, Front, Back).

?- split_at(2, [a, b, c, d, e], F, B).
F = [a, b], B = [c, d, e].

% Check list structure
is_proper_list([]).
is_proper_list([_|T]) :-
    is_proper_list(T).

% Important: Difference between | and comma
?- X = [a, b, c].
X = [a, b, c].  % Standard list

?- X = [a|[b, c]].
X = [a, b, c].  % Same result

?- X = [a|b].
X = [a|b].      % Partial list (not proper list)

% Advanced pattern matching
categorize_list([], empty).
categorize_list([_], single).
categorize_list([_, _], pair).
categorize_list([_, _, _|_], multiple).

?- categorize_list([a, b, c, d], C).
C = multiple.
```

---

## 8. Special Purpose Operators

### !/0 (Cut)
**Purpose**: Prevents backtracking past this point.

**Precedence**: Not applicable (special control construct)

**When to use**: Use to commit to choices, improve efficiency, or make predicates deterministic.

```prolog
% Basic cut usage
max(X, Y, X) :- X >= Y, !.
max(_, Y, Y).

% Without cut, max/3 might succeed multiple times
% With cut, once X >= Y succeeds, we commit to first clause

?- max(5, 3, M).
M = 5.  % Only one solution

% Practical example: First successful pattern
process_data(Data, Result) :-
    quick_method(Data, Result), !.
process_data(Data, Result) :-
    slow_method(Data, Result), !.
process_data(_, error).

% Cut in recursive predicates
factorial(0, 1) :- !.
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% Search with early termination
find_first_solution(Goal, Solution) :-
    call(Goal, Solution),
    !.  % Stop at first solution

% Important: Green cut vs Red cut
% Green cut: Doesn't change program meaning, just efficiency
member_check(X, [X|_]) :- !.     % Found it, stop searching
member_check(X, [_|T]) :- member_check(X, T).

% Red cut: Changes program meaning (dangerous)
min_dangerous(X, Y, X) :- X < Y, !.
min_dangerous(_, Y, Y).  % Assumes X >= Y, but doesn't check!

% Safer version without red cut
min_safe(X, Y, X) :- X =< Y, !.
min_safe(X, Y, Y) :- X > Y.

% Cut in if-then-else (automatic)
grade(Score, Grade) :-
    (   Score >= 90 -> Grade = 'A'
    ;   Score >= 80 -> Grade = 'B'
    ;   Score >= 70 -> Grade = 'C'
    ;   Grade = 'F'
    ).
% Cuts are implicit in -> construct

% Exception handling with cut
safe_operation(Input, Output) :-
    validate_input(Input), !,
    dangerous_operation(Input, Output).
safe_operation(_, default_output).
```

### =../2 (Univ)
**Purpose**: Converts between compound terms and lists.

**Precedence**: 700 (xfx - non-associative)

**When to use**: Use for meta-programming, generic term manipulation, or dynamic predicate construction.

```prolog
% Basic univ usage
?- person(john, 25, london) =.. List.
List = [person, john, 25, london].

?- Term =.. [student, mary, 20].
Term = student(mary, 20).

% Extract functor and arity
get_functor_info(Term, Functor, Arity) :-
    Term =.. [Functor|Args],
    length(Args, Arity).

?- get_functor_info(foo(a, b, c), F, A).
F = foo, A = 3.

% Generic term transformation
add_argument(OldTerm, NewArg, NewTerm) :-
    OldTerm =.. [Functor|Args],
    append(Args, [NewArg], NewArgs),
    NewTerm =.. [Functor|NewArgs].

?- add_argument(point(3, 4), 5, Result).
Result = point(3, 4, 5).

% Dynamic predicate construction
call_with_args(PredName, Args) :-
    Goal =.. [PredName|Args],
    call(Goal).

?- call_with_args(append, [[1, 2], [3, 4], X]).
X = [1, 2, 3, 4].

% Practical example: Generic getter/setter
get_field(Record, FieldNum, Value) :-
    Record =.. [_|Fields],
    nth1(FieldNum, Fields, Value).

set_field(OldRecord, FieldNum, NewValue, NewRecord) :-
    OldRecord =.. [Functor|Fields],
    nth1(FieldNum, Fields, _, RestFields),
    nth1(FieldNum, NewFields, NewValue, RestFields),
    NewRecord =.. [Functor|NewFields].

% Transform predicate names
transform_predicate(Pred, Suffix, NewPred) :-
    Pred =.. [Name|Args],
    atom_concat(Name, Suffix, NewName),
    NewPred =.. [NewName|Args].

?- transform_predicate(save(file), '_backup', P).
P = save_backup(file).

% Meta-interpreter helper
call_goal_with_context(Goal, Context) :-
    Goal =.. [Pred|Args],
    append(Args, [Context], NewArgs),
    NewGoal =.. [Pred|NewArgs],
    call(NewGoal).
```

### ^ (Existential Quantification)
**Purpose**: In meta-predicates, indicates variables to ignore in grouping.

**Precedence**: 200 (xfy - right-associative)

**When to use**: Use in bagof/3 and setof/3 to ignore certain variables.

```prolog
% Without existential quantification
parent(tom, bob).
parent(tom, ann).
parent(mary, bob).
parent(mary, ann).

% bagof groups by free variables
?- bagof(Child, parent(Parent, Child), Children).
Parent = mary, Children = [bob, ann] ;
Parent = tom, Children = [bob, ann].

% With existential quantification (ignore Parent)
?- bagof(Child, Parent^parent(Parent, Child), Children).
Children = [bob, ann, bob, ann].

% Multiple existential quantifiers
employee(john, sales, 50000).
employee(mary, it, 60000).
employee(bob, sales, 55000).

% Find all departments (ignoring name and salary)
?- setof(Dept, Name^Salary^employee(Name, Dept, Salary), Departments).
Departments = [it, sales].

% Practical example: Statistical queries
% Find average salary by department
average_salary_by_dept(Dept, Avg) :-
    bagof(Salary, Name^employee(Name, Dept, Salary), Salaries),
    sum_list(Salaries, Total),
    length(Salaries, Count),
    Avg is Total / Count.

?- average_salary_by_dept(sales, Avg).
Avg = 52500.0.

% Complex existential quantification
% Find all skills mentioned in employee records
all_skills(Skills) :-
    setof(Skill,
          Name^Dept^Salary^Skills^(
              employee_skills(Name, Dept, Salary, SkillList),
              member(Skill, SkillList)
          ),
          Skills).

% Nested quantification
complex_query(Results) :-
    bagof(
        Result,
        X^Y^Z^(
            data(X, Y, Z, Result),
            constraint1(X),
            constraint2(Y, Z)
        ),
        Results
    ).

% Important: ^ has higher precedence than most operators
% X^Y^goal is parsed as X^(Y^goal)
% Use parentheses for clarity: (X^Y)^goal vs X^(Y^goal)
```

---

## Operator Precedence Table

Complete precedence table for JProlog operators:

| Precedence | Type | Operators | Description |
|------------|------|-----------|-------------|
| 1200 | xfx | :- | Rule definition |
| 1200 | xfy | ; | Disjunction (OR) |
| 1100 | xfy | -> | If-then |
| 1000 | xfy | , | Conjunction (AND) |
| 900 | fy | \\+ | Negation as failure |
| 700 | xfx | = \\= == \\== | Unification operators |
| 700 | xfx | @< @=< @> @>= | Term comparison |
| 700 | xfx | =:= =\\= < =< > >= | Arithmetic comparison |
| 500 | yfx | + - | Addition, subtraction |
| 400 | yfx | * / // rem mod | Multiplication, division |
| 200 | xfx | ** | Exponentiation |
| 200 | xfy | ^ | Existential quantification |
| 200 | fy | + - | Unary plus, minus |

### Associativity Types:
- **xfx**: Non-associative (a op b op c is error)
- **xfy**: Right-associative (a op b op c = a op (b op c))
- **yfx**: Left-associative (a op b op c = (a op b) op c)
- **fy**: Prefix (op a)
- **yf**: Postfix (a op)

---

## Summary

This comprehensive reference covers JProlog's built-in operators organized by function and precedence. Key points:

1. **Precedence matters** - Lower numbers bind more tightly
2. **Associativity rules** - Determines grouping for same precedence
3. **Context sensitivity** - Arithmetic operators only work in arithmetic contexts
4. **Unification vs Comparison** - Different operators for different types of equality
5. **Logical operators** - Control program flow and backtracking
6. **Meta-operators** - Enable advanced programming techniques

Understanding these operators and their interactions is crucial for writing correct and efficient Prolog programs. Use parentheses when in doubt about precedence, and choose the right operator for your specific needs.