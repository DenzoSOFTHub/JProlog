# JProlog Built-in Predicates Reference

**Version**: JProlog v2.0.15  
**Last Updated**: 2025-08-20  
**Total Predicates**: 80+ predicates organized by functional category

This reference guide organizes JProlog's built-in predicates by their logical function and use case. Each section includes explanations suitable for users new to Prolog, with detailed examples showing practical applications.

---

## Table of Contents

1. [Type Checking Predicates](#1-type-checking-predicates)
2. [Term Manipulation](#2-term-manipulation)
3. [List Operations](#3-list-operations)
4. [Arithmetic Predicates](#4-arithmetic-predicates)
5. [Control Flow](#5-control-flow)
6. [Meta-Predicates](#6-meta-predicates)
7. [Input/Output](#7-inputoutput)
8. [Database Operations](#8-database-operations)
9. [Atom and String Operations](#9-atom-and-string-operations)
10. [Character Processing](#10-character-processing)
11. [DCG (Grammar) Predicates](#11-dcg-grammar-predicates)
12. [Exception Handling](#12-exception-handling)
13. [System Predicates](#13-system-predicates)

---

## 1. Type Checking Predicates

Type checking predicates are used to determine the type of a term or to ensure a term is of a specific type. These are essential for writing robust Prolog programs that handle different kinds of data correctly.

### Understanding Prolog Types

In Prolog, every piece of data is a **term**. Terms can be:
- **Variables**: Uninstantiated placeholders (e.g., `X`, `_Value`)
- **Atoms**: Constant symbols (e.g., `hello`, `'New York'`)
- **Numbers**: Integers or floats (e.g., `42`, `3.14`)
- **Compound Terms**: Structures with a functor and arguments (e.g., `person(john, 25)`)
- **Lists**: Special compound terms (e.g., `[1, 2, 3]`)

### var/1
**Purpose**: Checks if a term is an uninstantiated variable.

**When to use**: Use this when you need to handle unbound variables differently from bound values, often in conditional logic or to prevent errors.

```prolog
% Check if a variable is unbound before processing
process_value(X) :-
    var(X),
    !,
    write('Error: Variable must be instantiated'),
    fail.
process_value(X) :-
    write('Processing: '), write(X).

% Example usage:
?- process_value(X).
Error: Variable must be instantiated
false.

?- process_value(42).
Processing: 42
true.
```

### nonvar/1
**Purpose**: Checks if a term is NOT a variable (i.e., it's instantiated).

**When to use**: Use this to ensure a value has been provided before performing operations on it.

```prolog
% Safe division that checks arguments are instantiated
safe_divide(X, Y, Result) :-
    nonvar(X),
    nonvar(Y),
    Y \= 0,
    Result is X / Y.

% Example usage:
?- safe_divide(10, 2, R).
R = 5.0.

?- safe_divide(X, 2, R).
false.  % Fails because X is not instantiated
```

### atom/1
**Purpose**: Checks if a term is an atom (a constant symbol).

**When to use**: Use when you need to verify that input is a symbolic constant, not a number or structure.

```prolog
% A predicate that processes color names
set_color(Color) :-
    atom(Color),
    member(Color, [red, green, blue, yellow]),
    !,
    write('Color set to: '), write(Color).
set_color(_) :-
    write('Invalid color - must be an atom: red, green, blue, or yellow').

% Example usage:
?- set_color(red).
Color set to: red
true.

?- set_color(123).
Invalid color - must be an atom: red, green, blue, or yellow
true.
```

### number/1
**Purpose**: Checks if a term is a number (integer or float).

**When to use**: Use before arithmetic operations to ensure the term can be used in calculations.

```prolog
% Calculate the square of a number with type checking
square(X, Result) :-
    number(X),
    Result is X * X.
square(X, _) :-
    \+ number(X),
    write('Error: '), write(X), write(' is not a number'), nl,
    fail.

% Example usage:
?- square(5, R).
R = 25.

?- square(hello, R).
Error: hello is not a number
false.
```

### integer/1
**Purpose**: Checks if a term is specifically an integer (whole number).

**When to use**: Use when you need whole numbers only, such as for array indices or counting.

```prolog
% Get the Nth element of a list (N must be an integer)
get_nth(N, List, Element) :-
    integer(N),
    N > 0,
    nth1(N, List, Element).
get_nth(N, _, _) :-
    \+ integer(N),
    write('Error: Position must be a positive integer'), nl,
    fail.

% Example usage:
?- get_nth(2, [a, b, c], E).
E = b.

?- get_nth(2.5, [a, b, c], E).
Error: Position must be a positive integer
false.
```

### float/1
**Purpose**: Checks if a term is a floating-point number.

**When to use**: Use when you need to distinguish between exact integers and decimal numbers.

```prolog
% Format a number for display based on its type
format_number(N, Formatted) :-
    integer(N),
    !,
    atom_number(Formatted, N).
format_number(N, Formatted) :-
    float(N),
    !,
    format(atom(Formatted), '~2f', [N]).

% Example usage:
?- format_number(42, F).
F = '42'.

?- format_number(3.14159, F).
F = '3.14'.
```

### atomic/1
**Purpose**: Checks if a term is atomic (atom or number).

**When to use**: Use when you need simple, non-compound values.

```prolog
% Store simple values in a database
store_simple_value(Key, Value) :-
    atomic(Value),
    !,
    assertz(stored(Key, Value)).
store_simple_value(Key, Value) :-
    write('Cannot store complex term: '), write(Value), nl,
    fail.

% Example usage:
?- store_simple_value(age, 25).
true.

?- store_simple_value(name, 'John').
true.

?- store_simple_value(person, person(john, 25)).
Cannot store complex term: person(john, 25)
false.
```

### compound/1
**Purpose**: Checks if a term is a compound structure (has a functor and arguments).

**When to use**: Use to identify complex data structures.

```prolog
% Process different types of data differently
process_data(Data) :-
    atomic(Data),
    !,
    write('Simple value: '), write(Data).
process_data(Data) :-
    compound(Data),
    Data =.. [Functor|Args],
    write('Complex structure: '), write(Functor),
    write(' with '), length(Args, N), write(N), write(' arguments').

% Example usage:
?- process_data(42).
Simple value: 42
true.

?- process_data(person(john, 25, london)).
Complex structure: person with 3 arguments
true.
```

### callable/1
**Purpose**: Checks if a term can be called as a goal (atom or compound).

**When to use**: Use when you need to verify that a term can be executed as a Prolog goal.

```prolog
% Execute a goal safely
safe_call(Goal) :-
    callable(Goal),
    !,
    catch(call(Goal), Error, (write('Error: '), write(Error), nl, fail)).
safe_call(Goal) :-
    write('Not callable: '), write(Goal), nl,
    fail.

% Example usage:
?- safe_call(append([1], [2], X)).
X = [1, 2].

?- safe_call(123).
Not callable: 123
false.
```

### ground/1
**Purpose**: Checks if a term is fully instantiated (contains no variables).

**When to use**: Use to ensure all data is complete before processing or storing.

```prolog
% Only store fully instantiated facts
add_fact(Fact) :-
    ground(Fact),
    !,
    assertz(Fact),
    write('Fact added: '), write(Fact).
add_fact(Fact) :-
    write('Cannot add fact with variables: '), write(Fact), nl,
    fail.

% Example usage:
?- add_fact(parent(john, mary)).
Fact added: parent(john, mary)
true.

?- add_fact(parent(X, mary)).
Cannot add fact with variables: parent(_G123, mary)
false.
```

### is_list/1
**Purpose**: Checks if a term is a proper list.

**When to use**: Use before list operations to ensure the data structure is valid.

```prolog
% Sum all numbers in a list
sum_list(List, Sum) :-
    is_list(List),
    sum_list_helper(List, 0, Sum).
sum_list(NotList, _) :-
    \+ is_list(NotList),
    write('Error: Expected a list, got: '), write(NotList), nl,
    fail.

sum_list_helper([], Acc, Acc).
sum_list_helper([H|T], Acc, Sum) :-
    number(H),
    NewAcc is Acc + H,
    sum_list_helper(T, NewAcc, Sum).

% Example usage:
?- sum_list([1, 2, 3, 4], Sum).
Sum = 10.

?- sum_list(not_a_list, Sum).
Error: Expected a list, got: not_a_list
false.
```

---

## 2. Term Manipulation

Term manipulation predicates allow you to inspect and construct complex terms. These are essential for meta-programming and working with structured data.

### functor/3
**Purpose**: Relates a compound term to its name and arity (number of arguments).

**When to use**: Use to inspect the structure of compound terms or create new terms with a specific structure.

```prolog
% Example: Extract information about a term's structure
analyze_term(Term) :-
    functor(Term, Name, Arity),
    write('Functor: '), write(Name),
    write(', Arity: '), write(Arity), nl.

% Example usage:
?- analyze_term(person(john, 25, london)).
Functor: person, Arity: 3
true.

% Create a term with specific structure
?- functor(Term, employee, 4).
Term = employee(_G1, _G2, _G3, _G4).

% Practical example: Generic predicate to check term type
is_person_record(Term) :-
    functor(Term, person, 3).

?- is_person_record(person(john, 25, london)).
true.

?- is_person_record(employee(john, 25, london, 50000)).
false.
```

### arg/3
**Purpose**: Extracts or checks a specific argument from a compound term.

**When to use**: Use to access fields in structured data without pattern matching.

```prolog
% Example: Database of person records
% person(Name, Age, City)

% Get the age of a person (2nd argument)
get_age(PersonRecord, Age) :-
    compound(PersonRecord),
    functor(PersonRecord, person, 3),
    arg(2, PersonRecord, Age).

% Example usage:
?- get_age(person(john, 25, london), Age).
Age = 25.

% Update a specific field in a record
update_age(OldPerson, NewAge, NewPerson) :-
    OldPerson = person(Name, _, City),
    NewPerson = person(Name, NewAge, City).

% Or using arg/3 for generic field access:
get_field(Record, FieldNum, Value) :-
    compound(Record),
    arg(FieldNum, Record, Value).

?- get_field(person(john, 25, london), 3, City).
City = london.
```

### =../2 (univ)
**Purpose**: Converts between a compound term and a list (name followed by arguments).

**When to use**: Use for generic term manipulation, creating terms dynamically, or converting between representations.

```prolog
% Example: Convert term to list and back
?- person(john, 25, london) =.. List.
List = [person, john, 25, london].

?- Term =.. [student, mary, 20, 'computer science'].
Term = student(mary, 20, 'computer science').

% Practical example: Add one more argument to any term
add_argument(OldTerm, NewArg, NewTerm) :-
    OldTerm =.. [Functor|OldArgs],
    append(OldArgs, [NewArg], NewArgs),
    NewTerm =.. [Functor|NewArgs].

?- add_argument(point(3, 4), 5, Result).
Result = point(3, 4, 5).

% Generic term builder
build_term(Functor, Args, Term) :-
    Term =.. [Functor|Args].

?- build_term(employee, [john, manager, 75000], T).
T = employee(john, manager, 75000).
```

### copy_term/2
**Purpose**: Creates a copy of a term with fresh (renamed) variables.

**When to use**: Use when you need an independent copy of a term with its own variables, often in meta-programming or template processing.

```prolog
% Example: Template-based fact generation
person_template(person(Name, Age, _City)) :-
    atom(Name),
    number(Age),
    Age > 0.

create_person_facts :-
    Template = person(_, _, london),
    % Create multiple facts from template
    copy_term(Template, person(john, 25, london)),
    copy_term(Template, person(mary, 30, london)),
    copy_term(Template, person(bob, 35, london)).

% Example of variable independence:
?- Template = foo(X, X), copy_term(Template, Copy).
Template = foo(X, X),
Copy = foo(_G1, _G1).  % New variables but same binding pattern

% Practical use: Avoid variable conflicts in meta-predicates
apply_template(Template, Values, Result) :-
    copy_term(Template, Result),
    Result =.. [Functor|Values].
```

### compare/3
**Purpose**: Three-way comparison of terms using standard ordering.

**When to use**: Use for sorting, searching, or implementing ordered data structures.

```prolog
% Standard term ordering: variables < numbers < atoms < compound terms
% Within each category, there's a specific ordering

% Example: Compare any two terms
?- compare(Order, 3, 5).
Order = (<).  % 3 is less than 5

?- compare(Order, apple, banana).
Order = (<).  % Alphabetical ordering for atoms

?- compare(Order, foo(1), foo(2)).
Order = (<).  % Compound terms compared by arguments

% Practical example: Insert into sorted list
insert_sorted(X, [], [X]).
insert_sorted(X, [H|T], [X,H|T]) :-
    compare(<, X, H), !.
insert_sorted(X, [H|T], [H|Rest]) :-
    insert_sorted(X, T, Rest).

?- insert_sorted(3, [1, 2, 4, 5], Result).
Result = [1, 2, 3, 4, 5].

% Binary search tree insertion
insert_bst(X, nil, tree(X, nil, nil)).
insert_bst(X, tree(V, L, R), tree(V, NewL, R)) :-
    compare(<, X, V),
    insert_bst(X, L, NewL).
insert_bst(X, tree(V, L, R), tree(V, L, NewR)) :-
    compare(>, X, V),
    insert_bst(X, R, NewR).
insert_bst(X, tree(X, L, R), tree(X, L, R)).  % Already exists
```

### term_variables/2
**Purpose**: Extracts all variables from a term into a list.

**When to use**: Use to find all uninstantiated variables in complex terms.

```prolog
% Example: Find all variables in a term
?- term_variables(foo(X, bar(Y, X, 3), Z), Vars).
Vars = [X, Y, Z].

% Practical example: Check if a rule has unbound variables
check_rule_complete(Rule) :-
    term_variables(Rule, Vars),
    (   Vars = []
    ->  write('Rule is fully instantiated')
    ;   length(Vars, N),
        write('Rule has '), write(N), write(' unbound variables')
    ).

?- check_rule_complete(parent(john, mary)).
Rule is fully instantiated
true.

?- check_rule_complete(parent(X, mary)).
Rule has 1 unbound variables
true.
```

### subsumes_term/2
**Purpose**: Checks if one term is more general than another.

**When to use**: Use for pattern matching and generalization checking.

```prolog
% A term T1 subsumes T2 if T1 can be made identical to T2 by binding variables

% Examples:
?- subsumes_term(foo(X, Y), foo(a, b)).
true.  % foo(X,Y) is more general than foo(a,b)

?- subsumes_term(foo(X, X), foo(a, b)).
false.  % Would need X=a and X=b simultaneously

?- subsumes_term(foo(a, b), foo(X, Y)).
false.  % foo(a,b) is more specific, not more general

% Practical example: Find matching templates
find_matching_template(Data, Templates, Match) :-
    member(Match, Templates),
    subsumes_term(Match, Data).

% With templates:
templates([
    person(_, _, _),           % Any person
    person(john, _, _),         % John specifically
    person(_, Age, _) :- Age > 18  % Adults
]).

?- find_matching_template(person(john, 25, london), 
                          [person(_, _, paris), person(john, _, _)], 
                          Match).
Match = person(john, _, _).
```

---

## 3. List Operations

Lists are fundamental data structures in Prolog. These predicates provide essential list manipulation capabilities.

### Understanding Prolog Lists

A list in Prolog is either:
- Empty: `[]`
- Non-empty: `[Head|Tail]` where Head is an element and Tail is a list

Examples:
- `[1, 2, 3]` is syntactic sugar for `[1|[2|[3|[]]]]`
- `[a, b|Rest]` matches a list starting with `a`, `b`, with Rest as the remaining list

### append/3
**Purpose**: Concatenates two lists or splits a list into parts.

**When to use**: Use for joining lists, finding prefixes/suffixes, or generating list partitions.

```prolog
% Mode 1: Concatenate two lists (inputs: +List1, +List2, output: -List3)
?- append([1, 2], [3, 4], Result).
Result = [1, 2, 3, 4].

% Mode 2: Find all ways to split a list (input: +List3, outputs: -List1, -List2)
?- append(Left, Right, [a, b, c]).
Left = [], Right = [a, b, c] ;
Left = [a], Right = [b, c] ;
Left = [a, b], Right = [c] ;
Left = [a, b, c], Right = [].

% Practical example: Insert element in middle of list
insert_after(Element, After, List, NewList) :-
    append(Prefix, [After|Suffix], List),
    append(Prefix, [After, Element|Suffix], NewList).

?- insert_after(new, b, [a, b, c, d], Result).
Result = [a, b, new, c, d].

% Find if one list is contained in another
sublist(Sub, List) :-
    append(_, Rest, List),
    append(Sub, _, Rest).

?- sublist([b, c], [a, b, c, d]).
true.
```

### member/2
**Purpose**: Checks membership or generates elements of a list.

**When to use**: Use to test if an element is in a list or to iterate through list elements.

```prolog
% Mode 1: Check if element is in list
?- member(b, [a, b, c]).
true.

?- member(x, [a, b, c]).
false.

% Mode 2: Generate all elements of a list
?- member(X, [red, green, blue]).
X = red ;
X = green ;
X = blue.

% Practical example: Find common elements in two lists
common_elements(List1, List2, Common) :-
    findall(X, (member(X, List1), member(X, List2)), CommonList),
    list_to_set(CommonList, Common).  % Remove duplicates

?- common_elements([1, 2, 3, 4], [3, 4, 5, 6], Common).
Common = [3, 4].

% Validate input from a set of options
validate_option(Option, ValidOptions) :-
    member(Option, ValidOptions),
    !.
validate_option(Option, ValidOptions) :-
    write('Invalid option: '), write(Option),
    write('. Valid options are: '), write(ValidOptions), nl,
    fail.
```

### length/2
**Purpose**: Relates a list to its length.

**When to use**: Use to count elements, create lists of specific length, or constrain list size.

```prolog
% Mode 1: Find length of a list
?- length([a, b, c, d], Len).
Len = 4.

% Mode 2: Create a list of specific length with unbound variables
?- length(List, 3).
List = [_G1, _G2, _G3].

% Mode 3: Check if list has specific length
?- length([a, b, c], 3).
true.

% Practical example: Pad a list to specific length
pad_list(List, TargetLen, PadValue, PaddedList) :-
    length(List, CurrentLen),
    PadCount is TargetLen - CurrentLen,
    (   PadCount =< 0
    ->  PaddedList = List
    ;   length(Padding, PadCount),
        maplist(=(PadValue), Padding),
        append(List, Padding, PaddedList)
    ).

?- pad_list([a, b], 5, x, Result).
Result = [a, b, x, x, x].

% Generate lists of increasing length
generate_lists(Max) :-
    between(0, Max, N),
    length(List, N),
    write('List of length '), write(N), write(': '), write(List), nl,
    fail.
generate_lists(_).
```

### reverse/2
**Purpose**: Reverses the order of elements in a list.

**When to use**: Use for reversing sequences, implementing stacks, or palindrome checking.

```prolog
% Basic usage
?- reverse([1, 2, 3, 4], Rev).
Rev = [4, 3, 2, 1].

% Check if list is palindrome
is_palindrome(List) :-
    reverse(List, List).

?- is_palindrome([a, b, c, b, a]).
true.

?- is_palindrome([a, b, c]).
false.

% Practical example: Process list in reverse order without modifying original
process_reverse(List) :-
    reverse(List, Reversed),
    process_items(Reversed).

% Build a string in reverse (useful for efficiency)
build_string_reverse([], Acc, Result) :-
    reverse(Acc, Result).
build_string_reverse([H|T], Acc, Result) :-
    process_char(H, Processed),
    build_string_reverse(T, [Processed|Acc], Result).
```

### nth0/3 and nth1/3
**Purpose**: Access or find the position of an element in a list (0-based or 1-based indexing).

**When to use**: Use for indexed access to list elements or finding positions.

```prolog
% nth0/3 - Zero-based indexing (like arrays in most languages)
?- nth0(0, [a, b, c, d], Element).
Element = a.

?- nth0(2, [a, b, c, d], Element).
Element = c.

% nth1/3 - One-based indexing (more natural for humans)
?- nth1(1, [a, b, c, d], Element).
Element = a.

?- nth1(3, [a, b, c, d], Element).
Element = c.

% Find position of element
?- nth0(Position, [apple, banana, cherry], banana).
Position = 1.

?- nth1(Position, [apple, banana, cherry], banana).
Position = 2.

% Practical example: Replace element at position
replace_nth(N, List, NewElement, NewList) :-
    nth0(N, List, OldElement, Rest),
    nth0(N, NewList, NewElement, Rest).

?- replace_nth(1, [a, b, c, d], x, Result).
Result = [a, x, c, d].

% Get multiple elements by indices
get_elements_at([], _, []).
get_elements_at([Index|Indices], List, [Element|Elements]) :-
    nth0(Index, List, Element),
    get_elements_at(Indices, List, Elements).

?- get_elements_at([0, 2, 3], [a, b, c, d, e], Elements).
Elements = [a, c, d].
```

### select/3
**Purpose**: Selects an element from a list, returning the element and the rest.

**When to use**: Use for removing elements, permutations, or non-deterministic selection.

```prolog
% Remove an element from a list
?- select(b, [a, b, c, d], Rest).
Rest = [a, c, d].

% Non-deterministic selection - try all possibilities
?- select(X, [1, 2, 3], Rest).
X = 1, Rest = [2, 3] ;
X = 2, Rest = [1, 3] ;
X = 3, Rest = [1, 2].

% Insert an element (using select/3 backwards)
?- select(x, Result, [a, b, c]).
Result = [x, a, b, c] ;
Result = [a, x, b, c] ;
Result = [a, b, x, c] ;
Result = [a, b, c, x].

% Practical example: Remove all occurrences of an element
remove_all(_, [], []).
remove_all(X, List, Result) :-
    select(X, List, Rest),
    !,
    remove_all(X, Rest, Result).
remove_all(_, List, List).

?- remove_all(a, [a, b, a, c, a, d], Result).
Result = [b, c, d].

% Generate permutations
permutation([], []).
permutation(List, [H|Perm]) :-
    select(H, List, Rest),
    permutation(Rest, Perm).

?- permutation([a, b, c], P).
P = [a, b, c] ;
P = [a, c, b] ;
P = [b, a, c] ;
P = [b, c, a] ;
P = [c, a, b] ;
P = [c, b, a].
```

### sort/2 and msort/2
**Purpose**: Sort lists in standard order.
- `sort/2` removes duplicates
- `msort/2` keeps duplicates

**When to use**: Use for ordering data, removing duplicates, or preparing data for efficient searching.

```prolog
% sort/2 - Sort and remove duplicates
?- sort([3, 1, 4, 1, 5, 9, 2, 6], Sorted).
Sorted = [1, 2, 3, 4, 5, 6, 9].  % Note: only one 1

% msort/2 - Sort but keep duplicates
?- msort([3, 1, 4, 1, 5, 9, 2, 6], Sorted).
Sorted = [1, 1, 2, 3, 4, 5, 6, 9].  % Both 1s kept

% Sort complex terms (sorted by standard term ordering)
?- sort([person(john, 30), person(alice, 25), person(bob, 25)], Sorted).
Sorted = [person(alice, 25), person(bob, 25), person(john, 30)].

% Practical example: Find unique elements
unique_elements(List, Unique) :-
    sort(List, Unique).

?- unique_elements([a, b, a, c, b, d], Unique).
Unique = [a, b, c, d].

% Count occurrences after sorting
count_occurrences(List, Counts) :-
    msort(List, Sorted),
    count_consecutive(Sorted, Counts).

count_consecutive([], []).
count_consecutive([H|T], [H-Count|Rest]) :-
    count_same(H, T, 1, Count, Remaining),
    count_consecutive(Remaining, Rest).

count_same(X, [X|T], Acc, Count, Rest) :-
    !,
    Acc1 is Acc + 1,
    count_same(X, T, Acc1, Count, Rest).
count_same(_, List, Count, Count, List).
```

---

## 4. Arithmetic Predicates

Arithmetic predicates handle mathematical calculations and comparisons. Prolog uses special evaluation rules for arithmetic.

### Understanding Arithmetic in Prolog

Prolog treats arithmetic expressions differently from other terms:
- `X = 2 + 3` unifies X with the term `+(2, 3)`, NOT with 5
- `X is 2 + 3` evaluates the expression and unifies X with 5
- Arithmetic comparisons (`<`, `>`, etc.) automatically evaluate their arguments

### is/2
**Purpose**: Evaluates an arithmetic expression and unifies the result.

**When to use**: Use for all arithmetic calculations where you need the computed value.

```prolog
% Basic arithmetic
?- X is 2 + 3.
X = 5.

?- Y is 10 * (3 + 4).
Y = 70.

% Using variables (right side must be instantiated)
?- A = 5, B = 3, C is A * B.
A = 5, B = 3, C = 15.

% Common arithmetic functions
?- X is sqrt(16).
X = 4.0.

?- Y is sin(0).
Y = 0.0.

?- Z is max(5, 3).
Z = 5.

% Practical example: Calculate compound interest
compound_interest(Principal, Rate, Time, Amount) :-
    Amount is Principal * (1 + Rate/100) ** Time.

?- compound_interest(1000, 5, 3, Amount).
Amount = 1157.625.

% Factorial calculation
factorial(0, 1) :- !.
factorial(N, Result) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, R1),
    Result is N * R1.

?- factorial(5, R).
R = 120.
```

### Arithmetic Comparison Operators

These operators automatically evaluate arithmetic expressions before comparing.

```prolog
% Less than: <
?- 3 < 5.
true.

?- 2 + 3 < 4 + 2.  % Evaluates to: 5 < 6
true.

% Less than or equal: =<  (Note: not <= like other languages!)
?- 5 =< 5.
true.

% Greater than: >
?- 10 > 3.
true.

% Greater than or equal: >=
?- 7 >= 7.
true.

% Arithmetic equality: =:=
?- 2 + 3 =:= 5.
true.

?- 2 + 3 =:= 4 + 1.
true.

% Arithmetic inequality: =\=
?- 2 + 3 =\= 6.
true.

% Practical example: Temperature converter with validation
celsius_to_fahrenheit(C, F) :-
    C >= -273.15,  % Absolute zero check
    F is C * 9/5 + 32.

?- celsius_to_fahrenheit(100, F).
F = 212.0.

?- celsius_to_fahrenheit(-300, F).
false.  % Below absolute zero

% Finding values in range
in_range(X, Min, Max) :-
    X >= Min,
    X =< Max.

?- in_range(5, 1, 10).
true.

% Maximum of three numbers
max_of_three(A, B, C, Max) :-
    (   A >= B, A >= C -> Max = A
    ;   B >= A, B >= C -> Max = B
    ;   Max = C
    ).
```

### between/3
**Purpose**: Generates or tests integers within a range.

**When to use**: Use for generating sequences, validating ranges, or iteration.

```prolog
% Mode 1: Check if number is in range
?- between(1, 10, 5).
true.

?- between(1, 10, 15).
false.

% Mode 2: Generate all integers in range
?- between(1, 5, X).
X = 1 ;
X = 2 ;
X = 3 ;
X = 4 ;
X = 5.

% Practical example: Generate multiplication table
multiplication_table(N) :-
    between(1, 10, I),
    Result is N * I,
    format('~w x ~w = ~w~n', [N, I, Result]),
    fail.
multiplication_table(_).

?- multiplication_table(7).
7 x 1 = 7
7 x 2 = 14
7 x 3 = 21
...
7 x 10 = 70
true.

% Find perfect squares in range
perfect_squares(Min, Max, Squares) :-
    findall(X, (between(Min, Max, X), 
                Sqrt is sqrt(X),
                Sqrt =:= floor(Sqrt)), 
            Squares).

?- perfect_squares(1, 20, S).
S = [1, 4, 9, 16].
```

### succ/2
**Purpose**: Relates consecutive integers (successor relation).

**When to use**: Use for increment/decrement operations or defining natural number sequences.

```prolog
% Find successor
?- succ(5, X).
X = 6.

% Find predecessor
?- succ(X, 10).
X = 9.

% Check consecutive numbers
?- succ(7, 8).
true.

% Practical example: Count down
countdown(0) :- write('Blast off!'), nl, !.
countdown(N) :-
    N > 0,
    write(N), nl,
    succ(N1, N),
    countdown(N1).

?- countdown(3).
3
2
1
Blast off!
true.
```

### plus/3
**Purpose**: Relates three integers where the third is the sum of the first two.

**When to use**: Use for addition with multiple unknown values.

```prolog
% Normal addition
?- plus(3, 4, X).
X = 7.

% Find what to add
?- plus(3, X, 10).
X = 7.

% Find first addend
?- plus(X, 5, 12).
X = 7.

% Practical example: Calculate remaining budget
remaining_budget(Total, Spent, Remaining) :-
    plus(Spent, Remaining, Total).

?- remaining_budget(1000, 750, R).
R = 250.

?- remaining_budget(1000, S, 400).
S = 600.
```

---

## 5. Control Flow

Control flow predicates determine how Prolog executes programs, including conditionals, loops, and backtracking control.

### ! (cut)
**Purpose**: Prevents backtracking past this point.

**When to use**: Use to commit to a choice, improve efficiency, or implement deterministic predicates.

```prolog
% Example: Deterministic max/3 using cut
max(X, Y, X) :- X >= Y, !.
max(X, Y, Y).

% Without cut, max/3 might give multiple solutions
% With cut, once X >= Y succeeds, we commit to that answer

?- max(5, 3, M).
M = 5.  % Only one solution

% Practical example: Grade classification
grade(Score, Grade) :-
    Score >= 90, !,
    Grade = 'A'.
grade(Score, Grade) :-
    Score >= 80, !,
    Grade = 'B'.
grade(Score, Grade) :-
    Score >= 70, !,
    Grade = 'C'.
grade(Score, Grade) :-
    Score >= 60, !,
    Grade = 'D'.
grade(_, 'F').

?- grade(85, G).
G = 'B'.  % Only one answer, no backtracking

% Red cut vs Green cut
% Green cut: Doesn't change meaning, just efficiency
% Red cut: Changes program meaning

% Green cut example:
member_check(X, [X|_]) :- !.  % Found it, no need to search more
member_check(X, [_|T]) :- member_check(X, T).

% Red cut example (dangerous):
min(X, Y, X) :- X < Y, !.
min(_, Y, Y).  % Assumes X >= Y without checking
```

### -> (if-then) and ; (else)
**Purpose**: Implements conditional execution (if-then-else).

**When to use**: Use for conditional logic where you need different actions based on conditions.

```prolog
% Basic if-then syntax: (Condition -> ThenPart)
test_sign(X) :-
    (   X > 0
    ->  write('Positive')
    ).

% If-then-else syntax: (Condition -> ThenPart ; ElsePart)
sign(X, Sign) :-
    (   X > 0
    ->  Sign = positive
    ;   X < 0
    ->  Sign = negative
    ;   Sign = zero
    ).

?- sign(5, S).
S = positive.

?- sign(-3, S).
S = negative.

?- sign(0, S).
S = zero.

% Practical example: Safe division
safe_divide(X, Y, Result) :-
    (   Y =:= 0
    ->  write('Error: Division by zero'), nl,
        fail
    ;   Result is X / Y
    ).

?- safe_divide(10, 2, R).
R = 5.0.

?- safe_divide(10, 0, R).
Error: Division by zero
false.

% Nested conditions
classify_age(Age, Category) :-
    (   Age < 0
    ->  Category = invalid
    ;   Age < 13
    ->  Category = child
    ;   Age < 20
    ->  Category = teenager
    ;   Age < 60
    ->  Category = adult
    ;   Category = senior
    ).
```

### \+ (negation as failure)
**Purpose**: Succeeds if the goal fails (negation by failure).

**When to use**: Use to test that something is NOT true.

```prolog
% Basic negation
?- \+ member(x, [a, b, c]).
true.  % x is not a member

?- \+ member(b, [a, b, c]).
false.  % b is a member, so negation fails

% Practical example: Find elements not in a list
not_in_list([], _).
not_in_list([H|T], Exclude) :-
    \+ member(H, Exclude),
    not_in_list(T, Exclude).

?- not_in_list([a, b, c], [b, d, e]).
false.  % b is in the exclude list

?- not_in_list([a, c], [b, d, e]).
true.  % neither a nor c is in exclude list

% Check uniqueness
all_different([]).
all_different([H|T]) :-
    \+ member(H, T),
    all_different(T).

?- all_different([a, b, c]).
true.

?- all_different([a, b, a]).
false.

% Important: Negation by failure has limitations
% \+ X = 5 fails if X is unbound (can't prove X is not 5)
% Use with ground terms for predictable behavior
```

### once/1
**Purpose**: Succeeds at most once (finds first solution only).

**When to use**: Use to make non-deterministic predicates deterministic.

```prolog
% Without once/1 - multiple solutions
?- member(X, [a, b, c]).
X = a ;
X = b ;
X = c.

% With once/1 - first solution only
?- once(member(X, [a, b, c])).
X = a.

% Practical example: Find first matching record
find_first_adult(People, Adult) :-
    once((member(person(Name, Age), People), Age >= 18)),
    Adult = Name.

?- find_first_adult([person(tom, 12), person(jane, 20), person(bob, 25)], A).
A = jane.  % Only returns first adult, not all

% Optimization: Prevent unnecessary backtracking
expensive_check(X) :-
    once(complex_computation(X, Result)),
    Result > threshold.

% Ensure deterministic behavior
get_default(Key, Value) :-
    once((lookup(Key, Value) ; Value = default)).
```

### repeat/0
**Purpose**: Always succeeds and provides infinite choice points.

**When to use**: Use to create loops that retry on failure.

```prolog
% repeat/0 always succeeds and creates a choice point
% On backtracking, it succeeds again (infinitely)

% Interactive menu example
menu :-
    repeat,
    write('1. Option A'), nl,
    write('2. Option B'), nl,
    write('3. Quit'), nl,
    write('Enter choice: '),
    read(Choice),
    process_choice(Choice),
    Choice = 3,  % Exit condition
    !.

process_choice(1) :- write('You chose A'), nl.
process_choice(2) :- write('You chose B'), nl.
process_choice(3) :- write('Goodbye'), nl.
process_choice(_) :- write('Invalid choice'), nl.

% Read until valid input
get_positive_number(N) :-
    repeat,
    write('Enter a positive number: '),
    read(N),
    (   number(N), N > 0
    ->  !  % Cut to stop repeating
    ;   write('Invalid input, try again'), nl,
        fail  % Force backtrack to repeat
    ).
```

### forall/2
**Purpose**: Succeeds if Action succeeds for all solutions of Condition.

**When to use**: Use to verify universal conditions or perform actions on all solutions.

```prolog
% Syntax: forall(Condition, Action)
% Succeeds if Action succeeds for every solution of Condition

% Check if all elements satisfy a condition
?- forall(member(X, [2, 4, 6, 8]), X mod 2 =:= 0).
true.  % All are even

?- forall(member(X, [2, 4, 5, 8]), X mod 2 =:= 0).
false.  % 5 is not even

% Practical example: Validate all fields in a form
validate_form(Form) :-
    forall(member(field(Name, Value), Form),
           validate_field(Name, Value)).

validate_field(age, Value) :- number(Value), Value > 0, Value < 150.
validate_field(name, Value) :- atom(Value), Value \= ''.
validate_field(email, Value) :- atom(Value), sub_atom(Value, _, _, _, '@').

% Print all solutions
print_all_solutions(Goal) :-
    forall(Goal, (write(Goal), nl)).

?- print_all_solutions(between(1, 5, X)).
between(1, 5, 1)
between(1, 5, 2)
between(1, 5, 3)
between(1, 5, 4)
between(1, 5, 5)
true.

% Check database consistency
check_parent_child_consistency :-
    forall(parent(P, C), child(C, P)).
```

### ignore/1
**Purpose**: Always succeeds, whether the goal succeeds or fails.

**When to use**: Use for optional operations that shouldn't stop execution if they fail.

```prolog
% ignore/1 tries to execute the goal but always succeeds

% Example: Optional logging
process_data(Data) :-
    validate(Data),
    ignore(log_to_file(Data)),  % Don't fail if logging fails
    compute_result(Data, Result),
    display(Result).

% Optional cleanup
cleanup_resources :-
    ignore(close_file(F1)),
    ignore(close_connection(C1)),
    ignore(free_memory(M1)).

% Try to set optional configuration
initialize :-
    set_required_config,
    ignore(set_optional_feature(feature1)),
    ignore(set_optional_feature(feature2)),
    start_system.
```

---

## 6. Meta-Predicates

Meta-predicates operate on other predicates, enabling powerful programming techniques like finding all solutions, applying predicates to collections, and dynamic execution.

### call/1
**Purpose**: Executes a goal constructed at runtime.

**When to use**: Use for dynamic predicate execution, higher-order programming, or when the predicate to execute is determined at runtime.

```prolog
% Basic usage: Execute a goal
?- call(append([1], [2], X)).
X = [1, 2].

% Call with a variable goal
?- Goal = member(X, [a, b, c]), call(Goal).
Goal = member(a, [a, b, c]), X = a ;
Goal = member(b, [a, b, c]), X = b ;
Goal = member(c, [a, b, c]), X = c.

% Practical example: Generic filter predicate
filter([], _, []).
filter([H|T], Predicate, [H|Filtered]) :-
    call(Predicate, H),
    !,
    filter(T, Predicate, Filtered).
filter([_|T], Predicate, Filtered) :-
    filter(T, Predicate, Filtered).

% Define some test predicates
positive(X) :- number(X), X > 0.
even(X) :- number(X), 0 is X mod 2.

?- filter([1, -2, 3, -4, 5], positive, Result).
Result = [1, 3, 5].

?- filter([1, 2, 3, 4, 5, 6], even, Result).
Result = [2, 4, 6].

% Higher-order predicate: apply operation to list
map([], _, []).
map([H|T], Operation, [R|Results]) :-
    call(Operation, H, R),
    map(T, Operation, Results).

double(X, Y) :- Y is X * 2.
square(X, Y) :- Y is X * X.

?- map([1, 2, 3], double, Result).
Result = [2, 4, 6].

?- map([1, 2, 3, 4], square, Result).
Result = [1, 4, 9, 16].
```

### findall/3
**Purpose**: Collects all solutions to a goal into a list.

**When to use**: Use when you need all possible solutions collected together.

```prolog
% Syntax: findall(Template, Goal, List)
% Finds all solutions where Goal succeeds and collects Template values

% Basic example
?- findall(X, member(X, [a, b, c]), List).
List = [a, b, c].

% Collect specific information
?- findall(Name, person(Name, Age, _), Names).
Names = [john, mary, bob].  % Assuming those facts exist

% Practical example: Database queries
% Assume we have facts: employee(Name, Department, Salary)
employee(john, sales, 50000).
employee(mary, it, 60000).
employee(bob, sales, 55000).
employee(alice, it, 65000).

% Find all employees in IT
it_employees(Employees) :-
    findall(Name, employee(Name, it, _), Employees).

?- it_employees(E).
E = [mary, alice].

% Collect complex terms
high_earners(Earners) :-
    findall(
        emp(Name, Salary),
        (employee(Name, _, Salary), Salary > 55000),
        Earners
    ).

?- high_earners(E).
E = [emp(mary, 60000), emp(alice, 65000)].

% Calculate statistics
average_salary(Avg) :-
    findall(Salary, employee(_, _, Salary), Salaries),
    sum_list(Salaries, Total),
    length(Salaries, Count),
    Avg is Total / Count.

% Important: findall/3 returns [] if no solutions found
?- findall(X, member(X, []), List).
List = [].  % Empty list, not failure
```

### bagof/3
**Purpose**: Collects solutions like findall/3 but respects variable bindings.

**When to use**: Use when you want solutions grouped by free variables.

```prolog
% bagof/3 is like findall/3 but treats free variables differently

% With these facts:
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

% findall/3 collects all solutions
?- findall(Child, parent(_, Child), Children).
Children = [bob, liz, ann, pat, jim].

% bagof/3 groups by free variables
?- bagof(Child, parent(Parent, Child), Children).
Parent = bob, Children = [ann, pat] ;
Parent = pat, Children = [jim] ;
Parent = tom, Children = [bob, liz].

% Use ^ to existentially quantify variables (ignore them)
?- bagof(Child, Parent^parent(Parent, Child), Children).
Children = [bob, liz, ann, pat, jim].  % Like findall now

% Practical example: Group products by category
product(electronics, laptop, 1000).
product(electronics, phone, 500).
product(clothing, shirt, 30).
product(clothing, pants, 50).
product(food, apple, 2).
product(food, bread, 3).

products_by_category(Category, Products) :-
    bagof(Product, product(Category, Product, _), Products).

?- products_by_category(Cat, Prods).
Cat = electronics, Prods = [laptop, phone] ;
Cat = clothing, Prods = [shirt, pants] ;
Cat = food, Prods = [apple, bread].
```

### setof/3
**Purpose**: Like bagof/3 but removes duplicates and sorts results.

**When to use**: Use when you want unique, sorted solutions.

```prolog
% setof/3 = bagof/3 + sort + remove duplicates

% With duplicate data:
likes(mary, food).
likes(mary, wine).
likes(john, wine).
likes(john, wine).  % Duplicate
likes(bob, food).

% bagof/3 keeps duplicates
?- bagof(X, likes(john, X), Things).
Things = [wine, wine].

% setof/3 removes duplicates and sorts
?- setof(X, likes(john, X), Things).
Things = [wine].

% Collect all unique items liked
?- setof(Item, Person^likes(Person, Item), AllItems).
AllItems = [food, wine].  % Sorted and unique

% Practical example: Find unique skills
has_skill(john, python).
has_skill(john, java).
has_skill(mary, python).
has_skill(mary, python).  % Duplicate
has_skill(bob, javascript).
has_skill(bob, java).

unique_skills(Skills) :-
    setof(Skill, Person^has_skill(Person, Skill), Skills).

?- unique_skills(S).
S = [java, javascript, python].  % Alphabetically sorted

% Find people with common skills
people_with_skill(Skill, People) :-
    setof(Person, has_skill(Person, Skill), People).

?- people_with_skill(python, P).
P = [john, mary].  % Sorted list of people
```

---

## 7. Input/Output

I/O predicates handle reading from and writing to files and streams.

### Basic Output

### write/1 and writeln/1
**Purpose**: Output terms to the current output stream.

**When to use**: Use for displaying results, debugging, or user interaction.

```prolog
% write/1 - Output without newline
?- write('Hello'), write(' '), write('World').
Hello World
true.

% writeln/1 - Output with newline
?- writeln('Hello World').
Hello World
true.

% Writing different types of terms
?- write(42), nl, write([a, b, c]), nl, write(person(john, 25)).
42
[a, b, c]
person(john, 25)
true.

% Practical example: Formatted output
display_person(person(Name, Age, City)) :-
    write('Name: '), writeln(Name),
    write('Age: '), writeln(Age),
    write('City: '), writeln(City).

?- display_person(person(john, 25, london)).
Name: john
Age: 25
City: london
true.

% Building formatted messages
error_message(Code, Message) :-
    write('ERROR '), write(Code), write(': '), writeln(Message).

?- error_message(404, 'File not found').
ERROR 404: File not found
true.
```

### nl/0
**Purpose**: Outputs a newline character.

**When to use**: Use to control line breaks in output.

```prolog
% Basic usage
?- write('Line 1'), nl, write('Line 2'), nl.
Line 1
Line 2
true.

% Creating formatted reports
print_header :-
    writeln('=' * 40),  % Note: This would need special handling
    writeln('     REPORT TITLE'),
    writeln('=' * 40),
    nl.

% Spacing output
print_list([]).
print_list([H|T]) :-
    write('  - '), writeln(H),
    print_list(T).

?- print_list([apple, banana, cherry]).
  - apple
  - banana
  - cherry
true.
```

### Basic Input

### read/1
**Purpose**: Reads a Prolog term from input (must end with period).

**When to use**: Use for reading structured Prolog data.

```prolog
% Interactive reading
get_user_fact :-
    write('Enter a fact (end with .): '),
    read(Fact),
    assertz(Fact),
    write('Fact added: '), writeln(Fact).

% Example interaction:
?- get_user_fact.
Enter a fact (end with .): likes(john, pizza).
Fact added: likes(john, pizza)
true.

% Read and process commands
command_loop :-
    repeat,
    write('Command> '),
    read(Cmd),
    process_command(Cmd),
    Cmd = quit,
    !.

process_command(quit) :- writeln('Goodbye!').
process_command(help) :- writeln('Available commands: help, list, quit').
process_command(list) :- listing.
process_command(_) :- writeln('Unknown command. Type help for assistance.').
```

### Character I/O

### get_char/1 and put_char/1
**Purpose**: Read or write single characters.

**When to use**: Use for character-by-character processing.

```prolog
% Read a single character
?- get_char(C).
% User types: a
C = 'a'.

% Write a single character
?- put_char('X').
X
true.

% Read password with echo off (conceptual example)
read_password(Password) :-
    read_password_chars([], Password).

read_password_chars(Acc, Password) :-
    get_char(C),
    (   C = '\n'
    ->  reverse(Acc, Password)
    ;   put_char('*'),  % Echo asterisk instead
        read_password_chars([C|Acc], Password)
    ).
```

### File I/O

### open/3 and close/1
**Purpose**: Open and close file streams.

**When to use**: Use for file-based I/O operations.

```prolog
% Open file for reading
read_file_terms(Filename, Terms) :-
    open(Filename, read, Stream),
    read_all_terms(Stream, Terms),
    close(Stream).

read_all_terms(Stream, []) :-
    at_end_of_stream(Stream), !.
read_all_terms(Stream, [Term|Terms]) :-
    read(Stream, Term),
    read_all_terms(Stream, Terms).

% Open file for writing
write_to_file(Filename, Data) :-
    open(Filename, write, Stream),
    write(Stream, Data),
    write(Stream, '.'),  % Add period for Prolog term
    nl(Stream),
    close(Stream).

% Append to file
append_to_log(Message) :-
    open('log.txt', append, Stream),
    get_time(Time),
    write(Stream, Time), write(Stream, ': '),
    writeln(Stream, Message),
    close(Stream).
```

---

## 8. Database Operations

Database operations allow dynamic modification of the Prolog knowledge base at runtime.

### Understanding the Dynamic Database

Prolog's database can be modified during program execution:
- Add new facts and rules with `assert` family
- Remove facts and rules with `retract` family
- Query what's currently in the database with `listing`

### assert/1, asserta/1, assertz/1
**Purpose**: Add facts or rules to the database.
- `assert/1` and `assertz/1` add at the end
- `asserta/1` adds at the beginning

**When to use**: Use to store runtime data, learn new information, or build dynamic knowledge bases.

```prolog
% Add a simple fact
?- assertz(likes(john, pizza)).
true.

?- likes(john, X).
X = pizza.

% Add a rule
?- assertz((parent(X, Y) :- father(X, Y))).
true.

% Difference between asserta and assertz
?- assertz(color(red)).
true.
?- assertz(color(blue)).
true.
?- asserta(color(green)).  % Added at beginning
true.

?- color(X).
X = green ;  % First (added with asserta)
X = red ;    % Second
X = blue.    % Third

% Practical example: Learning system
learn_fact :-
    write('What did you learn? '), 
    read(Fact),
    (   \+ Fact  % Check if not already known
    ->  assertz(Fact),
        writeln('I learned something new!')
    ;   writeln('I already knew that!')
    ).

% Cache computation results
fibonacci_cached(N, Result) :-
    (   fib_cache(N, Result)  % Check cache first
    ->  true
    ;   calculate_fibonacci(N, Result),
        assertz(fib_cache(N, Result))  % Cache the result
    ).
```

### retract/1 and retractall/1
**Purpose**: Remove facts or rules from the database.
- `retract/1` removes first matching clause
- `retractall/1` removes all matching clauses

**When to use**: Use to remove outdated information, clean up temporary data, or implement undo functionality.

```prolog
% Remove a specific fact
?- assertz(temp(1)), assertz(temp(2)), assertz(temp(3)).
true.

?- retract(temp(2)).
true.

?- temp(X).
X = 1 ;
X = 3.

% Remove all matching facts
?- retractall(temp(_)).
true.

?- temp(X).
false.  % All removed

% Practical example: Session management
login(User) :-
    retractall(current_user(_)),  % Remove any existing login
    assertz(current_user(User)),
    write('Logged in as: '), writeln(User).

logout :-
    retractall(current_user(_)),
    writeln('Logged out').

% Update a fact (retract old, assert new)
update_score(Player, NewScore) :-
    retractall(score(Player, _)),
    assertz(score(Player, NewScore)).

% Temporary facts with cleanup
with_temp_fact(Fact, Goal) :-
    assertz(Fact),
    call(Goal),
    retract(Fact).
```

### abolish/1
**Purpose**: Removes all clauses of a predicate.

**When to use**: Use to completely remove a predicate definition.

```prolog
% Remove entire predicate
?- assertz(test(1)), assertz(test(2)), assertz((test(X) :- X > 10)).
true.

?- abolish(test/1).  % Remove all clauses of test/1
true.

?- test(X).
ERROR: Undefined predicate: test/1

% Practical example: Reset game state
reset_game :-
    abolish(player_position/2),
    abolish(player_score/2),
    abolish(game_object/3),
    initialize_game.

% Clear all temporary predicates
cleanup_temp :-
    abolish(temp/1),
    abolish(cache/2),
    abolish(session/1).
```

### listing/0 and listing/1
**Purpose**: Display current database contents.

**When to use**: Use for debugging, inspecting dynamic predicates, or showing current state.

```prolog
% List everything
?- listing.
% Shows all user-defined predicates

% List specific predicate
?- listing(likes/2).
likes(john, pizza).
likes(mary, wine).

% List predicates by name (all arities)
?- listing(parent).
parent(tom, bob).
parent(bob, ann).
parent(X, Y) :- father(X, Y).
parent(X, Y) :- mother(X, Y).

% Practical example: Show current configuration
show_config :-
    writeln('Current Configuration:'),
    writeln('====================='),
    listing(config/2),
    listing(option/1).
```

### current_predicate/1
**Purpose**: Check or enumerate defined predicates.

**When to use**: Use to check if predicates exist or list available predicates.

```prolog
% Check if predicate exists
?- current_predicate(member/2).
true.

?- current_predicate(nonexistent/3).
false.

% Enumerate predicates
?- current_predicate(Name/2).
Name = append ;
Name = member ;
Name = select ;
...

% Find all predicates with specific arity
?- findall(P, current_predicate(P/3), Predicates).
Predicates = [append/3, select/3, nth0/3, ...].

% Practical example: Safe predicate call
safe_call_predicate(Name, Arity, Args) :-
    current_predicate(Name/Arity),
    length(Args, Arity),
    Goal =.. [Name|Args],
    call(Goal).
safe_call_predicate(Name, Arity, _) :-
    \+ current_predicate(Name/Arity),
    write('Undefined predicate: '), write(Name/Arity), nl,
    fail.
```

---

## 9. Atom and String Operations

These predicates manipulate atoms (symbolic constants) and strings.

### atom_length/2
**Purpose**: Determines the length of an atom.

**When to use**: Use for validation, formatting, or string processing.

```prolog
% Get length of atom
?- atom_length(hello, Len).
Len = 5.

?- atom_length('Hello World', Len).
Len = 11.

% Validate input length
validate_username(Username) :-
    atom(Username),
    atom_length(Username, Len),
    Len >= 3,
    Len =< 20,
    !.
validate_username(_) :-
    writeln('Username must be 3-20 characters'),
    fail.

% Pad atom to specific length
pad_atom(Atom, TargetLen, PadChar, Padded) :-
    atom_length(Atom, CurrentLen),
    PadCount is TargetLen - CurrentLen,
    (   PadCount =< 0
    ->  Padded = Atom
    ;   create_padding(PadCount, PadChar, Padding),
        atom_concat(Atom, Padding, Padded)
    ).
```

### atom_concat/3
**Purpose**: Concatenates atoms or splits an atom.

**When to use**: Use for building identifiers, messages, or parsing.

```prolog
% Concatenate atoms
?- atom_concat(hello, world, Result).
Result = helloworld.

?- atom_concat('Hello ', 'World', Result).
Result = 'Hello World'.

% Split atom (finding possible splits)
?- atom_concat(Prefix, Suffix, helloworld).
Prefix = '', Suffix = helloworld ;
Prefix = h, Suffix = elloworld ;
Prefix = he, Suffix = lloworld ;
...

% Find specific prefix/suffix
?- atom_concat(hello, Suffix, helloworld).
Suffix = world.

?- atom_concat(Prefix, world, helloworld).
Prefix = hello.

% Practical example: Build file paths
build_path(Dir, File, Path) :-
    atom_concat(Dir, '/', Temp),
    atom_concat(Temp, File, Path).

?- build_path('/home/user', 'file.txt', Path).
Path = '/home/user/file.txt'.

% Generate unique identifiers
generate_id(Base, Counter, ID) :-
    atom_number(CounterAtom, Counter),
    atom_concat(Base, '_', Temp),
    atom_concat(Temp, CounterAtom, ID).

?- generate_id(user, 42, ID).
ID = user_42.
```

### sub_atom/5
**Purpose**: Extracts substrings from atoms.

**When to use**: Use for parsing, pattern matching, or string manipulation.

```prolog
% Syntax: sub_atom(+Atom, ?Before, ?Length, ?After, ?SubAtom)
% Before: characters before the subatom
% Length: length of the subatom
% After: characters after the subatom

% Extract substring
?- sub_atom(helloworld, 5, 5, 0, Sub).
Sub = world.

?- sub_atom(helloworld, 0, 5, _, Sub).
Sub = hello.

% Find position of substring
?- sub_atom(helloworld, Before, 5, After, world).
Before = 5, After = 0.

% Check if atom contains substring
contains_substring(Atom, Sub) :-
    sub_atom(Atom, _, _, _, Sub).

?- contains_substring('hello world', world).
true.

% Practical example: Extract file extension
get_extension(Filename, Ext) :-
    sub_atom(Filename, Before, 1, After, '.'),
    After > 0,  % Ensure dot is not at the end
    !,
    sub_atom(Filename, _, After, 0, Ext).

?- get_extension('document.pdf', Ext).
Ext = pdf.

% Parse email address
parse_email(Email, User, Domain) :-
    sub_atom(Email, Before, 1, After, '@'),
    sub_atom(Email, 0, Before, _, User),
    sub_atom(Email, _, After, 0, Domain).

?- parse_email('john@example.com', U, D).
U = john, D = 'example.com'.
```

### atom_chars/2 and atom_codes/2
**Purpose**: Convert between atoms and character/code lists.

**When to use**: Use for character-level processing or encoding conversions.

```prolog
% atom_chars/2 - Convert to/from character list
?- atom_chars(hello, Chars).
Chars = [h, e, l, l, o].

?- atom_chars(Atom, [h, e, l, l, o]).
Atom = hello.

% atom_codes/2 - Convert to/from ASCII codes
?- atom_codes(hello, Codes).
Codes = [104, 101, 108, 108, 111].

?- atom_codes(Atom, [72, 69, 76, 76, 79]).
Atom = 'HELLO'.

% Practical example: Reverse an atom
reverse_atom(Atom, Reversed) :-
    atom_chars(Atom, Chars),
    reverse(Chars, RevChars),
    atom_chars(Reversed, RevChars).

?- reverse_atom(hello, Rev).
Rev = olleh.

% Simple encryption (Caesar cipher)
caesar_cipher(Text, Shift, Encrypted) :-
    atom_codes(Text, Codes),
    maplist(shift_code(Shift), Codes, ShiftedCodes),
    atom_codes(Encrypted, ShiftedCodes).

shift_code(Shift, Code, Shifted) :-
    Shifted is Code + Shift.

?- caesar_cipher(abc, 1, Encrypted).
Encrypted = bcd.
```

### atom_number/2
**Purpose**: Converts between atoms and numbers.

**When to use**: Use for parsing numeric input or formatting numbers.

```prolog
% Convert atom to number
?- atom_number('42', N).
N = 42.

?- atom_number('3.14', N).
N = 3.14.

% Convert number to atom
?- atom_number(A, 42).
A = '42'.

% Practical example: Parse numeric input
parse_number(Input, Number) :-
    atom(Input),
    atom_number(Input, Number),
    !.
parse_number(Input, _) :-
    write('Invalid number: '), write(Input), nl,
    fail.

% Calculate from string expression
calculate_string(Expr, Result) :-
    atom_number(Expr, Result).  % Works for simple numbers

?- calculate_string('123', R).
R = 123.

% Format numbers with specific precision (conceptual)
format_currency(Amount, Formatted) :-
    Round is round(Amount * 100) / 100,
    atom_number(AtomAmount, Round),
    atom_concat('$', AtomAmount, Formatted).

?- format_currency(42.3456, F).
F = '$42.35'.
```

### String Processing

### split_string/4
**Purpose**: Splits strings by separators with padding removal.

**When to use**: Use for parsing CSV, processing user input, or tokenization.

```prolog
% Syntax: split_string(+String, +Separators, +PadChars, -SubStrings)

% Basic splitting
?- split_string("apple,banana,cherry", ",", "", L).
L = [apple, banana, cherry].

% Multiple separators
?- split_string("apple;banana,cherry:date", ";,:", "", L).
L = [apple, banana, cherry, date].

% Remove padding (spaces)
?- split_string("  apple , banana , cherry  ", ",", " ", L).
L = [apple, banana, cherry].

% Practical example: Parse CSV line
parse_csv_line(Line, Fields) :-
    split_string(Line, ",", " \t", Fields).

?- parse_csv_line("John, 25, London", Fields).
Fields = ['John', '25', 'London'].

% Parse configuration line
parse_config(Line, Key, Value) :-
    split_string(Line, "=", " ", [Key, Value]).

?- parse_config("username = john_doe", K, V).
K = username, V = john_doe.

% Tokenize sentence
tokenize(Sentence, Tokens) :-
    split_string(Sentence, " .,!?", " ", Tokens).

?- tokenize("Hello, world! How are you?", T).
T = ['Hello', 'world', 'How', 'are', 'you'].
```

### atomic_list_concat/3
**Purpose**: Joins atoms with separators or splits by separator.

**When to use**: Use for building formatted strings or parsing.

```prolog
% Join atoms with separator
?- atomic_list_concat([hello, world], ' ', Result).
Result = 'hello world'.

?- atomic_list_concat([one, two, three], '-', Result).
Result = 'one-two-three'.

% Split by separator
?- atomic_list_concat(List, '-', 'one-two-three').
List = [one, two, three].

% Join without separator (use atomic_list_concat/2)
?- atomic_list_concat([hello, world], Result).
Result = helloworld.

% Practical example: Build SQL query
build_insert(Table, Values, Query) :-
    atomic_list_concat(Values, ', ', ValueString),
    atomic_list_concat(['INSERT INTO ', Table, ' VALUES (', ValueString, ')'], Query).

?- build_insert(users, ['John', 25, 'London'], Q).
Q = 'INSERT INTO users VALUES (John, 25, London)'.

% Create formatted message
format_message(Template, Args, Message) :-
    atomic_list_concat(Args, ', ', ArgString),
    atomic_list_concat([Template, ': ', ArgString], Message).

?- format_message('Error', [404, 'Not Found'], Msg).
Msg = 'Error: 404, Not Found'.
```

---

## 10. Character Processing

Character processing predicates work with individual characters and their properties.

### char_code/2
**Purpose**: Converts between characters and their numeric codes.

**When to use**: Use for character encoding, ASCII operations, or character arithmetic.

```prolog
% Character to code
?- char_code(a, Code).
Code = 97.

?- char_code('A', Code).
Code = 65.

% Code to character
?- char_code(Char, 65).
Char = 'A'.

?- char_code(Char, 97).
Char = a.

% Practical example: Check character type
is_uppercase(Char) :-
    char_code(Char, Code),
    Code >= 65,
    Code =< 90.

is_lowercase(Char) :-
    char_code(Char, Code),
    Code >= 97,
    Code =< 122.

is_digit(Char) :-
    char_code(Char, Code),
    Code >= 48,
    Code =< 57.

?- is_uppercase('A').
true.

?- is_digit('5').
true.

% Convert case
to_uppercase(Lower, Upper) :-
    is_lowercase(Lower),
    char_code(Lower, LowerCode),
    UpperCode is LowerCode - 32,
    char_code(Upper, UpperCode).

?- to_uppercase(a, U).
U = 'A'.
```

### char_type/2
**Purpose**: Classifies characters into categories.

**When to use**: Use for parsing, validation, or text processing.

```prolog
% Check character type
?- char_type(a, alpha).
true.

?- char_type('5', digit).
true.

?- char_type(' ', space).
true.

% Find all types of a character
?- char_type('A', Type).
Type = alnum ;    % Alphanumeric
Type = alpha ;    % Alphabetic
Type = ascii ;    % ASCII character
Type = upper ;    % Uppercase
...

% Character type categories:
% - alnum: alphanumeric
% - alpha: alphabetic
% - ascii: ASCII character
% - cntrl: control character
% - digit: decimal digit
% - graph: graphical character
% - lower: lowercase letter
% - print: printable character
% - punct: punctuation
% - space: whitespace
% - upper: uppercase letter
% - xdigit: hexadecimal digit

% Practical example: Validate password
validate_password(Password) :-
    atom_chars(Password, Chars),
    length(Chars, Len),
    Len >= 8,
    member(Upper, Chars), char_type(Upper, upper),
    member(Lower, Chars), char_type(Lower, lower),
    member(Digit, Chars), char_type(Digit, digit),
    !.

% Extract specific character types
extract_digits(Text, Digits) :-
    atom_chars(Text, Chars),
    findall(D, (member(D, Chars), char_type(D, digit)), Digits).

?- extract_digits('abc123def456', D).
D = ['1', '2', '3', '4', '5', '6'].

% Remove non-alphabetic characters
clean_text(Input, Cleaned) :-
    atom_chars(Input, Chars),
    findall(C, (member(C, Chars), char_type(C, alpha)), CleanChars),
    atom_chars(Cleaned, CleanChars).

?- clean_text('hello123world!', C).
C = helloworld.
```

### upcase_atom/2 and downcase_atom/2
**Purpose**: Convert atom case.

**When to use**: Use for normalization, case-insensitive comparisons, or formatting.

```prolog
% Convert to uppercase
?- upcase_atom(hello, Upper).
Upper = 'HELLO'.

?- upcase_atom('Hello World', Upper).
Upper = 'HELLO WORLD'.

% Convert to lowercase
?- downcase_atom('HELLO', Lower).
Lower = hello.

?- downcase_atom('Hello World', Lower).
Lower = 'hello world'.

% Practical example: Case-insensitive comparison
equal_ignore_case(Atom1, Atom2) :-
    downcase_atom(Atom1, Lower1),
    downcase_atom(Atom2, Lower2),
    Lower1 = Lower2.

?- equal_ignore_case('Hello', 'HELLO').
true.

?- equal_ignore_case('Hello', 'World').
false.

% Normalize input
normalize_command(Input, Command) :-
    downcase_atom(Input, Lower),
    atom_string(Lower, TrimmedString),
    atom_string(Command, TrimmedString).

% Create formatted headers
format_header(Text, Header) :-
    upcase_atom(Text, Upper),
    atom_concat('=== ', Upper, Temp),
    atom_concat(Temp, ' ===', Header).

?- format_header('section title', H).
H = '=== SECTION TITLE ==='.
```

---

## 11. DCG (Grammar) Predicates

Definite Clause Grammars provide a high-level notation for parsing and generating sequences.

### Understanding DCGs

DCGs are a syntactic extension for writing parsers and generators:
- DCG rules use `-->` instead of `:-`
- Automatically handle list processing with difference lists
- Can embed Prolog goals using `{}`

```prolog
% Simple DCG rule
noun --> [cat].
noun --> [dog].

% Is translated internally to:
% noun([cat|Rest], Rest).
% noun([dog|Rest], Rest).
```

### phrase/2
**Purpose**: Executes DCG rules for parsing or generation.

**When to use**: Use to parse input with grammar rules or generate valid sequences.

```prolog
% Define a simple grammar
article --> [the].
article --> [a].
noun --> [cat].
noun --> [dog].
verb --> [chases].
verb --> [sees].

noun_phrase --> article, noun.
verb_phrase --> verb, noun_phrase.
sentence --> noun_phrase, verb_phrase.

% Parse with phrase/2
?- phrase(noun, [cat]).
true.

?- phrase(noun_phrase, [the, dog]).
true.

?- phrase(sentence, [the, cat, chases, a, dog]).
true.

% Generate valid sentences
?- phrase(sentence, S).
S = [the, cat, chases, the, cat] ;
S = [the, cat, chases, the, dog] ;
S = [the, cat, chases, a, cat] ;
...

% Practical example: Number parser
digit(D) --> [D], { char_type(D, digit) }.

digits([D|Ds]) --> digit(D), digits(Ds).
digits([D]) --> digit(D).

number(N) --> 
    digits(Ds),
    { atom_chars(Atom, Ds), atom_number(Atom, N) }.

?- phrase(number(N), ['1', '2', '3']).
N = 123.
```

### enhanced_phrase/2 and enhanced_phrase/3
**Purpose**: Enhanced DCG parsing with ISO/IEC DTS 13211-3 compliance.

**When to use**: Use for advanced parsing with complex grammars.

```prolog
% Enhanced parsing with complex control structures
expression --> term, expression_rest.
expression_rest --> ['+'], term, expression_rest.
expression_rest --> ['-'], term, expression_rest.
expression_rest --> [].

term --> factor, term_rest.
term_rest --> ['*'], factor, term_rest.
term_rest --> ['/'], factor, term_rest.
term_rest --> [].

factor --> ['('], expression, [')'].
factor --> number.

% Parse arithmetic expression
?- enhanced_phrase(expression, ['2', '*', '(', '3', '+', '4', ')']).
true.

% With remainder extraction
?- enhanced_phrase(expression, ['2', '+', '3', ';', 'rest'], Rest).
Rest = [';', 'rest'].

% Practical example: Configuration parser
config_line --> 
    key(K), 
    spaces, 
    ['='], 
    spaces, 
    value(V),
    { assertz(config(K, V)) }.

key(K) --> identifier(K).
value(V) --> quoted_string(V).
value(V) --> identifier(V).
value(V) --> number(V).

spaces --> [' '], spaces.
spaces --> [].

identifier(ID) --> 
    [C], 
    { char_type(C, alpha) },
    id_rest(Cs),
    { atom_chars(ID, [C|Cs]) }.

id_rest([C|Cs]) --> 
    [C], 
    { char_type(C, alnum) },
    id_rest(Cs).
id_rest([]) --> [].
```

### phrase_with_options/4
**Purpose**: DCG parsing with advanced control options.

**When to use**: Use when you need parsing with error handling, debugging, or depth limits.

```prolog
% Parse with options
Options = [
    syntax_errors(fail),    % How to handle syntax errors
    max_depth(100),         % Maximum recursion depth
    debug(true),           % Enable debug output
    trace(false)           % Disable tracing
].

% Safe parsing with depth limit
safe_parse(Rule, Input, Result) :-
    phrase_with_options(
        Rule, 
        Input, 
        [], 
        [max_depth(1000), syntax_errors(fail)]
    ),
    Result = success.
safe_parse(_, _, failed).

% Practical example: JSON parser with error handling
json_value --> json_object.
json_value --> json_array.
json_value --> json_string.
json_value --> json_number.
json_value --> json_boolean.
json_value --> json_null.

parse_json(Input, Result) :-
    phrase_with_options(
        json_value,
        Input,
        [],
        [syntax_errors(error), max_depth(100)]
    ),
    Result = valid_json.

% Debug parsing issues
debug_parse(Grammar, Input) :-
    phrase_with_options(
        Grammar,
        Input,
        Rest,
        [debug(true), trace(true)]
    ),
    write('Remaining input: '), writeln(Rest).
```

### call_dcg/3
**Purpose**: Calls DCG rules with explicit difference lists.

**When to use**: Use for meta-programming with DCGs or custom parsing control.

```prolog
% Direct DCG call with explicit lists
?- call_dcg(noun, [cat, runs], Rest).
Rest = [runs].

% Compose DCG rules dynamically
parse_sequence([], Input, Input).
parse_sequence([Rule|Rules], Input, Output) :-
    call_dcg(Rule, Input, Temp),
    parse_sequence(Rules, Temp, Output).

?- parse_sequence([article, noun, verb], [the, cat, runs], Rest).
Rest = [].

% Practical example: Dynamic grammar selection
parse_by_type(number, Input, Rest) :-
    call_dcg(number_parser, Input, Rest).
parse_by_type(word, Input, Rest) :-
    call_dcg(word_parser, Input, Rest).
parse_by_type(symbol, Input, Rest) :-
    call_dcg(symbol_parser, Input, Rest).

% Meta-DCG: Apply DCG rule multiple times
repeat_dcg(_, 0, Input, Input) :- !.
repeat_dcg(Rule, N, Input, Output) :-
    N > 0,
    call_dcg(Rule, Input, Temp),
    N1 is N - 1,
    repeat_dcg(Rule, N1, Temp, Output).

?- repeat_dcg(digit, 3, ['1', '2', '3', '4'], Rest).
Rest = ['4'].
```

### dcg_translate_rule/2
**Purpose**: Translates DCG rules to standard Prolog clauses.

**When to use**: Use for understanding DCG transformation or meta-programming.

```prolog
% See how DCG rules are translated
?- dcg_translate_rule((noun --> [cat]), Clause).
Clause = (noun([cat|S], S)).

?- dcg_translate_rule((noun_phrase --> article, noun), Clause).
Clause = (noun_phrase(S0, S) :- article(S0, S1), noun(S1, S)).

% Complex translation with embedded goals
?- dcg_translate_rule(
    (number(N) --> digits(D), { atom_number(D, N) }),
    Clause
).
Clause = (number(N, S0, S) :- 
    digits(D, S0, S), 
    atom_number(D, N)
).

% Practical example: Generate parser predicates
generate_parser(Grammar, Predicates) :-
    findall(Pred, 
            (member(Rule, Grammar),
             dcg_translate_rule(Rule, Pred)),
            Predicates).

% Analyze grammar complexity
analyze_dcg(Rule) :-
    dcg_translate_rule(Rule, Clause),
    Clause = (Head :- Body),
    functor(Head, Name, Arity),
    write('DCG rule: '), write(Name),
    write(' with arity '), write(Arity), nl,
    write('Translates to: '), write(Clause), nl.
```

---

## 12. Exception Handling

Exception handling provides robust error management and recovery mechanisms.

### throw/1
**Purpose**: Throws an exception to be caught by an enclosing catch/3.

**When to use**: Use to signal errors or exceptional conditions.

```prolog
% Throw simple exception
divide(_, 0, _) :- 
    throw(division_by_zero).
divide(X, Y, Result) :- 
    Result is X / Y.

?- divide(10, 0, R).
ERROR: Uncaught exception: division_by_zero

% Throw ISO error terms
validate_positive(X) :-
    (   var(X)
    ->  throw(error(instantiation_error, validate_positive/1))
    ;   \+ number(X)
    ->  throw(error(type_error(number, X), validate_positive/1))
    ;   X =< 0
    ->  throw(error(domain_error(positive_number, X), validate_positive/1))
    ;   true
    ).

% Practical example: File operations with error handling
read_file_safe(Filename, Content) :-
    (   \+ exists_file(Filename)
    ->  throw(error(existence_error(file, Filename), read_file_safe/2))
    ;   \+ access_file(Filename, read)
    ->  throw(error(permission_error(read, file, Filename), read_file_safe/2))
    ;   read_file_to_string(Filename, Content)
    ).

% Custom error types
process_data(Data) :-
    (   validate_format(Data)
    ->  transform_data(Data)
    ;   throw(custom_error(invalid_format, Data))
    ).
```

### catch/3
**Purpose**: Catches exceptions thrown by goals.

**When to use**: Use to handle errors gracefully and implement recovery strategies.

```prolog
% Syntax: catch(+Goal, +Catcher, +Recovery)

% Basic exception catching
safe_divide(X, Y, Result) :-
    catch(
        Result is X / Y,
        error(evaluation_error(zero_divisor), _),
        (write('Division by zero!'), nl, fail)
    ).

?- safe_divide(10, 0, R).
Division by zero!
false.

% Catch specific exceptions
process_with_retry(Data, Result) :-
    catch(
        process_data(Data, Result),
        network_error(timeout),
        (   writeln('Network timeout, retrying...'),
            sleep(1),
            process_with_retry(Data, Result)
        )
    ).

% Multiple exception types
robust_operation(Input, Output) :-
    catch(
        dangerous_operation(Input, Output),
        Error,
        handle_error(Error)
    ).

handle_error(error(type_error(_, _), _)) :-
    writeln('Type error: Invalid input type').
handle_error(error(domain_error(_, _), _)) :-
    writeln('Domain error: Value out of range').
handle_error(error(existence_error(_, _), _)) :-
    writeln('Existence error: Resource not found').
handle_error(Error) :-
    write('Unexpected error: '), writeln(Error).

% Practical example: Transaction with rollback
transaction(Actions, Result) :-
    catch(
        (   begin_transaction,
            perform_actions(Actions),
            commit_transaction,
            Result = success
        ),
        Error,
        (   rollback_transaction,
            write('Transaction failed: '), writeln(Error),
            Result = failed(Error)
        )
    ).

% Cleanup with exception safety
with_resource(Resource, Goal) :-
    acquire_resource(Resource),
    catch(
        call(Goal),
        Error,
        (release_resource(Resource), throw(Error))
    ),
    release_resource(Resource).
```

### halt/1
**Purpose**: Terminates the Prolog system with an exit code.

**When to use**: Use to exit the program, typically after fatal errors or completion.

```prolog
% Exit with success
?- halt(0).
% Program terminates with exit code 0

% Exit with error code
fatal_error(Message) :-
    write('FATAL ERROR: '), writeln(Message),
    halt(1).

% Practical example: Command-line application
main :-
    catch(
        run_application,
        Error,
        (   print_error(Error),
            halt(1)
        )
    ),
    halt(0).

run_application :-
    parse_arguments(Args),
    validate_arguments(Args),
    process_command(Args).

% Conditional exit
check_requirements :-
    (   missing_requirement(Req)
    ->  write('Missing requirement: '), writeln(Req),
        halt(2)
    ;   true
    ).
```

---

## 13. System Predicates

System predicates provide access to Prolog system features and configuration.

### current_prolog_flag/2
**Purpose**: Queries system flags and configuration.

**When to use**: Use to check system capabilities or configuration.

```prolog
% Query specific flag
?- current_prolog_flag(version, V).
V = '2.0.15'.

?- current_prolog_flag(bounded, B).
B = true.  % Integers are bounded

% Enumerate all flags
?- current_prolog_flag(Flag, Value).
Flag = bounded, Value = true ;
Flag = max_integer, Value = 9223372036854775807 ;
Flag = min_integer, Value = -9223372036854775808 ;
...

% Check system capabilities
check_unicode_support :-
    current_prolog_flag(encoding, Encoding),
    (   Encoding = utf8
    ->  writeln('Unicode supported')
    ;   writeln('Limited character support')
    ).

% Practical example: Adjust behavior based on flags
get_max_int(Max) :-
    (   current_prolog_flag(bounded, true)
    ->  current_prolog_flag(max_integer, Max)
    ;   Max = inf
    ).

% Check debug mode
is_debug_mode :-
    current_prolog_flag(debug, on).

debug_print(Message) :-
    (   is_debug_mode
    ->  write('[DEBUG] '), writeln(Message)
    ;   true
    ).
```

### set_prolog_flag/2
**Purpose**: Sets system flags (where allowed).

**When to use**: Use to configure system behavior.

```prolog
% Enable debug mode
?- set_prolog_flag(debug, on).
true.

% Set unknown predicate behavior
?- set_prolog_flag(unknown, fail).  % Fail on undefined predicates
true.

% Practical example: Configure application
initialize_app :-
    set_prolog_flag(debug, off),
    set_prolog_flag(unknown, error),
    set_prolog_flag(double_quotes, codes),
    writeln('Application configured').

% Toggle debug mode
toggle_debug :-
    current_prolog_flag(debug, Current),
    (   Current = on
    ->  set_prolog_flag(debug, off),
        writeln('Debug mode disabled')
    ;   set_prolog_flag(debug, on),
        writeln('Debug mode enabled')
    ).

% Set optimization level
set_optimization(Level) :-
    (   member(Level, [0, 1, 2, 3])
    ->  set_prolog_flag(optimize, Level)
    ;   writeln('Invalid optimization level')
    ).
```

### statistics/2
**Purpose**: Queries system statistics and performance metrics.

**When to use**: Use for performance monitoring and optimization.

```prolog
% Query runtime
?- statistics(runtime, [Total, Since]).
Total = 1234,  % Total milliseconds since start
Since = 10.    % Milliseconds since last call

% Memory usage
?- statistics(memory, [Used, Free]).
Used = 5242880,   % Bytes used
Free = 10485760.  % Bytes free

% Practical example: Measure execution time
time_goal(Goal, Time) :-
    statistics(runtime, [Start, _]),
    call(Goal),
    statistics(runtime, [End, _]),
    Time is End - Start.

?- time_goal(sleep(1), T).
T = 1000.  % Milliseconds

% Performance profiling
profile(Goal) :-
    statistics(runtime, [T0, _]),
    statistics(memory, [M0, _]),
    call(Goal),
    statistics(runtime, [T1, _]),
    statistics(memory, [M1, _]),
    Time is T1 - T0,
    Memory is M1 - M0,
    format('Execution time: ~w ms~n', [Time]),
    format('Memory used: ~w bytes~n', [Memory]).

% Monitor resource usage
monitor_resources :-
    repeat,
    statistics(memory, [Used, Free]),
    Total is Used + Free,
    Percentage is (Used * 100) / Total,
    format('Memory usage: ~2f%~n', [Percentage]),
    sleep(5),
    fail.
```

---

## Summary

This comprehensive reference covers JProlog's 80+ built-in predicates organized by their functional purpose. Each predicate includes:

1. **Clear purpose statement** - What the predicate does
2. **Usage guidance** - When and why to use it
3. **Practical examples** - Real-world applications
4. **Multiple modes** - Different ways to use the predicate
5. **Common patterns** - Idiomatic usage in Prolog programs

These predicates form the foundation for Prolog programming, enabling:
- **Data validation** through type checking
- **Complex data manipulation** with term operations
- **Efficient list processing** 
- **Robust error handling** with exceptions
- **Dynamic knowledge management** through database operations
- **Text processing** with atom and string operations
- **Advanced parsing** with DCG support
- **System integration** through I/O and system predicates

Use this reference as a guide for writing robust, efficient, and maintainable Prolog programs.