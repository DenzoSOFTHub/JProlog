# Introduction to Prolog Programming

## Table of Contents

1. [What is Prolog?](#what-is-prolog)
2. [Basic Concepts](#basic-concepts)
3. [Facts and Rules](#facts-and-rules)
4. [Queries and Goals](#queries-and-goals)
5. [Variables and Unification](#variables-and-unification)
6. [Operators and Expressions](#operators-and-expressions)
7. [Lists and Data Structures](#lists-and-data-structures)
8. [Control Structures](#control-structures)
9. [Recursion](#recursion)
10. [Example Programs](#example-programs)
11. [ISO Prolog Standard](#iso-prolog-standard)

---

## What is Prolog?

Prolog (Programming in Logic) is a declarative programming language based on formal logic. Unlike imperative languages that specify *how* to solve a problem, Prolog describes *what* the solution should look like. The Prolog system then uses logical inference to find solutions.

Key characteristics of Prolog:
- **Declarative**: You describe facts and relationships
- **Logic-based**: Built on first-order predicate logic
- **Pattern matching**: Uses unification to match terms
- **Backtracking**: Automatically explores alternative solutions
- **Interactive**: Supports query-driven development

---

## Basic Concepts

### Terms

Everything in Prolog is a **term**. There are four types of terms:

1. **Atoms**: Constant symbols starting with lowercase or quoted
   ```prolog
   hello
   'Hello World'
   abc123
   ```

2. **Numbers**: Integers or floating-point numbers
   ```prolog
   42
   3.14159
   -17
   ```

3. **Variables**: Symbols starting with uppercase or underscore
   ```prolog
   X
   Name
   _Result
   _
   ```

4. **Compound Terms**: Structures with a functor and arguments
   ```prolog
   person(john, 25)
   loves(mary, X)
   point(10, 20)
   ```

### Predicates

A **predicate** is a statement that can be true or false. In Prolog, predicates are expressed as terms:

```prolog
parent(tom, bob)        % "tom is a parent of bob"
likes(mary, pizza)      % "mary likes pizza"
greater(5, 3)          % "5 is greater than 3"
```

---

## Facts and Rules

### Facts

**Facts** are basic statements that are unconditionally true:

```prolog
% Family relationships
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).

% Gender information
male(tom).
male(bob).
female(liz).
female(ann).
female(pat).

% Simple facts
likes(mary, food).
likes(mary, wine).
likes(john, wine).
likes(john, mary).
```

### Rules

**Rules** define relationships using logical implication. A rule has a **head** and a **body** separated by `:-`:

```prolog
% Rule: X is a father of Y if X is a parent of Y and X is male
father(X, Y) :- parent(X, Y), male(X).

% Rule: X is a mother of Y if X is a parent of Y and X is female
mother(X, Y) :- parent(X, Y), female(X).

% Rule: X is a grandparent of Z if X is a parent of Y and Y is a parent of Z
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

% Rule: X and Y are siblings if they have the same parent and are different
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \== Y.
```

### Program Structure

A Prolog program consists of a **knowledge base** (facts and rules):

```prolog
% Knowledge base for family relationships

% Facts about parenthood
parent(abraham, isaac).
parent(isaac, jacob).
parent(jacob, joseph).

% Facts about gender
male(abraham).
male(isaac).
male(jacob).
male(joseph).

% Rules defining relationships
father(X, Y) :- parent(X, Y), male(X).
grandfather(X, Z) :- father(X, Y), parent(Y, Z).
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
```

---

## Queries and Goals

### Simple Queries

**Queries** ask questions about the knowledge base:

```prolog
?- parent(tom, bob).          % Is tom a parent of bob?
true.

?- parent(tom, alice).        % Is tom a parent of alice?
false.

?- male(bob).                 % Is bob male?
true.
```

### Variable Queries

Queries can contain variables to find solutions:

```prolog
?- parent(tom, X).            % Who are tom's children?
X = bob ;
X = liz.

?- parent(X, bob).            % Who are bob's parents?
X = tom.

?- father(X, Y).              % Find all father-child relationships
X = tom, Y = bob ;
X = tom, Y = liz.
```

### Compound Queries

Multiple goals can be combined:

```prolog
?- parent(X, Y), male(X).     % Find all fathers and their children
X = tom, Y = bob ;
X = tom, Y = liz.

?- parent(X, bob), female(X). % Find bob's mothers
false.
```

---

## Variables and Unification

### Variable Binding

Variables become **bound** when unified with values:

```prolog
?- X = hello.
X = hello.

?- point(X, Y) = point(10, 20).
X = 10, Y = 20.
```

### Unification

**Unification** is the process of making two terms identical:

```prolog
% Simple unification
?- f(a, b) = f(a, b).
true.

% Unification with variables
?- f(X, b) = f(a, Y).
X = a, Y = b.

% Complex unification
?- person(Name, Age) = person(john, 25).
Name = john, Age = 25.

% Unification failure
?- f(a, b) = g(a, b).
false.
```

### Anonymous Variables

The underscore `_` represents variables you don't care about:

```prolog
?- parent(tom, _).            % Does tom have any children?
true.

?- parent(_, bob).            % Does bob have any parents?
true.
```

**Important characteristics:**
- Each `_` is a unique variable - multiple `_` in the same clause are independent
- Anonymous variables don't appear in query results
- Useful for pattern matching when you only care about some arguments

```prolog
% Multiple anonymous variables are independent
?- data(a, _), data(b, _).    % Both _ are different variables

% Anonymous variables in list patterns
?- [_, Second, _] = [first, middle, last].
Second = middle.

% Anonymous variables in rule heads and bodies
process(_, Result) :- Result = processed.
has_data(X) :- data(X, _).
```

---

## Operators and Expressions

### Arithmetic Operators

Prolog supports standard arithmetic operators:

```prolog
?- X is 2 + 3.               % Addition
X = 5.

?- Y is 10 - 4.              % Subtraction
Y = 6.

?- Z is 3 * 4.               % Multiplication
Z = 12.

?- W is 15 / 3.              % Division
W = 5.0.

?- R is 17 mod 5.            % Modulo
R = 2.
```

### Comparison Operators

```prolog
?- 5 > 3.                    % Greater than
true.

?- 10 =< 10.                 % Less than or equal
true.

?- 2 + 3 =:= 5.              % Arithmetic equality
true.

?- hello == hello.           % Term identity
true.
```

### Logical Operators

```prolog
% Conjunction (AND)
?- 5 > 3, 10 < 20.
true.

% Disjunction (OR)
?- 5 > 10 ; 3 < 5.
true.

% If-then-else
?- (5 > 3 -> X = positive ; X = negative).
X = positive.
```

---

## Lists and Data Structures

### List Syntax

Lists are fundamental data structures in Prolog:

```prolog
[1, 2, 3, 4]                 % List of numbers
[a, b, c]                    % List of atoms
[hello, 42, world]           % Mixed list
[]                           % Empty list
[Head|Tail]                  % Head and tail notation
```

### List Operations

```prolog
% List membership
?- member(2, [1, 2, 3]).
true.

% List length
?- length([a, b, c], L).
L = 3.

% List concatenation
?- append([1, 2], [3, 4], L).
L = [1, 2, 3, 4].

% List reversal
?- reverse([1, 2, 3], R).
R = [3, 2, 1].
```

### Working with List Structure

```prolog
% Decomposing lists
?- [H|T] = [1, 2, 3, 4].
H = 1, T = [2, 3, 4].

% Pattern matching with lists
first([H|_], H).             % Get first element
last([X], X).                % Base case: single element
last([_|T], X) :- last(T, X). % Recursive case
```

---

## Control Structures

### Cut Operator (!)

The **cut** prevents backtracking:

```prolog
% Without cut - may give unexpected results on backtracking
max(X, Y, X) :- X >= Y.
max(X, Y, Y) :- X < Y.

% With cut - deterministic
max(X, Y, X) :- X >= Y, !.
max(X, Y, Y).
```

### If-Then-Else

Conditional execution using `->` and `;`:

```prolog
% Simple if-then
positive(X) :- X > 0 -> write('positive') ; write('not positive').

% If-then-else in rules
abs_value(X, X) :- X >= 0, !.
abs_value(X, Y) :- Y is -X.

% More complex conditionals
grade(Score, Grade) :-
    (Score >= 90 -> Grade = a ;
     Score >= 80 -> Grade = b ;
     Score >= 70 -> Grade = c ;
     Grade = f).
```

### Negation as Failure

The `\+` operator implements **negation as failure**:

```prolog
% Basic negation
?- \+ fail.
true.

?- \+ true.
false.

% Negation with facts
bird(robin).
bird(sparrow).

?- \+ bird(cat).           % cat is not a bird
true.

?- \+ bird(robin).         % robin is a bird
false.

% Negation in rules
not_a_bird(X) :- \+ bird(X).

% Testing for missing information
unknown_parent(Child) :- 
    person(Child),
    \+ parent(_, Child).
```

**Important notes:**
- `\+` succeeds if the goal **cannot be proven**
- This is not logical negation - it's "failure to prove"
- Variables inside `\+` are not bound in the outer scope
- Useful for closed-world assumption reasoning

### Repeat and Failure

```prolog
% Infinite choice points
process_input :-
    repeat,
    read(Input),
    process(Input),
    Input == stop.

% Explicit failure
impossible :- fail.
```

---

## Recursion

Recursion is fundamental in Prolog programming:

### Simple Recursion

```prolog
% Factorial
factorial(0, 1).
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% Fibonacci
fib(0, 0).
fib(1, 1).
fib(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2.
```

### List Recursion

```prolog
% Sum of list elements
sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, TailSum),
    Sum is H + TailSum.

% Find maximum in list
max_list([X], X).
max_list([H|T], Max) :-
    max_list(T, TailMax),
    (H > TailMax -> Max = H ; Max = TailMax).
```

### Tail Recursion

More efficient recursive patterns:

```prolog
% Tail recursive factorial
factorial(N, F) :- factorial(N, 1, F).

factorial(0, Acc, Acc).
factorial(N, Acc, F) :-
    N > 0,
    N1 is N - 1,
    Acc1 is N * Acc,
    factorial(N1, Acc1, F).

% Tail recursive reverse
reverse(List, Reversed) :- reverse(List, [], Reversed).

reverse([], Acc, Acc).
reverse([H|T], Acc, Reversed) :-
    reverse(T, [H|Acc], Reversed).
```

---

## Example Programs

### Family Tree System

```prolog
% Family relationships database
parent(elizabeth, charles).
parent(elizabeth, anne).
parent(charles, william).
parent(charles, harry).
parent(anne, peter).
parent(anne, zara).

male(charles).
male(william).
male(harry).
male(peter).

female(elizabeth).
female(anne).
female(zara).

% Derived relationships
father(X, Y) :- parent(X, Y), male(X).
mother(X, Y) :- parent(X, Y), female(X).
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \== Y.
uncle(X, Y) :- parent(Z, Y), sibling(X, Z), male(X).
aunt(X, Y) :- parent(Z, Y), sibling(X, Z), female(X).

% Queries:
% ?- grandparent(elizabeth, william).
% ?- sibling(william, harry).
% ?- uncle(X, william).
```

### Simple Expert System

```prolog
% Animal identification expert system
animal(dog) :- has_fur, barks, domesticated.
animal(cat) :- has_fur, meows, domesticated.
animal(lion) :- has_fur, roars, wild, carnivore.
animal(bird) :- has_feathers, flies.
animal(fish) :- lives_in_water, has_gills.

% Facts about specific animals
has_fur :- ask('Does it have fur?').
barks :- ask('Does it bark?').
meows :- ask('Does it meow?').
domesticated :- ask('Is it domesticated?').

% Interactive questioning
ask(Question) :-
    write(Question), write(' (yes/no): '),
    read(Answer),
    Answer == yes.

% Usage: ?- animal(X).
```

### Graph Algorithms

```prolog
% Graph representation
edge(a, b).
edge(b, c).
edge(c, d).
edge(a, d).
edge(b, d).

% Path finding
path(X, Y) :- edge(X, Y).
path(X, Y) :- edge(X, Z), path(Z, Y).

% Path with cycle detection
path(X, Y, Visited) :-
    edge(X, Y),
    \+ member(Y, Visited).
path(X, Y, Visited) :-
    edge(X, Z),
    \+ member(Z, Visited),
    path(Z, Y, [Z|Visited]).

% Shortest path using breadth-first search
shortest_path(Start, End, Path) :-
    breadth_first([[Start]], End, Path).

breadth_first([[End|Path]|_], End, [End|Path]).
breadth_first([Path|Paths], End, ShortestPath) :-
    extend(Path, NewPaths),
    append(Paths, NewPaths, AllPaths),
    breadth_first(AllPaths, End, ShortestPath).

extend([Node|Path], NewPaths) :-
    findall([NewNode, Node|Path],
            (edge(Node, NewNode), \+ member(NewNode, [Node|Path])),
            NewPaths).
```

### Natural Language Processing

```prolog
% Simple grammar
sentence(S) :- noun_phrase(NP), verb_phrase(VP), append(NP, VP, S).
noun_phrase(NP) :- determiner(D), noun(N), append(D, N, NP).
verb_phrase(VP) :- verb(V), noun_phrase(NP), append(V, NP, VP).

% Lexicon
determiner([the]).
determiner([a]).
noun([cat]).
noun([dog]).
noun([mouse]).
verb([chases]).
verb([sees]).

% Usage:
% ?- sentence([the, cat, chases, a, mouse]).
% ?- sentence(S).
```

---

## ISO Prolog Standard

### Core Features

The ISO Prolog standard (ISO/IEC 13211-1:1995) defines:

1. **Syntax and semantics** of Prolog programs
2. **Built-in predicates** that all implementations should support
3. **Error handling** mechanisms
4. **I/O operations** and stream handling
5. **Module system** (in some implementations)

### Standard Predicates

Key categories of ISO Prolog predicates:

**Type Testing:**
- `var/1`, `nonvar/1`, `atom/1`, `number/1`, `integer/1`, `float/1`
- `atomic/1`, `compound/1`, `callable/1`

**Term Comparison:**
- `==/2`, `\\==/2`, `@</2`, `@=</2`, `@>/2`, `@>=/2`

**Arithmetic:**
- `is/2`, `=:=/2`, `=\\=/2`, `</2`, `=</2`, `>/2`, `>=/2`

**Control:**
- `!/0`, `fail/0`, `true/0`, `call/1`, `once/1`

**I/O:**
- `read/1`, `write/1`, `get_char/1`, `put_char/1`

### Compliance Notes

JProlog implements a subset of ISO Prolog, focusing on:
- Core logical operations
- Essential built-in predicates
- Standard arithmetic and comparison
- Basic I/O operations
- List processing predicates

Some advanced ISO features not implemented:
- Full stream I/O system
- Module system
- Constraint handling
- Tabling/memoization
- Some meta-predicates

---

## Best Practices

### Programming Style

1. **Use meaningful names:**
   ```prolog
   % Good
   parent(Mother, Child) :- mother(Mother, Child).
   
   % Avoid
   p(X, Y) :- m(X, Y).
   ```

2. **Write clear rules:**
   ```prolog
   % Good - clear logic flow
   ancestor(X, Y) :- parent(X, Y).
   ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
   
   % Avoid - complex nested conditions
   ```

3. **Use cuts judiciously:**
   ```prolog
   % Good - deterministic when needed
   max(X, Y, X) :- X >= Y, !.
   max(_, Y, Y).
   
   % Avoid - overuse of cuts
   ```

### Debugging Tips

1. **Trace execution** to understand backtracking
2. **Use anonymous variables** for unused parameters
3. **Check base cases** in recursive predicates
4. **Test with simple queries** before complex ones
5. **Understand operator precedence** to avoid surprises

### Performance Considerations

1. **Put most restrictive goals first** in rule bodies
2. **Use tail recursion** when possible
3. **Consider cut placement** to eliminate unnecessary choice points
4. **Index on first argument** for better clause selection

This introduction covers the fundamental concepts of Prolog programming. For specific implementation details and advanced features, consult the JProlog documentation and built-in reference guide.