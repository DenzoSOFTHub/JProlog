# JProlog Built-in Predicates Reference

This document describes all built-in predicates and operators available in JProlog, organized alphabetically by name. Each entry includes description, syntax, examples, and ISO compliance information.

---

## =:=/2

**Category**: arithmetic  
**ISO Compliance**: yes

**Description**: Arithmetic equality comparison. Tests if two arithmetic expressions evaluate to equal numeric values.

**Syntax**: 
```prolog
+Expr1 =:= +Expr2
```

**Arguments**: 
- Expr1: arithmetic expression
- Expr2: arithmetic expression

**Examples**:
```prolog
% Example 1: Basic arithmetic equality
?- 5 =:= 5.
true.

% Example 2: Expression evaluation
?- 2 + 3 =:= 1 + 4.
true.

% Example 3: Using variables
?- X = 10, Y = 5, X =:= Y * 2.
true.

% Example 4: Floating point equality
?- 3.14 =:= 314/100.
true.
```

**Behavior**: Evaluates both expressions as arithmetic terms and compares their numeric values. Both arguments must be ground arithmetic expressions.

**See Also**: =\=/2, is/2, >/2, </2

---

## =\=/2

**Category**: arithmetic  
**ISO Compliance**: yes

**Description**: Arithmetic inequality comparison. Tests if two arithmetic expressions evaluate to different numeric values.

**Syntax**: 
```prolog
+Expr1 =\= +Expr2
```

**Arguments**: 
- Expr1: arithmetic expression
- Expr2: arithmetic expression

**Examples**:
```prolog
% Example 1: Basic arithmetic inequality
?- 5 =\= 3.
true.

% Example 2: Expression evaluation  
?- 2 + 3 =\= 2 * 2.
true.

% Example 3: Variables in expressions
?- X = 7, X =\= 5.
true.

% Example 4: Equal values fail
?- 10 =\= 5 + 5.
false.
```

**Behavior**: Evaluates both expressions as arithmetic terms and succeeds if their numeric values are different. Both arguments must be ground arithmetic expressions.

**See Also**: =:=/2, \=/2, is/2

---

## !/0

**Category**: control  
**ISO Compliance**: yes

**Description**: Cut operator that prevents backtracking beyond the current choice point.

**Syntax**: 
```prolog
!
```

**Arguments**: None

**Examples**:
```prolog
% Example 1: Basic cut usage
max(X, Y, X) :- X >= Y, !.
max(X, Y, Y).

?- max(5, 3, Z).
Z = 5.

% Example 2: Cut prevents alternative solutions
test_cut(X) :- X = 1, !.
test_cut(X) :- X = 2.

?- test_cut(X).
X = 1.
% No backtracking to X = 2
```

**Behavior**: Succeeds immediately and commits to all choices made up to this point. Prevents backtracking to alternative clauses or choice points that were created before the cut.

**See Also**: ->/2, ;/2

---

## ->/2

**Category**: control  
**ISO Compliance**: yes

**Description**: If-then operator for conditional execution. If the condition succeeds, execute the then part.

**Syntax**: 
```prolog
(Condition -> Then)
```

**Arguments**:
- `+Condition`: Condition to test
- `+Then`: Goal to execute if condition succeeds

**Examples**:
```prolog
% Example 1: Simple if-then
?- (5 > 3 -> write(yes)).
yes
true.

% Example 2: If-then fails when condition fails
?- (3 > 5 -> write(yes)).
false.

% Example 3: Used with variables
check_positive(X) :- (X > 0 -> write('positive')).

?- check_positive(5).
positive
true.
```

**Behavior**: Evaluates the condition. If it succeeds, executes the then part with the condition's bindings. If condition fails, the entire construct fails. Typically used within if-then-else (Condition -> Then ; Else).

**See Also**: ;/2, !/0

---

## ;/2

**Category**: control  
**ISO Compliance**: yes

**Description**: Disjunction (OR) operator and if-then-else operator when combined with ->.

**Syntax**: 
```prolog
(Goal1 ; Goal2)
(Condition -> Then ; Else)
```

**Arguments**:
- `+Goal1`: First alternative goal
- `+Goal2`: Second alternative goal

**Examples**:
```prolog
% Example 1: Simple disjunction
?- (true ; false).
true.

?- (false ; true).
true.

% Example 2: If-then-else
?- (5 > 3 -> write(yes) ; write(no)).
yes
true.

?- (3 > 5 -> write(yes) ; write(no)).
no
true.

% Example 3: Multiple alternatives
color(red).
color(blue).

?- (color(green) ; color(red)).
true.
```

**Behavior**: For simple disjunction, tries Goal1 first. If it succeeds, provides those solutions. Then tries Goal2 and provides its solutions. For if-then-else, if Condition succeeds, executes Then; otherwise executes Else.

**See Also**: ->/2, !/0

---

## =../2

**Category**: term_manipulation  
**ISO Compliance**: yes

**Description**: Univ operator that converts between a term and a list containing the functor and arguments.

**Syntax**: 
```prolog
+Term =.. ?List
?Term =.. +List
```

**Arguments**:
- `?Term`: Term to decompose or construct
- `?List`: List containing functor as first element, followed by arguments

**Examples**:
```prolog
% Example 1: Term to list decomposition
?- f(a, b, c) =.. L.
L = [f, a, b, c].

% Example 2: List to term construction
?- T =.. [foo, x, y].
T = foo(x, y).

% Example 3: Atom case
?- hello =.. L.
L = [hello].

% Example 4: Variable manipulation
?- functor(F, name, 3), F =.. [name, X, Y, Z].
F = name(X, Y, Z).
```

**Behavior**: Bidirectional conversion between terms and lists. For atoms, creates a single-element list. For compound terms, first element is the functor, remaining elements are arguments in order.

**See Also**: functor/3, arg/3

---

## =:=/2

**Category**: arithmetic  
**ISO Compliance**: yes

**Description**: Arithmetic equality comparison. Evaluates both sides as arithmetic expressions and tests for numerical equality.

**Syntax**: 
```prolog
+Expr1 =:= +Expr2
```

**Arguments**:
- `+Expr1`: First arithmetic expression
- `+Expr2`: Second arithmetic expression

**Examples**:
```prolog
% Example 1: Simple equality
?- 5 =:= 5.
true.

% Example 2: Expression evaluation
?- 2 + 3 =:= 5.
true.

?- 2 * 3 =:= 6.
true.

% Example 3: Failure case
?- 5 =:= 6.
false.

% Example 4: With variables
?- X = 5, Y = 5, X =:= Y.
X = 5, Y = 5.
```

**Behavior**: Evaluates both expressions arithmetically and succeeds if they are numerically equal. Both sides must be evaluable arithmetic expressions. Uses standard arithmetic evaluation.

**See Also**: =\\=/2, >/2, </2, >=/2, =</2, is/2

---

## =\\=/2

**Category**: arithmetic  
**ISO Compliance**: yes

**Description**: Arithmetic inequality comparison. Evaluates both sides as arithmetic expressions and tests for numerical inequality.

**Syntax**: 
```prolog
+Expr1 =\= +Expr2
```

**Arguments**:
- `+Expr1`: First arithmetic expression
- `+Expr2`: Second arithmetic expression

**Examples**:
```prolog
% Example 1: Simple inequality
?- 5 =\= 3.
true.

% Example 2: Expression evaluation
?- 2 + 3 =\= 4.
true.

?- 2 * 3 =\= 7.
true.

% Example 3: Failure case
?- 5 =\= 5.
false.

% Example 4: With variables
?- X = 5, Y = 6, X =\= Y.
X = 5, Y = 6.
```

**Behavior**: Evaluates both expressions arithmetically and succeeds if they are numerically different. Both sides must be evaluable arithmetic expressions. Fails if expressions are equal.

**See Also**: =:=/2, >/2, </2, >=/2, =</2, is/2

---

## arg/3

**Category**: term_manipulation  
**ISO Compliance**: yes

**Description**: Extracts the Nth argument from a compound term.

**Syntax**: 
```prolog
arg(+N, +Term, ?Arg)
```

**Arguments**:
- `+N`: Argument position (integer, 1-based)
- `+Term`: Compound term to extract from
- `?Arg`: The extracted argument

**Examples**:
```prolog
% Example 1: Extract first argument
?- arg(1, f(a, b, c), X).
X = a.

% Example 2: Extract second argument
?- arg(2, person(john, 25, student), Age).
Age = 25.

% Example 3: Extract third argument
?- arg(3, f(a, b, c), X).
X = c.

% Example 4: Out of bounds fails
?- arg(4, f(a, b, c), X).
false.

% Example 5: Not a compound term fails
?- arg(1, atom, X).
false.
```

**Behavior**: Extracts the Nth argument from a compound term (1-based indexing). Fails if N is out of bounds or if Term is not a compound term. Both N and Term must be ground.

**See Also**: functor/3, =../2

---

## functor/3

**Category**: term_manipulation  
**ISO Compliance**: yes

**Description**: Relates a term to its functor name and arity, or constructs a term from a functor and arity.

**Syntax**: 
```prolog
functor(?Term, ?Functor, ?Arity)
```

**Arguments**:
- `?Term`: Term to analyze or construct
- `?Functor`: Functor name (atom)
- `?Arity`: Number of arguments (integer)

**Examples**:
```prolog
% Example 1: Extract functor and arity
?- functor(f(a, b), F, A).
F = f, A = 2.

% Example 2: Atom case
?- functor(hello, F, A).
F = hello, A = 0.

% Example 3: Construct term from functor and arity
?- functor(T, foo, 2).
T = foo(_G1, _G2).

% Example 4: Construct atom
?- functor(T, hello, 0).
T = hello.

% Example 5: Verify functor/arity
?- functor(person(john, 25), person, 2).
true.
```

**Behavior**: Bidirectional predicate. If Term is ground, extracts its functor and arity. If Functor and Arity are ground, constructs a term with fresh variables as arguments. For atoms, arity is 0.

**See Also**: arg/3, =../2

---

## assert/1

**Category**: database  
**ISO Compliance**: yes (alias for assertz/1)

**Description**: Asserts a clause or fact to the end of the knowledge base. Equivalent to assertz/1.

**Syntax**: 
```prolog
assert(+Clause)
```

**Arguments**: 
- `+Clause`: Clause to assert (fact or rule)

**Examples**:
```prolog
% Example 1: Assert a fact
?- assert(likes(mary, wine)).
true.

% Example 2: Assert a rule
?- assert((parent(X, Y) :- father(X, Y))).
true.

% Example 3: Query asserted facts
?- assert(color(red)), color(red).
true.

% Example 4: Assert and retrieve
?- assert(number(42)), findall(X, number(X), L).
L = [42].
```

**Behavior**: Adds the clause to the end of the knowledge base. The clause becomes immediately available for queries. Changes are permanent until retracted.

**See Also**: asserta/1, assertz/1, retract/1

---

## atom_chars/2

**Category**: conversion  
**ISO Compliance**: yes

**Description**: Converts between an atom and a list of single-character atoms.

**Syntax**: 
```prolog
atom_chars(?Atom, ?Chars)
```

**Arguments**: 
- `?Atom`: Atom to convert or result atom
- `?Chars`: List of single-character atoms

**Examples**:
```prolog
% Example 1: Atom to character list
?- atom_chars(hello, Chars).
Chars = [h, e, l, l, o].

% Example 2: Character list to atom
?- atom_chars(Atom, [a, b, c]).
Atom = abc.

% Example 3: Check consistency
?- atom_chars(test, [t, e, s, t]).
true.

% Example 4: Working with variables
?- atom_chars(hello, [H|T]).
H = h, T = [e, l, l, o].
```

**Behavior**: Bidirectional conversion. Each character in the atom becomes a single-character atom in the list. Empty atom corresponds to empty list.

**See Also**: atom_codes/2, atom_concat/3

---

## atom_concat/3

**Category**: atom  
**ISO Compliance**: yes

**Description**: Concatenates two atoms or splits an atom into two parts.

**Syntax**: 
```prolog
atom_concat(?Atom1, ?Atom2, ?Atom12)
```

**Arguments**: 
- `?Atom1`: First atom or prefix
- `?Atom2`: Second atom or suffix  
- `?Atom12`: Concatenated result or source atom

**Examples**:
```prolog
% Example 1: Concatenate atoms
?- atom_concat(hello, world, Result).
Result = helloworld.

% Example 2: Split atom (multiple solutions)
?- atom_concat(X, Y, hello).
X = '', Y = hello ;
X = h, Y = ello ;
X = he, Y = llo ;
X = hel, Y = lo ;
X = hell, Y = o ;
X = hello, Y = ''.

% Example 3: Check if atom starts with prefix
?- atom_concat(test, _, testing).
true.

% Example 4: Find suffix
?- atom_concat(pre, Suffix, prefix).
Suffix = fix.
```

**Behavior**: Can concatenate, split, or verify concatenation. When splitting, generates all possible combinations through backtracking.

**See Also**: atom_length/2, sub_atom/5

---

## atom_length/2

**Category**: atom  
**ISO Compliance**: yes

**Description**: Gets or checks the length of an atom in characters.

**Syntax**: 
```prolog
atom_length(+Atom, ?Length)
```

**Arguments**: 
- `+Atom`: Atom whose length to measure
- `?Length`: Length in characters (integer)

**Examples**:
```prolog
% Example 1: Get atom length
?- atom_length(hello, N).
N = 5.

% Example 2: Check atom length
?- atom_length(test, 4).
true.

% Example 3: Empty atom
?- atom_length('', N).
N = 0.

% Example 4: Unicode characters
?- atom_length('café', N).
N = 4.
```

**Behavior**: Counts the number of characters in the atom. Works with empty atoms and unicode characters.

**See Also**: atom_chars/2, atom_concat/3

---

## bagof/3

**Category**: meta  
**ISO Compliance**: yes

**Description**: Collects all solutions to a goal, preserving duplicates and grouping by free variables.

**Syntax**: 
```prolog
bagof(?Template, +Goal, ?Bag)
```

**Arguments**: 
- `?Template`: Template for collected solutions
- `+Goal`: Goal to find solutions for
- `?Bag`: List of collected solutions

**Examples**:
```prolog
% Setup facts
?- assert(likes(mary, food)), assert(likes(mary, wine)), 
   assert(likes(john, wine)), assert(likes(bob, wine)).

% Example 1: Collect solutions
?- bagof(X, likes(X, wine), People).
People = [mary, john, bob].

% Example 2: Collect with duplicates
?- assert(likes(john, wine)), bagof(X, likes(X, wine), People).
People = [mary, john, john, bob].

% Example 3: Template extraction
?- bagof(Person, likes(Person, wine), L).
L = [mary, john, bob].

% Example 4: No solutions case
?- bagof(X, likes(X, beer), L).
false.
```

**Behavior**: Similar to findall/3 but preserves duplicates and can fail if no solutions exist. Groups solutions by free variables in the goal.

**See Also**: findall/3, setof/3

---

## setof/3

**Category**: meta  
**ISO Compliance**: yes

**Description**: Collects all unique solutions to a goal in sorted order.

**Syntax**: 
```prolog
setof(?Template, +Goal, ?Set)
```

**Arguments**: 
- `?Template`: Template for collected solutions
- `+Goal`: Goal to find solutions for
- `?Set`: Sorted list of unique solutions

**Examples**:
```prolog
% Setup facts
?- assert(likes(mary, food)), assert(likes(mary, wine)), 
   assert(likes(john, wine)), assert(likes(bob, wine)).

% Example 1: Collect unique sorted solutions
?- setof(X, likes(X, wine), People).
People = [bob, john, mary].

% Example 2: Removes duplicates  
?- assert(likes(john, wine)), setof(X, likes(X, wine), People).
People = [bob, john, mary].

% Example 3: Sorted order
?- setof(Item, likes(mary, Item), Items).
Items = [food, wine].

% Example 4: No solutions case
?- setof(X, likes(X, beer), L).
false.
```

**Behavior**: Like bagof/3 but removes duplicates and sorts results. Fails if no solutions exist.

**See Also**: findall/3, bagof/3

---

## sub_atom/5

**Category**: atom  
**ISO Compliance**: yes

**Description**: Extracts or verifies subatoms with position and length information.

**Syntax**: 
```prolog
sub_atom(+Atom, ?Before, ?Length, ?After, ?SubAtom)
```

**Arguments**: 
- `+Atom`: Source atom
- `?Before`: Characters before substring (integer)
- `?Length`: Length of substring (integer)  
- `?After`: Characters after substring (integer)
- `?SubAtom`: The substring atom

**Examples**:
```prolog
% Example 1: Extract substring at position
?- sub_atom(hello, 1, 3, 1, Sub).
Sub = ell.

% Example 2: Find all substrings of length 2
?- sub_atom(hello, Before, 2, After, Sub).
Before = 0, After = 3, Sub = he ;
Before = 1, After = 2, Sub = el ;
Before = 2, After = 1, Sub = ll ;
Before = 3, After = 0, Sub = lo.

% Example 3: Check if atom contains substring
?- sub_atom(testing, _, _, _, test).
true.

% Example 4: Get all possible extractions
?- sub_atom(ab, B, L, A, S).
% Multiple solutions for all possible combinations
```

**Behavior**: Very flexible predicate for substring operations. Can extract, search, verify, or generate all possible substrings through backtracking.

**See Also**: atom_concat/3, atom_chars/2

---

## Note

This file should be updated whenever built-in predicates are added, modified, or removed. Entries must be maintained in alphabetical order by predicate name. Each entry should include complete documentation with practical examples.

**Last Updated**: 2025-08-20  
**Total Predicates Documented**: 15

---

## Quick Reference Index

- !/0 - Cut operator
- ->/2 - If-then operator  
- ;/2 - Disjunction/if-then-else operator
- =../2 - Univ operator (term ↔ list)
- =:=/2 - Arithmetic equality
- =\\=/2 - Arithmetic inequality  
- arg/3 - Extract term argument
- assert/1 - Assert clause to knowledge base
- atom_chars/2 - Atom ↔ character list conversion
- atom_concat/3 - Atom concatenation/splitting
- atom_length/2 - Get atom length
- bagof/3 - Collect solutions preserving duplicates
- functor/3 - Term functor and arity
- setof/3 - Collect unique sorted solutions
- sub_atom/5 - Substring extraction and verification