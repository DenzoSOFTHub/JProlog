# JProlog Built-in Predicates and Operators Reference

## Overview

This document provides a comprehensive reference for all built-in predicates and operators implemented in the JProlog system. JProlog is a Java-based Prolog interpreter that supports a subset of ISO Prolog standard predicates along with additional useful predicates.

## Table of Contents

1. [Logical Operators](#logical-operators)
2. [Unification Predicates](#unification-predicates)
3. [Type Testing Predicates](#type-testing-predicates)
4. [Arithmetic Predicates](#arithmetic-predicates)
5. [Comparison Predicates](#comparison-predicates)
6. [Term Manipulation](#term-manipulation)
7. [List Operations](#list-operations)
8. [Control Predicates](#control-predicates)
9. [Database Predicates](#database-predicates)
10. [I/O Predicates](#io-predicates)
11. [Type Conversion](#type-conversion)
12. [Collection Predicates](#collection-predicates)
13. [Exception Handling (ISO)](#exception-handling-iso)
14. [Meta-Predicates (ISO)](#meta-predicates-iso)
15. [Dynamic Database Operations (ISO)](#dynamic-database-operations-iso)

---

## Logical Operators

### Conjunction (`,`)
**Syntax:** `Goal1, Goal2`
**Description:** Logical AND operator. Both goals must succeed.
**Example:**
```prolog
?- X = 5, Y = 10, X < Y.
X = 5, Y = 10.
```

### Disjunction (`;`)
**Syntax:** `Goal1 ; Goal2`
**Description:** Logical OR operator. Either goal can succeed.
**Example:**
```prolog
?- X = 1 ; X = 2.
X = 1 ;
X = 2.
```

### If-Then (`->`)
**Syntax:** `Condition -> Then`
**Description:** If-then construct. If Condition succeeds, execute Then.
**Example:**
```prolog
?- 5 > 0 -> write('positive').
positive
true.
```

### If-Then-Else (`->` with `;`)
**Syntax:** `(Condition -> Then ; Else)`
**Description:** Complete conditional construct.
**Example:**
```prolog
?- (5 > 0 -> X = positive ; X = negative).
X = positive.
```

### Negation as Failure (`\+`)
**Syntax:** `\+ Goal`
**Description:** Negation as failure operator. Succeeds if Goal fails, fails if Goal succeeds. This is not logical negation but rather the inability to prove the goal.
**Example:**
```prolog
?- \+ fail.
true.

?- \+ true.
false.

?- \+ (1 = 2).
true.

?- \+ (1 = 1).
false.
```
**Note:** Variables inside the negated goal are not bound in the outer context.

---

## Unification Predicates

### `=/2` - Unification
**Syntax:** `Term1 = Term2`
**Description:** Unifies two terms.
**Example:**
```prolog
?- X = hello.
X = hello.

?- f(a, b) = f(Y, Z).
Y = a, Z = b.
```

### `unify_with_occurs_check/2`
**Syntax:** `unify_with_occurs_check(Term1, Term2)`
**Description:** Unification with occurs check to prevent infinite structures.
**Example:**
```prolog
?- unify_with_occurs_check(X, f(X)).
false.
```

---

## Type Testing Predicates

### `var/1`
**Syntax:** `var(Term)`
**Description:** True if Term is an unbound variable.
**Example:**
```prolog
?- var(X).
true.

?- var(hello).
false.
```

### `nonvar/1`
**Syntax:** `nonvar(Term)`
**Description:** True if Term is not an unbound variable.
**Example:**
```prolog
?- nonvar(hello).
true.
```

### `atom/1`
**Syntax:** `atom(Term)`
**Description:** True if Term is an atom.
**Example:**
```prolog
?- atom(hello).
true.

?- atom(123).
false.
```

### `integer/1`
**Syntax:** `integer(Term)`
**Description:** True if Term is an integer.
**Example:**
```prolog
?- integer(42).
true.

?- integer(3.14).
false.
```

### `float/1`
**Syntax:** `float(Term)`
**Description:** True if Term is a floating-point number.
**Example:**
```prolog
?- float(3.14).
true.
```

### `number/1`
**Syntax:** `number(Term)`
**Description:** True if Term is a number (integer or float).
**Example:**
```prolog
?- number(42).
true.

?- number(3.14).
true.
```

### `atomic/1`
**Syntax:** `atomic(Term)`
**Description:** True if Term is atomic (atom or number).
**Example:**
```prolog
?- atomic(hello).
true.

?- atomic(42).
true.
```

### `compound/1`
**Syntax:** `compound(Term)`
**Description:** True if Term is a compound term.
**Example:**
```prolog
?- compound(f(a, b)).
true.

?- compound(hello).
false.
```

---

## Arithmetic Predicates

### `is/2`
**Syntax:** `Result is Expression`
**Description:** Evaluates arithmetic expression and unifies with Result.
**Supported operations:** `+`, `-`, `*`, `/`, `mod`, `**`, `abs`, `min`, `max`
**Example:**
```prolog
?- X is 2 + 3 * 4.
X = 14.0.

?- Y is abs(-5).
Y = 5.0.

?- Z is 7 mod 3.
Z = 1.0.

?- W is 2 ** 3.
W = 8.0.
```

### Arithmetic Comparison

#### `=:=/2` - Arithmetic Equal
**Syntax:** `Expr1 =:= Expr2`
**Example:**
```prolog
?- 2 + 3 =:= 5.
true.
```

#### `=\\=/2` - Arithmetic Not Equal
**Syntax:** `Expr1 =\\= Expr2`
**Example:**
```prolog
?- 2 + 3 =\\= 6.
true.
```

#### `</2` - Less Than
**Syntax:** `Expr1 < Expr2`
**Example:**
```prolog
?- 3 < 5.
true.
```

#### `=</2` - Less Than or Equal
**Syntax:** `Expr1 =< Expr2`
**Example:**
```prolog
?- 5 =< 5.
true.
```

#### `>/2` - Greater Than
**Syntax:** `Expr1 > Expr2`
**Example:**
```prolog
?- 7 > 3.
true.
```

#### `>=/2` - Greater Than or Equal
**Syntax:** `Expr1 >= Expr2`
**Example:**
```prolog
?- 5 >= 5.
true.
```

---

## Term Comparison

### `==/2` - Term Identity
**Syntax:** `Term1 == Term2`
**Description:** True if terms are identical.
**Example:**
```prolog
?- hello == hello.
true.

?- X == Y.
false.
```

### `\\==/2` - Term Non-Identity
**Syntax:** `Term1 \\== Term2`
**Description:** True if terms are not identical.

### Term Ordering

#### `@</2` - Term Less Than
**Syntax:** `Term1 @< Term2`
**Description:** Standard term ordering comparison.

#### `@=</2` - Term Less Than or Equal
**Syntax:** `Term1 @=< Term2`

#### `@>/2` - Term Greater Than
**Syntax:** `Term1 @> Term2`

#### `@>=/2` - Term Greater Than or Equal
**Syntax:** `Term1 @>= Term2`

---

## Term Manipulation

### `functor/3`
**Syntax:** `functor(Term, Name, Arity)`
**Description:** Relates a term to its functor name and arity.
**Example:**
```prolog
?- functor(f(a, b), Name, Arity).
Name = f, Arity = 2.

?- functor(Term, hello, 0).
Term = hello.
```

### `arg/3`
**Syntax:** `arg(N, Term, Arg)`
**Description:** Extracts the Nth argument of a compound term.
**Example:**
```prolog
?- arg(2, f(a, b, c), X).
X = b.
```

### `=../2` (univ)
**Syntax:** `Term =.. List`
**Description:** Converts between term and list representation.
**Example:**
```prolog
?- f(a, b) =.. List.
List = [f, a, b].

?- Term =.. [hello, world].
Term = hello(world).
```

### `copy_term/2`
**Syntax:** `copy_term(Term, Copy)`
**Description:** Creates a copy of a term with fresh variables.
**Example:**
```prolog
?- copy_term(f(X, X), Copy).
Copy = f(_G123, _G123).
```

---

## List Operations

### `append/3`
**Syntax:** `append(List1, List2, List3)`
**Description:** Concatenates List1 and List2 to produce List3.
**Example:**
```prolog
?- append([1, 2], [3, 4], L).
L = [1, 2, 3, 4].

?- append(X, [3, 4], [1, 2, 3, 4]).
X = [1, 2].
```

### `length/2`
**Syntax:** `length(List, Length)`
**Description:** Relates a list to its length.
**Example:**
```prolog
?- length([1, 2, 3], L).
L = 3.

?- length(List, 3).
List = [_G1, _G2, _G3].
```

### `member/2`
**Syntax:** `member(Element, List)`
**Description:** True if Element is a member of List.
**Example:**
```prolog
?- member(2, [1, 2, 3]).
true.

?- member(X, [a, b, c]).
X = a ;
X = b ;
X = c.
```

### `nth0/3` and `nth1/3`
**Syntax:** `nth0(Index, List, Element)` / `nth1(Index, List, Element)`
**Description:** Relates list position to element (0-based/1-based indexing).
**Example:**
```prolog
?- nth0(1, [a, b, c], X).
X = b.

?- nth1(2, [a, b, c], X).
X = b.
```

### `reverse/2`
**Syntax:** `reverse(List, Reversed)`
**Description:** Reverses a list.
**Example:**
```prolog
?- reverse([1, 2, 3], R).
R = [3, 2, 1].
```

### `sort/2` and `msort/2`
**Syntax:** `sort(List, Sorted)` / `msort(List, Sorted)`
**Description:** Sorts a list (sort removes duplicates, msort preserves them).
**Example:**
```prolog
?- sort([3, 1, 2, 1], S).
S = [1, 2, 3].

?- msort([3, 1, 2, 1], S).
S = [1, 1, 2, 3].
```

### `select/3`
**Syntax:** `select(Element, List, Rest)`
**Description:** Selects an element from a list, leaving the rest.
**Example:**
```prolog
?- select(2, [1, 2, 3], Rest).
Rest = [1, 3].
```

---

## Control Predicates

### `!/0` - Cut
**Syntax:** `!`
**Description:** Prevents backtracking past this point.
**Example:**
```prolog
max(X, Y, X) :- X >= Y, !.
max(X, Y, Y).
```

### `repeat/0`
**Syntax:** `repeat`
**Description:** Always succeeds and provides infinite choice points.
**Example:**
```prolog
?- repeat, read(X), X == stop.
```

---

## Database Predicates

### `listing/0` and `listing/1`
**Syntax:** `listing` / `listing(Predicate)`
**Description:** Lists all clauses or clauses for a specific predicate.
**Example:**
```prolog
?- listing.
% Lists all clauses

?- listing(parent/2).
% Lists clauses for parent/2
```

---

## I/O Predicates

### `write/1`
**Syntax:** `write(Term)`
**Description:** Writes a term to output.
**Example:**
```prolog
?- write('Hello World').
Hello World
true.
```

### `writeln/1`
**Syntax:** `writeln(Term)`
**Description:** Writes a term followed by a newline.
**Example:**
```prolog
?- writeln('Hello World').
Hello World
true.
```

### `nl/0`
**Syntax:** `nl`
**Description:** Writes a newline.
**Example:**
```prolog
?- write('Hello'), nl, write('World').
Hello
World
true.
```

### `read/1`
**Syntax:** `read(Term)`
**Description:** Reads a term from input.

---

## Type Conversion

### `atom_number/2`
**Syntax:** `atom_number(Atom, Number)`
**Description:** Converts between atom and number.
**Example:**
```prolog
?- atom_number('123', N).
N = 123.

?- atom_number(A, 456).
A = '456'.
```

### `atom_chars/2`
**Syntax:** `atom_chars(Atom, Chars)`
**Description:** Converts between atom and list of characters.
**Example:**
```prolog
?- atom_chars(hello, Chars).
Chars = [h, e, l, l, o].

?- atom_chars(Atom, [a, b, c]).
Atom = abc.
```

### `atom_codes/2`
**Syntax:** `atom_codes(Atom, Codes)`
**Description:** Converts between atom and list of character codes (ASCII values).
**Example:**
```prolog
?- atom_codes(hello, Codes).
Codes = [104, 101, 108, 108, 111].

?- atom_codes(Atom, [104, 101, 108, 108, 111]).
Atom = hello.
```

### `number_chars/2`
**Syntax:** `number_chars(Number, Chars)`
**Description:** Converts between number and list of characters.
**Example:**
```prolog
?- number_chars(123, Chars).
Chars = ['1', '2', '3'].
```

---

## Collection Predicates

### `findall/3`
**Syntax:** `findall(Template, Goal, List)`
**Description:** Finds all solutions to Goal and collects Template instances.
**Example:**
```prolog
?- findall(X, member(X, [1, 2, 3]), L).
L = [1, 2, 3].

?- findall(Name, parent(Name, _), Parents).
Parents = [tom, tom, bob, bob, pat].
```

---

## Atom Operations

### `atom_length/2`
**Syntax:** `atom_length(Atom, Length)`
**Description:** Determines the length of an atom.
**Example:**
```prolog
?- atom_length(hello, L).
L = 5.
```

### `atom_concat/3`
**Syntax:** `atom_concat(Atom1, Atom2, Atom3)`
**Description:** Concatenates atoms.
**Example:**
```prolog
?- atom_concat(hello, world, Result).
Result = helloworld.

?- atom_concat(X, world, helloworld).
X = hello.
```

### `sub_atom/5`
**Syntax:** `sub_atom(Atom, Before, Length, After, SubAtom)`
**Description:** Extracts or locates subatoms. Before characters precede the substring, Length is the substring length, After characters follow the substring.
**Example:**
```prolog
?- sub_atom(hello, 1, 3, 1, X).
X = ell.

?- sub_atom(hello, X, 2, Y, el).
X = 1, Y = 2.

?- sub_atom(hello, _, _, _, X).
X = '' ; X = h ; X = he ; X = hel ; X = hell ; X = hello ; ...
```

---

## Operator Precedence

JProlog follows standard Prolog operator precedence:

| Precedence | Operators | Associativity |
|------------|-----------|---------------|
| 1200 | `:-` | xfx |
| 1100 | `;` | xfy |
| 1050 | `->` | xfy |
| 1000 | `,` | xfy |
| 700 | `=`, `==`, `\\==`, `=:=`, `=\\=`, `<`, `=<`, `>`, `>=`, `@<`, `@=<`, `@>`, `@>=`, `is`, `=..` | xfx |
| 500 | `+`, `-` | yfx |
| 400 | `*`, `/`, `mod` | yfx |
| 200 | `**`, `^` | xfx |

---

---

## Character I/O (ISO)

### get_char/1
**Syntax:** `get_char(?Char)`
**Description:** Reads a single character from standard input and unifies it with Char. Returns 'end_of_file' at end of stream.
**Example:**
```prolog
?- get_char(X).
a
X = a.
```

### put_char/1
**Syntax:** `put_char(+Char)`
**Description:** Writes a single character to standard output.
**Example:**
```prolog
?- put_char(a).
a
true.
```

### get_code/1
**Syntax:** `get_code(?Code)`
**Description:** Reads a single character from standard input and unifies Code with its character code. Returns -1 at end of stream.
**Example:**
```prolog
?- get_code(X).
a
X = 97.
```

### put_code/1
**Syntax:** `put_code(+Code)`
**Description:** Writes a character to standard output based on its character code.
**Example:**
```prolog
?- put_code(97).
a
true.
```

---

## Advanced Arithmetic (ISO)

### between/3
**Syntax:** `between(+Low, +High, ?Value)`
**Description:** True if Low ≤ Value ≤ High. On backtracking, generates all integer values in the range.
**Example:**
```prolog
?- between(1, 3, X).
X = 1 ;
X = 2 ;
X = 3.

?- between(1, 5, 3).
true.
```

### succ/2
**Syntax:** `succ(?Int1, ?Int2)`
**Description:** True if Int2 = Int1 + 1 and both are non-negative integers.
**Example:**
```prolog
?- succ(1, X).
X = 2.

?- succ(X, 5).
X = 4.
```

### plus/3
**Syntax:** `plus(?Int1, ?Int2, ?Int3)`
**Description:** True if Int1 + Int2 = Int3. Can be used for addition, subtraction, or checking arithmetic relationships.
**Example:**
```prolog
?- plus(2, 3, X).
X = 5.

?- plus(X, 3, 8).
X = 5.
```

---

## Debugging Predicates (ISO)

### trace/0
**Syntax:** `trace`
**Description:** Enables tracing mode for debugging goal execution.
**Example:**
```prolog
?- trace.
% Tracing enabled
true.
```

### notrace/0
**Syntax:** `notrace`
**Description:** Disables tracing mode.
**Example:**
```prolog
?- notrace.
% Tracing disabled
true.
```

### spy/1
**Syntax:** `spy(+PredicateIndicator)`
**Description:** Sets a spy point on the specified predicate (Name/Arity format).
**Example:**
```prolog
?- spy(/(member, 2)).
% Spy point set on member/2
true.
```

### nospy/1
**Syntax:** `nospy(+PredicateIndicator)`
**Description:** Removes a spy point from the specified predicate. Use variable to remove all spy points.
**Example:**
```prolog
?- nospy(/(member, 2)).
% Spy point removed from member/2
true.

?- nospy(_).
% All spy points removed
true.
```

---

## Exception Handling (ISO)

### catch/3
**Syntax:** `catch(Goal, Catcher, Recovery)`
**Description:** ISO Prolog exception handling. Executes Goal, and if an exception matching Catcher is thrown, executes Recovery.
**Example:**
```prolog
?- catch(throw(error_123), Error, write('Caught: '), write(Error)).
Caught: error_123
```

### throw/1
**Syntax:** `throw(Ball)`
**Description:** Throws an exception with term Ball. The exception propagates until caught by a matching catch/3.
**Example:**
```prolog
?- catch(throw(my_error), my_error, write('Success')).
Success
```

### halt/0, halt/1
**Syntax:** `halt` or `halt(ExitCode)`
**Description:** Terminates the program execution. halt/0 exits with code 0, halt/1 with specified code.
**Example:**
```prolog
?- halt(42).  % Exits with code 42
```

---

## Meta-Predicates (ISO)

### call/1-8
**Syntax:** `call(Goal)`, `call(Goal, Arg1)`, etc.
**Description:** Meta-call predicate. Calls Goal as a goal, optionally adding extra arguments.
**Example:**
```prolog
?- call(write('Hello')).
Hello
?- call(append([1,2]), [3,4], Result).
Result = [1,2,3,4]
```

### once/1
**Syntax:** `once(Goal)`
**Description:** Succeeds at most once. If Goal succeeds, cuts choice points; if Goal fails, once/1 fails.
**Example:**
```prolog
?- once(member(X, [1,2,3])).
X = 1.
```

### ignore/1
**Syntax:** `ignore(Goal)`
**Description:** Always succeeds. If Goal succeeds, ignore/1 succeeds; if Goal fails, ignore/1 still succeeds.
**Example:**
```prolog
?- ignore(fail).
true.
```

### forall/2
**Syntax:** `forall(Condition, Action)`
**Description:** For all solutions of Condition, Action must succeed. Fails if Action fails for any solution.
**Example:**
```prolog
?- forall(member(X, [1,2,3]), (X > 0)).
true.
```

---

## Dynamic Database Operations (ISO)

### asserta/1
**Syntax:** `asserta(Clause)`
**Description:** Adds Clause to the beginning of the database. New clauses are tried first.
**Example:**
```prolog
?- asserta(fact(new)).
?- asserta(fact(newer)).
?- fact(X).
X = newer ;
X = new.
```

### assertz/1
**Syntax:** `assertz(Clause)`
**Description:** Adds Clause to the end of the database. New clauses are tried last.
**Example:**
```prolog
?- assertz(fact(first)).
?- assertz(fact(second)).
?- fact(X).
X = first ;
X = second.
```

### retract/1
**Syntax:** `retract(Clause)`
**Description:** Removes the first clause from the database that unifies with Clause.
**Example:**
```prolog
?- retract(fact(unwanted)).
true.
```

### retractall/1
**Syntax:** `retractall(Head)`
**Description:** Removes all clauses whose head unifies with Head. Always succeeds.
**Example:**
```prolog
?- retractall(temp_fact(_)).
true.
```

### abolish/1
**Syntax:** `abolish(PredicateIndicator)`
**Description:** Removes all clauses for the predicate specified by Functor/Arity.
**Example:**
```prolog
?- abolish(temp_predicate/2).
true.
```

### current_predicate/1
**Syntax:** `current_predicate(PredicateIndicator)`
**Description:** Succeeds if the predicate specified by Functor/Arity exists in the database.
**Example:**
```prolog
?- current_predicate(member/2).
true.
```

---

## Notes

- All arithmetic operations return floating-point numbers
- List syntax uses `[Head|Tail]` notation
- Cut (`!`) affects backtracking within the current clause
- Type checking predicates fail silently for wrong types
- Error handling provides informative exception messages
- Variable binding follows standard Prolog unification rules
- **New ISO features**: Exception handling, meta-predicates, and dynamic database operations provide enhanced ISO Prolog compliance
- Exception handling follows ISO Prolog standard with catch/3 and throw/1
- Meta-predicates allow higher-order programming with call/1-8, once/1, ignore/1, and forall/2
- Dynamic database operations enable runtime modification of the knowledge base

This reference covers all built-in predicates and operators available in JProlog. For usage examples and programming patterns, see the Prolog programming guide and integration documentation.