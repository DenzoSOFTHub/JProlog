# Part I — Introduction

## About this manual

This manual is the complete reference to the **default** JProlog system: every operator in the
initial operator table and every built-in predicate registered when a `Prolog` engine is created
(the CLI, the IDE and the Java embedding API all start from the same registry). Each predicate
entry gives its purpose, the argument modes, notes on behaviour that differs from other Prolog
systems, and at least one runnable code example. Libraries that ship as source packages but are not
loaded by default (expert-system, ASP, CHR, CLP(R), … extensions) are listed in Appendix E and are
not described here.

Conventions used throughout:

- `name/N` — a predicate with N arguments; `name//N` — a DCG non-terminal.
- Argument modes: `+Arg` must be instantiated, `-Arg` is an output (normally unbound), `?Arg` may be
  either, `@Arg` is not further instantiated by the call, `:Goal` is a callable meta-argument.
- Interactive examples show the query after the `?-` prompt followed by the answer as printed by the
  JProlog CLI: bindings such as `X = 3.`, `true.` for success without bindings, `false.` for failure,
  and `ERROR: ...` when an exception escapes to the top level. Alternative solutions are separated by
  `;`.
- Program examples are ordinary clauses you can put in a file and load with `consult/1` or `:consult`.
- Answers are written in the usual Prolog notation, which is what the JProlog console prints
  since v3.14.0 — except that this manual puts a space after a comma inside a compound term or a
  list (`[1, 2]`, `f(a, b)`) for readability, where the console writes none (`[1,2]`, `f(a,b)`):
  bindings appear in quoted operator notation — `a-1`, `foo/1`, `(p, q)`, the atom
  `'42'` — and a variable that is still free prints as `_A`, `_B`, ... (a query variable that comes
  back unbound keeps its own name, so `X = f(Y)` prints as `X = f(Y)`). If an answer still carries
  constraints, they are listed after the bindings: `freeze(X, Goal)`, `dif(X, a)`, `X in 1..3`.

## Running JProlog

JProlog is a Prolog interpreter written in Java (source level 1.8, runs on any modern JDK). It has no
runtime dependencies beyond the JDK.

**Command-line console** — start it with `java -cp target/classes it.denzosoft.jprolog.PrologCLI`,
or build the runnable jar with `mvn package -DskipTests` and run `java -jar target/jprolog.jar`.
Queries are typed after the `?-` prompt and end with a full stop.

Command line: `jprolog [options] [file.pl ...]` consults the files in order, then:

| Option | Effect |
|---|---|
| `-g Goal` | run Goal once after loading (repeatable); failure or an uncaught error exits with status 1 |
| `-t Goal` | run Goal as the toplevel instead of the interactive loop (`-t halt` ends after `-g`) |
| `--safe` | safe mode: no files, processes, network, threads, CSV files or log file |
| `--budget N` | inference budget per query |
| `--max-solutions N` | print at most N answers per query |
| `--demo` | load the demo facts (`father/2`, `mother/2`, `parent/2`, `likes/2`, `color/1`) |
| `--batch` / `-q` | print answers without prompting (`-q` also drops the banner) |
| `--interactive` | prompt for `;` even when stdin is not a terminal |

`halt(N)` ends the process with status N, and a file containing `:- initialization(main, main).`
runs `main` after loading and then exits (0 on success, 1 on failure or an uncaught error) — a
Prolog script. The console also understands these commands:

| Command | Effect |
|---|---|
| `:consult <file>` (`:c`) | Load facts and rules from a file |
| `:compile <file>` (`:cc`) | Compile a `.pl` file to the binary `.jpc` format |
| `:listing` (`:l`) | Show all loaded clauses |
| `:save <file>` (`:s`) | Save the current knowledge base to a file |
| `:clear` | Remove all user clauses |
| `:trace [on\|off]` | Toggle four-port call tracing (same as `trace.` / `notrace.`) |
| `:help` (`:h`) | Show the command list |
| `:quit` (`:q`) | Exit |

Answers are computed one at a time (v4.5.0): the console prints an answer and, when more may
follow, writes ` ;` and waits — type `;` and Enter to compute the next one, or Enter alone to stop.
An answer that is certainly the last ends in `.`; a `;` that finds nothing more prints `false.`.
**When stdin is not a terminal** — a pipe, a here-document, a redirected file, a CI job — or when
the console is started with `--batch` or `-q`, it prints the answers as they are found, separated by
` ;` and terminated by `.`, and reads nothing back (an infinite enumeration therefore prints forever
in constant memory; bound it with `--max-solutions`):

```
$ printf 'between(1, 3, X).\n:quit\n' | java -cp target/classes it.denzosoft.jprolog.PrologCLI
?- X = 1 ;
X = 2 ;
X = 3.
```

**IDE** — `java -cp target/classes it.denzosoft.jprolog.editor.PrologIDE` (or `mvn exec:java@run-ide`)
opens the Swing IDE: a project tree, a multi-tab editor with syntax highlighting, completion and a
source formatter, a Build tab with line-accurate compile diagnostics, a Run console for queries with
streaming/table results, and a four-port debugger with breakpoints, stepping, variable and watch views.

**Embedding in Java** — the engine is the class `it.denzosoft.jprolog.core.engine.Prolog`:

```java
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import java.util.List;
import java.util.Map;

Prolog prolog = new Prolog();
prolog.consult("parent(tom, bob).\nparent(bob, ann).\n"
             + "grandparent(X, Z) :- parent(X, Y), parent(Y, Z).");

List<Map<String, Term>> solutions = prolog.solve("grandparent(tom, Who)");
for (Map<String, Term> s : solutions) {
    System.out.println(s.get("Who"));          // ann
}

// Lazy, cancellable enumeration: the sink returns false to stop early
prolog.solveStream("between(1, inf, N)", sol -> ((Number) sol.get("N")).intValue() < 5);

// Hardening for untrusted programs (see Appendix B)
prolog.enableSafeMode();          // remove OS/file/network/JDBC/FFI/thread built-ins
// or: prolog.enableSafeMode(new SafeModeOptions().allowFileRead("data"));  // read-only, one tree
prolog.setInferenceBudget(1_000_000);   // abort runaway queries
```

`consultWithDiagnostics(source, fileName)` compiles clause by clause and returns every syntax error
with its line number instead of stopping at the first one; `compileFile("prog.pl")` produces
`prog.jpc`, which `consultSmart("prog.pl")` loads when it is newer than the source.

# Part II — Language essentials

This part is a compact tour of the Prolog language as implemented by JProlog, enough to read the
reference entries. JProlog follows ISO/IEC 13211-1 for syntax, the standard order of terms,
arithmetic and error terms, and adds the SWI-Prolog extensions that most programs rely on (strings,
global variables, coroutining, CLP(FD), tabling, `format/2`, `must_be/2`, `pairs_*`, and the `apply` and `lists` libraries).

## Terms

Every datum is a *term*:

- **Atoms** — symbolic constants: `hello`, `'New York'`, `[]`, `+`. Quote an atom when it does not
  start with a lowercase letter or contains characters other than letters, digits and `_`. Quoted
  atoms accept the ISO escapes `\n`, `\t`, `\\`, `\'`, `\xHH\` and `\NNN\` (octal).
- **Numbers** — integers of unbounded size (`42`, `-7`, `123456789012345678901234567890`, `0xFF`,
  `0o17`, `0b1010`, `0'a` for a character code) and IEEE double floats (`3.14`, `1.0e10`, `-2.5E-3`).
  An integer and a float are *different terms* even when numerically equal: `1 == 1.0` fails,
  `1 =:= 1.0` succeeds.
- **Strings** — `"text"` is a string object by default (`double_quotes` flag = `string`); set the
  flag to `codes`, `chars` or `atom` for the traditional readings. Back-quoted text `` `abc` `` is
  always a code list.
- **Variables** — start with an uppercase letter or `_`: `X`, `Result`, `_Tmp`. The anonymous
  variable `_` is a fresh variable at every occurrence.
- **Compound terms** — a functor with arguments: `point(1, 2)`, `person(Name, Age)`,
  `'-'(a, b)`. Operators are syntactic sugar for compound terms: `1 + 2` is `+(1, 2)`,
  `X = Y` is `=(X, Y)`.
- **Lists** — `[1, 2, 3]` is sugar for `'.'(1, '.'(2, '.'(3, [])))`; `[H|T]` splits head and
  tail; `[a, b|T]` is a partial list. Strings written in double quotes are not lists unless the
  `double_quotes` flag says so.
- **Curly terms** — `{a, b}` is `'{}'((a, b))`, used by DCG and CLP(FD) syntax.

Comments run from `%` to the end of the line or between `/*` and `*/`.

## Clauses, programs and queries

A program is a sequence of clauses terminated by `.` and whitespace:

```prolog
% Facts: unconditionally true
parent(tom, bob).
parent(bob, ann).

% Rules: Head :- Body. The head is true when the body goals succeed in order.
grandparent(GP, GC) :-
    parent(GP, P),
    parent(P, GC).

% Directives are executed while loading
:- dynamic counter/1.
:- initialization(main).
```

Clauses for the same predicate (`name/arity`) should be contiguous; predicates that are modified at
runtime with `assert`/`retract` are declared `dynamic`. A query is a goal (or conjunction of goals)
evaluated against the program; Prolog searches clauses top to bottom, tries goals left to right, and
*backtracks* to the most recent choice point on failure:

```prolog
?- grandparent(tom, Who).
Who = ann.

?- parent(X, Y).
X = tom, Y = bob ;
X = bob, Y = ann.
```

## Unification and control

- `=/2` unifies two terms, binding variables as needed; `\=/2` succeeds when they do not unify;
  `==/2` tests structural identity without binding.
- Conjunction `,`, disjunction `;`, if-then-else `(Cond -> Then ; Else)`, soft-cut
  `(Cond *-> Then ; Else)`, negation as failure `\+ Goal`, and the cut `!` (commits to the current
  clause and to the choices made so far in its body).
- `call/1..8` calls a goal built at runtime, adding extra arguments: `call(plus(1), 2, X)` gives
  `X = 3`. `findall/3`, `bagof/3`, `setof/3`, `forall/2` and `aggregate_all/3` collect or check all
  solutions of a goal. Lambda expressions of SWI-Prolog's `library(yall)` (`[X]>>Goal`) are **not**
  supported: pass a named auxiliary predicate to `maplist/2..5`, `include/3`, `foldl/4..6`, ….
- Exceptions: `throw(Ball)` unwinds to the nearest `catch(Goal, Catcher, Recovery)` whose `Catcher`
  unifies with the ball. Built-ins raise ISO error terms of the form `error(Formal, Context)`, e.g.
  `error(type_error(integer, abc), context(atom_length/2, _))` — see Appendix C.

```prolog
max(X, Y, X) :- X >= Y, !.
max(_, Y, Y).

safe_div(X, Y, Z) :-
    catch(Z is X / Y, error(evaluation_error(zero_divisor), _), Z = undefined).

?- safe_div(1, 0, Z).
Z = undefined.
```

## Arithmetic

Arithmetic is evaluated only by `is/2` and the comparison predicates `=:=`, `=\=`, `<`, `>`, `=<`,
`>=`. Integers are exact and unbounded; `/` yields an integer when the division is exact and a float
otherwise; `//`, `mod`, `rem`, `div` and the bitwise operators require integers. Every evaluable
functor is listed in Part III (operators) and Chapter 4 (arithmetic predicates).

```prolog
?- X is 2 ** 10, Y is 2 ^ 100, Z is 7 mod -2, W is max(3, 4.0).
X = 1024.0, Y = 1267650600228229401496703205376, Z = -1, W = 4.0.
```

## Definite clause grammars

`Head --> Body` clauses are translated into ordinary predicates with two extra arguments (the
difference list being parsed). Terminals are written as lists or strings, `{Goal}` embeds Prolog
goals, `!`, `\+`, `->`, `;` and `call//N` are supported, and `phrase/2,3` runs a grammar:

```prolog
greeting --> [hello], name.
name --> [world].
name --> [prolog].

?- phrase(greeting, [hello, prolog]).
true.
```

## Modules and tabling

`:- module(Name, [pred/arity, ...])` starts a module; `:- use_module(Name)` imports its exports, and
`Module:Goal` calls a predicate in a specific module. `:- table pred/arity` enables SLG tabling
(memoisation with left-recursion support) for a predicate — see Chapter 25.

# Part III — Operators

Operators let terms be written in infix, prefix or postfix notation. Each operator has a
**priority** (1..1200; lower binds tighter) and a **type** that fixes its position and
associativity: `xfx` (infix, non-associative), `xfy` (infix, right-associative), `yfx` (infix,
left-associative), `fy`/`fx` (prefix), `yf`/`xf` (postfix). In a type, `x` marks an argument whose
priority must be strictly lower than the operator's, `y` an argument whose priority may be equal.
Parentheses always override precedence, and an operator written in canonical form — `'+'(1, 2)` —
is an ordinary compound term. New operators are declared with `op/3`; `current_op/3` enumerates the
active table.

## Default operator table

| Priority | Type | Operators | Meaning |
|---|---|---|---|
| 1200 | xfx | `:-`  `-->` | Clause, DCG rule |
| 1200 | fx | `:-`  `?-` | Directive, query |
| 1150 | fx | `dynamic` `discontiguous` `multifile` `table` `meta_predicate` `module_transparent` | Declarations |
| 1100 | xfy | `;` | Disjunction |
| 1050 | xfy | `->`  `*->` | If-then, soft-cut |
| 1000 | xfy | `,` | Conjunction |
| 900 | fy | `\+` | Negation as failure |
| 760 | yfx | `#<==>` | CLP(FD) reified equivalence |
| 750 | xfy / yfx | `#==>` / `#<==` | CLP(FD) reified implication |
| 740 | yfx | `#\/` | CLP(FD) reified disjunction |
| 730 | yfx | `#\` | CLP(FD) reified exclusive or |
| 720 | yfx | `#/\` | CLP(FD) reified conjunction |
| 710 | fy | `#\` | CLP(FD) reified negation |
| 700 | xfx | `=` `\=` `==` `\==` `@<` `@=<` `@>` `@>=` `=..` `is` `=:=` `=\=` `<` `=<` `>` `>=` | Unification, comparison, arithmetic |
| 700 | xfx | `in` `ins` `#=` `#\=` `#<` `#>` `#=<` `#>=` | CLP(FD) constraints |
| 600 | xfy | `:` | Module qualification |
| 500 | yfx | `+` `-` `/\` `\/` `xor` | Additive and bitwise |
| 450 | xfx | `..` | CLP(FD) domain range |
| 400 | yfx | `*` `/` `//` `rem` `mod` `div` `rdiv` `<<` `>>` | Multiplicative and shifts |
| 200 | xfx | `**` | Float exponentiation |
| 200 | xfy | `^` | Integer power / existential quantifier |
| 200 | fy | `-` `+` `\` | Unary minus, plus, bitwise complement |

The list constructor `|` and the curly braces `{}` are part of the term syntax rather than
operators.

## Clause and directive operators

### :-/2, :-/1, ?-/1, -->/2

`Head :- Body` is a rule; `:- Goal` is a directive executed at load time; `?- Goal` is an
interactive query (also accepted in files); `Head --> Body` is a DCG rule.

```prolog
:- dynamic visited/1.
:- initialization((write('loaded'), nl)).

double(X, Y) :- Y is X * 2.
digits --> [D], { code_type(D, digit) }, digits.
digits --> [].
```

### dynamic/1, discontiguous/1, multifile/1, table/1, meta_predicate/1, module_transparent/1

Prefix declaration operators; they take a predicate indicator or a comma/list of indicators.

```prolog
:- dynamic stock/2, price/2.
:- discontiguous rule/2.
:- table fib/2.
:- meta_predicate twice(0).

twice(G) :- call(G), call(G).
```

## Control operators

### ,/2 — conjunction

Both goals must succeed, left to right; bindings made by the left goal are visible to the right one.

```prolog
?- X = 3, Y is X * 2.
X = 3, Y = 6.
```

### ;/2 — disjunction

Succeeds if either branch succeeds; on backtracking the second branch is tried.

```prolog
?- (X = 1 ; X = 2).
X = 1 ;
X = 2.
```

### ->/2 and *->/2 — if-then(-else) and soft-cut

`(C -> T ; E)` runs `T` after the *first* solution of `C`, otherwise `E`; without an else branch it
fails when `C` fails. `(C *-> T ; E)` runs `T` for *every* solution of `C` and `E` only if `C` has
none. A cut inside `C` is local to the condition.

```prolog
sign(X, S) :- ( X > 0 -> S = positive ; X < 0 -> S = negative ; S = zero ).

?- sign(-4, S).
S = negative.

?- (member(X, [1,2]) *-> true ; X = none).
X = 1 ;
X = 2.
```

### \+/1 — negation as failure

`\+ G` succeeds iff `G` has no solution; it never binds variables.

```prolog
?- \+ member(4, [1,2,3]).
true.
```

### !/0 — cut

Discards the choice points created since the clause was entered, including the remaining clauses of
the predicate. `call/1`, `\+`, `findall/3` and the condition of `->` are opaque to cut.

```prolog
first_even([X|_], X) :- 0 is X mod 2, !.
first_even([_|T], X) :- first_even(T, X).
```

## Unification and comparison operators

### =/2 and \=/2

```prolog
?- f(X, b) = f(a, Y).
X = a, Y = b.

?- f(X) \= g(X).
true.
```

### ==/2, \==/2, @</2, @=</2, @>/2, @>=/2 — identity and standard order

Structural comparison without unification. The standard order of terms is
`Var < Number < Atom < String < Compound`; numbers compare by value (a float precedes an equal
integer), atoms and strings alphabetically, compound terms by arity, then name, then arguments.

```prolog
?- X == Y.
false.

?- 1.0 @< 1, a @< b, f(a) @> z.
true.

?- msort([b, 2, a, 1.5, f(x), "s"], L).
L = [1.5, 2, a, b, "s", f(x)].
```

### =../2 — univ

Converts between a term and the list `[Functor|Args]`.

```prolog
?- foo(a, b) =.. L.
L = [foo, a, b].

?- T =.. [point, 1, 2].
T = point(1, 2).
```

### is/2 and the arithmetic comparisons =:=/2, =\=/2, </2, =</2, >/2, >=/2

`X is Expr` evaluates `Expr`; the comparisons evaluate both sides.

```prolog
?- X is (3 + 4) * 2, 10 =:= 10.0, 2 =\= 3, 1 < 2.
X = 14.
```

### :/2 — module qualification

Calls a predicate defined in a named module (see *Modules and tabling*). Built-in predicates are
global and cannot be qualified with a library name (`lists:append(...)` fails).

```prolog
:- module(shapes, [area/2]).
area(square(S), A) :- A is S * S.
area(circle(R), A) :- A is pi * R * R.

?- shapes:area(square(3), A).
A = 9.
```

## Arithmetic operators (evaluable functors)

All of these are evaluated inside `is/2` and the arithmetic comparisons.

### +/2, -/2, */2 — exact on integers, float otherwise

```prolog
?- X is 2 + 3 * 4, Y is 2.5 * 2, Z is 10 - 12.
X = 14, Y = 5.0, Z = -2.
```

### //2, ///2, div/2, mod/2, rem/2, rdiv/2

`/` is exact division (integer result when it divides evenly, float otherwise); `//` truncates
toward zero; `div` floors; `mod` takes the sign of the divisor, `rem` the sign of the dividend;
`rdiv` builds a rational number.

```prolog
?- A is 7 / 2, B is 8 / 2, C is -7 // 2, D is -7 div 2, E is -7 mod 2, F is -7 rem 2.
A = 3.5, B = 4, C = -3, D = -4, E = 1, F = -1.
```

### **/2 and ^/2 — exponentiation

`**` always returns a float; `^` is the integer power when both operands are integers (an error for
a negative exponent unless the base is 1 or -1) and a float otherwise.

```prolog
?- X is 2 ** 3, Y is 2 ^ 3, Z is 2 ^ 200, W is 2.0 ^ 0.5.
X = 8.0, Y = 8, Z = 1606938044258990275541962092341162602522202993782792835301376, W = 1.4142135623730951.
```

### Unary -/1, +/1 and \/1

```prolog
?- X is - (3), Y is \ 5, Z is +(7).
X = -3, Y = -6, Z = 7.
```

### /\/2, \//2, xor/2, <</2, >>/2 — bitwise and shifts

```prolog
?- A is 12 /\ 10, B is 12 \/ 10, C is 12 xor 10, D is 1 << 10, E is 1024 >> 3.
A = 8, B = 14, C = 6, D = 1024, E = 128.
```

### ^/2 as existential quantifier

Inside `bagof/3` and `setof/3`, `Var^Goal` excludes `Var` from the free variables that split the
result into groups. Elsewhere `V^G` simply calls `G`.

```prolog
age(ann, 30). age(bob, 25). age(cid, 30).

?- setof(Name, Age^age(Name, Age), Names).
Names = [ann, bob, cid].

?- bagof(Name, age(Name, Age), Names).
Age = 25, Names = [bob] ;
Age = 30, Names = [ann, cid].
```

## CLP(FD) operators

### in/2, ins/2, ../2, #=/2, #\=/2, #</2, #>/2, #=</2, #>=/2

Constraint syntax for the finite-domain solver (Chapter 24): `X in 1..9` restricts a domain and
the `#`-comparisons post arithmetic constraints that are propagated before and during `label/1`.
`ins/2` is present in the operator table for compatibility but is not implemented as a predicate:
restrict list elements with `in/2` (or `maplist/2` with an auxiliary predicate).

```prolog
?- X in 1..10, Y in 1..10, X + Y #= 12, X - Y #= 4, label([X, Y]).
X = 8, Y = 4.

?- A in 0..1, B in 0..1, C in 0..1, A + B + C #= 2, A #\= B, label([A, B, C]).
A = 0, B = 1, C = 1 ;
A = 1, B = 0, C = 1.
```

## User-defined operators

`op(+Priority, +Type, +Name)` adds (or, with priority 0, removes) an operator; `Name` may be a list.
Definitions made in a file apply from that point on and are stored with compiled `.jpc` files. A
query is parsed as a whole before it runs, so an operator must be declared in an earlier directive
or query before it can be used in operator syntax.

```prolog
:- op(700, xfx, ===>).
:- op(200, xfy, likes).

rule(rain ===> wet).
fact(mary likes wine).

?- rule(A ===> B), X = (john likes (mary likes wine)).
A = rain, B = wet, X = john likes mary likes wine.

?- op(650, xfx, is_bigger), current_op(P, T, is_bigger).
P = 650, T = xfx.
```

Since v3.14.0 there is a single operator store per engine, so `current_op/3` reports operators
declared with `op/3` at the top level **and** those declared by `:- op/3` directives while consulting
a file. An `op/3` executed while a module file is being consulted is local to that module for
`current_op/3`; it is still honoured by the parser everywhere in the session.


# Part IV — Built-in predicates

The chapters that follow describe every registered built-in predicate, grouped by function. The
first thirteen chapters cover the ISO core and the classic library; the remaining chapters cover the
extended libraries that JProlog loads by default (cryptography, JSON, date/time, filesystem, OS,
regular expressions, XML, threads, CSV, logging, CLP(FD), tabling, HTTP, persistence, graphs,
concurrency, Java FFI, the module system, JDBC, networking, strings, streams, global variables,
coroutining and profiling).
