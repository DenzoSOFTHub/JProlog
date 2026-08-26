## String predicates

JProlog strings (`"text"` with the default `double_quotes=string` flag) are a distinct atomic type.
All `string_*` predicates also accept atoms and numbers as text input, and the `atom_*` predicates
accept strings, so the two families interoperate freely.

### string/1
**Purpose**: Type check — succeeds if the argument is a string object.

```prolog
?- string("abc").
true.

?- string(abc).
false.
```

### string_length/2
**Purpose**: `string_length(+Text, -Length)` — number of characters in a string (or atom/number).

```prolog
?- string_length("hello", N).
N = 5.

?- string_length(hello, N).
N = 5.
```

### string_concat/3
**Purpose**: `string_concat(?S1, ?S2, ?S3)` — concatenation of strings; with `S3` bound and the
others unbound it enumerates every split on backtracking.

```prolog
?- string_concat("ab", "cd", S).
S = "abcd".

?- string_concat(X, Y, "ab").
X = "", Y = "ab" ;
X = "a", Y = "b" ;
X = "ab", Y = "".
```

### string_chars/2, string_codes/2
**Purpose**: Convert between a string and its list of characters / character codes; both
directions work.

```prolog
?- string_chars("hi", Cs), string_codes("hi", Codes).
Cs = [h, i], Codes = [104, 105].

?- string_chars(S, [o, k]).
S = "ok".
```

### string_code/3
**Purpose**: `string_code(+Index, +String, -Code)` — code of the character at a 1-based index.

```prolog
?- string_code(2, "abc", C).
C = 98.
```

### sub_string/5
**Purpose**: `sub_string(+String, ?Before, ?Length, ?After, ?Sub)` — enumerate or test substrings,
exactly like `sub_atom/5` but producing strings.

```prolog
?- sub_string("hello world", 6, 5, _, S).
S = "world".

?- sub_string("abc", B, 1, A, S).
B = 0, A = 2, S = "a" ;
B = 1, A = 1, S = "b" ;
B = 2, A = 0, S = "c".
```

### string_to_atom/2, atom_string/2
**Purpose**: Convert between strings and atoms in either direction (`string_to_atom(?String, ?Atom)`,
`atom_string(?Atom, ?String)`). At least one argument must be bound.

```prolog
?- atom_string(hello, S), string_to_atom("world", A).
S = "hello", A = world.

?- atom_string(A, "42").
A = '42'.
```

### number_string/2
**Purpose**: `number_string(?Number, ?String)` — parse a string as a number (leading/trailing
whitespace allowed) or render a number as a string. A float keeps its type.

```prolog
?- number_string(N, " 3.0 "), number_string(42, S).
N = 3.0, S = "42".

?- number_string(N, "abc").
false.
```

Unlike `number_codes/2`, which raises `syntax_error(illegal_number)`, `number_string/2` fails
silently on text that is not a number (SWI-Prolog behaviour).

```prolog
?- catch(number_codes(N, "abc"), error(E, _), true).
E = syntax_error(illegal_number).
```

### split_string/4
**Purpose**: `split_string(+String, +SepChars, +PadChars, -SubStrings)` — split on any separator
character, stripping pad characters from each part.

```prolog
?- split_string("a,b,,c", ",", "", P).
P = ["a", "b", "", "c"].

?- split_string("  key = value ", "=", " ", P).
P = ["key", "value"].
```

### number_codes/2, atom_to_number/2, number_to_atom/2, to_codes/2
**Purpose**: `number_codes(?Number, ?Codes)` is the ISO conversion between a number and its code
list (accepting `0x`, `0o`, `0b`, `0'c` notations and raising `syntax_error` on bad text).
`atom_to_number(+Atom, -Number)` and `number_to_atom(+Number, -Atom)` are convenience aliases of
`atom_number/2`; `to_codes(+Text, -Codes)` turns any atom, string or number into a code list.

```prolog
?- number_codes(N, "0x1F"), number_codes(2.5, C).
N = 31, C = [50, 46, 53].

?- atom_to_number('12', N), number_to_atom(3.5, A), to_codes(hi, Cs).
N = 12, A = '3.5', Cs = [104, 105].
```

### upcase_atom/2, downcase_atom/2
**Purpose**: Case conversion of an atom (or string) into an atom.

```prolog
?- upcase_atom('Hello', U), downcase_atom('Hello', D).
U = 'HELLO', D = hello.
```

## Stream predicates

Streams are created by `open/3,4` and referred to by the stream term it returns or by an alias
(`user_input`, `user_output`, `user_error`, or an alias given with `open/4`). The predicates
below complete the file/stream chapter of the ISO core.

### current_input/1, current_output/1
**Purpose**: Unify the argument with the current input / output stream. The current output stream
is per thread, so a query running in the IDE or in a worker thread sees its own console.

```prolog
?- current_output(S), write(S, hello), nl(S).
hello
S = user_output.
```

### set_input/1, set_output/1
**Purpose**: Make a stream (or alias) the current input / output for the subsequent `read/1`,
`write/1`, `nl/0` … calls that take no explicit stream.

```prolog
capture(File, Goal) :-
    open(File, write, S),
    current_output(Old),
    set_output(S),
    call(Goal),
    set_output(Old),
    close(S).

?- capture('out.txt', (write(hello), nl)).
true.
```

### flush_output/0, flush_output/1
**Purpose**: Force buffered output to be written (to the current output stream, or to the given one).
Useful before a blocking read or a long computation.

```prolog
?- write('Name? '), flush_output, read(Name).
```

### close/1, close/2
**Purpose**: Close a stream. `close(Stream, Options)` accepts `force(true)` to ignore errors while
closing. Closing `user_input`/`user_output` is a no-op.

```prolog
?- open('data.txt', read, S), read(S, T), close(S, [force(true)]).
```

### stream_property/2
**Purpose**: `stream_property(?Stream, ?Property)` — enumerate stream properties. Both arguments may
be unbound, so the predicate enumerates every open stream and every property of it. The complete
ISO set is reported: `file_name(F)`, `mode(M)`, `input`, `output`, `alias(A)`,
`position(P)`, `end_of_stream(E)`, `eof_action(A)`, `reposition(B)`, `type(T)`, `text`/`binary`,
`encoding(E)` and `line_count(N)`.

```prolog
?- stream_property(user_error, alias(A)).
A = user_error.

?- open('data.txt', read, S), stream_property(S, mode(M)), stream_property(S, type(T)), close(S).
M = read, T = text.
```

An unbound first argument is bound to the **canonical** stream term (`'$stream'(N)`), never to an
alias: `stream_property(S, alias(user_error))` gives `S = '$stream'(2)`. Enumerating never blocks —
`end_of_stream` is reported as `not` for a stream that cannot be repositioned (stdin, a socket),
because deciding it there would mean waiting for input.

### current_stream/3, set_stream/2
**Purpose**: `current_stream(?File, ?Mode, ?Stream)` enumerates the open streams with their file
name and mode. `set_stream(+Stream, +Property)` changes a property of an open stream; `alias(A)`
adds an alias that every stream argument then accepts.

```prolog
?- open('data.txt', read, S), current_stream(F, M, S), close(S).
F = 'data.txt', M = read.

?- open('data.txt', read, S), set_stream(S, alias(input_file)),
   get_char(input_file, C), close(S).
C = h.
```

### stream_position/2, set_stream_position/2, seek/4
**Purpose**: `stream_position(+Stream, -Pos)` reads the byte position of a file stream;
`set_stream_position(+Stream, +Pos)` repositions it (`permission_error(reposition, stream, S)` for
streams that cannot be repositioned); `seek(+Stream, +Offset, +Method, -NewPos)` moves relative to
`bof`, `current` or `eof`. Since v3.14.0 a **text** stream is decoded one code point at a time
through its own decoder, so repositioning really does change what the next `get_char/2` reads —
it is no longer binary-only.

```prolog
% data.txt contains the text "hello"
?- open('data.txt', read, S), get_char(S, C1), stream_position(S, P),
   get_char(S, C2), set_stream_position(S, P), get_char(S, C3), close(S).
C1 = h, P = 1, C2 = e, C3 = e.

?- open('data.txt', read, S, [type(binary)]), get_byte(S, B1),
   seek(S, 0, bof, N), get_byte(S, B2), close(S).
B1 = 104, N = 0, B2 = 104.
```

### character_count/2, line_count/2, line_position/2, stream_position_data/3
**Purpose**: `character_count(+Stream, -N)`, `line_count(+Stream, -N)` and
`line_position(+Stream, -N)` report how far a stream has been read or written: the number of
characters consumed, the current line number (1-based) and the column within it (0-based).
`stream_position_data(+Field, +Position, -Value)` pulls the same three fields out of the opaque
position term that `stream_property(S, position(P))` yields; `Field` is `char_count`, `line_count`
or `line_position`.

```prolog
% data.txt contains the text "hello"
?- open('data.txt', read, S), get_char(S, _), get_char(S, _),
   character_count(S, N), line_count(S, L), line_position(S, C), close(S).
N = 2, L = 1, C = 2.

?- open('data.txt', read, S), get_char(S, _),
   stream_property(S, position(P)), stream_position_data(char_count, P, N), close(S).
N = 1.
```

### peek_char/1,2, peek_code/1,2, peek_byte/1,2
**Purpose**: Look at the next character / code / byte of the input without consuming it. Return
`end_of_file` (or -1 for codes and bytes) at the end of the stream.

```prolog
skip_spaces(S) :- peek_char(S, C), C == ' ', !, get_char(S, _), skip_spaces(S).
skip_spaces(_).
```

### put_byte/1,2, get_byte/1,2
**Purpose**: Binary I/O on streams opened with `type(binary)`.

```prolog
?- open('raw.bin', write, S, [type(binary)]), put_byte(S, 255), put_byte(S, 0), close(S),
   open('raw.bin', read, R, [type(binary)]), get_byte(R, B1), get_byte(R, B2), close(R).
B1 = 255, B2 = 0.
```

### at_end_of_stream/0,1
**Purpose**: Succeeds when the (current or given) input stream has no more data.

```prolog
read_all(S, []) :- at_end_of_stream(S), !.
read_all(S, [C|Cs]) :- get_char(S, C), read_all(S, Cs).
```

## Global variables

Global variables are a per-engine key/value store. `nb_setval/2` copies the value and keeps it
across backtracking; `b_setval/2` stores the value so that it is restored on backtracking.

### nb_setval/2, nb_getval/2
**Purpose**: Set / read a non-backtrackable global variable. Reading an unset name raises
`existence_error(variable, Name)`.

```prolog
count_solutions(Goal, N) :-
    nb_setval(cnt, 0),
    ( call(Goal), nb_getval(cnt, C0), C is C0 + 1, nb_setval(cnt, C), fail ; true ),
    nb_getval(cnt, N).

?- count_solutions(member(_, [a, b, c]), N).
N = 3.
```

### b_setval/2, b_getval/2
**Purpose**: Backtrackable global variables: the previous value is restored when execution
backtracks over the `b_setval/2` call.

```prolog
?- b_setval(v, 1), ( b_setval(v, 2), fail ; true ), b_getval(v, V).
V = 1.
```

### nb_current/2, nb_delete/1
**Purpose**: `nb_current(?Name, ?Value)` enumerates the defined global variables;
`nb_delete(+Name)` removes one (silently succeeds if it does not exist).

```prolog
?- nb_setval(a, 1), nb_setval(b, two), findall(K-V, nb_current(K, V), L), nb_delete(a).
L = [a-1, b-two].
```

## Attributed variables and coroutining

Attributed variables carry extra data that is consulted when the variable is bound. They are the
basis of `freeze/2`, `dif/2`, `when/2` and of the CLP(FD) solver. Attributes survive between
top-level queries for the same variable name in the same engine.

### put_attr/3, get_attr/3, del_attr/2, attvar/1
**Purpose**: `put_attr(+Var, +Module, +Value)` attaches (or replaces) the attribute `Module` of an
unbound variable; `get_attr(+Var, +Module, -Value)` reads it; `del_attr(+Var, +Module)` removes it;
`attvar(@Term)` succeeds if the term is a variable with at least one attribute.

```prolog
?- put_attr(X, colour, red), get_attr(X, colour, C), attvar(X).
C = red.

?- put_attr(X, colour, red), del_attr(X, colour), attvar(X).
false.
```

### freeze/2
**Purpose**: `freeze(?Var, :Goal)` — delay `Goal` until `Var` is bound; if `Var` is already bound
the goal runs immediately. The goal runs at the moment of binding, inside the unification.

```prolog
?- freeze(X, (write(bound(X)), nl)), X = 42.
bound(42)
X = 42.

?- freeze(X, X > 3), member(X, [1, 5, 2, 7]).
X = 5 ;
X = 7.
```

### dif/2
**Purpose**: `dif(?X, ?Y)` — constrains two terms to be different; fails at once when they are
identical, succeeds when they can never unify, and otherwise suspends until the decision can be made.

```prolog
?- dif(X, a), member(X, [a, b, c]).
X = b ;
X = c.

?- dif(f(X), f(Y)), X = 1, Y = 1.
false.
```

### when/2
**Purpose**: `when(+Condition, :Goal)` — run `Goal` as soon as `Condition` holds. Conditions:
`nonvar(X)`, `ground(T)`, `?=(X, Y)` (decidable comparison), and conjunctions/disjunctions of these.

```prolog
?- when(ground(X-Y), (Z is X + Y, write(sum(Z)), nl)), X = 1, Y = 2.
sum(3)
X = 1, Y = 2, Z = 3.

?- when((nonvar(A) ; nonvar(B)), write(ready)), B = 1.
ready
B = 1.
```

A woken goal runs in the query's own binding context, so the bindings it makes (`Z` above) are
ordinary bindings, it is traced like any other goal, it is charged to the inference budget, and it
can throw into the enclosing `catch/3`. (Before 4.0.0 a `when/2`-woken goal's bindings were lost on
the then-default engine and only its side effects were visible; that is fixed.)

### frozen/2, ?=/2
**Purpose**: `frozen(?Var, -Goal)` returns the conjunction of goals currently delayed on `Var`, or
`true` when there are none. `?=(X, Y)` succeeds when the comparison of `X` and `Y` is already
decidable — that is, when they are identical or can never unify.

```prolog
?- freeze(X, writeln(hi)), frozen(X, G).
G = writeln(hi).

?- frozen(Y, G).
G = true.

?- ?=(a, b).
true.

?- ?=(X, Y).
false.
```

### term_attvars/2, copy_term/3, unifiable/3
**Purpose**: `term_attvars(+Term, -Vars)` collects the attributed variables reachable from `Term`.
`copy_term(+Term, -Copy, -Attributes)` copies `Term` without its attributes and returns them as a
list of goals that would restore them. `unifiable(@X, @Y, -Unifier)` returns the list of
`Var = Value` bindings that unifying `X` and `Y` would make, **without** making them.

```prolog
?- put_attr(X, mymod, 1), term_attvars(f(X, Y), Vs).
Vs = [X].

?- put_attr(X, mymod, 1), copy_term(X, Y, Attrs).
Attrs = [put_attr(Y, mymod, 1)].

?- unifiable(f(X, b), f(a, Y), U).
U = [X = a, Y = b].
```

## More list and pair predicates

### msort/2
**Purpose**: `msort(+List, -Sorted)` — sort in the standard order of terms **without** removing
duplicates (`sort/2` removes them).

```prolog
?- msort([c, a, b, a], L).
L = [a, a, b, c].
```

### predsort/3
**Purpose**: `predsort(:Pred, +List, -Sorted)` — sort with a user comparison called as
`call(Pred, Order, A, B)` where `Order` is `<`, `=` or `>`. Elements that compare `=` are
merged into one.

```prolog
by_length(O, A, B) :- atom_length(A, LA), atom_length(B, LB), compare(O, LA-A, LB-B).

?- predsort(by_length, [ccc, a, bb], L).
L = [a, bb, ccc].
```

### min_list/2, max_list/2, sum_list/2, sumlist/2
**Purpose**: Minimum, maximum and sum of a list of numbers (`sumlist/2` is an alias of
`sum_list/2`). The result keeps the type of the operands: a list containing a float sums to a
float.

```prolog
?- min_list([3, 1.5, 2], Min), max_list([3, 1.5, 2], Max), sum_list([1, 2, 3], S), sum_list([1.5, 1.5], F).
Min = 1.5, Max = 3, S = 6, F = 3.0.
```

### nth0/3, nth1/3
**Purpose**: Element at a 0-based / 1-based index; with the index unbound they enumerate.

```prolog
?- nth1(2, [a, b, c], X), nth0(I, [a, b, c], c).
X = b, I = 2.
```

### partition/4, partition/5
**Purpose**: `partition(:Pred, +List, ?Included, ?Excluded)` splits a list in one pass;
`partition(:Pred, +List, ?Less, ?Equal, ?Greater)` uses `call(Pred, X, Order)` with `Order` one of
`<`, `=`, `>`. Both are library predicates written in Prolog, so **a user definition of
`partition/4` overrides them** (many textbook quicksorts define their own, taking a pivot rather
than a goal).

```prolog
?- partition([X]>>(X > 2), [1, 2, 3, 4], Big, Small).
Big = [3, 4], Small = [1, 2].
```

### exclude/3, include/3
**Purpose**: Filter a list with a goal: `include(:Goal, +List, -Kept)` keeps the elements for
which `call(Goal, Elem)` succeeds, `exclude/3` keeps the others. (`partition/4` is deliberately not
provided as a built-in so that programs may define their own `partition/4`, as quicksort examples
commonly do.)

```prolog
even(X) :- 0 is X mod 2.

?- exclude(even, [1, 2, 3, 4], Odd), include(even, [1, 2, 3, 4], Even).
Odd = [1, 3], Even = [2, 4].
```

### pairs_keys_values/3, pairs_keys/2, pairs_values/2
**Purpose**: Convert between a list of `Key-Value` pairs and its keys / values.

```prolog
?- pairs_keys_values(P, [a, b], [1, 2]), pairs_keys(P, K), pairs_values(P, V).
P = [a-1, b-2], K = [a, b], V = [1, 2].
```

## Term inspection and modification

### put_code/1,2
**Purpose**: `put_code(+Code)` writes the character with the given code to the current output;
`put_code(+Stream, +Code)` writes it to `Stream`. The counterpart of `get_code/1,2`.

```prolog
?- put_code(0'h), put_code(0'i), nl.
hi
true.
```

*v4.2.0*: the two-argument form actually writes to `Stream`. Before, the arity entry existed but
the implementation raised "put_code/1 requires exactly 1 argument".

### unify_with_occurs_check/2
**Purpose**: ISO unification **with** the occurs check, whatever the `occurs_check` flag says:
it fails rather than building a cyclic term.

```prolog
?- unify_with_occurs_check(X, f(Y)).
X = f(Y).

?- unify_with_occurs_check(X, f(X)).
false.
```

### setarg/3
**Purpose**: `setarg(+Index, +Term, +Value)` — destructively replace the Index-th argument of a
compound term; the change is undone on backtracking.

```prolog
?- T = f(a, b), setarg(1, T, z).
T = f(z, b).

?- T = f(a), ( setarg(1, T, z), fail ; true ).
T = f(a).
```

### nb_setarg/3
**Purpose**: `nb_setarg(+N, +Compound, +Value)` replaces the N-th argument like `setarg/3`, but the
change is **not undone on backtracking**. Used to accumulate a result across a failure-driven loop.

```prolog
?- T = counter(0), forall(member(_, [a,b,c]),
       (arg(1, T, C), C1 is C + 1, nb_setarg(1, T, C1))), arg(1, T, N).
T = counter(3), N = 3.
```

### numbervars/3, number_vars/3
**Purpose**: `numbervars(+Term, +Start, -End)` binds every free variable of `Term` to `'$VAR'(N)`
with consecutive `N` from `Start`; `End` is the next free number. `write/1` and `print/1` render
`'$VAR'(0)` as `A`, `'$VAR'(1)` as `B`, and so on. `number_vars/3` is an alias.

```prolog
?- T = f(X, Y, X), numbervars(T, 0, End), print(T), nl.
f(A, B, A)
End = 2.
```

### must_be/2
**Purpose**: `must_be(+Type, @Value)` — succeed if `Value` has the type, otherwise throw the
appropriate ISO error (`instantiation_error` for an unbound value when the type requires one,
`type_error(Type, Value)` or `domain_error(Type, Value)` otherwise). Types: `atom`, `atomic`,
`integer`, `float`, `number`, `callable`, `compound`, `var`, `nonvar`, `ground`, `list`,
`boolean`, `positive_integer`, `nonneg`.

```prolog
?- must_be(integer, 3).
true.

?- catch(must_be(positive_integer, 0), error(E, _), true).
E = type_error(positive_integer, 0).
```

### simple/1, partial_list/1, rational/1, acyclic_term/1
**Purpose**: Additional type tests: `simple/1` is true for atomic terms (atom, number or string); `partial_list/1` for a list whose tail is unbound; `rational/1` for a
rational number term; `acyclic_term/1` succeeds for terms without cycles.

```prolog
?- simple(abc), partial_list([a, b|_]), acyclic_term(f(X)).
true.

?- partial_list([a, b]).
false.
```

## Clause inspection and operators

### clause/2
**Purpose**: `clause(+Head, ?Body)` — enumerate the clauses of a user predicate; facts have body
`true`. Built-in predicates raise `permission_error(access, private_procedure, PI)`.

```prolog
grandparent(GP, GC) :- parent(GP, P), parent(P, GC).

?- clause(grandparent(A, B), Body).
Body = (parent(A, _P), parent(_P, B)).
```

### predicate_property/2
**Purpose**: `predicate_property(:Head, ?Property)` — properties of a predicate: `built_in`,
`dynamic`, `static`, `defined`, `undefined`. With `Property` unbound it enumerates them.

```prolog
?- predicate_property(append(_, _, _), built_in).
true.

?- assertz(fact(1)), predicate_property(fact(_), dynamic).
true.
```

### op/3, current_op/3
**Purpose**: `op(+Priority, +Type, +Name)` defines (priority 1..1200) or removes (priority 0) an
operator; `Name` may be a list of atoms. `current_op(?Priority, ?Type, ?Name)` enumerates the
active operators. Part III lists the default table.

The operator store belongs to the `Prolog` instance: two engines in one JVM do not see each other's
operators, an `op/3` inside a module file is local to that module for `current_op/3`, and an
`op/3` executed in a branch that later fails is undone — `(op(700, xfx, tmp), fail ; true)` leaves
no `tmp` operator.

```prolog
?- op(700, xfx, is_bigger), X =.. [is_bigger, elephant, mouse].
X = elephant is_bigger mouse.

?- findall(P-T, current_op(P, T, mod), L).
L = [400-yfx].
```

### char_conversion/2, current_char_conversion/2
**Purpose**: ISO character conversion table applied while reading terms when the `char_conversion`
flag is `true`. `current_char_conversion(?In, ?Out)` enumerates the active mappings — the declared
ones first, then the identity mapping of every other printable ASCII character. The table belongs to
the `Prolog` instance, and a conversion declared in a branch that later fails is undone.
`char_conversion(C, C)` removes the mapping for `C`.

```prolog
?- char_conversion(a, b), current_char_conversion(a, X).
X = b.
```

### code_type/2, char_type/2
**Purpose**: Classify a character code (`code_type/2`) or a character (`char_type/2`). Both accept
the same classes: `alpha`, `alnum`, `digit`, `xdigit`, `space`, `white`, `layout`, `upper`, `lower`,
`punct`, `csym`, `csymf`, `end_of_line`, `newline`, `end_of_file`, `graph`, `print`, `ascii`,
`cntrl`, `meta`, `solo`, `symbol`, `period`, `quote`, `paren`.

Both are nondeterministic: with the character unbound they generate (over the ASCII range), with the
type unbound they enumerate every class the character belongs to, and with both unbound they
enumerate every pair.

The **parametric forms** — `digit(Weight)`, `upper(Lower)`, `lower(Upper)`, `to_lower(Lower)` and
`to_upper(Upper)` — work in every mode: bound they test, unbound they bind, and with the character
unbound they generate. `char_type/2` gives a character where `code_type/2` gives a code;
`digit(Weight)` gives an integer weight in both.

```prolog
?- code_type(0'7, digit), char_type('A', upper), char_type(a, lower), char_type(' ', space).
true.

?- char_type('7', digit(W)).
W = 7.

?- char_type('A', upper(L)).
L = a.

?- char_type(a, to_upper(U)).
U = 'A'.

?- code_type(0'a, lower(U)).
U = 65.

?- char_type(X, to_upper('A')).
X = 'A' ;
X = a.
```

## Debugging and profiling

### profile/0, noprofile/0, profile_data/1, reset_profile/0
**Purpose**: Count predicate calls. `profile/0` starts counting, `noprofile/0` stops it,
`profile_data(-Data)` unifies a list of `Name/Arity-Count` pairs sorted by count, and
`reset_profile/0` clears the counters.

Only user-defined predicates are counted.

```prolog
foo(1). foo(2).

?- reset_profile, profile, (foo(_), fail ; true), noprofile, profile_data(D).
D = [foo/1-1].
```

### spy/1, nospy/1, spying/1, debugging/0, leash/1
**Purpose**: Spy points mark predicates whose ports are shown by the tracer. `spy(+PI)` and
`nospy(+PI)` add/remove a spy point (`Name/Arity`), `spying(?PI)` enumerates them, `debugging/0`
prints the trace state and spy points, and `leash(+Ports)` selects the ports (`call`, `exit`,
`redo`, `fail`, `all`, `none`) at which an interactive tracer stops.

```prolog
?- spy(foo/2), spying(P).
% Spy point set on foo/2
P = foo/2.

?- debugging.
Tracing is OFF
Spy points:
  foo/2
true.
```

### trace/0, notrace/0
**Purpose**: Enable / disable four-port tracing (`Call`, `Exit`, `Redo`, `Fail` lines are written
to the current output for every user predicate and traced built-in).

```prolog
?- trace, member(X, [1, 2]), X > 1, notrace.
% Tracing enabled
Call: (0) member(X,[1,2])
Exit: (0) member(1,[1,2])
Redo: (0) member(2,[1,2])
Exit: (0) member(2,[1,2])
  Call: (1) notrace
% Tracing disabled
X = 2.
```

### cut/0
**Purpose**: A callable synonym of `!` kept for programs that write the cut as a plain atom goal
through `call/1`; inside a clause body always use `!`.

## System and memory

### atom_gc/0, atom_table_size/1
**Purpose**: `atom_gc/0` releases interned atoms that are no longer referenced;
`atom_table_size(-N)` reports the number of interned atoms.

```prolog
?- atom_gc, atom_table_size(N).
N = 75.
```

### shell2/2, sleep/1
**Purpose**: `shell2(+Command, -ExitCode)` runs a shell command and returns its exit status
(`shell/1,2` are the other forms); `sleep(+Seconds)` pauses the current thread (fractions allowed).

```prolog
?- shell2('ls /nonexistent', Code).
Code = 2.

?- sleep(0.5).
true.
```

### working_directory/2, file_modified/2, delete_directory/1
**Purpose**: `working_directory(-Old, +New)` reads and changes the process working directory
(`working_directory(D, D)` only reads it); `file_modified(+Path, -Millis)` gives the modification
time in epoch milliseconds; `delete_directory(+Path)` removes an empty directory.

```prolog
?- working_directory(D, D).
D = '/home/user/project'.

?- open('note.txt', write, W), close(W), file_modified('note.txt', T).
T = 1787644907965.
```

### format_time/3, parse_time/3
**Purpose**: `format_time(+Pattern, +Timestamp, -Text)` formats epoch milliseconds (or a datetime
atom) with a Java `DateTimeFormatter` pattern; `parse_time(+Pattern, +Text, -Millis)` is the
inverse.

```prolog
?- parse_time('yyyy-MM-dd HH:mm:ss', '2026-03-20 14:30:00', Ms),
   format_time('dd/MM/yyyy', Ms, Day).
Ms = 1774013400000, Day = '20/03/2026'.
```

The millisecond value depends on the local time zone.

## JDBC database access

The JDBC predicates connect a Prolog program to any database with a JDBC driver on the class path.
Connections, prepared statements and callable statements are represented by handle atoms
(`'$jdbc_conn_1'`, …). All predicates raise `existence_error` for an unknown handle and a
`jdbc_error(Message)` ball for SQL failures. They are removed by `enableSafeMode()`.

### jdbc_driver_load/1
**Purpose**: Load a driver class by name (needed only by drivers that do not self-register).

```prolog
?- jdbc_driver_load('org.h2.Driver').
true.
```

### jdbc_connect/2, jdbc_connect/4, jdbc_disconnect/1
**Purpose**: Open a connection from a JDBC URL, optionally with user and password; close it with
`jdbc_disconnect/1`.

```prolog
?- jdbc_connect('jdbc:h2:mem:testdb', C).
C = '$jdbc_conn_1'.

?- jdbc_connect('jdbc:postgresql://localhost/mydb', 'user', 'pass', C), jdbc_disconnect(C).
```

### jdbc_query/3, jdbc_execute_update/3
**Purpose**: `jdbc_query(+Conn, +SQL, -Rows)` runs a SELECT and returns a list of `row(Col1, ...)`
terms; `jdbc_execute_update(+Conn, +SQL, -Count)` runs INSERT/UPDATE/DELETE/DDL and returns the
number of affected rows.

```prolog
?- jdbc_execute_update(C, 'CREATE TABLE users(id INT, name VARCHAR(50))', _),
   jdbc_execute_update(C, 'INSERT INTO users VALUES (1, ''Alice'')', N),
   jdbc_query(C, 'SELECT id, name FROM users', Rows).
N = 1, Rows = [row(1, 'Alice')].
```

### jdbc_prepare/3, jdbc_set_param/3, jdbc_set_params/2, jdbc_execute_prepared_query/2, jdbc_execute_prepared_update/2, jdbc_close_statement/1
**Purpose**: Prepared statements with `?` placeholders: prepare, bind parameters (1-based, or all
at once from a list; use the atom `null` for SQL NULL), execute as a query or an update, close.

```prolog
insert_user(C, Id, Name) :-
    jdbc_prepare(C, 'INSERT INTO users(id, name) VALUES (?, ?)', S),
    jdbc_set_params(S, [Id, Name]),
    jdbc_execute_prepared_update(S, 1),
    jdbc_close_statement(S).

older_than(C, Age, Rows) :-
    jdbc_prepare(C, 'SELECT name FROM users WHERE age > ?', S),
    jdbc_set_param(S, 1, Age),
    jdbc_execute_prepared_query(S, Rows),
    jdbc_close_statement(S).
```

### jdbc_set_autocommit/2, jdbc_commit/1, jdbc_rollback/1
**Purpose**: Transaction control. Disable autocommit, run statements, then commit or roll back.

```prolog
transfer(C, From, To, Amount) :-
    jdbc_set_autocommit(C, false),
    catch(( debit(C, From, Amount), credit(C, To, Amount), jdbc_commit(C) ),
          E,
          ( jdbc_rollback(C), throw(E) )),
    jdbc_set_autocommit(C, true).
```

### jdbc_tables/2, jdbc_columns/3
**Purpose**: Metadata: the list of table names, and the columns of a table as
`column(Name, TypeName, Size)` terms.

```prolog
?- jdbc_tables(C, Ts), jdbc_columns(C, 'USERS', Cols).
Ts = ['USERS'], Cols = [column('ID', 'INTEGER', 10), column('NAME', 'VARCHAR', 50)].
```

### jdbc_prepare_call/3, jdbc_call_set_param/3, jdbc_call_register_out/3, jdbc_call_execute/1, jdbc_call_get_result/3, jdbc_call_get_resultset/2
**Purpose**: Stored procedures through callable statements: prepare `{call proc(?, ?)}`, set IN
parameters, register OUT parameters with a type (`integer`, `bigint`, `double`, `decimal`,
`varchar`, `boolean`, `date`, `timestamp`), execute, then read OUT values or the returned result
set as `row/N` terms.

```prolog
user_count(C, Table, Count) :-
    jdbc_prepare_call(C, '{call get_user_count(?, ?)}', S),
    jdbc_call_set_param(S, 1, Table),
    jdbc_call_register_out(S, 2, integer),
    jdbc_call_execute(S),
    jdbc_call_get_result(S, 2, Count),
    jdbc_close_statement(S).
```

### jdbc_set_clob/3, jdbc_get_clob/3, jdbc_set_blob/3, jdbc_set_blob_bytes/3, jdbc_get_blob_bytes/3, jdbc_get_blob_to_file/3
**Purpose**: Large objects: set a CLOB parameter from text, a BLOB parameter from a file path or a
byte list; read a CLOB as an atom, a BLOB as a byte list, or save a BLOB straight to a file. The
`get` variants take a SELECT returning a single LOB column.

```prolog
store_doc(C, Id, Text) :-
    jdbc_prepare(C, 'INSERT INTO docs(id, body) VALUES (?, ?)', S),
    jdbc_set_param(S, 1, Id), jdbc_set_clob(S, 2, Text),
    jdbc_execute_prepared_update(S, _), jdbc_close_statement(S).

?- jdbc_get_clob(C, 'SELECT body FROM docs WHERE id = 1', Text).
Text = 'A long document ...'.

?- jdbc_get_blob_bytes(C, 'SELECT data FROM images WHERE id = 1', Bytes).
Bytes = [137, 80, 78, 71|...].
```

## TCP, UDP and DNS

Socket predicates exchange UTF-8 text; handles are atoms such as `'$socket_1'`. `tcp_accept/2`,
`tcp_receive/3` and `udp_receive/4` block until data arrives; a closed peer yields the atom
`end_of_stream`. These predicates are removed by `enableSafeMode()`.

### tcp_connect/3, tcp_send/2, tcp_receive/3, tcp_close/1
**Purpose**: TCP client: connect to `Host:Port`, send a string, receive up to `MaxBytes` bytes,
close.

```prolog
echo_client(Host, Port, Msg, Reply) :-
    tcp_connect(Host, Port, S),
    tcp_send(S, Msg),
    tcp_receive(S, Reply, 4096),
    tcp_close(S).

?- echo_client(localhost, 7777, 'ping', R).
R = pong.
```

### tcp_server_socket/2, tcp_accept/2
**Purpose**: TCP server: bind a listening socket to a port, then accept client connections one at a
time.

```prolog
serve_once(Port) :-
    tcp_server_socket(Port, SS),
    tcp_accept(SS, Client),
    tcp_receive(Client, Data, 4096),
    tcp_send(Client, Data),          % echo
    tcp_close(Client),
    tcp_close(SS).
```

### udp_socket/2, udp_send/4, udp_receive/4, udp_close/1
**Purpose**: UDP datagrams: `udp_socket(+Port, -Sock)` binds a socket (port 0 = any free port),
`udp_send(+Sock, +Host, +Port, +Data)`, `udp_receive(+Sock, -Data, -From, +MaxBytes)` with
`From = from(IP, Port)`.

```prolog
udp_ping(Host, Port, Reply) :-
    udp_socket(0, S),
    udp_send(S, Host, Port, ping),
    udp_receive(S, Reply, from(_, _), 1024),
    udp_close(S).
```

### hostname_address/2, hostname/1
**Purpose**: `hostname_address(+Name, -IP)` resolves a host name; `hostname(-Name)` returns the
local host name.

```prolog
?- hostname_address(localhost, IP).
IP = '127.0.0.1'.
```

### http_request/4, http_post/4
**Purpose**: Minimal HTTP client: `http_request(+Method, +URL, -Status, -Body)` with `Method` one of
`get`, `head`, `delete`, `put`, `post`; `http_post(+URL, +RequestBody, -Status, -Body)` sends a
form-encoded body. Both use 30-second timeouts. The richer `http_get/3`, `http_open/3` and the
JSON-aware `http_client_*` predicates are described in Chapter 26.

```prolog
?- http_request(get, 'http://httpbin.org/get', Status, Body), Status =:= 200.
Status = 200, Body = '{"args":{}, ...}'.
```

## DCG support predicates

### dcg_translate_rule/2, dcg_body//2
**Purpose**: `dcg_translate_rule(+Rule, -Clause)` returns the ordinary clause a `-->` rule is
translated to, exposing the translator used by `consult/1` and `assertz/1`. `dcg_body//2` is a
reserved grammar non-terminal (two arguments plus the difference list) whose current
implementation simply succeeds; it is kept for source compatibility.

```prolog
?- dcg_translate_rule((a --> [x], b), C).
C = (a(S0, S) :- S0 = [x|S1], b(S1, S)).
```
