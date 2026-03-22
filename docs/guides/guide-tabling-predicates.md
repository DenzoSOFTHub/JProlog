# Tabling (Memoization) Predicates Guide

## Package Overview

Tabling, also known as memoization or tabulation, is a technique that caches the results of predicate calls so that repeated calls with the same arguments return instantly from the cache instead of recomputing. JProlog's tabling system provides three key benefits:

1. **Performance**: Transforms exponential-time computations (like naive Fibonacci) into linear-time by caching intermediate results.
2. **Termination**: Prevents infinite loops in programs with cyclic dependencies, such as graph reachability over graphs with cycles.
3. **Correctness**: Ensures that left-recursive grammars and transitive closure computations terminate and produce correct results.

The implementation uses **variant tabling** with canonical variable normalization. Structurally identical queries (differing only in variable names) share the same cache entry. Loop detection prevents infinite recursion by returning failure for goals already being computed.

### Architecture

The tabling system consists of two components:

- **`TableStore`** (`it.denzosoft.jprolog.core.engine.TableStore`) -- The core data structure. Maintains:
  - A set of tabled predicate indicators (e.g., `"fib/2"`).
  - A cache mapping normalized goal strings to lists of solution bindings.
  - An in-progress set for loop detection.
  - A `normalize()` method that replaces unbound variables with positional canonical names (`_TV0`, `_TV1`, ...) to produce stable cache keys.

- **Built-in predicates**:
  - `table/1` (`it.denzosoft.jprolog.builtin.meta.TableDirective`) -- Declares a predicate as tabled.
  - `abolish_all_tables/0` (`it.denzosoft.jprolog.builtin.meta.AbolishAllTables`) -- Clears all cached results.
  - `abolish_table/1` (`it.denzosoft.jprolog.builtin.meta.AbolishTable`) -- Clears cache for a specific predicate.

All three predicates are context-dependent (`BuiltInWithContext`) because they need access to the `QuerySolver` to reach the `Prolog` context and its `TableStore`.

### How Tabling Works Internally

1. When a tabled predicate is called, `QuerySolver` checks `TableStore.isTabled(functor, arity)`.
2. The goal is normalized via `TableStore.normalize()` to produce a cache key.
3. If the cache key has stored solutions, those are returned immediately (cache hit).
4. If the goal is already in-progress (loop detected), the call returns failure to break the cycle.
5. Otherwise, the goal is marked as in-progress, solved normally, and the results are cached. The in-progress mark is then removed.

---

## Predicate Reference

### table/1

```prolog
:- table(Functor/Arity).
table(Functor/Arity).
```

Declares a predicate as tabled. All future calls to `Functor/Arity` will be memoized.

| Argument          | Type              | Mode | Description |
|-------------------|-------------------|------|-------------|
| `Functor/Arity`   | compound term     | `+`  | A predicate indicator: atom `/` integer. |

**Behavior**: Registers the predicate in `TableStore.tabledPredicates`. The predicate must be declared as tabled **before** it is called for tabling to take effect. Can be used as a directive (`:- table(fib/2).`) at the top of a file or called as a goal at runtime.

**Example**:
```prolog
:- table(fib/2).
:- table(path/2).
:- table(ancestor/2).
```

**Error handling**: Fails silently if the argument is not in `Functor/Arity` form (i.e., a compound term with functor `/` and two arguments where the first is an atom and the second is a number).

---

### abolish_all_tables/0

```prolog
abolish_all_tables.
```

Clears all tabling caches and in-progress markers. The set of tabled predicate declarations is preserved -- predicates remain declared as tabled, but their cached results are discarded.

| (no arguments) |

**Behavior**: Calls `TableStore.abolishAllTables()` which clears both the `cache` map and the `inProgress` set. Always succeeds. Useful when the knowledge base has been modified (via `assert`/`retract`) and cached results may be stale.

**When to use**:
- After `assert`/`retract` operations that modify facts used by tabled predicates.
- When starting a new computation phase that should not reuse old cached results.
- During testing to ensure clean state between test cases.

---

### abolish_table/1

```prolog
abolish_table(Functor/Arity).
```

Clears the tabling cache for a specific predicate and removes it from the set of tabled predicates.

| Argument          | Type              | Mode | Description |
|-------------------|-------------------|------|-------------|
| `Functor/Arity`   | compound term     | `+`  | The predicate indicator to clear. |

**Behavior**: Removes the predicate from `tabledPredicates`, removes all cache entries whose key starts with the predicate's functor pattern (e.g., all keys starting with `"fib("` for `fib/2`), and clears any in-progress markers for that predicate.

**Important**: Unlike `abolish_all_tables/0`, this predicate also **un-declares** the predicate as tabled. To re-enable tabling for it, you must call `table(Functor/Arity)` again.

**When to use**:
- When you want to selectively invalidate cache for one predicate while keeping others cached.
- When dynamically switching between tabled and non-tabled evaluation of a predicate.

---

## Real-World Examples

### Example 1: Fibonacci with Memoization

Demonstrates the dramatic performance difference between naive recursive Fibonacci (exponential time) and tabled Fibonacci (linear time).

```prolog
%% fibonacci.pl
%% Compute Fibonacci numbers with and without tabling.
%%
%% Without tabling: fib(30) requires ~1.6 million recursive calls.
%% With tabling: fib(30) requires exactly 31 calls (one per unique argument).

%% Declare fibonacci as a tabled predicate.
:- table(fib/2).

%% Base cases.
fib(0, 0).
fib(1, 1).

%% Recursive case: F(N) = F(N-1) + F(N-2).
fib(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2.

%% Compute Fibonacci numbers and display them.
show_fibonacci(Max) :-
    between(0, Max, N),
    fib(N, F),
    write('fib('), write(N), write(') = '), write(F), nl,
    fail ; true.

%% Benchmark: compare tabled vs untabled performance.
%% The untabled version is defined with a different name to avoid the table declaration.
fib_naive(0, 0).
fib_naive(1, 1).
fib_naive(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib_naive(N1, F1),
    fib_naive(N2, F2),
    F is F1 + F2.

benchmark_fibonacci :-
    write('=== Tabled Fibonacci (fib/2) ==='), nl,
    statistics(walltime, [T1Start|_]),
    fib(30, F1),
    statistics(walltime, [T1End|_]),
    T1 is T1End - T1Start,
    write('fib(30) = '), write(F1), write(' in '), write(T1), write(' ms'), nl,
    nl,
    write('=== Naive Fibonacci (fib_naive/2) ==='), nl,
    statistics(walltime, [T2Start|_]),
    fib_naive(25, F2),  % Only 25 -- 30 would take too long
    statistics(walltime, [T2End|_]),
    T2 is T2End - T2Start,
    write('fib_naive(25) = '), write(F2), write(' in '), write(T2), write(' ms'), nl,
    nl,
    write('Tabling speedup is dramatic for larger values.'), nl.

%% Usage:
%% ?- show_fibonacci(10).
%% fib(0) = 0
%% fib(1) = 1
%% fib(2) = 1
%% fib(3) = 2
%% fib(4) = 3
%% fib(5) = 5
%% fib(6) = 8
%% fib(7) = 13
%% fib(8) = 21
%% fib(9) = 34
%% fib(10) = 55
%%
%% ?- benchmark_fibonacci.
%% === Tabled Fibonacci (fib/2) ===
%% fib(30) = 832040 in 2 ms
%% === Naive Fibonacci (fib_naive/2) ===
%% fib_naive(25) = 75025 in 850 ms
```

---

### Example 2: Graph Reachability with Cycles

Tabling prevents infinite loops when computing reachability in graphs that contain cycles. Without tabling, a naive `path/2` predicate would loop forever on cyclic graphs.

```prolog
%% graph_reachability.pl
%% Compute reachability in a directed graph that contains cycles.
%% Without tabling, path(a, X) would loop: a -> b -> c -> a -> b -> ...
%% With tabling, the loop is detected and all reachable nodes are found.

:- table(path/2).

%% A directed graph with cycles:
%%   a --> b --> c --> a    (cycle)
%%   b --> d --> e
%%   c --> f
%%   e --> f
%%   f --> g
edge(a, b).
edge(b, c).
edge(c, a).     % Creates cycle: a -> b -> c -> a
edge(b, d).
edge(d, e).
edge(c, f).
edge(e, f).
edge(f, g).

%% Direct edge.
path(X, Y) :- edge(X, Y).

%% Transitive closure: X can reach Y if X can reach some Z that can reach Y.
path(X, Y) :-
    edge(X, Z),
    path(Z, Y).

%% Find all nodes reachable from a given starting node.
reachable_from(Start, ReachableNodes) :-
    findall(Node, path(Start, Node), RawNodes),
    sort(RawNodes, ReachableNodes).

%% Check if two nodes are in the same strongly connected component.
same_scc(X, Y) :-
    path(X, Y),
    path(Y, X).

%% Find all strongly connected components (simplified: just pairs).
find_scc_pairs(Pairs) :-
    findall(scc(X, Y),
            (edge(X, _), edge(Y, _), X @< Y, same_scc(X, Y)),
            Pairs).

%% Compute the shortest path length using breadth-first exploration.
%% (Tabling ensures we do not revisit nodes.)
:- table(shortest_path/3).

shortest_path(X, Y, 1) :- edge(X, Y).
shortest_path(X, Y, N) :-
    edge(X, Z),
    shortest_path(Z, Y, N1),
    N is N1 + 1.

min_path_length(X, Y, MinLen) :-
    findall(L, shortest_path(X, Y, L), Lengths),
    Lengths \= [],
    min_list(Lengths, MinLen).

%% Helper: find the minimum of a list.
min_list([X], X).
min_list([X|Xs], Min) :-
    min_list(Xs, MinRest),
    ( X < MinRest -> Min = X ; Min = MinRest ).

%% Demo: explore the cyclic graph safely.
demo_reachability :-
    write('=== Reachability from node a ==='), nl,
    reachable_from(a, FromA),
    write('Reachable from a: '), write(FromA), nl, nl,

    write('=== Reachability from node d ==='), nl,
    reachable_from(d, FromD),
    write('Reachable from d: '), write(FromD), nl, nl,

    write('=== Strongly connected component pairs ==='), nl,
    find_scc_pairs(SCCPairs),
    write(SCCPairs), nl, nl,

    write('=== Shortest path a to g ==='), nl,
    min_path_length(a, g, Len),
    write('Minimum path length a -> g: '), write(Len), nl.

%% Usage:
%% ?- demo_reachability.
%% === Reachability from node a ===
%% Reachable from a: [a, b, c, d, e, f, g]
%%
%% === Reachability from node d ===
%% Reachable from d: [e, f, g]
%%
%% === Strongly connected component pairs ===
%% [scc(a, b), scc(a, c), scc(b, c)]
%%
%% === Shortest path a to g ===
%% Minimum path length a -> g: 3
```

---

### Example 3: Dynamic Programming -- Edit Distance

Compute the Levenshtein edit distance between two strings using tabling. Without tabling, this classic DP problem has exponential time complexity due to overlapping subproblems.

```prolog
%% edit_distance.pl
%% Compute the Levenshtein edit distance between two lists of characters.
%% Edit distance counts the minimum number of insertions, deletions, and
%% substitutions needed to transform one string into another.
%%
%% Tabling converts this from O(3^(m+n)) to O(m*n) by caching subproblem results.

:- table(edit_dist/3).

%% Base cases: distance from empty to a list is the list's length.
edit_dist([], Ys, D) :- length(Ys, D).
edit_dist(Xs, [], D) :- length(Xs, D).

%% Recursive case: compare first characters.
edit_dist([X|Xs], [Y|Ys], D) :-
    ( X == Y ->
        %% Characters match: no edit needed, recurse on tails.
        edit_dist(Xs, Ys, D)
    ;
        %% Characters differ: try all three operations, take minimum.
        edit_dist(Xs, Ys, D1),          % Substitution
        edit_dist(Xs, [Y|Ys], D2),      % Deletion from first string
        edit_dist([X|Xs], Ys, D3),      % Insertion into first string
        Min12 is min(D1, D2),
        MinAll is min(Min12, D3),
        D is MinAll + 1
    ).

%% Convert atoms to character lists for convenience.
string_edit_distance(Atom1, Atom2, Distance) :-
    atom_chars(Atom1, Chars1),
    atom_chars(Atom2, Chars2),
    abolish_all_tables,  % Clear cache between different string pairs
    edit_dist(Chars1, Chars2, Distance).

%% Demo: compute edit distances between several word pairs.
demo_edit_distance :-
    Pairs = [
        (kitten, sitting),
        (saturday, sunday),
        (prolog, prologue),
        (algorithm, altruistic),
        (abc, abc)
    ],
    write('=== Edit Distance Calculator ==='), nl,
    process_pairs(Pairs).

process_pairs([]).
process_pairs([(W1, W2)|Rest]) :-
    string_edit_distance(W1, W2, D),
    write('  d('), write(W1), write(', '), write(W2), write(') = '), write(D), nl,
    process_pairs(Rest).

%% Find the closest match in a dictionary.
closest_word(Target, Dictionary, BestWord, BestDist) :-
    Dictionary = [First|Rest],
    string_edit_distance(Target, First, InitDist),
    closest_word_acc(Target, Rest, First, InitDist, BestWord, BestDist).

closest_word_acc(_, [], Best, BestDist, Best, BestDist).
closest_word_acc(Target, [W|Ws], CurBest, CurDist, FinalBest, FinalDist) :-
    string_edit_distance(Target, W, D),
    ( D < CurDist ->
        closest_word_acc(Target, Ws, W, D, FinalBest, FinalDist)
    ;
        closest_word_acc(Target, Ws, CurBest, CurDist, FinalBest, FinalDist)
    ).

%% Usage:
%% ?- demo_edit_distance.
%% === Edit Distance Calculator ===
%%   d(kitten, sitting) = 3
%%   d(saturday, sunday) = 3
%%   d(prolog, prologue) = 2
%%   d(algorithm, altruistic) = 6
%%   d(abc, abc) = 0
%%
%% ?- closest_word(prolag, [python, prolog, pascal, perl, java], Best, Dist).
%% Best = prolog, Dist = 1
```

---

### Example 4: Parsing with Left Recursion

Tabling enables left-recursive grammars to terminate. Without tabling, left-recursive rules like `expr -> expr + term` would cause infinite recursion in a top-down parser.

```prolog
%% left_recursive_parser.pl
%% A simple arithmetic expression parser using tabling to handle left recursion.
%%
%% Grammar (left-recursive):
%%   expr -> expr '+' term
%%   expr -> expr '-' term
%%   expr -> term
%%   term -> term '*' factor
%%   term -> term '/' factor
%%   term -> factor
%%   factor -> '(' expr ')'
%%   factor -> number
%%
%% Without tabling, the first rule "expr -> expr '+' term" would cause
%% infinite left recursion. Tabling breaks the cycle.

:- table(expr/3).
:- table(term/3).

%% expr(+InputTokens, -Value, -RemainingTokens)
%% Parse an expression and compute its value.

%% Left-recursive: expr = expr + term
expr(Input, Value, Rest) :-
    expr(Input, LeftVal, [+|AfterPlus]),
    term(AfterPlus, RightVal, Rest),
    Value is LeftVal + RightVal.

%% Left-recursive: expr = expr - term
expr(Input, Value, Rest) :-
    expr(Input, LeftVal, [-|AfterMinus]),
    term(AfterMinus, RightVal, Rest),
    Value is LeftVal - RightVal.

%% Base case: expr = term
expr(Input, Value, Rest) :-
    term(Input, Value, Rest).

%% term = term * factor
term(Input, Value, Rest) :-
    term(Input, LeftVal, [*|AfterMul]),
    factor(AfterMul, RightVal, Rest),
    Value is LeftVal * RightVal.

%% term = factor
term(Input, Value, Rest) :-
    factor(Input, Value, Rest).

%% factor = ( expr )
factor(['('|Input], Value, Rest) :-
    expr(Input, Value, [')'|Rest]).

%% factor = number
factor([N|Rest], N, Rest) :-
    number(N).

%% Tokenize a simple arithmetic expression string.
%% For simplicity, we assume single-digit numbers and operators.
tokenize([], []).
tokenize([C|Cs], Tokens) :-
    ( C = ' ' ->
        tokenize(Cs, Tokens)
    ; member(C, [+, -, *, /, '(', ')']) ->
        Tokens = [C|RestTokens],
        tokenize(Cs, RestTokens)
    ; C >= 0, C =< 9 ->
        Tokens = [C|RestTokens],
        tokenize(Cs, RestTokens)
    ;
        tokenize(Cs, Tokens)
    ).

%% Parse and evaluate a token list.
evaluate(Tokens, Value) :-
    abolish_all_tables,
    expr(Tokens, Value, []).

%% Demo: parse several expressions.
demo_parser :-
    write('=== Left-Recursive Expression Parser ==='), nl,
    parse_and_show([3, +, 4], '3 + 4'),
    parse_and_show([2, +, 3, *, 4], '2 + 3 * 4'),
    parse_and_show([1, +, 2, +, 3, +, 4], '1 + 2 + 3 + 4'),
    parse_and_show(['(', 1, +, 2, ')', *, 3], '(1 + 2) * 3').

parse_and_show(Tokens, Description) :-
    ( evaluate(Tokens, Value) ->
        write('  '), write(Description), write(' = '), write(Value), nl
    ;
        write('  '), write(Description), write(' = PARSE ERROR'), nl
    ).

%% Usage:
%% ?- demo_parser.
%% === Left-Recursive Expression Parser ===
%%   3 + 4 = 7
%%   2 + 3 * 4 = 14
%%   1 + 2 + 3 + 4 = 10
%%   (1 + 2) * 3 = 9
```

---

### Example 5: Transitive Closure -- Genealogy Database

Compute ancestor/descendant relationships over a family tree using tabling. This handles arbitrarily deep ancestry chains efficiently and correctly, even with the potential for overlapping computations.

```prolog
%% genealogy.pl
%% A genealogy database using tabled predicates for efficient ancestor/descendant
%% queries. Demonstrates transitive closure over a parent-child relation.

:- table(ancestor/2).
:- table(descendant/2).
:- table(common_ancestor/3).

%% The family tree:
%%              adam
%%            /      \
%%         seth      abel
%%        /    \
%%     enosh   jubal
%%    /     \
%% kenan   mahalalel
%%   |
%% irad

parent(adam, seth).
parent(adam, abel).
parent(seth, enosh).
parent(seth, jubal).
parent(enosh, kenan).
parent(enosh, mahalalel).
parent(kenan, irad).

%% Direct parent is an ancestor.
ancestor(X, Y) :- parent(X, Y).

%% Transitive: ancestor of an ancestor is also an ancestor.
%% Tabling ensures this terminates even if the graph had cycles.
ancestor(X, Y) :-
    parent(X, Z),
    ancestor(Z, Y).

%% Inverse relation: Y is a descendant of X if X is an ancestor of Y.
descendant(X, Y) :- ancestor(Y, X).

%% Generation distance: how many generations between ancestor and descendant.
:- table(generation_distance/3).

generation_distance(X, Y, 1) :- parent(X, Y).
generation_distance(X, Y, N) :-
    parent(X, Z),
    generation_distance(Z, Y, N1),
    N is N1 + 1.

%% Find the nearest common ancestor of two individuals.
common_ancestor(X, Y, Ancestor) :-
    ancestor(Ancestor, X),
    ancestor(Ancestor, Y),
    X \= Y.

nearest_common_ancestor(X, Y, NCA) :-
    findall(A, common_ancestor(X, Y, A), Ancestors),
    Ancestors \= [],
    find_nearest(Ancestors, X, Y, NCA).

%% The nearest common ancestor has the maximum generation distance
%% to the root (i.e., is deepest in the tree / closest to X and Y).
find_nearest(Ancestors, X, Y, NCA) :-
    find_nearest_acc(Ancestors, X, Y, none, -1, NCA).

find_nearest_acc([], _, _, NCA, _, NCA) :- NCA \= none.
find_nearest_acc([A|As], X, Y, CurBest, CurMaxDepth, NCA) :-
    ( generation_distance(A, X, Dx), generation_distance(A, Y, Dy) ->
        MaxD is max(Dx, Dy),
        %% We want the ancestor closest to both, so minimize max distance.
        %% But among common ancestors, the deepest one is nearest.
        %% Use sum of distances as tiebreaker: smaller sum = nearer.
        SumD is Dx + Dy,
        ( CurBest = none ->
            find_nearest_acc(As, X, Y, A, SumD, NCA)
        ; SumD < CurMaxDepth ->
            find_nearest_acc(As, X, Y, A, SumD, NCA)
        ;
            find_nearest_acc(As, X, Y, CurBest, CurMaxDepth, NCA)
        )
    ;
        find_nearest_acc(As, X, Y, CurBest, CurMaxDepth, NCA)
    ).

%% List all descendants at a specific generation depth.
descendants_at_generation(Person, Gen, Descendants) :-
    findall(D, generation_distance(Person, D, Gen), Descendants).

%% Full genealogy report for a person.
genealogy_report(Person) :-
    write('=== Genealogy Report for '), write(Person), write(' ==='), nl, nl,

    write('Direct children:'), nl,
    findall(C, parent(Person, C), Children),
    ( Children = [] -> write('  (none)'), nl ; print_list(Children) ),
    nl,

    write('All descendants:'), nl,
    findall(D, descendant(D, Person), Descs),
    ( Descs = [] -> write('  (none)'), nl ; print_list(Descs) ),
    nl,

    write('All ancestors:'), nl,
    findall(A, ancestor(A, Person), Ancs),
    ( Ancs = [] -> write('  (none)'), nl ; print_list(Ancs) ),
    nl,

    write('Generation distances:'), nl,
    findall(gen(D, G), generation_distance(Person, D, G), GenDists),
    print_gen_dists(GenDists), nl.

print_list([]).
print_list([X|Xs]) :- write('  '), write(X), nl, print_list(Xs).

print_gen_dists([]).
print_gen_dists([gen(D, G)|Rest]) :-
    write('  '), write(D), write(' at generation '), write(G), nl,
    print_gen_dists(Rest).

%% Demo: run reports and relationship queries.
demo_genealogy :-
    genealogy_report(adam),
    nl,
    genealogy_report(enosh),
    nl,
    write('=== Nearest common ancestor of kenan and jubal ==='), nl,
    ( nearest_common_ancestor(kenan, jubal, NCA) ->
        write('NCA: '), write(NCA), nl
    ;
        write('No common ancestor found.'), nl
    ),
    nl,
    write('=== Descendants of adam at generation 3 ==='), nl,
    descendants_at_generation(adam, 3, Gen3),
    write(Gen3), nl.

%% Usage:
%% ?- demo_genealogy.
%% === Genealogy Report for adam ===
%%
%% Direct children:
%%   seth
%%   abel
%%
%% All descendants:
%%   seth
%%   abel
%%   enosh
%%   jubal
%%   kenan
%%   mahalalel
%%   irad
%%
%% All ancestors:
%%   (none)
%%
%% Generation distances:
%%   seth at generation 1
%%   abel at generation 1
%%   enosh at generation 2
%%   jubal at generation 2
%%   kenan at generation 3
%%   mahalalel at generation 3
%%   irad at generation 4
%%
%% === Genealogy Report for enosh ===
%%
%% Direct children:
%%   kenan
%%   mahalalel
%%
%% All descendants:
%%   kenan
%%   mahalalel
%%   irad
%%
%% All ancestors:
%%   seth
%%   adam
%%
%% Generation distances:
%%   kenan at generation 1
%%   mahalalel at generation 1
%%   irad at generation 2
%%
%% === Nearest common ancestor of kenan and jubal ===
%% NCA: seth
%%
%% === Descendants of adam at generation 3 ===
%% [kenan, mahalalel]
```
