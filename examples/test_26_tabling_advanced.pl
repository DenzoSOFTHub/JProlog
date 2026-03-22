% Test 26: Advanced Tabling Tests
% ISS-2025-0166 - Tabling edge cases

% === Test 1: Basic tabling with fibonacci ===
:- table fib/2.
fib(0, 0) :- !.
fib(1, 1) :- !.
fib(N, F) :- N > 1, N1 is N - 1, N2 is N - 2, fib(N1, F1), fib(N2, F2), F is F1 + F2.

test_fib_tabling :-
    fib(10, 55).

% === Test 2: Tabling with path finding (transitive closure) ===
:- table path/2.
edge(a, b).
edge(b, c).
edge(c, d).
edge(d, e).

path(X, Y) :- edge(X, Y).
path(X, Y) :- edge(X, Z), path(Z, Y).

test_path_tabling :-
    path(a, e),
    path(a, c),
    \+ path(e, a).

% === Test 3: Tabling preserves all solutions ===
:- table reachable/2.
link(1, 2).
link(1, 3).
link(2, 4).
link(3, 4).

reachable(X, Y) :- link(X, Y).
reachable(X, Y) :- link(X, Z), reachable(Z, Y).

test_tabling_all_solutions :-
    findall(Y, reachable(1, Y), Ys),
    length(Ys, N),
    N >= 3.  % Should find at least 2, 3, 4

% === Test 4: Abolish tables and recompute ===
test_abolish_recompute :-
    fib(8, 21),
    abolish_all_tables,
    fib(8, 21).  % Should recompute correctly

% === Test 5: Tabling with ground queries ===
test_tabling_ground :-
    fib(5, 5),
    fib(6, 8),
    fib(7, 13).

% === Run all tests ===
run_test(Name, Goal) :-
    (call(Goal) ->
        write('PASS: '), write(Name), nl
    ;
        write('FAIL: '), write(Name), nl
    ).

:- write('=== Advanced Tabling Tests ==='), nl.
:- run_test('Fibonacci tabling', test_fib_tabling).
:- run_test('Path finding tabling', test_path_tabling).
:- run_test('Tabling all solutions', test_tabling_all_solutions).
:- run_test('Abolish and recompute', test_abolish_recompute).
:- run_test('Ground query tabling', test_tabling_ground).
:- write('=== Advanced Tabling Tests Complete ==='), nl.
