% Test 42: Tabling (Memoization)
% Tests: table/1, abolish_all_tables/0, abolish_table/1

% ============================================================
% Test framework
% ============================================================
:- dynamic(test_passed/1).
:- dynamic(test_failed/2).
:- dynamic(test_count/1).
:- assert(test_count(0)).

run_test(Name, Goal) :-
    retract(test_count(N)), N1 is N + 1, assert(test_count(N1)),
    copy_term(Goal, GoalCopy),
    ( catch(call(GoalCopy), E, (assert(test_failed(Name, E)), fail))
    -> assert(test_passed(Name)),
       write('  PASS: '), write(Name), nl
    ;  ( \+ test_failed(Name, _) -> assert(test_failed(Name, failed)) ; true ),
       write('  FAIL: '), write(Name), nl
    ).

% ============================================================
% Fibonacci (classic tabling example)
% ============================================================
:- table(fib/2).
fib(0, 0).
fib(1, 1).
fib(N, F) :- N > 1, N1 is N - 1, N2 is N - 2, fib(N1, F1), fib(N2, F2), F is F1 + F2.

% ============================================================
% Path finding with cycles (tabling prevents infinite loops)
% ============================================================
:- table(path/2).
edge(a, b).
edge(b, c).
edge(c, d).
edge(d, a).  % cycle!
edge(b, d).

path(X, Y) :- edge(X, Y).
path(X, Y) :- edge(X, Z), path(Z, Y).

% ============================================================
% Factorial
% ============================================================
:- table(fact/2).
fact(0, 1).
fact(N, F) :- N > 0, N1 is N - 1, fact(N1, F1), F is N * F1.

run_all_tests :-
    write('=== Test 42: Tabling Predicates ==='), nl, nl,
    test_fibonacci,
    test_path,
    test_factorial,
    test_table_management,
    nl, write('--- Results ---'), nl,
    aggregate_all(count, test_passed(_), Passed),
    aggregate_all(count, test_failed(_, _), Failed),
    test_count(Total),
    write('Passed: '), write(Passed), write('/'), write(Total), nl,
    write('Failed: '), write(Failed), nl,
    ( Failed > 0
    -> forall(test_failed(N, R), (write('  '), write(N), write(': '), write(R), nl))
    ; true
    ).

% ============================================================
% 1. Fibonacci tests
% ============================================================
test_fibonacci :-
    write('--- Fibonacci ---'), nl,
    run_test(fib_0,
        (fib(0, F), F == 0)),
    run_test(fib_1,
        (fib(1, F), F == 1)),
    run_test(fib_5,
        (fib(5, F), F == 5)),
    run_test(fib_10,
        (fib(10, F), F == 55)),
    run_test(fib_15,
        (fib(15, F), F == 610)),
    run_test(fib_20,
        (fib(20, F), F == 6765)),
    run_test(fib_deterministic,
        (fib(10, F1), fib(10, F2), F1 == F2)).

% ============================================================
% 2. Path finding (cycle detection)
% ============================================================
test_path :-
    write('--- Path Finding ---'), nl,
    run_test(path_direct,
        path(a, b)),
    run_test(path_transitive,
        path(a, c)),
    run_test(path_through_cycle,
        path(a, d)),
    run_test(path_long,
        path(a, d)),
    run_test(path_no_path,
        (\+ path(d, e))).

% ============================================================
% 3. Factorial tests
% ============================================================
test_factorial :-
    write('--- Factorial ---'), nl,
    run_test(fact_0,
        (fact(0, F), F == 1)),
    run_test(fact_5,
        (fact(5, F), F == 120)),
    run_test(fact_10,
        (fact(10, F), F == 3628800)).

% ============================================================
% 4. Table management
% ============================================================
test_table_management :-
    write('--- Table Management ---'), nl,
    run_test(abolish_all_tables_succeeds,
        abolish_all_tables),
    run_test(fib_after_clear,
        (abolish_all_tables, fib(10, F), F == 55)),
    run_test(abolish_table_specific,
        (abolish_table(fib/2), fib(5, F), F == 5)).

:- run_all_tests.
