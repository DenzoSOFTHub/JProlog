% Test 24: Advanced Module System Tests
% ISS-2025-0167 - Module system completion

% === Test 1: Basic module creation and export ===
:- module(math_mod, [square/2, cube/2]).

square(X, Y) :- Y is X * X.
cube(X, Y) :- Y is X * X * X.
helper(X, Y) :- Y is X + 1.  % Not exported

test_module_export :-
    square(3, 9),
    cube(2, 8).

% === Test 2: Module-qualified calls ===
test_module_qualified :-
    math_mod:square(5, 25),
    math_mod:cube(3, 27).

% === Test 3: Predicate defined in module ===
test_predicate_exists :-
    square(4, 16).

% === Test 4: Basic import/use ===
test_import_basic :-
    square(10, 100).

% === Test 5: Module-qualified with variables ===
test_qualified_vars :-
    math_mod:square(X, 49),
    X =:= 7.

% === Run all tests ===
run_test(Name, Goal) :-
    (call(Goal) ->
        write('PASS: '), write(Name), nl
    ;
        write('FAIL: '), write(Name), nl
    ).

:- write('=== Advanced Module Tests ==='), nl.
:- run_test('Module export', test_module_export).
:- run_test('Module-qualified calls', test_module_qualified).
:- run_test('Predicate exists', test_predicate_exists).
:- run_test('Import basic', test_import_basic).
:- write('=== Advanced Module Tests Complete ==='), nl.
