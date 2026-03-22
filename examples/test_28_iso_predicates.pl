% Test 28: New ISO Predicates and Functions
% ISS-2025-0170 - Missing ISO predicates

% === Test 1: acyclic_term/1 ===
test_acyclic_atom :-
    acyclic_term(hello).

test_acyclic_number :-
    acyclic_term(42).

test_acyclic_compound :-
    acyclic_term(f(a, b, c)).

test_acyclic_list :-
    acyclic_term([1, 2, 3]).

test_acyclic_nested :-
    acyclic_term(f(g(h(1)), [a, b])).

% === Test 2: proper_list/1 ===
test_proper_list_basic :-
    proper_list([1, 2, 3]).

test_proper_list_empty :-
    proper_list([]).

test_proper_list_nested :-
    proper_list([[1], [2], [3]]).

test_not_proper_list_atom :-
    \+ proper_list(hello).

test_not_proper_list_number :-
    \+ proper_list(42).

% === Test 3: msb/1 ===
test_msb_8 :-
    X is msb(8),
    X =:= 3.

test_msb_1 :-
    X is msb(1),
    X =:= 0.

test_msb_255 :-
    X is msb(255),
    X =:= 7.

% === Test 4: lsb/1 ===
test_lsb_8 :-
    X is lsb(8),
    X =:= 3.

test_lsb_1 :-
    X is lsb(1),
    X =:= 0.

test_lsb_12 :-
    X is lsb(12),
    X =:= 2.

% === Test 5: popcount/1 ===
test_popcount_0 :-
    X is popcount(0),
    X =:= 0.

test_popcount_255 :-
    X is popcount(255),
    X =:= 8.

test_popcount_7 :-
    X is popcount(7),
    X =:= 3.

% === Run all tests ===
run_test(Name, Goal) :-
    (call(Goal) ->
        write('PASS: '), write(Name), nl
    ;
        write('FAIL: '), write(Name), nl
    ).

:- write('=== ISO Predicates Tests (Phase 9) ==='), nl.
:- run_test('acyclic_term atom', test_acyclic_atom).
:- run_test('acyclic_term number', test_acyclic_number).
:- run_test('acyclic_term compound', test_acyclic_compound).
:- run_test('acyclic_term list', test_acyclic_list).
:- run_test('acyclic_term nested', test_acyclic_nested).
:- run_test('proper_list basic', test_proper_list_basic).
:- run_test('proper_list empty', test_proper_list_empty).
:- run_test('proper_list nested', test_proper_list_nested).
:- run_test('not proper_list atom', test_not_proper_list_atom).
:- run_test('not proper_list number', test_not_proper_list_number).
:- run_test('msb(8) = 3', test_msb_8).
:- run_test('msb(1) = 0', test_msb_1).
:- run_test('msb(255) = 7', test_msb_255).
:- run_test('lsb(8) = 3', test_lsb_8).
:- run_test('lsb(1) = 0', test_lsb_1).
:- run_test('lsb(12) = 2', test_lsb_12).
:- run_test('popcount(0) = 0', test_popcount_0).
:- run_test('popcount(255) = 8', test_popcount_255).
:- run_test('popcount(7) = 3', test_popcount_7).
:- write('=== ISO Predicates Tests Complete ==='), nl.
