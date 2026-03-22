% Test 27: Bug Fix Verification Tests
% ISS-2025-0169 - Phase 8 bug fixes

% === Test 1: succ/2 with 0 ===
test_succ_zero :-
    succ(0, 1).

test_succ_reverse :-
    succ(X, 1),
    X =:= 0.

test_succ_normal :-
    succ(3, 4).

% === Test 2: Flatten handles nested lists ===
test_flatten_basic :-
    flatten([1, [2, 3], [4, [5]]], Flat),
    Flat = [1, 2, 3, 4, 5].

test_flatten_empty :-
    flatten([[], [1], []], Flat),
    Flat = [1].

test_flatten_deep :-
    flatten([[[1]], [[2]], [[3]]], Flat),
    Flat = [1, 2, 3].

% === Test 3: Bitwise NOT requires integer ===
test_bitwise_not_integer :-
    X is \5,
    X =:= -6.

% === Test 4: Arithmetic functions work ===
test_abs :-
    X is abs(-5),
    X =:= 5.

test_sign :-
    X is sign(-42),
    X =:= -1.

test_min_max :-
    X is min(3, 5),
    Y is max(3, 5),
    X =:= 3,
    Y =:= 5.

% === Test 5: sub_atom with bound args ===
test_sub_atom_bound :-
    sub_atom(hello, 1, 3, _, Sub),
    Sub = ell.

test_sub_atom_search :-
    sub_atom(abcabc, Before, 3, _, abc),
    Before =:= 0.

% === Run all tests ===
run_test(Name, Goal) :-
    (call(Goal) ->
        write('PASS: '), write(Name), nl
    ;
        write('FAIL: '), write(Name), nl
    ).

:- write('=== Bug Fix Tests (Phase 8) ==='), nl.
:- run_test('succ(0, 1)', test_succ_zero).
:- run_test('succ(X, 1) binds X=0', test_succ_reverse).
:- run_test('succ(3, 4)', test_succ_normal).
:- run_test('flatten basic', test_flatten_basic).
:- run_test('flatten empty', test_flatten_empty).
:- run_test('flatten deep', test_flatten_deep).
:- run_test('bitwise NOT integer', test_bitwise_not_integer).
:- run_test('abs', test_abs).
:- run_test('sign', test_sign).
:- run_test('min/max', test_min_max).
:- run_test('sub_atom bound args', test_sub_atom_bound).
:- run_test('sub_atom search', test_sub_atom_search).
:- write('=== Bug Fix Tests Complete ==='), nl.
