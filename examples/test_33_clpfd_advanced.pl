% Test 33: CLP(FD) Advanced Tests
% ISS-2025-0175 - CLP(FD) bounds consistency

% === Test 1: Basic CLP(FD) constraints (prefix notation) ===
test_clpfd_basic :-
    in(X, '..'(1,10)),
    '#='(X, 5),
    label([X]),
    X =:= 5.

% === Test 2: All different ===
test_all_different :-
    in(X, '..'(1,3)),
    in(Y, '..'(1,3)),
    in(Z, '..'(1,3)),
    all_different([X, Y, Z]),
    label([X, Y, Z]),
    X \== Y, Y \== Z, X \== Z.

% === Test 3: Comparison constraints ===
test_comparison :-
    in(X, '..'(1,10)),
    in(Y, '..'(1,10)),
    '#<'(X, Y),
    '#='(X, 3),
    label([X, Y]),
    X < Y.

% === Test 4: Simple domain ===
test_domain :-
    in(X, '..'(1,5)),
    '#>'(X, 3),
    label([X]),
    X > 3.

% === Run all tests ===
run_test(Name, Goal) :-
    (call(Goal) ->
        write('PASS: '), write(Name), nl
    ;
        write('FAIL: '), write(Name), nl
    ).

:- write('=== CLP(FD) Advanced Tests ==='), nl.
:- run_test('Basic CLP(FD)', test_clpfd_basic).
:- run_test('All different', test_all_different).
:- run_test('Comparison constraints', test_comparison).
:- run_test('Domain constraint', test_domain).
:- write('=== CLP(FD) Tests Complete ==='), nl.
