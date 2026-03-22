% Test 23: Assert/Retract during active goal execution
% ISS-2025-0166 - Database modification safety

% === Test 1: Assert during findall ===
test_assert_in_findall :-
    retractall(temp_fact(_)),
    assert(temp_fact(1)),
    assert(temp_fact(2)),
    findall(X, (temp_fact(X), assert(temp_fact(X + 10))), Xs),
    Xs = [1, 2],
    retractall(temp_fact(_)).

% === Test 2: Retract during findall ===
test_retract_in_findall :-
    retractall(item(_)),
    assert(item(a)),
    assert(item(b)),
    assert(item(c)),
    findall(X, (item(X), retract(item(X))), Xs),
    Xs = [a, b, c].

% === Test 3: Assert then query in same goal ===
test_assert_then_query :-
    retractall(counter(_)),
    assert(counter(0)),
    retract(counter(0)),
    assert(counter(1)),
    counter(1).

% === Test 4: Multiple retract in backtracking ===
test_retract_backtrack :-
    retractall(val(_)),
    assert(val(10)),
    assert(val(20)),
    assert(val(30)),
    findall(V, retract(val(V)), Vs),
    Vs = [10, 20, 30].

% === Test 5: Assertz ordering preserved ===
test_assertz_order :-
    retractall(ord(_)),
    assertz(ord(1)),
    assertz(ord(2)),
    assertz(ord(3)),
    findall(X, ord(X), Xs),
    Xs = [1, 2, 3],
    retractall(ord(_)).

% === Test 6: Asserta ordering preserved ===
test_asserta_order :-
    retractall(prio(_)),
    assertz(prio(1)),
    asserta(prio(0)),
    assertz(prio(2)),
    findall(X, prio(X), Xs),
    Xs = [0, 1, 2],
    retractall(prio(_)).

% === Run all tests ===
run_test(Name, Goal) :-
    (call(Goal) ->
        write('PASS: '), write(Name), nl
    ;
        write('FAIL: '), write(Name), nl
    ).

:- write('=== Assert/Retract Active Execution Tests ==='), nl.
:- run_test('Assert during findall', test_assert_in_findall).
:- run_test('Retract during findall', test_retract_in_findall).
:- run_test('Assert then query same goal', test_assert_then_query).
:- run_test('Multiple retract backtracking', test_retract_backtrack).
:- run_test('Assertz ordering', test_assertz_order).
:- run_test('Asserta ordering', test_asserta_order).
:- write('=== Assert/Retract Tests Complete ==='), nl.
