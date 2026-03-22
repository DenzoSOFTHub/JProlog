% Test 22: Core Robustness Tests (Phase 1 fixes)
% ISS-2025-0163

% === Fix 1: Unknown atoms in arithmetic throw error, not silent 0 ===
test_atom_error :-
    catch(
        (X is foo + 1, fail),
        error(type_error(evaluable, _), _),
        true
    ).

test_pi :-
    X is pi, X > 3.14, X < 3.15.

test_e :-
    X is e, X > 2.71, X < 2.72.

% === Fix 3: Cut propagates from disjunction branches ===
choice_a(1).
choice_a(2).
choice_a(3).

% Cut inside disjunction should stop further backtracking on choice_a
first_even(X) :- choice_a(X), (X =:= 2, ! ; fail).

test_cut_in_disjunction :-
    findall(X, first_even(X), Solutions),
    Solutions = [2].

% === Fix 5: Non-tail recursion hits depth limit with proper error ===
% Use conjunction after recursive call - LCO cannot optimize this since
% the recursive call is NOT the last goal in the body
count_up(N, Max) :- N < Max, N1 is N + 1, count_up(N1, Max), true.

test_recursion_error :-
    catch(
        (count_up(0, 5000), fail),
        error(resource_error(_), _),
        true
    ).

% === Run all tests ===
run_test(Name, Goal) :-
    (call(Goal) ->
        write('PASS: '), write(Name), nl
    ;
        write('FAIL: '), write(Name), nl
    ).

:- write('=== Core Robustness Tests ==='), nl.
:- run_test('Unknown atom in is/2 throws type_error', test_atom_error).
:- run_test('pi constant works', test_pi).
:- run_test('e constant works', test_e).
:- run_test('Cut in disjunction branch propagates', test_cut_in_disjunction).
:- run_test('Deep non-tail recursion throws resource_error', test_recursion_error).
:- write('=== Core Robustness Tests Complete ==='), nl.
