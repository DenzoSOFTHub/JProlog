% Test 25: Recursion Depth and Binding Chain Tests
% ISS-2025-0166 - Circular binding detection and depth limits

% === Test 1: Deep tail recursion with LCO ===
count_lco(0) :- !.
count_lco(N) :- N > 0, N1 is N - 1, count_lco(N1).

test_deep_lco :-
    count_lco(5000).

% === Test 2: Non-tail recursion hits depth limit ===
count_nontail(0, 0) :- !.
count_nontail(N, R) :- N > 0, N1 is N - 1, count_nontail(N1, R1), R is R1 + 1.

test_depth_limit :-
    catch(
        (count_nontail(5000, _), fail),
        error(resource_error(_), _),
        true
    ).

% === Test 3: Variable binding chain resolution ===
test_binding_chain :-
    X = Y, Y = Z, Z = hello,
    X == hello.

% === Test 4: Deep binding chain ===
test_deep_binding :-
    A = B, B = C, C = D, D = E, E = F,
    F = G, G = H, H = I, I = J, J = 42,
    A =:= 42.

% === Test 5: Arithmetic with variable chains ===
test_arith_chain :-
    X = Y, Y = 10,
    R is X + 5,
    R =:= 15.

% === Test 6: Unification with complex terms through chains ===
test_complex_chain :-
    X = f(A, B),
    A = g(C),
    C = hello,
    B = world,
    X = f(g(hello), world).

% === Test 7: Mutual recursion depth ===
even(0) :- !.
even(N) :- N > 0, N1 is N - 1, odd(N1).
odd(N) :- N > 0, N1 is N - 1, even(N1).

test_mutual_recursion :-
    even(100),
    odd(99).

% === Run all tests ===
run_test(Name, Goal) :-
    (call(Goal) ->
        write('PASS: '), write(Name), nl
    ;
        write('FAIL: '), write(Name), nl
    ).

:- write('=== Recursion & Binding Chain Tests ==='), nl.
:- run_test('Deep tail recursion (LCO)', test_deep_lco).
:- run_test('Non-tail recursion depth limit', test_depth_limit).
:- run_test('Variable binding chain', test_binding_chain).
:- run_test('Deep binding chain', test_deep_binding).
:- run_test('Arithmetic with chains', test_arith_chain).
:- run_test('Complex term chains', test_complex_chain).
:- run_test('Mutual recursion', test_mutual_recursion).
:- write('=== Recursion & Binding Tests Complete ==='), nl.
