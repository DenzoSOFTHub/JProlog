% ===================================================================
% TEST 21: Advanced Unification Patterns
% ===================================================================
% Tests: Complex unification, occurs check, cyclic terms

% Complex structure unification
test_complex_unify(X, Y) :-
    X = point(A, B),
    Y = point(3, B),
    A = 3.

% Occurs check test
test_occurs_check(X) :-
    X = f(X).  % Should fail with occurs check

% Deep structure unification
test_deep_structure :-
    Tree = node(left(leaf(1)), right(node(leaf(2), leaf(3)))),
    Tree = node(L, R),
    L = left(leaf(Value)),
    Value = 1.

% Partial structure unification
test_partial_unify(X, Y) :-
    X = [H|T],
    Y = [1,2,3],
    X = Y,
    H = 1,
    T = [2,3].

% Unification with variables in different positions
test_var_positions :-
    f(X, g(X)) = f(a, g(Y)),
    X = a,
    Y = a.

% Test queries to run:
% ?- test_complex_unify(X, Y).
% ?- test_occurs_check(X).
% ?- test_deep_structure.
% ?- test_partial_unify(X, Y).
% ?- test_var_positions.