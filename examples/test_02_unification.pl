% ===================================================================
% TEST 02: Unification and Variable Binding
% ===================================================================
% Tests: Variable unification, complex term matching, occurs check

% Complex term structures
person(john, age(25), address(street('Main St'), city(boston))).
person(mary, age(30), address(street('Oak Ave'), city(chicago))).

% Unification patterns
likes(john, food(pizza)).
likes(mary, food(pasta)).
likes(X, food(Y)) :- person(X, _, _), healthy(Y).

healthy(salad).
healthy(fruit).

% Complex unification test
same_structure(f(X, Y), f(A, B)) :- X = A, Y = B.
complex_match(data(X, [H|T], kv(key, Value)), Result) :- 
    X = identifier,
    H = first,
    Value = important,
    Result = matched.

% Test queries:
% ?- person(john, Age, address(Street, City)).
% ?- likes(john, What).
% ?- same_structure(f(a, b), f(c, d)).
% ?- X = f(X).                 % Should fail with occurs check
% ?- complex_match(data(identifier, [first|rest], kv(key, important)), R).