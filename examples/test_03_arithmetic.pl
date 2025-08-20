% ===================================================================
% TEST 03: Arithmetic Operations and 'is' Predicate
% ===================================================================
% Tests: Arithmetic evaluation, is/2, comparison operators

% Basic arithmetic facts
cost(apple, 2).
cost(banana, 1).
cost(orange, 3).

% Arithmetic calculations
total_cost(Items, Total) :-
    calculate_sum(Items, 0, Total).

calculate_sum([], Acc, Acc).
calculate_sum([Item|Rest], Acc, Total) :-
    cost(Item, Price),
    NewAcc is Acc + Price,
    calculate_sum(Rest, NewAcc, Total).

% Mathematical functions
factorial(0, 1) :- !.
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% Arithmetic comparisons
expensive(Item) :-
    cost(Item, Price),
    Price > 2.

cheap(Item) :-
    cost(Item, Price),
    Price =< 1.

% Advanced arithmetic
hypotenuse(A, B, C) :-
    C is sqrt(A*A + B*B).

area_circle(Radius, Area) :-
    Area is pi * Radius * Radius.

% Test queries:
% ?- X is 2 + 3 * 4.           % Should be 14
% ?- total_cost([apple, banana], Total).
% ?- factorial(5, F).
% ?- expensive(orange).
% ?- hypotenuse(3, 4, C).
% ?- area_circle(2, Area).