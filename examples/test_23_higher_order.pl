% ===================================================================
% TEST 23: Higher-Order Predicates and Meta-Programming
% ===================================================================
% Tests: call/N, apply, maplist, foldr, higher-order operations

% Higher-order list operations
maplist(_, []).
maplist(Pred, [H|T]) :-
    call(Pred, H),
    maplist(Pred, T).

maplist(_, [], []).
maplist(Pred, [H1|T1], [H2|T2]) :-
    call(Pred, H1, H2),
    maplist(Pred, T1, T2).

% Fold operations
foldr(_, V, [], V).
foldr(F, V0, [H|T], V) :-
    foldr(F, V0, T, V1),
    call(F, H, V1, V).

foldl(_, V, [], V).
foldl(F, V0, [H|T], V) :-
    call(F, H, V0, V1),
    foldl(F, V1, T, V).

% Function composition
compose(F, G, X, Z) :-
    call(G, X, Y),
    call(F, Y, Z).

% Currying
curry(F, X, Y, Z) :-
    call(F, X, Y, Z).

% Apply predicate with argument list
apply(Pred, Args) :-
    Term =.. [Pred|Args],
    call(Term).

% Test predicates
double(X, Y) :- Y is X * 2.
add(X, Y, Z) :- Z is X + Y.
positive(X) :- X > 0.

% Partial application
add_five(X, Y) :- add(5, X, Y).

% Test queries:
% ?- maplist(positive, [1,2,3]).
% ?- maplist(double, [1,2,3], Result).
% ?- foldr(add, 0, [1,2,3,4], Sum).
% ?- compose(double, add_five, 3, Result).
% ?- apply(member, [2, [1,2,3]]).