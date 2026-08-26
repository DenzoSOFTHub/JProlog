% ---------------------------------------------------------------------------
% JProlog engine v4 prelude — library(apply)
%
% ISS-2025-0454 (design B.10, decision 4): these predicates are Prolog clauses,
% not Java built-ins. On the v4 machine they are therefore lazy (a maplist over
% a partial list backtracks properly), traceable through the four ports,
% cancellable by the inference budget and the Stop interrupt, and they cost no
% Java stack at all — the Java versions materialised every solution of every
% element eagerly and recursed once per element.
%
% ISS-2025-0469: the meta_predicate/1 declarations below are what makes them
% module-transparent. When module M calls maplist(mk, L, L2), the machine
% qualifies the first argument as M:mk before unifying the head, so the
% call(G, X, Y) inside the body runs mk/2 in M — not in `apply`, and not in
% whichever module happens to define an mk/2 in `user`.
%
% Loaded into the v4 module `apply` on the first reference to one of its
% predicates. The KnowledgeBase is untouched, so listing/1, the IDE and the
% legacy/v2 engines never see these clauses; a user definition of the same
% indicator (the partition/4 of examples/test_16_sorting.pl) simply wins,
% because a library module is the last step of the resolution order.
% ---------------------------------------------------------------------------

:- module(apply, [maplist/2, maplist/3, maplist/4, maplist/5, maplist/6, maplist/7,
                  foldl/4, foldl/5, foldl/6, foldl/7,
                  include/3, exclude/3, partition/4, partition/5]).

:- meta_predicate(maplist(1, ?)).
:- meta_predicate(maplist(2, ?, ?)).
:- meta_predicate(maplist(3, ?, ?, ?)).
:- meta_predicate(maplist(4, ?, ?, ?, ?)).
:- meta_predicate(maplist(5, ?, ?, ?, ?, ?)).
:- meta_predicate(maplist(6, ?, ?, ?, ?, ?, ?)).
:- meta_predicate(foldl(3, ?, +, -)).
:- meta_predicate(foldl(4, ?, ?, +, -)).
:- meta_predicate(foldl(5, ?, ?, ?, +, -)).
:- meta_predicate(foldl(6, ?, ?, ?, ?, +, -)).
:- meta_predicate(include(1, ?, ?)).
:- meta_predicate(exclude(1, ?, ?)).
:- meta_predicate(partition(1, ?, ?, ?)).
:- meta_predicate(partition(2, ?, ?, ?, ?)).

maplist(_, []).
maplist(G, [X|Xs]) :-
    call(G, X),
    maplist(G, Xs).

maplist(_, [], []).
maplist(G, [X|Xs], [Y|Ys]) :-
    call(G, X, Y),
    maplist(G, Xs, Ys).

maplist(_, [], [], []).
maplist(G, [X|Xs], [Y|Ys], [Z|Zs]) :-
    call(G, X, Y, Z),
    maplist(G, Xs, Ys, Zs).

maplist(_, [], [], [], []).
maplist(G, [X|Xs], [Y|Ys], [Z|Zs], [W|Ws]) :-
    call(G, X, Y, Z, W),
    maplist(G, Xs, Ys, Zs, Ws).

maplist(_, [], [], [], [], []).
maplist(G, [X|Xs], [Y|Ys], [Z|Zs], [W|Ws], [V|Vs]) :-
    call(G, X, Y, Z, W, V),
    maplist(G, Xs, Ys, Zs, Ws, Vs).

maplist(_, [], [], [], [], [], []).
maplist(G, [X|Xs], [Y|Ys], [Z|Zs], [W|Ws], [V|Vs], [U|Us]) :-
    call(G, X, Y, Z, W, V, U),
    maplist(G, Xs, Ys, Zs, Ws, Vs, Us).

foldl(_, [], A, A).
foldl(G, [X|Xs], A0, A) :-
    call(G, X, A0, A1),
    foldl(G, Xs, A1, A).

foldl(_, [], [], A, A).
foldl(G, [X|Xs], [Y|Ys], A0, A) :-
    call(G, X, Y, A0, A1),
    foldl(G, Xs, Ys, A1, A).

foldl(_, [], [], [], A, A).
foldl(G, [X|Xs], [Y|Ys], [Z|Zs], A0, A) :-
    call(G, X, Y, Z, A0, A1),
    foldl(G, Xs, Ys, Zs, A1, A).

foldl(_, [], [], [], [], A, A).
foldl(G, [X|Xs], [Y|Ys], [Z|Zs], [W|Ws], A0, A) :-
    call(G, X, Y, Z, W, A0, A1),
    foldl(G, Xs, Ys, Zs, Ws, A1, A).

include(_, [], []).
include(G, [X|Xs], Ys) :-
    (   call(G, X)
    ->  Ys = [X|Ys1]
    ;   Ys = Ys1
    ),
    include(G, Xs, Ys1).

exclude(_, [], []).
exclude(G, [X|Xs], Ys) :-
    (   call(G, X)
    ->  Ys = Ys1
    ;   Ys = [X|Ys1]
    ),
    exclude(G, Xs, Ys1).

partition(_, [], [], []).
partition(G, [X|Xs], Incl, Excl) :-
    (   call(G, X)
    ->  Incl = [X|I1], Excl = E1
    ;   Incl = I1, Excl = [X|E1]
    ),
    partition(G, Xs, I1, E1).

partition(_, [], [], [], []).
partition(G, [X|Xs], Less, Equal, Greater) :-
    call(G, X, Order),
    '$partition_order'(Order, X, Less, Equal, Greater, L1, E1, G1),
    partition(G, Xs, L1, E1, G1).

'$partition_order'(<, X, [X|L], E, G, L, E, G).
'$partition_order'(=, X, L, [X|E], G, L, E, G).
'$partition_order'(>, X, L, E, [X|G], L, E, G).
