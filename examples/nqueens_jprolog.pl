/* ============================================
   N-Queens compatible with JProlog
   ============================================ */

% Genera [N,N-1,...,1] - with exclamation mark cut
upto(0, []) :- !.
upto(N, [N|T]) :-
    N > 0,
    N1 is N - 1,
    upto(N1, T).

% select/3 (senza tagli)
select(X, [X|Xs], Xs).
select(X, [Y|Ys], [Y|Zs]) :-
    select(X, Ys, Zs).

% permutazione
perm([], []).
perm(L, [X|Xs]) :-
    select(X, L, R),
    perm(R, Xs).

% Sicurezza: nessuna collisione in diagonale
safe([]).
safe([Q|Qs]) :-
    safe1(Q, Qs, 1),
    safe(Qs).

% safe1: controlla diagonali
safe1(_, [], _).
safe1(Q, [Q1|Qs], D) :-
    Q1 =\= Q + D,
    Q1 =\= Q - D,
    D1 is D + 1,
    safe1(Q, Qs, D1).

% queens: trova soluzione per N regine
queens(N, Qs) :-
    N >= 0,
    upto(N, Ns),
    perm(Ns, Qs),
    safe(Qs).

% solve8: per 8 regine
solve8(Qs) :- queens(8, Qs).

% solve4: per 4 regine (più veloce per test)
solve4(Qs) :- queens(4, Qs).

% Test semplice per 3 regine (nessuna soluzione)
solve3(Qs) :- queens(3, Qs).

/* Test predicates */
test_upto :- upto(3, [3,2,1]).
test_select :- select(2, [1,2,3], [1,3]).
test_perm :- perm([1,2], [2,1]).
test_safe :- safe([1,3,2]).

% Example facts for testing
example_solution([1,5,8,6,3,7,2,4]).
test_solution(Qs) :- example_solution(Qs), safe(Qs).