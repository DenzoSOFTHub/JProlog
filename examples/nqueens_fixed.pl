/* ============================================
   N-Queens - JProlog Compatible Version
   ============================================ */

% Genera [N,N-1,...,1]
upto(0, []) :- !.
upto(N, [N|T]) :-
    N > 0,
    N1 is N - 1,
    upto(N1, T).

% select/3 (ISO-style, senza tagli)
select(X, [X|Xs], Xs).
select(X, [Y|Ys], [Y|Zs]) :-
    select(X, Ys, Zs).

% permutazione (permut/2)
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

% Utilità semplici senza if-then-else complesso
% Stampa di base
print_solution(Qs) :-
    write('Solution: '), write(Qs), nl.

% Test semplici
test_upto :- upto(3, [3,2,1]).
test_select :- select(2, [1,2,3], [1,3]).
test_perm :- perm([1,2], [2,1]).
test_safe :- safe([1,3,2]).

% Esempio di soluzione nota per 8 regine
example_solution([1,5,8,6,3,7,2,4]).