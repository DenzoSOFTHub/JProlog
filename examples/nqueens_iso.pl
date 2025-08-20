/* ============================================
   N-Queens in ISO Prolog (nessuna libreria)
   Rappresentazione: Qs è una lista di lunghezza N,
   dove l'i-esimo elemento è la colonna della regina
   posta alla riga i (righe 1..N, colonne 1..N).
   Esempio per 8 regine: ?- queens(8, Qs).
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
% safe(Qs): controlla tutte le coppie
safe([]).
safe([Q|Qs]) :-
    safe1(Q, Qs, 1),
    safe(Qs).

% safe1(Q, Rest, D): Q non è in diagonale con ciascun elemento di Rest
% con distanza di riga D, D+1, ...
safe1(_, [], _).
safe1(Q, [Q1|Qs], D) :-
    Q1 =\= Q + D,
    Q1 =\= Q - D,
    D1 is D + 1,
    safe1(Q, Qs, D1).

% queens(N, Qs): trova una soluzione per N regine
queens(N, Qs) :-
    N >= 0,
    upto(N, Ns),
    perm(Ns, Qs),
    safe(Qs).

% solve8(Qs): comodità per 8 regine
solve8(Qs) :- queens(8, Qs).

/* --------------------------
   Utilità opzionali
   -------------------------- */

% Conta il numero di soluzioni per N
solutions_count(N, Count) :-
    findall(Qs, queens(N, Qs), All),
    length(All, Count).

% Stampa semplice della scacchiera ('.' vuoto, 'Q' regina)
print_board(Qs) :-
    length(Qs, N),
    print_rows(Qs, N).

print_rows([], _).
print_rows([Col|Rest], N) :-
    print_row(N, Col, 1),
    nl,
    print_rows(Rest, N).

print_row(N, Col, C) :-
    ( C =< N ->
        ( C =:= Col -> write('Q') ; write('.') ),
        ( C =:= N -> true ; write(' ') ),
        C1 is C + 1,
        print_row(N, Col, C1)
    ; true ).