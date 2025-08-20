% ===================================================================
% TEST 22: Constraint Solving and Logic Programming
% ===================================================================
% Tests: Generate-and-test, constraint satisfaction, logic puzzles

% N-Queens problem with constraints
n_queens(N, Queens) :-
    length(Queens, N),
    board(Queens, Board, 0, N, _, _),
    Queens = Board.

board([], [], N, N, _, _).
board([_|Queens], [Col|Board], Row0, N, [_|VD], [_|VU]) :-
    Row is Row0 + 1,
    board(Queens, Board, Row, N, VD, VU),
    Col in 1..N,
    all_different([Col|Board]),
    safe([Col|Board]).

safe([]).
safe([Queen|Others]) :-
    safe(Others, Queen, 1).

safe([], _, _).
safe([Queen|Others], Queen0, Nb0) :-
    Queen0 + Nb0 #\= Queen,
    Queen0 - Nb0 #\= Queen,
    Nb is Nb0 + 1,
    safe(Others, Queen0, Nb).

% Send More Money puzzle
send_more_money(Digits) :-
    Digits = [S,E,N,D,M,O,R,Y],
    Digits ins 0..9,
    all_different(Digits),
    S #\= 0, M #\= 0,
    1000*S + 100*E + 10*N + D +
    1000*M + 100*O + 10*R + E #=
    10000*M + 1000*O + 100*N + 10*E + Y.

% Map coloring problem
color_map(Countries, Colors) :-
    Countries = [WA, NT, Q, NSW, V, SA, T],
    Colors = [red, green, blue],
    member(WA, Colors), member(NT, Colors), member(Q, Colors),
    member(NSW, Colors), member(V, Colors), member(SA, Colors), member(T, Colors),
    WA \= NT, WA \= SA,
    NT \= WA, NT \= Q, NT \= SA,
    Q \= NT, Q \= NSW, Q \= SA,
    NSW \= Q, NSW \= V, NSW \= SA,
    V \= NSW, V \= SA,
    SA \= WA, SA \= NT, SA \= Q, SA \= NSW, SA \= V.

% Test queries:
% ?- n_queens(4, Queens).
% ?- send_more_money(Digits).
% ?- color_map(Countries, Colors).