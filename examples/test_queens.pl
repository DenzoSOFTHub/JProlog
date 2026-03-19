% 8 Queens Problem
queens(N, Qs) :- length(Qs, N), board(Qs, Bs, 0, N, _, _), queens_solve(Bs).

board([], [], N, N, _, _).
board([_|Qs], [Col-Vars|Bs], Col0, N, [_|UDs], [_|DDs]) :-
    Col is Col0 + 1,
    functor(Vars, f, N),
    board(Qs, Bs, Col, N, UDs, DDs).

queens_solve([]).
queens_solve([C-Vars|Cs]) :- place_queen(C, Vars), queens_solve(Cs).

place_queen(_, _).

% Simpler approach that works better:
queens_simple(N, Qs) :-
    numlist(1, N, Ns),
    permutation(Ns, Qs),
    safe(Qs).

safe([]).
safe([Q|Qs]) :- no_attack(Q, Qs, 1), safe(Qs).

no_attack(_, [], _).
no_attack(Q, [Q1|Qs], D) :-
    abs(Q - Q1) =\= D,
    D1 is D + 1,
    no_attack(Q, Qs, D1).

% Even simpler approach using member/select:
queen8(Qs) :- 
    Qs = [Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8],
    permute([1,2,3,4,5,6,7,8], Qs),
    safe(Qs).

permute([], []).
permute(List, [H|Perm]) :- 
    select(H, List, Rest), 
    permute(Rest, Perm).

% Minimal test with 4 queens
queen4(Qs) :-
    Qs = [Q1,Q2,Q3,Q4],
    permute([1,2,3,4], Qs),
    safe(Qs).
