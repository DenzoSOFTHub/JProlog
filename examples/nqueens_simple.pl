% ============================================
% N-Queens Simplified Version for JProlog
% Avoids parsing issues with complex operators
% ============================================

% Generate list [1,2,...,N]
range(1, 1, [1]) :- !.
range(1, N, [1|Rest]) :-
    N > 1,
    N1 is N - 1,
    range(2, N, Rest).
    
range(Start, End, [Start|Rest]) :-
    Start < End,
    Start1 is Start + 1,
    range(Start1, End, Rest).
range(Start, Start, [Start]).

% Permutation using built-in select/3
permutation([], []).
permutation(List, [Head|Tail]) :-
    select(Head, List, Rest),
    permutation(Rest, Tail).

% Check if two queens attack each other diagonally
safe_diag(Q1, Q2, Dist) :-
    Diag1 is Q1 + Dist,
    Diag2 is Q1 - Dist,
    Q2 =:= Diag1,
    !,
    fail.
safe_diag(Q1, Q2, Dist) :-
    Diag2 is Q1 - Dist,
    Q2 =:= Diag2,
    !,
    fail.
safe_diag(_, _, _).

% Check if a queen position is safe from all previous queens
check_safe(_, [], _).
check_safe(Queen, [Q|Queens], Dist) :-
    safe_diag(Queen, Q, Dist),
    Dist1 is Dist + 1,
    check_safe(Queen, Queens, Dist1).

% Check if entire board is safe
all_safe([]).
all_safe([Q|Queens]) :-
    check_safe(Q, Queens, 1),
    all_safe(Queens).

% Main N-Queens predicate
queens(N, Solution) :-
    N > 0,
    range(1, N, Columns),
    permutation(Columns, Solution),
    all_safe(Solution).

% Convenience predicate for 4 queens
solve4(Solution) :- queens(4, Solution).

% Convenience predicate for 8 queens  
solve8(Solution) :- queens(8, Solution).

% Count all solutions for N queens
count_solutions(N, Count) :-
    findall(Sol, queens(N, Sol), AllSolutions),
    length(AllSolutions, Count).

% Print a simple representation
print_solution([]).
print_solution([Col|Rest]) :-
    write('Column: '), write(Col), nl,
    print_solution(Rest).