% ===================================================================
% TEST 17: Constraint-like Programming
% ===================================================================
% Tests: Constraint-style predicates, generate and test, finite domains

% Generate and test patterns
between_test(Low, High, X) :-
    between(Low, High, X).

% Finite domain simulation (without CLP(FD))
digit(0). digit(1). digit(2). digit(3). digit(4).
digit(5). digit(6). digit(7). digit(8). digit(9).

% Send + More = Money puzzle (simplified)
send_more_money(S, E, N, D, M, O, R, Y) :-
    digit(S), S > 0,           % S cannot be 0 (leading digit)
    digit(E), digit(N), digit(D),
    digit(M), M > 0,           % M cannot be 0 (leading digit)  
    digit(O), digit(R), digit(Y),
    all_different([S, E, N, D, M, O, R, Y]),
    SEND is S*1000 + E*100 + N*10 + D,
    MORE is M*1000 + O*100 + R*10 + E,
    MONEY is M*10000 + O*1000 + N*100 + E*10 + Y,
    SEND + MORE =:= MONEY.

all_different([]).
all_different([H|T]) :-
    \+ member(H, T),
    all_different(T).

% N-Queens constraint simulation (4x4 board)
n_queens_4(Queens) :-
    Queens = [Q1, Q2, Q3, Q4],
    between(1, 4, Q1),
    between(1, 4, Q2),
    between(1, 4, Q3),
    between(1, 4, Q4),
    all_different(Queens),
    safe_queens(Queens).

safe_queens(Queens) :-
    safe_diagonal(Queens, 1).

safe_diagonal([], _).
safe_diagonal([Q|Queens], Col) :-
    safe_from_others(Q, Col, Queens, 1),
    Col1 is Col + 1,
    safe_diagonal(Queens, Col1).

safe_from_others(_, _, [], _).
safe_from_others(Q, Col, [Q1|Queens], Dist) :-
    Q =\= Q1 + Dist,           % No diagonal attack down-right
    Q =\= Q1 - Dist,           % No diagonal attack up-right
    Dist1 is Dist + 1,
    safe_from_others(Q, Col, Queens, Dist1).

% Map coloring constraint
% Countries: A, B, C, D forming a diamond
% Constraints: Adjacent countries must have different colors
color(red). color(green). color(blue).

map_coloring(A, B, C, D) :-
    color(A), color(B), color(C), color(D),
    A \= B,    % A adjacent to B
    A \= C,    % A adjacent to C  
    B \= D,    % B adjacent to D
    C \= D.    % C adjacent to D

% Scheduling constraint  
time_slot(9). time_slot(10). time_slot(11). time_slot(14). time_slot(15).

schedule_meetings(Meeting1, Meeting2, Meeting3) :-
    time_slot(Meeting1),
    time_slot(Meeting2), 
    time_slot(Meeting3),
    all_different([Meeting1, Meeting2, Meeting3]),
    Meeting1 < Meeting2,       % Meeting1 before Meeting2
    abs(Meeting2 - Meeting3) >= 2.  % At least 1 hour gap

% Resource allocation
resource(cpu1). resource(cpu2). resource(memory1). resource(memory2).

allocate_resources(Task1, Task2, Task3) :-
    resource(Task1),
    resource(Task2),
    resource(Task3),
    all_different([Task1, Task2, Task3]).

% Cryptarithmetic helper
carry_arithmetic(A, B, C, Carry, Sum) :-
    TempSum is A + B + Carry,
    Sum is TempSum mod 10,
    NewCarry is TempSum // 10,
    between(0, 1, NewCarry).

% Test queries:
% ?- between_test(1, 5, X).           % Should generate 1,2,3,4,5
% ?- send_more_money(S,E,N,D,M,O,R,Y). % Might be slow
% ?- n_queens_4(Queens).              % Should find solutions
% ?- map_coloring(A, B, C, D).        % Should find valid colorings
% ?- schedule_meetings(M1, M2, M3).   % Should find valid schedules
% ?- allocate_resources(T1, T2, T3).  % Should allocate resources