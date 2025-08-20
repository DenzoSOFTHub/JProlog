% ===================================================================
% TEST 06: Cut and Control Structures
% ===================================================================
% Tests: Cut (!), if-then-else, negation as failure

% Cut examples
max_cut(X, Y, X) :- X >= Y, !.
max_cut(_, Y, Y).

det_member(X, [X|_]) :- !.
det_member(X, [_|T]) :- det_member(X, T).

% Classification with cut
classify_number(N, negative) :- N < 0, !.
classify_number(0, zero) :- !.
classify_number(N, positive) :- N > 0.

% If-then-else constructs
absolute_value(X, X) :- X >= 0, !.
absolute_value(X, Y) :- Y is -X.

better_absolute(X, Y) :-
    (X >= 0 -> Y = X ; Y is -X).

grade(Score, Grade) :-
    (Score >= 90 -> Grade = a
    ; Score >= 80 -> Grade = b  
    ; Score >= 70 -> Grade = c
    ; Score >= 60 -> Grade = d
    ; Grade = f).

% Negation as failure
not_member(_, []).
not_member(X, [H|T]) :-
    X \= H,
    not_member(X, T).

single_solution(X, Y) :-
    member(X, [1, 2, 3]),
    member(Y, [a, b]),
    !.

% Deterministic choice
first_solution(Goal) :-
    call(Goal), !.

% Test queries:
% ?- max_cut(3, 5, Max).
% ?- det_member(b, [a, b, c]).    % Should not backtrack
% ?- classify_number(-5, Type).
% ?- better_absolute(-7, Abs).
% ?- grade(85, G).
% ?- not_member(d, [a, b, c]).
% ?- single_solution(X, Y).       % Should give only one solution