% ===================================================================
% TEST 04: List Processing
% ===================================================================
% Tests: List unification, recursion, built-in list predicates

% List membership and manipulation
my_member(X, [X|_]).
my_member(X, [_|Tail]) :- my_member(X, Tail).

my_append([], L, L).
my_append([H|T1], L2, [H|T3]) :- my_append(T1, L2, T3).

my_length([], 0).
my_length([_|T], N) :- my_length(T, N1), N is N1 + 1.

% List processing operations
sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, RestSum),
    Sum is H + RestSum.

max_list([X], X).
max_list([H|T], Max) :-
    max_list(T, TMax),
    (H > TMax -> Max = H ; Max = TMax).

% List transformations
double_elements([], []).
double_elements([H|T], [H2|T2]) :-
    H2 is H * 2,
    double_elements(T, T2).

filter_positive([], []).
filter_positive([H|T], [H|FilteredT]) :-
    H > 0, !,
    filter_positive(T, FilteredT).
filter_positive([_|T], FilteredT) :-
    filter_positive(T, FilteredT).

% Test queries:
% ?- my_member(b, [a, b, c]).
% ?- my_append([1, 2], [3, 4], L).
% ?- my_length([a, b, c, d], N).
% ?- sum_list([1, 2, 3, 4, 5], Sum).
% ?- max_list([3, 1, 4, 1, 5], Max).
% ?- double_elements([1, 2, 3], Doubled).
% ?- filter_positive([-1, 2, -3, 4], Positive).