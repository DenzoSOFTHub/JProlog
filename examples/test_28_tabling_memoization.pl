% ===================================================================
% TEST 28: Tabling and Memoization
% ===================================================================
% Tests: table/1 directive, memoization, cycle resolution

% Fibonacci with tabling
:- table fibonacci/2.

fibonacci(0, 1).
fibonacci(1, 1).
fibonacci(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, F1),
    fibonacci(N2, F2),
    F is F1 + F2.

% Path finding with cycle detection
:- table path/2.

edge(a, b).
edge(b, c).
edge(c, d).
edge(d, a).
edge(b, e).

path(X, X).
path(X, Z) :-
    edge(X, Y),
    path(Y, Z).

% Mutual recursion with tabling
:- table even/1, odd/1.

even(0).
even(N) :-
    N > 0,
    N1 is N - 1,
    odd(N1).

odd(1).
odd(N) :-
    N > 1,
    N1 is N - 1,
    even(N1).

% Grammar with left recursion (requires tabling)
:- table expr_left/3.

expr_left(plus(E1, E2)) --> expr_left(E1), [+], term(E2).
expr_left(E) --> term(E).

term(num(N)) --> [N], { number(N) }.

% Memoization for expensive computations
:- table expensive_computation/2.

expensive_computation(Input, Result) :-
    complex_calculation(Input, Result).

complex_calculation(N, Result) :-
    (   N =< 1
    ->  Result = 1
    ;   N1 is N - 1,
        complex_calculation(N1, R1),
        Result is N * R1
    ).

% Call subsumption and variant checking
:- table subset_sum/3 as subsumptive.

subset_sum([], 0, []).
subset_sum([H|T], Sum, [H|Selected]) :-
    Sum >= H,
    Sum1 is Sum - H,
    subset_sum(T, Sum1, Selected).
subset_sum([_|T], Sum, Selected) :-
    subset_sum(T, Sum, Selected).

% Test queries:
% ?- fibonacci(10, F).
% ?- path(a, e).
% ?- even(100).
% ?- phrase(expr_left(E), [1, +, 2, +, 3]).
% ?- expensive_computation(5, R).
% ?- subset_sum([1,2,3,4,5], 7, Solution).