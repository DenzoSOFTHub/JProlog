% ===================================================================
% TEST 05: Recursion and Backtracking
% ===================================================================
% Tests: Recursive predicates, backtracking, choice points

% Tree traversal
node(1, [2, 3]).
node(2, [4, 5]).
node(3, [6]).
node(4, []).
node(5, [7, 8]).
node(6, []).
node(7, []).
node(8, []).

% Tree operations
descendant(X, Y) :- node(X, Children), member(Y, Children).
descendant(X, Z) :- node(X, Children), member(Y, Children), descendant(Y, Z).

path_to_leaf(Leaf, [Leaf]) :- node(Leaf, []).
path_to_leaf(Node, [Node|Path]) :-
    node(Node, Children),
    Children \= [],
    member(Child, Children),
    path_to_leaf(Child, Path).

% Fibonacci sequence
fib(0, 0) :- !.
fib(1, 1) :- !.
fib(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2.

% Generate sequences
count_up(N, N).
count_up(Start, N) :-
    Start < N,
    Next is Start + 1,
    count_up(Next, N).

count_down(N, N).
count_down(Start, N) :-
    Start > N,
    Next is Start - 1,
    count_down(Next, N).

% Permutations
perm([], []).
perm([H|T], P) :-
    perm(T, PT),
    select(H, P, PT).

% Test queries:
% ?- descendant(1, X).         % Should find all descendants
% ?- path_to_leaf(1, Path).    % Should find paths to leaves
% ?- fib(6, F).
% ?- count_up(1, 5).          % Should succeed
% ?- count_down(5, 1).        % Should succeed  
% ?- perm([a, b, c], P).      % Should generate permutations