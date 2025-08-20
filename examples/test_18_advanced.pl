% ===================================================================
% TEST 18: Advanced Prolog Features
% ===================================================================
% Tests: Complex recursion, tabling simulation, memoization

% Fibonacci with memoization simulation
:- dynamic(fib_memo/2).

fib_memo_clear :-
    retractall(fib_memo(_, _)).

fib_with_memo(N, F) :-
    fib_memo(N, F), !.
fib_with_memo(N, F) :-
    fib_compute(N, F),
    assertz(fib_memo(N, F)).

fib_compute(0, 0) :- !.
fib_compute(1, 1) :- !.
fib_compute(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib_with_memo(N1, F1),
    fib_with_memo(N2, F2),
    F is F1 + F2.

% Ackermann function (computationally intensive)
ackermann(0, N, Result) :- !,
    Result is N + 1.
ackermann(M, 0, Result) :- !,
    M > 0,
    M1 is M - 1,
    ackermann(M1, 1, Result).
ackermann(M, N, Result) :-
    M > 0, N > 0,
    M1 is M - 1,
    N1 is N - 1,
    ackermann(M, N1, Temp),
    ackermann(M1, Temp, Result).

% Towers of Hanoi
hanoi(1, From, To, _, [move(From, To)]) :- !.
hanoi(N, From, To, Via, Moves) :-
    N > 1,
    N1 is N - 1,
    hanoi(N1, From, Via, To, Moves1),
    hanoi(1, From, To, Via, Moves2),  
    hanoi(N1, Via, To, From, Moves3),
    append(Moves1, Moves2, TempMoves),
    append(TempMoves, Moves3, Moves).

% Dynamic programming - Longest Common Subsequence
:- dynamic(lcs_memo/3).

lcs_clear :-
    retractall(lcs_memo(_, _, _)).

lcs([], _, []) :- !.
lcs(_, [], []) :- !.
lcs([H|T1], [H|T2], [H|LCS]) :- !,
    lcs(T1, T2, LCS).
lcs([H1|T1], [H2|T2], LCS) :-
    H1 \= H2,
    lcs([H1|T1], T2, LCS1),
    lcs(T1, [H2|T2], LCS2),
    (length(LCS1, Len1), length(LCS2, Len2),
     (Len1 >= Len2 -> LCS = LCS1 ; LCS = LCS2)).

% Graph algorithms - Depth First Search
edge(a, b). edge(a, c). edge(b, d). edge(c, d). 
edge(c, e). edge(d, f). edge(e, f).

dfs_path(Start, End, Path) :-
    dfs_path(Start, End, [Start], Path).

dfs_path(End, End, Acc, Path) :-
    reverse(Acc, Path).
dfs_path(Current, End, Acc, Path) :-
    edge(Current, Next),
    \+ member(Next, Acc),
    dfs_path(Next, End, [Next|Acc], Path).

% All paths between nodes
all_paths(Start, End, Paths) :-
    findall(Path, dfs_path(Start, End, Path), Paths).

% Cycle detection
has_cycle(Start) :-
    has_cycle(Start, [Start]).

has_cycle(Current, Visited) :-
    edge(Current, Next),
    (member(Next, Visited) ->
        true
    ;   has_cycle(Next, [Next|Visited])
    ).

% Complex data structure - Binary tree operations
% Tree: tree(Value, Left, Right) or empty
tree_insert(X, empty, tree(X, empty, empty)).
tree_insert(X, tree(V, L, R), tree(V, NewL, R)) :-
    X < V, !,
    tree_insert(X, L, NewL).
tree_insert(X, tree(V, L, R), tree(V, L, NewR)) :-
    X >= V,
    tree_insert(X, R, NewR).

tree_search(X, tree(X, _, _)).
tree_search(X, tree(V, L, _)) :-
    X < V,
    tree_search(X, L).
tree_search(X, tree(V, _, R)) :-
    X > V,
    tree_search(X, R).

tree_inorder(empty, []).
tree_inorder(tree(V, L, R), Traversal) :-
    tree_inorder(L, LeftTraversal),
    tree_inorder(R, RightTraversal),
    append(LeftTraversal, [V|RightTraversal], Traversal).

% Test queries:
% ?- fib_with_memo(10, F).        % Should be fast with memoization
% ?- ackermann(3, 2, R).          % Should be 9, but slow
% ?- hanoi(3, left, right, middle, Moves).
% ?- lcs([a,b,c,d], [a,c,e,d], LCS).
% ?- dfs_path(a, f, Path).
% ?- all_paths(a, f, Paths).
% ?- tree_insert(5, empty, T1), tree_insert(3, T1, T2), tree_inorder(T2, Order).