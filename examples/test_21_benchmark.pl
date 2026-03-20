% Performance benchmark tests
% Tests recursive queries, list operations, and unification-heavy workloads

% --- Fibonacci (deep recursion + arithmetic) ---
fib(0, 0) :- !.
fib(1, 1) :- !.
fib(N, F) :- N > 1, N1 is N - 1, N2 is N - 2, fib(N1, F1), fib(N2, F2), F is F1 + F2.

% --- Naive reverse (quadratic list operations) ---
nrev([], []).
nrev([H|T], R) :- nrev(T, RT), append(RT, [H], R).

% --- Generate list ---
gen_list(0, []) :- !.
gen_list(N, [N|T]) :- N > 0, N1 is N - 1, gen_list(N1, T).

% --- Map coloring (backtracking heavy) ---
color(red). color(green). color(blue). color(yellow).
adjacent(1,2). adjacent(1,3). adjacent(1,4). adjacent(2,3). adjacent(2,4). adjacent(3,4).
valid_coloring(C1,C2,C3,C4) :-
    color(C1), color(C2), color(C3), color(C4),
    \+ (adjacent(X,Y), nth1(X,[C1,C2,C3,C4],CX), nth1(Y,[C1,C2,C3,C4],CY), CX == CY).

% --- Unification heavy (compound term matching) ---
tree(leaf(X), X).
tree(node(L, V, R), X) :- tree(L, X) ; X = V ; tree(R, X).
sample_tree(node(node(leaf(1), 2, leaf(3)), 4, node(leaf(5), 6, leaf(7)))).

% --- Queens (backtracking + list ops) ---
queens(N, Qs) :- gen_list(N, Ns), permutation(Ns, Qs), safe(Qs).
safe([]).
safe([Q|Qs]) :- no_attack(Q, Qs, 1), safe(Qs).
no_attack(_, [], _).
no_attack(Q, [Q1|Qs], D) :- Q =\= Q1+D, Q =\= Q1-D, D1 is D+1, no_attack(Q, Qs, D1).
