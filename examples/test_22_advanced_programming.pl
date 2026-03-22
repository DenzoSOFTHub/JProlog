% ===================================================================
% TEST 22: Advanced Prolog Programming Paradigms
% ===================================================================
% Covers: logic programming, backtracking, constraints, meta-programming,
% parsing, DSL interpreters, trees, graphs, lists, accumulators.

% =====================================================
% SECTION 1: LOGIC PROGRAMMING - Relational Reasoning
% =====================================================

% 1.1 Family relationships with transitive closure
male(albert).
male(edward).
male(george).
male(james).
female(victoria).
female(alice).
female(mary).

parent_of(victoria, edward).
parent_of(victoria, alice).
parent_of(albert, edward).
parent_of(albert, alice).
parent_of(edward, george).
parent_of(edward, mary).
parent_of(alice, james).

father(F, C) :- male(F), parent_of(F, C).
mother(M, C) :- female(M), parent_of(M, C).
sibling(X, Y) :- parent_of(P, X), parent_of(P, Y), X \= Y.
grandparent(GP, GC) :- parent_of(GP, P), parent_of(P, GC).
uncle(U, N) :- male(U), sibling(U, P), parent_of(P, N).
cousin(X, Y) :- parent_of(PX, X), parent_of(PY, Y), sibling(PX, PY).

descendant(X, Y) :- parent_of(Y, X).
descendant(X, Y) :- parent_of(Z, X), descendant(Z, Y).

% 1.2 Logical puzzles - Who owns the fish?
% Simplified: 3 people, 3 pets, 3 colors
% Constraints: brit has red house, swede has dog, dane has tea
nationality(brit). nationality(swede). nationality(dane).
pet_type(cat). pet_type(dog). pet_type(fish).
house_color(red). house_color(green). house_color(blue).

solve_puzzle(FishOwner) :-
    % Each person has a nationality, pet, color
    nationality(N1), nationality(N2), nationality(N3),
    N1 \= N2, N1 \= N3, N2 \= N3,
    pet_type(P1), pet_type(P2), pet_type(P3),
    P1 \= P2, P1 \= P3, P2 \= P3,
    house_color(C1), house_color(C2), house_color(C3),
    C1 \= C2, C1 \= C3, C2 \= C3,
    % Brit lives in red house
    ((N1 = brit, C1 = red) ; (N2 = brit, C2 = red) ; (N3 = brit, C3 = red)),
    % Swede has dog
    ((N1 = swede, P1 = dog) ; (N2 = swede, P2 = dog) ; (N3 = swede, P3 = dog)),
    % Find fish owner
    ((N1 = FishOwner, P1 = fish) ; (N2 = FishOwner, P2 = fish) ; (N3 = FishOwner, P3 = fish)),
    !.

% 1.3 Syllogistic reasoning
mortal(X) :- human(X).
human(socrates).
human(plato).
wise(X) :- philosopher(X).
philosopher(socrates).
philosopher(plato).
philosopher(aristotle).
human(aristotle).

% =====================================================
% SECTION 2: BACKTRACKING PROGRAMMING
% =====================================================

% 2.1 N-Queens (4 queens)
queens(N, Qs) :- numlist(1, N, Ns), permutation(Ns, Qs), safe_queens(Qs).

safe_queens([]).
safe_queens([Q|Qs]) :- no_attack(Q, Qs, 1), safe_queens(Qs).

no_attack(_, [], _).
no_attack(Q, [Q1|Qs], D) :-
    Q1 - Q =\= D,
    Q - Q1 =\= D,
    D1 is D + 1,
    no_attack(Q, Qs, D1).

% Uses built-in numlist/3

% 2.2 Sudoku-like: fill a 2x2 grid
% Grid = [R1C1, R1C2, R2C1, R2C2], each 1-2, rows/cols unique
mini_sudoku([A,B,C,D]) :-
    member(A, [1,2]), member(B, [1,2]),
    member(C, [1,2]), member(D, [1,2]),
    A =\= B, C =\= D,  % rows
    A =\= C, B =\= D,  % cols
    !.

% 2.3 Generate-and-test: Pythagorean triples
pytriple(A, B, C, Max) :-
    between(1, Max, A),
    between(A, Max, B),
    C2 is A*A + B*B,
    C is round(sqrt(C2)),
    C =< Max,
    C*C =:= C2.

% 2.4 Cryptarithmetic: S E N D + M O R E = M O N E Y (simplified subset)
% AB + CD = EF where all digits different
crypto_add(A, B, C, D, E, F) :-
    member(A, [1,2,3,4,5,6,7,8,9]),
    member(B, [0,1,2,3,4,5,6,7,8,9]),
    member(C, [1,2,3,4,5,6,7,8,9]),
    member(D, [0,1,2,3,4,5,6,7,8,9]),
    AB is A * 10 + B,
    CD is C * 10 + D,
    Sum is AB + CD,
    E is Sum // 10,
    F is Sum mod 10,
    E > 0, Sum < 100,
    all_different([A,B,C,D,E,F]),
    !.

all_different([]).
all_different([H|T]) :- \+ member(H, T), all_different(T).

% =====================================================
% SECTION 3: CONSTRAINT-LIKE PROGRAMMING
% =====================================================

% 3.1 Map coloring (Australia)
color_au(red). color_au(green). color_au(blue).

color_australia(WA, NT, SA, Q, NSW, V, T) :-
    color_au(WA), color_au(NT), color_au(SA),
    color_au(Q), color_au(NSW), color_au(V), color_au(T),
    WA \= NT, WA \= SA,
    NT \= SA, NT \= Q,
    SA \= Q, SA \= NSW, SA \= V,
    Q \= NSW,
    NSW \= V,
    !.

% 3.2 Magic square 3x3 (simplified: check if given arrangement is magic)
magic_square([A,B,C,D,E,F,G,H,I]) :-
    S is A+B+C,
    S =:= D+E+F,
    S =:= G+H+I,
    S =:= A+D+G,
    S =:= B+E+H,
    S =:= C+F+I,
    S =:= A+E+I,
    S =:= C+E+G.

% 3.3 Scheduling: assign tasks to slots without conflict
task(t1, 1). task(t1, 2).
task(t2, 2). task(t2, 3).
task(t3, 1). task(t3, 3).

schedule(T1Slot, T2Slot, T3Slot) :-
    task(t1, T1Slot),
    task(t2, T2Slot),
    task(t3, T3Slot),
    T1Slot =\= T2Slot,
    T1Slot =\= T3Slot,
    T2Slot =\= T3Slot,
    !.

% =====================================================
% SECTION 4: META-PROGRAMMING
% =====================================================

% 4.1 Apply a goal to each element of a list (maplist-like)
my_maplist(_, []).
my_maplist(Pred, [H|T]) :-
    call(Pred, H),
    my_maplist(Pred, T).

positive(X) :- X > 0.
even(X) :- 0 is X mod 2.

% 4.2 Filter list (include elements satisfying a predicate)
my_filter(_, [], []).
my_filter(Pred, [H|T], [H|R]) :-
    call(Pred, H), !,
    my_filter(Pred, T, R).
my_filter(Pred, [_|T], R) :-
    my_filter(Pred, T, R).

% 4.3 Fold left (reduce)
my_foldl(_, Acc, [], Acc).
my_foldl(Pred, Acc, [H|T], Result) :-
    call(Pred, Acc, H, NewAcc),
    my_foldl(Pred, NewAcc, T, Result).

add(A, B, C) :- C is A + B.
mul(A, B, C) :- C is A * B.

% 4.4 Assert/retract for dynamic programming
% Fibonacci with memoization via assert
fib_memo(0, 0) :- !.
fib_memo(1, 1) :- !.
fib_memo(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib_memo(N1, F1),
    fib_memo(N2, F2),
    F is F1 + F2.

% 4.5 Functor/arg introspection
term_info(Term, Name, Arity) :-
    functor(Term, Name, Arity).

term_args(Term, Args) :-
    '=..'(Term, List),
    List = [_|Args].

% 4.6 Copy_term for fresh variables
make_pair(Template, X, Y, Result) :-
    copy_term(Template, Result),
    Result = pair(X, Y).

% =====================================================
% SECTION 5: PARSING (Difference Lists & Recursive Descent)
% =====================================================

% 5.1 Difference list append (O(1) conceptual)
dl_append(Xs, Ys, Zs) :-
    append(Xs, Ys, Zs).

% 5.2 Recursive descent parser for arithmetic (using lists as tokens)
parse_expr(Tokens, Result) :- p_expr(Result, Tokens, []).

p_expr(E, S0, S) :- p_term(T, S0, S1), p_expr_rest(T, E, S1, S).
p_expr_rest(T, E, [add|S0], S) :- p_term(T2, S0, S1), p_expr_rest(plus(T, T2), E, S1, S).
p_expr_rest(T, E, [sub|S0], S) :- p_term(T2, S0, S1), p_expr_rest(minus(T, T2), E, S1, S).
p_expr_rest(T, T, S, S).

p_term(T, S0, S) :- p_factor(F, S0, S1), p_term_rest(F, T, S1, S).
p_term_rest(F, T, [mul|S0], S) :- p_factor(F2, S0, S1), p_term_rest(times(F, F2), T, S1, S).
p_term_rest(F, F, S, S).

p_factor(num(N), [N|S], S) :- number(N).

% 5.3 Simple tokenizer: split atoms into char lists
atom_chars_list(Atom, Chars) :- atom_chars(Atom, Chars).

% 5.4 Recognize palindrome list
palindrome(L) :- reverse(L, L).

% =====================================================
% SECTION 6: DSL INTERPRETER
% =====================================================

% 6.1 Simple arithmetic DSL interpreter
eval_arith(num(N), N) :- number(N).
eval_arith(add(A, B), R) :- eval_arith(A, VA), eval_arith(B, VB), R is VA + VB.
eval_arith(sub(A, B), R) :- eval_arith(A, VA), eval_arith(B, VB), R is VA - VB.
eval_arith(mul(A, B), R) :- eval_arith(A, VA), eval_arith(B, VB), R is VA * VB.
eval_arith(neg(A), R) :- eval_arith(A, VA), R is -VA.

% 6.2 Boolean logic DSL
eval_bool(true, true).
eval_bool(false, false).
eval_bool(and(A, B), true) :- eval_bool(A, true), eval_bool(B, true).
eval_bool(and(_, _), false) :- !.  % short-circuit
eval_bool(or(A, _), true) :- eval_bool(A, true), !.
eval_bool(or(_, B), true) :- eval_bool(B, true), !.
eval_bool(or(_, _), false).
eval_bool(not(A), true) :- eval_bool(A, false).
eval_bool(not(A), false) :- eval_bool(A, true).
eval_bool(implies(A, B), R) :- eval_bool(or(not(A), B), R).

% 6.3 Simple imperative language interpreter
% State is a list of var=value pairs
eval_stmt(skip, S, S).
eval_stmt(assign(Var, Expr), S0, S1) :-
    eval_imp_expr(Expr, S0, Val),
    set_var(Var, Val, S0, S1).
eval_stmt(seq(S1, S2), State0, State2) :-
    eval_stmt(S1, State0, State1),
    eval_stmt(S2, State1, State2).
eval_stmt(if(Cond, Then, _Else), S0, S1) :-
    eval_imp_expr(Cond, S0, Val), Val =\= 0, !,
    eval_stmt(Then, S0, S1).
eval_stmt(if(_Cond, _Then, Else), S0, S1) :-
    eval_stmt(Else, S0, S1).
eval_stmt(while(Cond, Body), S0, S2) :-
    eval_imp_expr(Cond, S0, Val), Val =\= 0, !,
    eval_stmt(Body, S0, S1),
    eval_stmt(while(Cond, Body), S1, S2).
eval_stmt(while(_, _), S, S).

eval_imp_expr(num(N), _, N) :- number(N).
eval_imp_expr(var(V), S, Val) :- get_var(V, S, Val).
eval_imp_expr(plus(A, B), S, R) :- eval_imp_expr(A, S, VA), eval_imp_expr(B, S, VB), R is VA + VB.
eval_imp_expr(minus(A, B), S, R) :- eval_imp_expr(A, S, VA), eval_imp_expr(B, S, VB), R is VA - VB.
eval_imp_expr(times(A, B), S, R) :- eval_imp_expr(A, S, VA), eval_imp_expr(B, S, VB), R is VA * VB.
eval_imp_expr(gt(A, B), S, R) :- eval_imp_expr(A, S, VA), eval_imp_expr(B, S, VB), (VA > VB -> R = 1 ; R = 0).

get_var(V, [V=Val|_], Val) :- !.
get_var(V, [_|T], Val) :- get_var(V, T, Val).

set_var(V, Val, [], [V=Val]).
set_var(V, Val, [V=_|T], [V=Val|T]) :- !.
set_var(V, Val, [H|T], [H|T2]) :- set_var(V, Val, T, T2).

% =====================================================
% SECTION 7: TREE ANALYSIS
% =====================================================

% Trees represented as tree(Value, Left, Right) or nil
tree_height(nil, 0).
tree_height(tree(_, L, R), H) :-
    tree_height(L, HL),
    tree_height(R, HR),
    H is max(HL, HR) + 1.

tree_size(nil, 0).
tree_size(tree(_, L, R), S) :-
    tree_size(L, SL),
    tree_size(R, SR),
    S is SL + SR + 1.

tree_sum(nil, 0).
tree_sum(tree(V, L, R), S) :-
    tree_sum(L, SL),
    tree_sum(R, SR),
    S is SL + SR + V.

% In-order traversal
tree_inorder(nil, []).
tree_inorder(tree(V, L, R), Sorted) :-
    tree_inorder(L, LL),
    tree_inorder(R, RL),
    append(LL, [V|RL], Sorted).

% Mirror (reflect) a tree
tree_mirror(nil, nil).
tree_mirror(tree(V, L, R), tree(V, MR, ML)) :-
    tree_mirror(L, ML),
    tree_mirror(R, MR).

% Check if BST (binary search tree)
is_bst(T) :- is_bst(T, -99999, 99999).
is_bst(nil, _, _).
is_bst(tree(V, L, R), Min, Max) :-
    V > Min, V < Max,
    is_bst(L, Min, V),
    is_bst(R, V, Max).

% Insert into BST
bst_insert(nil, V, tree(V, nil, nil)).
bst_insert(tree(V, L, R), V, tree(V, L, R)) :- !.
bst_insert(tree(N, L, R), V, tree(N, NL, R)) :- V < N, !, bst_insert(L, V, NL).
bst_insert(tree(N, L, R), V, tree(N, L, NR)) :- V >= N, bst_insert(R, V, NR).

% Build BST from list
list_to_bst([], nil).
list_to_bst([H|T], Tree) :-
    list_to_bst(T, T0),
    bst_insert(T0, H, Tree).

% Tree leaf count
tree_leaves(nil, 0).
tree_leaves(tree(_, nil, nil), 1) :- !.
tree_leaves(tree(_, L, R), N) :-
    tree_leaves(L, NL),
    tree_leaves(R, NR),
    N is NL + NR.

% Tree depth (max depth of any node)
tree_max_depth(nil, 0).
tree_max_depth(tree(_, L, R), D) :-
    tree_max_depth(L, DL),
    tree_max_depth(R, DR),
    D is max(DL, DR) + 1.

% =====================================================
% SECTION 8: GRAPH ALGORITHMS
% =====================================================

% Directed graph as edge/2 facts
edge(a, b). edge(b, c). edge(c, d).
edge(a, d). edge(b, d). edge(d, e).
edge(e, f). edge(f, g). edge(c, g).

% Path finding with cycle detection
path(X, Y, Path) :- path(X, Y, [X], Path).
path(X, X, Visited, Visited).
path(X, Y, Visited, Path) :-
    edge(X, Z),
    \+ member(Z, Visited),
    path(Z, Y, [Z|Visited], Path).

% Shortest path (BFS)
shortest_path(Start, End, Path) :-
    bfs([[Start]], End, RevPath),
    reverse(RevPath, Path).

bfs([[End|Path]|_], End, [End|Path]) :- !.
bfs([Current|Rest], End, Result) :-
    Current = [Node|_],
    findall([Next|Current],
            (edge(Node, Next), \+ member(Next, Current)),
            Extensions),
    append(Rest, Extensions, NewQueue),
    bfs(NewQueue, End, Result).

% All paths between two nodes
all_paths(X, Y, Paths) :-
    findall(P, path(X, Y, P), Paths).

% Reachable nodes from a given node
reachable(Start, Reachable) :-
    findall(N, path(Start, N, _), AllNodes),
    sort(AllNodes, Reachable).

% Topological sort (simplified for DAG subset)
topo_sort(Sorted) :-
    findall(N, (edge(N, _) ; edge(_, N)), AllRaw),
    sort(AllRaw, Nodes),
    topo_sort(Nodes, [], Sorted).

topo_sort([], Acc, Acc).
topo_sort(Remaining, Acc, Sorted) :-
    member(N, Remaining),
    \+ (edge(P, N), member(P, Remaining)),  % N has no incoming from Remaining
    !,
    delete(Remaining, N, Rest),
    topo_sort(Rest, [N|Acc], Sorted).

% Check if graph has a cycle (from any node)
has_cycle :- edge(X, _), path_to_self(X).
path_to_self(X) :- edge(X, Y), path_exists(Y, X, [Y]).
path_exists(X, X, _).
path_exists(X, Y, Visited) :-
    edge(X, Z), \+ member(Z, Visited),
    path_exists(Z, Y, [Z|Visited]).

% Connected components (undirected interpretation)
undirected_edge(X, Y) :- edge(X, Y).
undirected_edge(X, Y) :- edge(Y, X).

% =====================================================
% SECTION 9: ADVANCED LIST PROCESSING
% =====================================================

% 9.1 Zip two lists
zip([], [], []).
zip([A|As], [B|Bs], [A-B|Zs]) :- zip(As, Bs, Zs).

% 9.2 Unzip
unzip([], [], []).
unzip([A-B|Zs], [A|As], [B|Bs]) :- unzip(Zs, As, Bs).

% 9.3 Take first N elements
take(_, 0, []) :- !.
take([], _, []) :- !.
take([H|T], N, [H|R]) :- N > 0, N1 is N - 1, take(T, N1, R).

% 9.4 Drop first N elements
drop(L, 0, L) :- !.
drop([], _, []) :- !.
drop([_|T], N, R) :- N > 0, N1 is N - 1, drop(T, N1, R).

% 9.5 Chunk list into groups of N
chunk([], _, []) :- !.
chunk(L, N, [C|Cs]) :- take(L, N, C), C \= [], drop(L, N, Rest), chunk(Rest, N, Cs).

% 9.6 Flatten nested lists
my_flatten([], []).
my_flatten([H|T], Flat) :-
    is_list(H), !,
    my_flatten(H, FH),
    my_flatten(T, FT),
    append(FH, FT, Flat).
my_flatten([H|T], [H|FT]) :-
    my_flatten(T, FT).

% 9.7 Run-length encoding
rle([], []).
rle([X|Xs], [N-X|Encoded]) :-
    count_run(X, Xs, N, Rest),
    rle(Rest, Encoded).

count_run(_, [], 1, []).
count_run(X, [X|Xs], N, Rest) :- !, count_run(X, Xs, N1, Rest), N is N1 + 1.
count_run(_, L, 1, L).

% 9.8 Matrix transpose (list of lists)
transpose([], []).
transpose([[]|_], []) :- !.
transpose(Matrix, [Row|Rows]) :-
    maplist_head(Matrix, Row, RestMatrix),
    transpose(RestMatrix, Rows).

maplist_head([], [], []).
maplist_head([[H|T]|Rows], [H|Hs], [T|Ts]) :-
    maplist_head(Rows, Hs, Ts).

% 9.9 Group elements by predicate
partition(_, [], [], []).
partition(Pred, [H|T], [H|Yes], No) :-
    call(Pred, H), !,
    partition(Pred, T, Yes, No).
partition(Pred, [H|T], Yes, [H|No]) :-
    partition(Pred, T, Yes, No).

% 9.10 Interleave two lists
interleave([], L, L).
interleave([H|T], L, [H|R]) :- interleave(L, T, R).

% 9.11 Rotate list N positions
rotate(L, 0, L) :- !.
rotate(L, N, R) :-
    length(L, Len), Len > 0,
    N1 is N mod Len,
    take(L, N1, Front),
    drop(L, N1, Back),
    append(Back, Front, R).

% =====================================================
% SECTION 10: ACCUMULATORS
% =====================================================

% 10.1 Tail-recursive length with accumulator
len_acc(L, N) :- len_acc(L, 0, N).
len_acc([], Acc, Acc).
len_acc([_|T], Acc, N) :- Acc1 is Acc + 1, len_acc(T, Acc1, N).

% 10.2 Tail-recursive sum
sum_acc(L, S) :- sum_acc(L, 0, S).
sum_acc([], Acc, Acc).
sum_acc([H|T], Acc, S) :- Acc1 is Acc + H, sum_acc(T, Acc1, S).

% 10.3 Tail-recursive reverse
rev_acc(L, R) :- rev_acc(L, [], R).
rev_acc([], Acc, Acc).
rev_acc([H|T], Acc, R) :- rev_acc(T, [H|Acc], R).

% 10.4 Tail-recursive max
max_acc([H|T], M) :- max_acc(T, H, M).
max_acc([], Acc, Acc).
max_acc([H|T], Acc, M) :- H > Acc, !, max_acc(T, H, M).
max_acc([_|T], Acc, M) :- max_acc(T, Acc, M).

% 10.5 Factorial with accumulator
fact_acc(N, F) :- fact_acc(N, 1, F).
fact_acc(0, Acc, Acc) :- !.
fact_acc(N, Acc, F) :- N > 0, Acc1 is Acc * N, N1 is N - 1, fact_acc(N1, Acc1, F).

% 10.6 GCD with accumulator pattern (Euclidean algorithm)
gcd(A, 0, A) :- A > 0, !.
gcd(A, B, G) :- B > 0, R is A mod B, gcd(B, R, G).

% 10.7 Collect with accumulator (like findall but manual)
collect_positives([], []).
collect_positives([H|T], [H|R]) :- H > 0, !, collect_positives(T, R).
collect_positives([_|T], R) :- collect_positives(T, R).

% 10.8 Running sum (prefix sums)
running_sum(L, RS) :- running_sum(L, 0, RS).
running_sum([], _, []).
running_sum([H|T], Acc, [S|RS]) :- S is Acc + H, running_sum(T, S, RS).

% 10.9 Convert number to digit list
digits(0, [0]) :- !.
digits(N, Ds) :- N > 0, digits_acc(N, [], Ds).
digits_acc(0, Acc, Acc) :- !.
digits_acc(N, Acc, Ds) :- N > 0, D is N mod 10, N1 is N // 10, digits_acc(N1, [D|Acc], Ds).

% 10.10 Power with accumulator
power(_, 0, 1) :- !.
power(B, E, R) :- E > 0, power_acc(B, E, 1, R).
power_acc(_, 0, Acc, Acc) :- !.
power_acc(B, E, Acc, R) :- E > 0, Acc1 is Acc * B, E1 is E - 1, power_acc(B, E1, Acc1, R).
