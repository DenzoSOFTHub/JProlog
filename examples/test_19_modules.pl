% ===================================================================
% TEST 19: Module System (If Supported)
% ===================================================================
% Tests: module/2, use_module/1, module qualification
% NOTE: Modules are ISO standard but not implemented in all Prologs

% Module declaration (if supported)
% :- module(math_utils, [factorial/2, gcd/3, lcm/3]).

% If modules not supported, use regular predicates with prefixes
math_factorial(0, 1) :- !.
math_factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    math_factorial(N1, F1),
    F is N * F1.

math_gcd(X, 0, X) :- !.
math_gcd(X, Y, GCD) :-
    Y > 0,
    R is X mod Y,
    math_gcd(Y, R, GCD).

math_lcm(X, Y, LCM) :-
    math_gcd(X, Y, GCD),
    LCM is (X * Y) // GCD.

% Another module simulation
% :- module(list_utils, [flatten/2, zip/3, transpose/2]).

list_flatten([], []) :- !.
list_flatten([H|T], Flattened) :-
    is_list(H), !,
    list_flatten(H, FlatH),
    list_flatten(T, FlatT),
    append(FlatH, FlatT, Flattened).
list_flatten([H|T], [H|FlatT]) :-
    list_flatten(T, FlatT).

list_zip([], [], []).
list_zip([H1|T1], [H2|T2], [pair(H1,H2)|Zipped]) :-
    list_zip(T1, T2, Zipped).

list_transpose([], []).
list_transpose([[]|_], []) :- !.
list_transpose(Matrix, [Row|Transposed]) :-
    extract_column(Matrix, Row, RestMatrix),
    list_transpose(RestMatrix, Transposed).

extract_column([], [], []).
extract_column([[H|T]|Rows], [H|Column], [T|RestRows]) :-
    extract_column(Rows, Column, RestRows).

% Module usage simulation (without actual module system)
% Instead of: use_module(math_utils).
% We just use the prefixed predicates directly

client_calculate_area(Length, Width, Area) :-
    Area is Length * Width.

client_factorial_sum(N, Sum) :-
    findall(F, (between(1, N, I), math_factorial(I, F)), Factorials),
    sum_list(Factorials, Sum).

client_process_matrix(Matrix, Result) :-
    list_transpose(Matrix, Transposed),
    list_flatten(Transposed, Flattened),
    sort(Flattened, Result).

% Namespace conflict simulation
% Different "modules" with same predicate names
module_a_process(Data, processed_by_a(Data)).
module_b_process(Data, processed_by_b(Data)).

use_different_modules(Data, ResultA, ResultB) :-
    module_a_process(Data, ResultA),
    module_b_process(Data, ResultB).

% Module-qualified calls simulation
% Instead of: math_utils:factorial(5, F)
% We use: math_factorial(5, F)

qualified_call_test :-
    math_factorial(5, F),
    writeln(factorial_5(F)),
    math_gcd(12, 8, G),
    writeln(gcd_12_8(G)),
    list_zip([1,2,3], [a,b,c], Zipped),
    writeln(zipped(Zipped)).

% Export/import list simulation
exported_predicates([
    math_factorial/2,
    math_gcd/3,
    math_lcm/3,
    list_flatten/2,
    list_zip/3,
    list_transpose/2
]).

check_exported_predicate(Pred) :-
    exported_predicates(List),
    member(Pred, List).

% Test queries:
% ?- math_factorial(5, F).
% ?- math_gcd(12, 8, G).
% ?- math_lcm(4, 6, L).
% ?- list_flatten([[1,2],[3,[4,5]]], Flat).
% ?- list_zip([a,b,c], [1,2,3], Zipped).
% ?- list_transpose([[1,2,3],[4,5,6]], T).
% ?- qualified_call_test.
% ?- check_exported_predicate(math_factorial/2).