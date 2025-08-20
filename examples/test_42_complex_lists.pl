% ===================================================================
% TEST 42: Complex List Processing Examples  
% ===================================================================
% Advanced list manipulation patterns and algorithms

% Example 1: Quick Sort Implementation
% Classic divide-and-conquer sorting algorithm
quicksort([], []).
quicksort([Pivot|Rest], Sorted) :-
    partition(Rest, Pivot, Smaller, Larger),
    quicksort(Smaller, SortedSmaller),
    quicksort(Larger, SortedLarger),
    append(SortedSmaller, [Pivot|SortedLarger], Sorted).

partition([], _, [], []).
partition([H|T], Pivot, [H|Smaller], Larger) :-
    H =< Pivot, !,
    partition(T, Pivot, Smaller, Larger).
partition([H|T], Pivot, Smaller, [H|Larger]) :-
    partition(T, Pivot, Smaller, Larger).

% Test: quicksort([3,1,4,1,5,9,2,6,5], Sorted).
% Expected: Sorted = [1,1,2,3,4,5,5,6,9]

% Example 2: List Permutation Generator
% Generates all permutations of a list
list_permutation([], []).
list_permutation([H|T], Perm) :-
    list_permutation(T, PermT),
    select(H, Perm, PermT).

% Alternative implementation using insert_anywhere
all_permutations([], [[]]).
all_permutations([H|T], AllPerms) :-
    all_permutations(T, TailPerms),
    findall(Perm, (member(TailPerm, TailPerms), insert_anywhere(H, TailPerm, Perm)), AllPerms).

insert_anywhere(X, List, [X|List]).
insert_anywhere(X, [H|T], [H|Result]) :-
    insert_anywhere(X, T, Result).

% Test: all_permutations([a,b,c], Perms).
% Expected: All 6 permutations of [a,b,c]

% Example 3: Matrix Operations (Lists of Lists)
% Transpose a matrix represented as list of rows
transpose([], []).
transpose([[]|_], []).
transpose(Matrix, [Column|RestColumns]) :-
    extract_column(Matrix, Column, RestMatrix),
    transpose(RestMatrix, RestColumns).

extract_column([], [], []).
extract_column([[H|T]|RestRows], [H|RestColumn], [T|RestMatrix]) :-
    extract_column(RestRows, RestColumn, RestMatrix).

% Matrix multiplication
matrix_multiply([], _, []).
matrix_multiply([Row|RestRows], Matrix2, [Product|RestProducts]) :-
    transpose(Matrix2, Matrix2T),
    row_multiply(Row, Matrix2T, Product),
    matrix_multiply(RestRows, Matrix2, RestProducts).

row_multiply(_, [], []).
row_multiply(Row, [Col|RestCols], [Dot|RestDots]) :-
    dot_product(Row, Col, Dot),
    row_multiply(Row, RestCols, RestDots).

dot_product([], [], 0).
dot_product([X|Xs], [Y|Ys], Dot) :-
    dot_product(Xs, Ys, RestDot),
    Dot is X * Y + RestDot.

% Test: transpose([[1,2,3],[4,5,6]], T).
% Expected: T = [[1,4],[2,5],[3,6]]

% Example 4: Sublist Operations
% Find all sublists of a given length
sublists_of_length(_, 0, [[]]).
sublists_of_length([], _, []).
sublists_of_length([H|T], N, Sublists) :-
    N > 0,
    N1 is N - 1,
    sublists_of_length(T, N, SublistsWithoutH),
    sublists_of_length(T, N1, ShorterSublists),
    add_element_to_sublists(H, ShorterSublists, SublistsWithH),
    append(SublistsWithH, SublistsWithoutH, Sublists).

add_element_to_sublists(_, [], []).
add_element_to_sublists(Element, [Sublist|Rest], [[Element|Sublist]|ModifiedRest]) :-
    add_element_to_sublists(Element, Rest, ModifiedRest).

% Find longest common subsequence  
longest_common_subsequence([], _, []).
longest_common_subsequence(_, [], []).
longest_common_subsequence([H|T1], [H|T2], [H|LCS]) :-
    longest_common_subsequence(T1, T2, LCS).
longest_common_subsequence([H1|T1], [H2|T2], LCS) :-
    \\+ (H1 = H2),
    longest_common_subsequence([H1|T1], T2, LCS1),
    longest_common_subsequence(T1, [H2|T2], LCS2),
    longer_list(LCS1, LCS2, LCS).

longer_list(List1, List2, List1) :-
    length(List1, Len1),
    length(List2, Len2),
    Len1 >= Len2, !.
longer_list(_, List2, List2).

% Test: longest_common_subsequence([a,b,c,d,e], [a,c,e,f,g], LCS).
% Expected: LCS = [a,c,e]

% Example 5: List Compression and Decompression
% Run-length encoding: compress consecutive identical elements
compress_list([], []).
compress_list([X|Xs], [count(N, X)|Compressed]) :-
    take_while_same(X, [X|Xs], N, Rest),
    compress_list(Rest, Compressed).

take_while_same(_, [], 0, []).
take_while_same(X, [X|Xs], N, Rest) :-
    take_while_same(X, Xs, N1, Rest),
    N is N1 + 1.
take_while_same(X, [Y|Ys], 0, [Y|Ys]) :-
    \\+ (X = Y).

% Decompress run-length encoded list
decompress_list([], []).
decompress_list([count(N, X)|Rest], Decompressed) :-
    replicate(N, X, Replicated),
    decompress_list(Rest, RestDecompressed),
    append(Replicated, RestDecompressed, Decompressed).

replicate(0, _, []) :- !.
replicate(N, X, [X|Rest]) :-
    N > 0,
    N1 is N - 1,
    replicate(N1, X, Rest).

% Test: compress_list([a,a,a,b,b,c,c,c,c], C).
% Expected: C = [count(3,a), count(2,b), count(4,c)]

% Example 6: Tree Operations with Lists
% Binary tree represented as tree(Value, Left, Right) or leaf(Value)
tree_to_list(leaf(X), [X]).
tree_to_list(tree(Root, Left, Right), List) :-
    tree_to_list(Left, LeftList),
    tree_to_list(Right, RightList),
    append(LeftList, [Root|RightList], List).

list_to_balanced_tree([], empty).
list_to_balanced_tree([X], leaf(X)).
list_to_balanced_tree(List, tree(Root, Left, Right)) :-
    length(List, Len),
    Len > 1,
    Mid is Len // 2,
    split_at(Mid, List, LeftPart, [Root|RightPart]),
    list_to_balanced_tree(LeftPart, Left),
    list_to_balanced_tree(RightPart, Right).

split_at(0, List, [], List) :- !.
split_at(N, [H|T], [H|LeftRest], Right) :-
    N > 0,
    N1 is N - 1,
    split_at(N1, T, LeftRest, Right).

% Test: list_to_balanced_tree([1,2,3,4,5], Tree).
% Expected: Balanced binary tree

% Example 7: Advanced List Filtering and Mapping
% Filter with index information
filter_with_index(_, [], []).
filter_with_index(Predicate, [H|T], FilteredWithIndex) :-
    filter_with_index_helper(Predicate, [H|T], 0, FilteredWithIndex).

filter_with_index_helper(_, [], _, []).
filter_with_index_helper(Predicate, [H|T], Index, [indexed(Index, H)|Rest]) :-
    call(Predicate, H),
    !,
    Index1 is Index + 1,
    filter_with_index_helper(Predicate, T, Index1, Rest).
filter_with_index_helper(Predicate, [_|T], Index, Rest) :-
    Index1 is Index + 1,
    filter_with_index_helper(Predicate, T, Index1, Rest).

% Map with accumulator
map_with_accumulator(_, [], Acc, Acc, []).
map_with_accumulator(Predicate, [H|T], AccIn, AccOut, [Result|RestResults]) :-
    call(Predicate, H, AccIn, AccNext, Result),
    map_with_accumulator(Predicate, T, AccNext, AccOut, RestResults).

% Example predicate for map_with_accumulator
sum_and_double(X, AccIn, AccOut, Result) :-
    AccOut is AccIn + X,
    Result is X * 2.

% Test: map_with_accumulator(sum_and_double, [1,2,3,4], 0, Sum, Doubled).
% Expected: Sum = 10, Doubled = [2,4,6,8]

% Example 8: List Sliding Window Operations
% Generate sliding windows of size N
sliding_window(_, [], []).
sliding_window(N, List, [Window|RestWindows]) :-
    length(List, Len),
    Len >= N,
    take_n(N, List, Window),
    List = [_|RestList],
    sliding_window(N, RestList, RestWindows).
sliding_window(N, List, []) :-
    length(List, Len),
    Len < N.

take_n(0, _, []) :- !.
take_n(N, [H|T], [H|Rest]) :-
    N > 0,
    N1 is N - 1,
    take_n(N1, T, Rest).

% Rolling statistics
rolling_average(WindowSize, List, Averages) :-
    sliding_window(WindowSize, List, Windows),
    maplist(list_average, Windows, Averages).

list_average(List, Average) :-
    sum_list(List, Sum),
    length(List, Len),
    Average is Sum / Len.

% Test: rolling_average(3, [1,2,3,4,5,6,7], Avgs).
% Expected: Avgs = [2.0, 3.0, 4.0, 5.0, 6.0]

% Example 9: List Difference and Intersection Operations
% Symmetric difference: elements in either list but not both
symmetric_difference(List1, List2, SymDiff) :-
    list_difference(List1, List2, Diff1),
    list_difference(List2, List1, Diff2),
    append(Diff1, Diff2, SymDiff).

list_difference([], _, []).
list_difference([H|T], List2, Diff) :-
    member(H, List2),
    !,
    list_difference(T, List2, Diff).
list_difference([H|T], List2, [H|RestDiff]) :-
    list_difference(T, List2, RestDiff).

% Multiple list intersection
multi_intersection([], []).
multi_intersection([List], List) :- !.
multi_intersection([List1, List2|RestLists], Intersection) :-
    list_intersection(List1, List2, TempIntersection),
    multi_intersection([TempIntersection|RestLists], Intersection).

list_intersection([], _, []).
list_intersection([H|T], List2, [H|RestIntersection]) :-
    member(H, List2),
    !,
    list_intersection(T, List2, RestIntersection).
list_intersection([_|T], List2, Intersection) :-
    list_intersection(T, List2, Intersection).

% Test: multi_intersection([[1,2,3,4], [2,3,4,5], [3,4,5,6]], I).
% Expected: I = [3,4]

% Example 10: Advanced List Generation Patterns
% Generate Fibonacci sequence as list
fibonacci_list(0, []) :- !.
fibonacci_list(1, [1]) :- !.  
fibonacci_list(N, FibList) :-
    N > 1,
    fibonacci_list_helper(N, 0, 1, [1], FibList).

fibonacci_list_helper(1, _, _, Acc, FibList) :-
    reverse(Acc, FibList), !.
fibonacci_list_helper(N, Prev, Curr, Acc, FibList) :-
    N > 1,
    Next is Prev + Curr,
    N1 is N - 1,
    fibonacci_list_helper(N1, Curr, Next, [Next|Acc], FibList).

% Generate prime numbers using Sieve of Eratosthenes
primes_up_to(N, Primes) :-
    N >= 2,
    range(2, N, Numbers),
    sieve(Numbers, Primes).

range(Start, End, [Start|Rest]) :-
    Start =< End,
    Start1 is Start + 1,
    range(Start1, End, Rest).
range(Start, End, []) :-
    Start > End.

sieve([], []).
sieve([Prime|Rest], [Prime|PrimesRest]) :-
    filter_multiples(Prime, Rest, Filtered),
    sieve(Filtered, PrimesRest).

filter_multiples(_, [], []).
filter_multiples(Prime, [H|T], Filtered) :-
    H mod Prime =:= 0,
    !,
    filter_multiples(Prime, T, Filtered).
filter_multiples(Prime, [H|T], [H|FilteredRest]) :-
    filter_multiples(Prime, T, FilteredRest).

% Test: primes_up_to(20, Primes).
% Expected: Primes = [2,3,5,7,11,13,17,19]

% Testing utilities
test_quicksort :-
    quicksort([3,1,4,1,5,9,2,6,5], Sorted),
    write('Quicksort result: '), write(Sorted), nl.

test_matrix_transpose :-
    transpose([[1,2,3],[4,5,6]], T),
    write('Matrix transpose: '), write(T), nl.

test_compress_decompress :-
    compress_list([a,a,a,b,b,c,c,c,c], Compressed),
    write('Compressed: '), write(Compressed), nl,
    decompress_list(Compressed, Decompressed),
    write('Decompressed: '), write(Decompressed), nl.

test_fibonacci :-
    fibonacci_list(10, Fib),
    write('Fibonacci list: '), write(Fib), nl.

test_primes :-
    primes_up_to(20, Primes),
    write('Primes up to 20: '), write(Primes), nl.

% Run all list tests
run_all_list_tests :-
    write('=== Complex List Processing Tests ==='), nl,
    test_quicksort,
    test_matrix_transpose,
    test_compress_decompress,
    test_fibonacci,
    test_primes,
    write('All list tests completed'), nl.