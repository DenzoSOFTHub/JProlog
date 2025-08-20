% ===================================================================
% TEST 42: Complex List Processing (JProlog Compatible)  
% ===================================================================
% Advanced list operations simplified for JProlog

% Example 1: Simple QuickSort
simple_quicksort([], []).
simple_quicksort([Pivot|Rest], Sorted) :-
    simple_partition(Rest, Pivot, Smaller, Larger),
    simple_quicksort(Smaller, SortedSmaller),
    simple_quicksort(Larger, SortedLarger),
    append(SortedSmaller, [Pivot|SortedLarger], Sorted).

simple_partition([], _, [], []).
simple_partition([H|T], Pivot, [H|Smaller], Larger) :-
    H =< Pivot, !,
    simple_partition(T, Pivot, Smaller, Larger).
simple_partition([H|T], Pivot, Smaller, [H|Larger]) :-
    simple_partition(T, Pivot, Smaller, Larger).

% Test: simple_quicksort([3,1,4,1,5,9,2,6], Sorted).
% Expected: Sorted = [1,1,2,3,4,5,6,9]

% Example 2: List Rotation Operations
rotate_left([], []).
rotate_left([H|T], Rotated) :-
    append(T, [H], Rotated).

rotate_right([], []).
rotate_right(List, Rotated) :-
    append(Init, [Last], List),
    append([Last], Init, Rotated).

rotate_n(List, 0, List) :- !.
rotate_n(List, N, Result) :-
    N > 0,
    rotate_left(List, Rotated),
    N1 is N - 1,
    rotate_n(Rotated, N1, Result).
rotate_n(List, N, Result) :-
    N < 0,
    rotate_right(List, Rotated),
    N1 is N + 1,
    rotate_n(Rotated, N1, Result).

% Test: rotate_n([a,b,c,d,e], 2, R).
% Expected: R = [c,d,e,a,b]

% Example 3: List Chunking
chunk_list([], _, []).
chunk_list(List, ChunkSize, [Chunk|RestChunks]) :-
    take_first_n(ChunkSize, List, Chunk, Rest),
    chunk_list(Rest, ChunkSize, RestChunks).

take_first_n(0, Rest, [], Rest) :- !.
take_first_n(_, [], [], []) :- !.
take_first_n(N, [H|T], [H|ChunkRest], Rest) :-
    N > 0,
    N1 is N - 1,
    take_first_n(N1, T, ChunkRest, Rest).

% Test: chunk_list([a,b,c,d,e,f,g], 3, Chunks).
% Expected: Chunks = [[a,b,c], [d,e,f], [g]]

% Example 4: List Flattening
flatten_list([], []).
flatten_list([H|T], Flattened) :-
    is_list(H), !,
    flatten_list(H, FlatH),
    flatten_list(T, FlatT),
    append(FlatH, FlatT, Flattened).
flatten_list([H|T], [H|FlatT]) :-
    flatten_list(T, FlatT).

% Test: flatten_list([a, [b, [c, d]], e], F).
% Expected: F = [a,b,c,d,e]

% Example 5: List Duplicate Removal
remove_duplicates([], []).
remove_duplicates([H|T], [H|Result]) :-
    remove_all_occurrences(H, T, TWithoutH),
    remove_duplicates(TWithoutH, Result).

remove_all_occurrences(_, [], []).
remove_all_occurrences(X, [X|T], Result) :- !,
    remove_all_occurrences(X, T, Result).
remove_all_occurrences(X, [H|T], [H|Result]) :-
    remove_all_occurrences(X, T, Result).

% Test: remove_duplicates([a,b,a,c,b,d,a], R).
% Expected: R = [a,b,c,d]

% Example 6: List Zip Operations
zip_lists([], [], []).
zip_lists([H1|T1], [H2|T2], [pair(H1,H2)|ZipRest]) :-
    zip_lists(T1, T2, ZipRest).

zip_with_index(List, ZippedWithIndex) :-
    zip_with_index_helper(List, 0, ZippedWithIndex).

zip_with_index_helper([], _, []).
zip_with_index_helper([H|T], Index, [indexed(Index, H)|Rest]) :-
    Index1 is Index + 1,
    zip_with_index_helper(T, Index1, Rest).

% Test: zip_lists([a,b,c], [1,2,3], Z).
% Expected: Z = [pair(a,1), pair(b,2), pair(c,3)]

% Example 7: List Statistics
list_sum([], 0).
list_sum([H|T], Sum) :-
    list_sum(T, RestSum),
    Sum is H + RestSum.

list_max([H], H) :- !.
list_max([H|T], Max) :-
    list_max(T, TMax),
    (H >= TMax -> Max = H ; Max = TMax).

list_min([H], H) :- !.
list_min([H|T], Min) :-
    list_min(T, TMin),
    (H =< TMin -> Min = H ; Min = TMin).

list_average(List, Average) :-
    list_sum(List, Sum),
    length(List, Len),
    Len > 0,
    Average is Sum / Len.

% Test: list_average([1,2,3,4,5], Avg).
% Expected: Avg = 3.0

% Example 8: List Permutation Check
is_permutation(List1, List2) :-
    length(List1, Len),
    length(List2, Len),
    all_elements_present(List1, List2).

all_elements_present([], _).
all_elements_present([H|T], List2) :-
    select(H, List2, Rest2),
    all_elements_present(T, Rest2).

% Test: is_permutation([a,b,c], [c,a,b]).
% Expected: true

% Example 9: Run Length Encoding (Simplified)
simple_rle([], []).
simple_rle([H|T], [count(1, H)|EncodedRest]) :-
    simple_rle(T, EncodedRest).

% More sophisticated version
rle_encode([], []).
rle_encode([H|T], [run(Count, H)|EncodedRest]) :-
    count_consecutive(H, [H|T], Count, Remaining),
    rle_encode(Remaining, EncodedRest).

count_consecutive(_, [], 0, []).
count_consecutive(X, [X|T], Count, Remaining) :-
    count_consecutive(X, T, RestCount, Remaining),
    Count is RestCount + 1.
count_consecutive(X, [Y|T], 0, [Y|T]) :-
    X \= Y.

rle_decode([], []).
rle_decode([run(Count, Element)|Rest], Decoded) :-
    replicate_element(Count, Element, Replicated),
    rle_decode(Rest, RestDecoded),
    append(Replicated, RestDecoded, Decoded).

replicate_element(0, _, []) :- !.
replicate_element(N, Element, [Element|Rest]) :-
    N > 0,
    N1 is N - 1,
    replicate_element(N1, Element, Rest).

% Test: rle_encode([a,a,a,b,b,c], E).
% Expected: E = [run(3,a), run(2,b), run(1,c)]

% Example 10: List Search and Replace
replace_all([], _, _, []).
replace_all([Old|T], Old, New, [New|RestReplaced]) :- !,
    replace_all(T, Old, New, RestReplaced).
replace_all([H|T], Old, New, [H|RestReplaced]) :-
    replace_all(T, Old, New, RestReplaced).

replace_first([], _, _, []).
replace_first([Old|T], Old, New, [New|T]) :- !.
replace_first([H|T], Old, New, [H|RestReplaced]) :-
    replace_first(T, Old, New, RestReplaced).

% Test: replace_all([a,b,a,c,a], a, x, R).
% Expected: R = [x,b,x,c,x]

% Testing predicates
test_simple_quicksort :-
    simple_quicksort([3,1,4,1,5,9,2,6], Sorted),
    write('QuickSort: '), write(Sorted), nl.

test_rotate :-
    rotate_n([a,b,c,d,e], 2, Rotated),
    write('Rotated: '), write(Rotated), nl.

test_chunk :-
    chunk_list([a,b,c,d,e,f,g], 3, Chunks),
    write('Chunked: '), write(Chunks), nl.

test_flatten :-
    flatten_list([a, [b, [c, d]], e], Flattened),
    write('Flattened: '), write(Flattened), nl.

test_remove_dups :-
    remove_duplicates([a,b,a,c,b,d,a], NoDups),
    write('No duplicates: '), write(NoDups), nl.

test_zip :-
    zip_lists([a,b,c], [1,2,3], Zipped),
    write('Zipped: '), write(Zipped), nl.

test_stats :-
    list_average([1,2,3,4,5], Avg),
    write('Average: '), write(Avg), nl.

test_rle :-
    rle_encode([a,a,a,b,b,c], Encoded),
    write('RLE Encoded: '), write(Encoded), nl.

test_replace :-
    replace_all([a,b,a,c,a], a, x, Replaced),
    write('Replaced: '), write(Replaced), nl.

test_permutation :-
    (is_permutation([a,b,c], [c,a,b]) ->
        write('Permutation check: true') ;
        write('Permutation check: false')
    ), nl.

% Run all list tests
run_list_tests :-
    write('=== Complex List Tests ==='), nl,
    test_simple_quicksort,
    test_rotate,
    test_chunk,
    test_flatten,
    test_remove_dups,
    test_zip,
    test_stats,
    test_rle,
    test_replace,
    test_permutation,
    write('List tests completed'), nl.