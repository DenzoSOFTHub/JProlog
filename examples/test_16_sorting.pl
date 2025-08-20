% ===================================================================
% TEST 16: Sorting and List Operations
% ===================================================================
% Tests: sort/2, msort/2, keysort/2, custom sorting

% Basic sorting tests
test_basic_sort :-
    sort([3, 1, 4, 1, 5, 9, 2, 6], Sorted),
    writeln(sorted(Sorted)).

test_msort :-
    msort([3, 1, 4, 1, 5, 9, 2, 6], MSorted),
    writeln(msorted(MSorted)).

% Key-value sorting (if keysort/2 is supported)
test_keysort :-
    Data = [3-alice, 1-bob, 4-charlie, 1-diana],
    (current_predicate(keysort/2) ->
        (keysort(Data, KeySorted),
         writeln(keysorted(KeySorted)))
    ;   writeln('keysort/2 not supported')
    ).

% Custom sorting implementations
bubble_sort(List, Sorted) :-
    swap(List, List1), !,
    bubble_sort(List1, Sorted).
bubble_sort(Sorted, Sorted).

swap([X, Y|Rest], [Y, X|Rest]) :-
    X @> Y.
swap([X|Rest], [X|Rest1]) :-
    swap(Rest, Rest1).

% Merge sort implementation
merge_sort([], []) :- !.
merge_sort([X], [X]) :- !.
merge_sort(List, Sorted) :-
    split(List, Left, Right),
    merge_sort(Left, SortedLeft),
    merge_sort(Right, SortedRight),
    merge(SortedLeft, SortedRight, Sorted).

split([], [], []).
split([X], [X], []).
split([X, Y|Rest], [X|Left], [Y|Right]) :-
    split(Rest, Left, Right).

merge([], List, List) :- !.
merge(List, [], List) :- !.
merge([X|Xs], [Y|Ys], [X|Merged]) :-
    X @=< Y, !,
    merge(Xs, [Y|Ys], Merged).
merge([X|Xs], [Y|Ys], [Y|Merged]) :-
    merge([X|Xs], Ys, Merged).

% Quick sort implementation
quick_sort([], []).
quick_sort([H|T], Sorted) :-
    partition(T, H, Smaller, Larger),
    quick_sort(Smaller, SortedSmaller),
    quick_sort(Larger, SortedLarger),
    append(SortedSmaller, [H|SortedLarger], Sorted).

partition([], _, [], []).
partition([H|T], Pivot, [H|Smaller], Larger) :-
    H @=< Pivot, !,
    partition(T, Pivot, Smaller, Larger).
partition([H|T], Pivot, Smaller, [H|Larger]) :-
    partition(T, Pivot, Smaller, Larger).

% Sorting with custom comparison
sort_by_length(Lists, Sorted) :-
    map_lengths(Lists, Tagged),
    keysort(Tagged, SortedTagged),
    extract_values(SortedTagged, Sorted).

map_lengths([], []).
map_lengths([List|Lists], [Len-List|Tagged]) :-
    length(List, Len),
    map_lengths(Lists, Tagged).

extract_values([], []).
extract_values([_-Value|Tagged], [Value|Values]) :-
    extract_values(Tagged, Values).

% Testing different sorting algorithms
compare_sorts(List) :-
    writeln(original(List)),
    sort(List, BuiltinSorted),
    writeln(builtin_sort(BuiltinSorted)),
    bubble_sort(List, BubbleSorted),
    writeln(bubble_sort(BubbleSorted)),
    merge_sort(List, MergeSorted),
    writeln(merge_sort(MergeSorted)),
    quick_sort(List, QuickSorted),
    writeln(quick_sort(QuickSorted)).

% Test queries:
% ?- test_basic_sort.
% ?- test_msort.
% ?- test_keysort.
% ?- bubble_sort([3, 1, 4, 1, 5], Sorted).
% ?- merge_sort([3, 1, 4, 1, 5], Sorted).
% ?- quick_sort([3, 1, 4, 1, 5], Sorted).
% ?- compare_sorts([5, 2, 8, 1, 9]).
% ?- sort_by_length([[a], [b, c], [], [d, e, f]], Sorted).