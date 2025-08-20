# List Operations Guide - Comprehensive List Handling in JProlog

## Table of Contents
1. [Introduction to Lists](#introduction-to-lists)
2. [List Syntax and Structure](#list-syntax-and-structure)
3. [Basic List Operations](#basic-list-operations)
4. [Built-in List Predicates](#built-in-list-predicates)
5. [List Construction and Deconstruction](#list-construction-and-deconstruction)
6. [Advanced List Processing](#advanced-list-processing)
7. [List Algorithms](#list-algorithms)
8. [Performance Considerations](#performance-considerations)
9. [Common Use Cases](#common-use-cases)
10. [Best Practices](#best-practices)

---

## Introduction to Lists

**Lists** are one of the most fundamental and powerful data structures in Prolog. They provide a flexible way to represent sequences, collections, and structured data. JProlog offers comprehensive list support with both built-in predicates and the ability to define custom list operations.

### Key Features of JProlog Lists:
- **Dynamic Size**: Lists can grow and shrink during execution
- **Heterogeneous Elements**: Lists can contain different types of data
- **Recursive Structure**: Perfect for recursive algorithms
- **Pattern Matching**: Powerful unification with list patterns
- **Built-in Support**: Extensive library of list manipulation predicates
- **ISO Compliance**: Standard Prolog list behavior and formatting

---

## List Syntax and Structure

### Basic List Notation

```prolog
% Empty list
?- X = [].
X = [].

% Single element list
?- X = [hello].
X = [hello].

% Multiple elements
?- X = [1, 2, 3, 4, 5].
X = [1, 2, 3, 4, 5].

% Mixed data types
?- X = [atom, 42, 3.14, [nested, list]].
X = [atom, 42, 3.14, [nested, list]].
```

### Head and Tail Notation

```prolog
% Head|Tail pattern
?- [H|T] = [1, 2, 3, 4].
H = 1,
T = [2, 3, 4].

% Multiple heads
?- [H1, H2|T] = [a, b, c, d, e].
H1 = a,
H2 = b,
T = [c, d, e].

% Tail matching
?- [_|T] = [first, second, third].
T = [second, third].
```

### List Construction Patterns

```prolog
% Building lists with variables
?- X = [1, 2, 3|Rest].
X = [1, 2, 3|Rest].

% Prepending elements
?- X = [new_element|[1, 2, 3]].
X = [new_element, 1, 2, 3].

% Complex patterns
?- [A, B|[C, D|Rest]] = [1, 2, 3, 4, 5, 6].
A = 1,
B = 2,
C = 3,
D = 4,
Rest = [5, 6].
```

---

## Basic List Operations

### List Membership

```prolog
% Check if element is in list
?- member(2, [1, 2, 3, 4]).
true.

?- member(X, [apple, banana, cherry]).
X = apple ;
X = banana ;
X = cherry.

% Find all positions where element occurs
?- findall(X, member(X, [1, 2, 1, 3, 1]), Results).
Results = [1, 2, 1, 3, 1].
```

### List Length

```prolog
% Get length of list
?- length([1, 2, 3, 4, 5], N).
N = 5.

% Generate list of specific length
?- length(L, 3).
L = [_1, _2, _3].

% Generate and constrain
?- length(L, 3), L = [a, b, c].
L = [a, b, c].
```

### List Concatenation

```prolog
% Append two lists
?- append([1, 2], [3, 4], Result).
Result = [1, 2, 3, 4].

% Split list at specific point
?- append(X, Y, [1, 2, 3, 4, 5]).
X = [],
Y = [1, 2, 3, 4, 5] ;
X = [1],
Y = [2, 3, 4, 5] ;
X = [1, 2],
Y = [3, 4, 5] ;
% ... and so on

% Append multiple lists
?- append([1, 2], [3, 4], Temp), append(Temp, [5, 6], Result).
Result = [1, 2, 3, 4, 5, 6].
```

---

## Built-in List Predicates

### Core List Predicates

#### **member/2** - List Membership
```prolog
% Check membership
?- member(b, [a, b, c]).
true.

% Generate members
?- member(X, [red, green, blue]).
X = red ;
X = green ;
X = blue.

% Use in rules
likes_color(Person, Color) :-
    person(Person),
    member(Color, [red, blue, green]).
```

#### **append/3** - List Concatenation
```prolog
% Concatenate lists
?- append([1, 2], [3, 4], L).
L = [1, 2, 3, 4].

% Find prefix and suffix
?- append(Prefix, [4, 5], [1, 2, 3, 4, 5]).
Prefix = [1, 2, 3].

% Check if list is prefix
is_prefix(Prefix, List) :-
    append(Prefix, _, List).

% Usage:
% ?- is_prefix([1, 2], [1, 2, 3, 4, 5]).
% true.
```

#### **length/2** - List Length
```prolog
% Get list length
?- length([a, b, c, d], N).
N = 4.

% Create list of specific length
?- length(L, 5), maplist(=(x), L).
L = [x, x, x, x, x].

% Constraint satisfaction
same_length(List1, List2) :-
    length(List1, N),
    length(List2, N).
```

#### **reverse/2** - List Reversal
```prolog
% Reverse a list
?- reverse([1, 2, 3, 4], R).
R = [4, 3, 2, 1].

% Check if list is palindrome
palindrome(List) :-
    reverse(List, List).

% Usage:
% ?- palindrome([a, b, a]).
% true.
```

### Sorting Predicates

#### **sort/2** - Sort and Remove Duplicates
```prolog
% Sort list removing duplicates
?- sort([3, 1, 4, 1, 5, 9, 2, 6], Sorted).
Sorted = [1, 2, 3, 4, 5, 6, 9].

% Remove duplicates without sorting
remove_duplicates(List, Unique) :-
    sort(List, Unique).
```

#### **msort/2** - Sort Keeping Duplicates
```prolog
% Sort keeping all elements
?- msort([3, 1, 4, 1, 5, 9, 2, 6], Sorted).
Sorted = [1, 1, 2, 3, 4, 5, 6, 9].

% Stable sort preserves order of equal elements
?- msort([pair(3,a), pair(1,b), pair(3,c)], Sorted).
Sorted = [pair(1,b), pair(3,a), pair(3,c)].
```

### List Selection Predicates

#### **nth0/3** and **nth1/3** - Element Access
```prolog
% Zero-based indexing (nth0)
?- nth0(2, [a, b, c, d, e], Element).
Element = c.

% One-based indexing (nth1)
?- nth1(3, [a, b, c, d, e], Element).
Element = c.

% Find index of element
?- nth1(Index, [x, y, z, y, w], y).
Index = 2 ;
Index = 4.

% Replace element at position
replace_nth0(List, Index, NewElement, NewList) :-
    nth0(Index, List, _, Rest),
    nth0(Index, NewList, NewElement, Rest).
```

#### **select/3** - Element Removal
```prolog
% Remove element from list
?- select(b, [a, b, c, d], Rest).
Rest = [a, c, d].

% Remove first occurrence
?- select(x, [a, x, b, x, c], Rest).
Rest = [a, b, x, c].

% Insert element
?- select(new, NewList, [a, b, c]).
NewList = [new, a, b, c] ;
NewList = [a, new, b, c] ;
NewList = [a, b, new, c] ;
NewList = [a, b, c, new].
```

---

## List Construction and Deconstruction

### Building Lists Recursively

```prolog
% Generate list of numbers from 1 to N
numbers_up_to(0, []).
numbers_up_to(N, [N|Rest]) :-
    N > 0,
    N1 is N - 1,
    numbers_up_to(N1, Rest).

% Usage:
% ?- numbers_up_to(5, L).
% L = [5, 4, 3, 2, 1].

% Generate ascending list
numbers_1_to_n(N, List) :-
    numbers_up_to(N, Reversed),
    reverse(Reversed, List).

% Usage:
% ?- numbers_1_to_n(5, L).
% L = [1, 2, 3, 4, 5].
```

### List Filtering

```prolog
% Filter even numbers
filter_even([], []).
filter_even([H|T], [H|FilteredT]) :-
    0 =:= H mod 2,
    filter_even(T, FilteredT).
filter_even([H|T], FilteredT) :-
    1 =:= H mod 2,
    filter_even(T, FilteredT).

% Usage:
% ?- filter_even([1, 2, 3, 4, 5, 6], Evens).
% Evens = [2, 4, 6].

% Generic filter predicate
filter([], _, []).
filter([H|T], Condition, [H|FilteredT]) :-
    call(Condition, H),
    filter(T, Condition, FilteredT).
filter([H|T], Condition, FilteredT) :-
    \+ call(Condition, H),
    filter(T, Condition, FilteredT).

% Usage:
% ?- filter([1, 2, 3, 4, 5], >(3), Result).
% Result = [4, 5].
```

### List Transformation

```prolog
% Map function over list
map_list([], _, []).
map_list([H|T], Predicate, [MappedH|MappedT]) :-
    call(Predicate, H, MappedH),
    map_list(T, Predicate, MappedT).

% Double all numbers
double(X, Y) :- Y is X * 2.

% Usage:
% ?- map_list([1, 2, 3, 4], double, Result).
% Result = [2, 4, 6, 8].

% Square all numbers
square(X, Y) :- Y is X * X.

% Usage:
% ?- map_list([1, 2, 3, 4], square, Result).
% Result = [1, 4, 9, 16].
```

---

## Advanced List Processing

### List Folding (Reduction)

```prolog
% Fold left (accumulator-based reduction)
foldl(_, [], Acc, Acc).
foldl(Predicate, [H|T], Acc0, AccN) :-
    call(Predicate, H, Acc0, Acc1),
    foldl(Predicate, T, Acc1, AccN).

% Sum of list using foldl
add(X, Acc, NewAcc) :- NewAcc is X + Acc.

sum_list(List, Sum) :-
    foldl(add, List, 0, Sum).

% Usage:
% ?- sum_list([1, 2, 3, 4, 5], S).
% S = 15.

% Product of list
multiply(X, Acc, NewAcc) :- NewAcc is X * Acc.

product_list(List, Product) :-
    foldl(multiply, List, 1, Product).

% Usage:
% ?- product_list([2, 3, 4], P).
% P = 24.
```

### List Partitioning

```prolog
% Partition list based on condition
partition([], _, [], []).
partition([H|T], Condition, [H|Satisfies], DoesNotSatisfy) :-
    call(Condition, H),
    partition(T, Condition, Satisfies, DoesNotSatisfy).
partition([H|T], Condition, Satisfies, [H|DoesNotSatisfy]) :-
    \+ call(Condition, H),
    partition(T, Condition, Satisfies, DoesNotSatisfy).

% Separate positive and negative numbers
positive(X) :- X > 0.

% Usage:
% ?- partition([-2, 3, -1, 4, 5], positive, Pos, Neg).
% Pos = [3, 4, 5],
% Neg = [-2, -1].
```

### List Grouping

```prolog
% Group consecutive equal elements
group_consecutive([], []).
group_consecutive([X], [[X]]).
group_consecutive([X,X|T], [[X|Group1]|Groups]) :-
    group_consecutive([X|T], [Group1|Groups]).
group_consecutive([X,Y|T], [[X]|Groups]) :-
    X \= Y,
    group_consecutive([Y|T], Groups).

% Usage:
% ?- group_consecutive([a,a,a,b,b,c,c,c,c], Groups).
% Groups = [[a,a,a], [b,b], [c,c,c,c]].
```

---

## List Algorithms

### Searching Algorithms

```prolog
% Linear search with position
linear_search(Element, List, Position) :-
    linear_search_helper(Element, List, 0, Position).

linear_search_helper(Element, [Element|_], Position, Position).
linear_search_helper(Element, [_|T], CurrentPos, Position) :-
    NextPos is CurrentPos + 1,
    linear_search_helper(Element, T, NextPos, Position).

% Usage:
% ?- linear_search(c, [a, b, c, d, e], Pos).
% Pos = 2.

% Binary search (requires sorted list)
binary_search(Element, List, Position) :-
    length(List, Length),
    binary_search_helper(Element, List, 0, Length, Position).

binary_search_helper(Element, List, Low, High, Position) :-
    Low < High,
    Mid is (Low + High) // 2,
    nth0(Mid, List, MidElement),
    compare_and_search(Element, MidElement, List, Low, Mid, High, Position).

compare_and_search(Element, Element, _, _, Position, _, Position).
compare_and_search(Element, MidElement, List, Low, Mid, High, Position) :-
    Element @< MidElement,
    binary_search_helper(Element, List, Low, Mid, Position).
compare_and_search(Element, MidElement, List, Low, Mid, High, Position) :-
    Element @> MidElement,
    NewLow is Mid + 1,
    binary_search_helper(Element, List, NewLow, High, Position).
```

### Sorting Algorithms

```prolog
% Quick sort implementation
quicksort([], []).
quicksort([Pivot|T], Sorted) :-
    partition_around_pivot(T, Pivot, Smaller, Larger),
    quicksort(Smaller, SortedSmaller),
    quicksort(Larger, SortedLarger),
    append(SortedSmaller, [Pivot|SortedLarger], Sorted).

partition_around_pivot([], _, [], []).
partition_around_pivot([H|T], Pivot, [H|Smaller], Larger) :-
    H =< Pivot,
    partition_around_pivot(T, Pivot, Smaller, Larger).
partition_around_pivot([H|T], Pivot, Smaller, [H|Larger]) :-
    H > Pivot,
    partition_around_pivot(T, Pivot, Smaller, Larger).

% Usage:
% ?- quicksort([3, 1, 4, 1, 5, 9, 2, 6], Sorted).
% Sorted = [1, 1, 2, 3, 4, 5, 6, 9].

% Merge sort implementation
merge_sort([], []).
merge_sort([X], [X]).
merge_sort(List, Sorted) :-
    length(List, Length),
    Length > 1,
    Half is Length // 2,
    split_at(List, Half, Left, Right),
    merge_sort(Left, SortedLeft),
    merge_sort(Right, SortedRight),
    merge_lists(SortedLeft, SortedRight, Sorted).

split_at(List, 0, [], List).
split_at([H|T], N, [H|Left], Right) :-
    N > 0,
    N1 is N - 1,
    split_at(T, N1, Left, Right).

merge_lists([], List, List).
merge_lists(List, [], List).
merge_lists([H1|T1], [H2|T2], [H1|Merged]) :-
    H1 =< H2,
    merge_lists(T1, [H2|T2], Merged).
merge_lists([H1|T1], [H2|T2], [H2|Merged]) :-
    H1 > H2,
    merge_lists([H1|T1], T2, Merged).
```

### List Utilities

```prolog
% Check if list is sorted
is_sorted([]).
is_sorted([_]).
is_sorted([X, Y|T]) :-
    X =< Y,
    is_sorted([Y|T]).

% Find minimum and maximum
min_list([X], X).
min_list([H|T], Min) :-
    min_list(T, MinT),
    (H =< MinT -> Min = H; Min = MinT).

max_list([X], X).
max_list([H|T], Max) :-
    max_list(T, MaxT),
    (H >= MaxT -> Max = H; Max = MaxT).

% Remove all occurrences of element
remove_all(_, [], []).
remove_all(X, [X|T], Result) :-
    remove_all(X, T, Result).
remove_all(X, [H|T], [H|Result]) :-
    X \= H,
    remove_all(X, T, Result).

% Usage:
% ?- remove_all(a, [a, b, a, c, a, d], Result).
% Result = [b, c, d].
```

---

## Performance Considerations

### Tail Recursion Optimization

```prolog
% Inefficient: Not tail recursive
sum_list_slow([], 0).
sum_list_slow([H|T], Sum) :-
    sum_list_slow(T, TSum),
    Sum is H + TSum.

% Efficient: Tail recursive with accumulator
sum_list_fast(List, Sum) :-
    sum_list_acc(List, 0, Sum).

sum_list_acc([], Acc, Acc).
sum_list_acc([H|T], Acc, Sum) :-
    NewAcc is Acc + H,
    sum_list_acc(T, NewAcc, Sum).

% Efficient reverse using accumulator
reverse_fast(List, Reversed) :-
    reverse_acc(List, [], Reversed).

reverse_acc([], Acc, Acc).
reverse_acc([H|T], Acc, Reversed) :-
    reverse_acc(T, [H|Acc], Reversed).
```

### Memory Efficiency

```prolog
% Memory-efficient list processing
% Use difference lists for efficient appending
diff_append(X-Y, Y-Z, X-Z).

% Convert regular list to difference list
to_diff_list([], X-X).
to_diff_list([H|T], [H|X]-Y) :-
    to_diff_list(T, X-Y).

% Convert difference list to regular list
from_diff_list(X-[], X).

% Efficient concatenation of many lists
concat_many_lists(Lists, Result) :-
    maplist(to_diff_list, Lists, DiffLists),
    concat_diff_lists(DiffLists, Result-[]),
    from_diff_list(Result-[], Result).

concat_diff_lists([], X-X).
concat_diff_lists([DL|DLs], Result) :-
    concat_diff_lists(DLs, Rest),
    diff_append(DL, Rest, Result).
```

---

## Common Use Cases

### Data Processing

```prolog
% Process CSV-like data
process_records([], []).
process_records([Record|Records], [Processed|ProcessedRecords]) :-
    process_record(Record, Processed),
    process_records(Records, ProcessedRecords).

process_record([Name, Age, City], person(Name, AgeNum, City)) :-
    atom_number(Age, AgeNum).

% Usage:
% ?- process_records([[john, '25', london], [mary, '30', paris]], Result).
% Result = [person(john, 25, london), person(mary, 30, paris)].

% Filter and transform data
adults_in_city(Records, City, Adults) :-
    filter(Records, in_city(City), InCity),
    filter(InCity, is_adult, Adults).

in_city(City, person(_, _, City)).
is_adult(person(_, Age, _)) :- Age >= 18.
```

### Mathematical Operations

```prolog
% Statistical functions
mean(List, Mean) :-
    sum_list(List, Sum),
    length(List, Length),
    Mean is Sum / Length.

% Standard deviation
std_deviation(List, StdDev) :-
    mean(List, Mean),
    map_list(List, deviation_squared(Mean), Deviations),
    mean(Deviations, Variance),
    StdDev is sqrt(Variance).

deviation_squared(Mean, X, DevSq) :-
    Dev is X - Mean,
    DevSq is Dev * Dev.

% Range
range(List, Range) :-
    min_list(List, Min),
    max_list(List, Max),
    Range is Max - Min.
```

### Text Processing

```prolog
% Word frequency counting
count_words(Words, Frequencies) :-
    sort(Words, UniqueWords),
    count_occurrences(UniqueWords, Words, Frequencies).

count_occurrences([], _, []).
count_occurrences([Word|Words], AllWords, [Word-Count|Counts]) :-
    count_word(Word, AllWords, Count),
    count_occurrences(Words, AllWords, Counts).

count_word(Word, Words, Count) :-
    filter(Words, =(Word), Occurrences),
    length(Occurrences, Count).

% Usage:
% ?- count_words([the, cat, sat, on, the, mat], Freq).
% Freq = [cat-1, mat-1, on-1, sat-1, the-2].
```

---

## Best Practices

### 1. Use Appropriate Data Structures

```prolog
% Good: Use lists for sequential data
process_sequence([]).
process_sequence([H|T]) :-
    process_item(H),
    process_sequence(T).

% Consider: Use difference lists for frequent appending
efficient_append(L1, L2, Result) :-
    to_diff_list(L1, DL1),
    to_diff_list(L2, DL2),
    diff_append(DL1, DL2, ResultDL),
    from_diff_list(ResultDL, Result).
```

### 2. Prefer Built-in Predicates

```prolog
% Good: Use built-in predicates when available
?- member(X, [1, 2, 3]).
?- append([1, 2], [3, 4], L).
?- length([a, b, c], N).

% Avoid: Reimplementing when not necessary
% (unless for learning purposes)
```

### 3. Handle Edge Cases

```prolog
% Good: Handle empty lists
safe_head([], default_value).
safe_head([H|_], H).

safe_tail([], []).
safe_tail([_|T], T).

% Good: Validate input
positive_sum(List, Sum) :-
    is_list(List),
    maplist(number, List),
    maplist(>(0), List),
    sum_list(List, Sum).
```

### 4. Use Meaningful Names

```prolog
% Good: Descriptive predicate names
remove_duplicates(List, UniqueList) :-
    sort(List, UniqueList).

filter_positive_numbers(Numbers, PositiveNumbers) :-
    filter(Numbers, >(0), PositiveNumbers).

% Good: Clear variable names
process_student_records(StudentRecords, ProcessedRecords) :-
    maplist(validate_student, StudentRecords, ProcessedRecords).
```

### 5. Test Thoroughly

```prolog
% Test edge cases
test_sum_list :-
    sum_list([], 0),                    % Empty list
    sum_list([5], 5),                   % Single element
    sum_list([1, 2, 3, 4], 10),        % Multiple elements
    sum_list([-1, -2, -3], -6).        % Negative numbers

% Test with different data types
test_member :-
    member(a, [a, b, c]),              % Atoms
    member(1, [1, 2, 3]),              % Numbers
    member([x], [[x], [y], [z]]).      % Complex terms
```

---

## Version Information

This guide is current as of **JProlog v2.0.6**. All list operations and built-in predicates described are fully functional and tested.

### List Features in v2.0.6:
- ✅ **ISO-Compliant List Formatting**: [a,b,c] instead of dot notation
- ✅ **Complete Built-in Library**: append/3, member/2, length/2, reverse/2
- ✅ **Sorting Predicates**: sort/2, msort/2 fully operational
- ✅ **List Selection**: nth0/3, nth1/3, select/3 working correctly
- ✅ **Meta-predicates**: findall/3, bagof/3, setof/3 with lists
- ✅ **Pattern Matching**: Full [H|T] pattern support
- ✅ **Performance**: Optimized list operations with tail recursion

### Testing List Operations

```prolog
% Test basic operations
?- append([1,2], [3,4], L).      % L = [1,2,3,4]
?- member(2, [1,2,3]).           % true
?- length([a,b,c,d], N).         % N = 4
?- reverse([1,2,3], R).          % R = [3,2,1]
?- sort([3,1,4,1], S).           % S = [1,3,4]

% Test pattern matching
?- [H|T] = [first, second, third].
% H = first, T = [second, third]
```

---

**JProlog List Operations Guide** - Master list processing and manipulation in Prolog

*Version 2.0.6 | DenzoSOFT | https://denzosoft.it*

*This comprehensive guide covers all list operations available in JProlog v2.0.6. For additional examples and test cases, see the programs in the `examples/` directory.*