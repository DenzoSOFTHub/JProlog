% ---------------------------------------------------------------------------
% JProlog engine v4 prelude — library(ordsets)
%
% ISS-2025-0790 (wave Q7): ordered sets — lists sorted by the standard order of
% terms without duplicates — with SWI-Prolog 9's library(ordsets) definitions
% (the merge-by-compare/3 formulation, deterministic through first-argument
% indexing). ord_insert/3 is the DEC-10/YAP name of ord_add_element/3.
% Autoloaded on first reference, like every prelude module.
% ---------------------------------------------------------------------------

:- module(ordsets, [is_ordset/1, ord_empty/1, list_to_ord_set/2,
                    ord_memberchk/2, ord_add_element/3, ord_insert/3,
                    ord_del_element/3, ord_selectchk/3,
                    ord_union/2, ord_union/3, ord_subtract/3,
                    ord_intersection/2, ord_intersection/3,
                    ord_intersect/2, ord_disjoint/2, ord_subset/2,
                    ord_seteq/2, ord_symdiff/3]).

is_ordset(Term) :-
    is_list(Term),
    '$is_ordset2'(Term).

'$is_ordset2'([]).
'$is_ordset2'([H|T]) :-
    '$is_ordset3'(T, H).

'$is_ordset3'([], _).
'$is_ordset3'([H2|T], H) :-
    H2 @> H,
    '$is_ordset3'(T, H2).

ord_empty([]).

list_to_ord_set(List, Set) :-
    sort(List, Set).

ord_memberchk(Item, [X|Xs]) :-
    compare(Order, Item, X),
    '$ord_memberchk'(Order, Item, Xs).

'$ord_memberchk'(=, _, _).
'$ord_memberchk'(>, Item, Xs) :-
    ord_memberchk(Item, Xs).

ord_add_element(Set1, Element, Set2) :-
    ord_union(Set1, [Element], Set2).

ord_insert(Set1, Element, Set2) :-
    ord_union(Set1, [Element], Set2).

ord_del_element(Set, Element, NewSet) :-
    ord_subtract(Set, [Element], NewSet).

ord_selectchk(Item, Set, Rest) :-
    ord_memberchk(Item, Set),
    ord_subtract(Set, [Item], Rest).

ord_union(ListOfSets, Set) :-
    append(ListOfSets, List),
    sort(List, Set).

ord_union([], Union, Union).
ord_union([H1|T1], L2, Union) :-
    '$union2'(L2, H1, T1, Union).

'$union2'([], H1, T1, [H1|T1]).
'$union2'([H2|T2], H1, T1, Union) :-
    compare(Order, H1, H2),
    '$union3'(Order, H1, T1, H2, T2, Union).

'$union3'(<, H1, T1, H2, T2, [H1|Union]) :-
    '$union2'(T1, H2, T2, Union).
'$union3'(=, H1, T1, _, T2, [H1|Union]) :-
    ord_union(T1, T2, Union).
'$union3'(>, H1, T1, H2, T2, [H2|Union]) :-
    '$union2'(T2, H1, T1, Union).

ord_subtract([], _, []).
ord_subtract([H1|T1], L2, Diff) :-
    '$diff21'(L2, H1, T1, Diff).

'$diff21'([], H1, T1, [H1|T1]).
'$diff21'([H2|T2], H1, T1, Diff) :-
    compare(Order, H1, H2),
    '$diff3'(Order, H1, T1, H2, T2, Diff).

'$diff12'([], _, _, []).
'$diff12'([H1|T1], H2, T2, Diff) :-
    compare(Order, H1, H2),
    '$diff3'(Order, H1, T1, H2, T2, Diff).

'$diff3'(<, H1, T1, H2, T2, [H1|Diff]) :-
    '$diff12'(T1, H2, T2, Diff).
'$diff3'(=, _, T1, _, T2, Diff) :-
    ord_subtract(T1, T2, Diff).
'$diff3'(>, H1, T1, _, T2, Diff) :-
    '$diff21'(T2, H1, T1, Diff).

ord_intersection([], []).
ord_intersection([S|Ss], Int) :-
    '$ord_isect_all'(Ss, S, Int).

'$ord_isect_all'([], Int, Int).
'$ord_isect_all'([S|Ss], Int0, Int) :-
    ord_intersection(Int0, S, Int1),
    '$ord_isect_all'(Ss, Int1, Int).

ord_intersection([], _, []).
ord_intersection([H1|T1], L2, Int) :-
    '$isect2'(L2, H1, T1, Int).

'$isect2'([], _, _, []).
'$isect2'([H2|T2], H1, T1, Int) :-
    compare(Order, H1, H2),
    '$isect3'(Order, H1, T1, H2, T2, Int).

'$isect3'(<, _, T1, H2, T2, Int) :-
    '$isect2'(T1, H2, T2, Int).
'$isect3'(=, H1, T1, _, T2, [H1|Int]) :-
    ord_intersection(T1, T2, Int).
'$isect3'(>, H1, T1, _, T2, Int) :-
    '$isect2'(T2, H1, T1, Int).

ord_intersect([H1|T1], L2) :-
    '$ord_intersect2'(L2, H1, T1).

'$ord_intersect2'([H2|T2], H1, T1) :-
    compare(Order, H1, H2),
    '$ord_intersect3'(Order, H1, T1, H2, T2).

'$ord_intersect3'(<, _, T1, H2, T2) :-
    '$ord_intersect2'(T1, H2, T2).
'$ord_intersect3'(=, _, _, _, _).
'$ord_intersect3'(>, H1, T1, _, T2) :-
    '$ord_intersect2'(T2, H1, T1).

ord_disjoint(Set1, Set2) :-
    \+ ord_intersect(Set1, Set2).

ord_subset([], _).
ord_subset([H1|T1], [H2|T2]) :-
    compare(Order, H1, H2),
    '$ord_subset_'(Order, H1, T1, T2).

'$ord_subset_'(>, H1, T1, [H2|T2]) :-
    compare(Order, H1, H2),
    '$ord_subset_'(Order, H1, T1, T2).
'$ord_subset_'(=, _, T1, T2) :-
    ord_subset(T1, T2).

ord_seteq(Set1, Set2) :-
    Set1 == Set2.

ord_symdiff(Set1, Set2, Difference) :-
    ord_subtract(Set1, Set2, D1),
    ord_subtract(Set2, Set1, D2),
    ord_union(D1, D2, Difference).
