% ---------------------------------------------------------------------------
% JProlog engine v4 prelude — library(lists)
%
% ISS-2025-0468 (design B.10, decision 4): the list predicates whose Java
% versions were redundant are Prolog clauses. On the v4 machine they are then
% lazy, traceable through the four ports, cancellable by the inference budget
% and the Stop interrupt, they cost no Java stack whatever the list length, and
% — the point of this wave — they have the REAL relational modes the eager Java
% built-ins could not offer:
%
%   append(X, Y, Z)   with all three open enumerates instead of giving one answer
%   member(X, L)      with L partial extends L instead of failing at the tail
%
% Only member/2 and append/3 are clauses: they are the two whose relational
% modes the Java versions could not express, and the two-clause definitions give
% them for free. Every other export below (select/3, nth0/3, nth1/3, last/2,
% reverse/2, memberchk/2, length/2, msort/2, ...) is a v4 native or a registry
% built-in and is listed here only so that `lists:last(L, X)` resolves: on a
% 1 000 000-element list those generators are 2x to 15x faster than the same
% definition in Prolog, because they walk the spine in one Java loop and push
% ONE choice point instead of one per element (report-engine-v4-progress §12.4).
%
% Loaded into the v4 module `lists` on the first reference to one of its
% predicates (autoload by indicator). The KnowledgeBase is untouched, so
% listing/1, the IDE and the legacy/v2 engines never see any of this, and a
% user definition of the same indicator simply wins.
% ---------------------------------------------------------------------------

:- module(lists, [member/2, memberchk/2, append/3, select/3, selectchk/3,
                  nth0/3, nth1/3, last/2, reverse/2,
                  length/2, msort/2, sort/2, sum_list/2, sumlist/2, numlist/3,
                  permutation/2, max_list/2, min_list/2,
                  subtract/3, intersection/3, union/3, sort/4,
                  append/2, nextto/3, max_member/2, min_member/2,
                  max_member/3, min_member/3, list_to_set/2, proper_length/2]).

:- meta_predicate(max_member(2, -, +)).
:- meta_predicate(min_member(2, -, +)).

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

% ISS-2025-0548 (wave P2.10): the split mode of the native append/3 — List1 not
% a proper list, List3 a proper list — runs these two clauses. The native used
% to build a fresh n-element prefix for every split, so append(_, [Last], L)
% was quadratic (1e4: 8 s). Here each split costs O(1): the prefix is shared
% through the bindings the recursion leaves behind. The FIRST argument is
% List3, so first-argument indexing makes the last split deterministic (at
% List3 = [] only the first clause is a candidate), as the native was.
'$append_split'(L, [], L).
'$append_split'([H|R], [H|T], L) :- '$append_split'(R, T, L).

% ISS-2025-0603 (wave P4.10): permutation/2, intersection/3, union/3 and
% subtract/3 are SWI-Prolog's library(lists) clauses. The registry versions
% enumerated permutations in a different order ([3,2,1] before [3,1,2]),
% computed all n! answers eagerly, and removed duplicates from intersection/3
% and union/3 (SWI keeps them: intersection([1,1,2],[1,2],X) gives [1,1,2]).
permutation(Xs, Ys) :-
    (   is_list(Xs) -> length(Xs, N), length(Ys, N)
    ;   is_list(Ys) -> length(Ys, N), length(Xs, N)
    ;   length(Xs, N), length(Ys, N)
    ),
    '$perm'(Xs, Ys).

'$perm'([], []).
'$perm'(List, [First|Perm]) :-
    select(First, List, Rest),
    '$perm'(Rest, Perm).

intersection([], _, []) :- !.
intersection([X|T], L, Intersect) :-
    (   memberchk(X, L)
    ->  Intersect = [X|R],
        intersection(T, L, R)
    ;   intersection(T, L, Intersect)
    ).

union([], L, L) :- !.
union([H|T], L, R) :-
    memberchk(H, L),
    !,
    union(T, L, R).
union([H|T], L, [H|R]) :-
    union(T, L, R).

subtract([], _, []) :- !.
subtract([E|T], D, R) :-
    memberchk(E, D),
    !,
    subtract(T, D, R).
subtract([H|T], D, [H|R]) :-
    subtract(T, D, R).

% ISS-2025-0790 (wave Q7): the rest of SWI-Prolog 9's library(lists) that JProlog lacked,
% as SWI defines them (library/lists.pl): append/2, nextto/3, max_member/2,3,
% min_member/2,3 (standard order of terms, or the given order predicate),
% list_to_set/2 (first occurrences kept, compared with ==) and proper_length/2.
append(ListOfLists, List) :-
    must_be(list, ListOfLists),
    '$append_'(ListOfLists, List).

'$append_'([], []).
'$append_'([L|Ls], As) :-
    append(L, Ws, As),
    '$append_'(Ls, Ws).

nextto(X, Y, [X,Y|_]).
nextto(X, Y, [_|Zs]) :-
    nextto(X, Y, Zs).

max_member(Max, [H|T]) :-
    '$max_member_'(T, H, Max).

'$max_member_'([], Max0, Max) :-
    Max = Max0.
'$max_member_'([H|T], Max0, Max) :-
    (   H @=< Max0
    ->  Max1 = Max0
    ;   Max1 = H
    ),
    '$max_member_'(T, Max1, Max).

min_member(Min, [H|T]) :-
    '$min_member_'(T, H, Min).

'$min_member_'([], Min0, Min) :-
    Min = Min0.
'$min_member_'([H|T], Min0, Min) :-
    (   H @>= Min0
    ->  Min1 = Min0
    ;   Min1 = H
    ),
    '$min_member_'(T, Min1, Min).

max_member(Pred, Max, [H|T]) :-
    '$max_member_p'(T, Pred, H, Max).

'$max_member_p'([], _, Max, Max).
'$max_member_p'([H|T], Pred, Max0, Max) :-
    (   call(Pred, H, Max0)
    ->  '$max_member_p'(T, Pred, Max0, Max)
    ;   '$max_member_p'(T, Pred, H, Max)
    ).

min_member(Pred, Min, [H|T]) :-
    '$min_member_p'(T, Pred, H, Min).

'$min_member_p'([], _, Min, Min).
'$min_member_p'([H|T], Pred, Min0, Min) :-
    (   call(Pred, Min0, H)
    ->  '$min_member_p'(T, Pred, Min0, Min)
    ;   '$min_member_p'(T, Pred, H, Min)
    ).

list_to_set(List, Set) :-
    must_be(list, List),
    '$lts_number'(List, 1, Numbered),
    sort(1, @=<, Numbered, ONum),
    '$lts_remove_dup_keys'(ONum, NumSet),
    sort(2, @=<, NumSet, ONumSet),
    '$lts_keys'(ONumSet, Set).

'$lts_number'([], _, []).
'$lts_number'([H|T0], N, [H-N|T]) :-
    N1 is N+1,
    '$lts_number'(T0, N1, T).

'$lts_remove_dup_keys'([], []).
'$lts_remove_dup_keys'([H|T0], [H|T]) :-
    H = V-_,
    '$lts_remove_same_key'(T0, V, T1),
    '$lts_remove_dup_keys'(T1, T).

'$lts_remove_same_key'([V1-_|T0], V, T) :-
    V1 == V,
    !,
    '$lts_remove_same_key'(T0, V, T).
'$lts_remove_same_key'(L, _, L).

'$lts_keys'([], []).
'$lts_keys'([K-_|T], [K|Ks]) :-
    '$lts_keys'(T, Ks).

proper_length(List, Length) :-
    is_list(List),
    length(List, Length).
