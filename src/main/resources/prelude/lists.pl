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
                  subtract/3, intersection/3, union/3, sort/4]).

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).
