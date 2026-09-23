% ---------------------------------------------------------------------------
% JProlog engine v4 prelude — library(clpfd), the global predicates
%
% ISS-2025-0646 / ISS-2025-0650 (4.5 wave P5): sum/3, scalar_product/4,
% element/3, tuples_in/2, global_cardinality/2 and transpose/2. The constraint
% work is done by the internal v4 natives '$clpfd_*' (core.engine.v4.ClpfdNative
% over builtin.clpfd.v2); the public names live HERE, in a library module,
% because a library module is the last step of the resolution order: a user
% program that defines its own sum/3 or element/3 keeps its own definition.
% The operator-named predicates (in/2, ins/2, #=/2, #<==>/2, ...) and the
% labeling/domain predicates are natives.
% ---------------------------------------------------------------------------

:- module(clpfd, [sum/3, scalar_product/4, element/3, tuples_in/2,
                  global_cardinality/2, transpose/2]).

sum(Vs, Op, Expr) :- '$clpfd_sum'(Vs, Op, Expr).

scalar_product(Cs, Vs, Op, Expr) :- '$clpfd_scalar_product'(Cs, Vs, Op, Expr).

element(I, Xs, V) :- '$clpfd_element'(I, Xs, V).

tuples_in(Tuples, Relation) :- '$clpfd_tuples_in'(Tuples, Relation).

global_cardinality(Vs, Pairs) :- '$clpfd_gcc'(Vs, Pairs).

% transpose(+Matrix, ?Transposed): SWI library(clpfd) transpose/2 for a list of
% equal-length lists (the sudoku idiom).
transpose([], []).
transpose([F|Fs], Ts) :- '$clpfd_transpose'(F, [F|Fs], Ts).

'$clpfd_transpose'([], _, []).
'$clpfd_transpose'([_|Rs], Ms, [Ts|Tss]) :-
    '$clpfd_firsts_rests'(Ms, Ts, Ms1),
    '$clpfd_transpose'(Rs, Ms1, Tss).

'$clpfd_firsts_rests'([], [], []).
'$clpfd_firsts_rests'([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    '$clpfd_firsts_rests'(Rest, Fs, Oss).
