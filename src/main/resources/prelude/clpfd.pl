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
%
% ISS-2025-0760..0767 (4.6 wave Q5): global_cardinality/3, circuit/1,
% cumulative/1,2, disjoint2/1, lex_chain/1, chain/2, automaton/3,8, zcompare/3
% and attribute_goals//1 (no comment may go INSIDE the export list: the
% library index reads it textually).
% ---------------------------------------------------------------------------

:- module(clpfd, [sum/3, scalar_product/4, element/3, tuples_in/2,
                  global_cardinality/2, transpose/2,
                  global_cardinality/3, circuit/1, cumulative/1, cumulative/2,
                  disjoint2/1, lex_chain/1, chain/2, automaton/3, automaton/8,
                  zcompare/3, attribute_goals/3]).

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

% ---------------------------------------------------------------------------
% START_CHANGE: ISS-2025-0760..0767 - 4.6 wave Q5.1 / Q5.2
% ---------------------------------------------------------------------------

% attribute_goals//1 (ISS-2025-0760): the residual goals of an FD variable, in
% SWI's printed forms -- what copy_term/3 and the toplevel show.
attribute_goals(X, S0, S) :-
    '$clpfd_attribute_goals'(X, Gs),
    append(Gs, S, S0).

% circuit/1 (ISS-2025-0762) and cumulative/1,2 (ISS-2025-0763): natives.
circuit(Vs) :- '$clpfd_circuit'(Vs).

cumulative(Tasks) :- '$clpfd_cumulative'(Tasks, [limit(1)]).
cumulative(Tasks, Options) :- '$clpfd_cumulative'(Tasks, Options).

% chain(+Zs, +Relation) (ISS-2025-0764): Relation between neighbours.
chain(Zs, Relation) :-
    must_be(list, Zs),
    '$clpfd_chain_relation'(Relation),
    '$clpfd_chain'(Zs, Relation).

'$clpfd_chain_relation'(R) :- var(R), !,
    throw(error(instantiation_error, context(chain/2, _))).
'$clpfd_chain_relation'(R) :- memberchk(R, [#=, #=<, #>=, #<, #>]), !.
'$clpfd_chain_relation'(R) :-
    throw(error(domain_error(chain_relation, R), context(chain/2, _))).

'$clpfd_chain'([], _).
'$clpfd_chain'([X|Xs], R) :- '$clpfd_chain_'(Xs, X, R).

'$clpfd_chain_'([], _, _).
'$clpfd_chain_'([Y|Ys], X, R) :-
    G =.. [R, X, Y], call(G),
    '$clpfd_chain_'(Ys, Y, R).

% lex_chain(+Lss) (ISS-2025-0764): consecutive lists are lexicographically
% non-decreasing. L_i <=> the suffixes from position i are lex-ordered:
% L_i #<==> (A_i #< B_i #\/ (A_i #= B_i #/\ L_i+1)), plus L_i #==> A_i #=< B_i.
lex_chain(Lss) :-
    must_be(list, Lss),
    '$clpfd_lists'(Lss),
    '$clpfd_lex_chain'(Lss).

'$clpfd_lists'([]).
'$clpfd_lists'([L|Ls]) :- must_be(list, L), '$clpfd_lists'(Ls).

'$clpfd_lex_chain'([]).
'$clpfd_lex_chain'([L|Ls]) :- '$clpfd_lex_chain_'(Ls, L).

'$clpfd_lex_chain_'([], _).
'$clpfd_lex_chain_'([L|Ls], Prev) :-
    '$clpfd_lex_le'(Prev, L, 1),
    '$clpfd_lex_chain_'(Ls, L).

'$clpfd_lex_le'([], [], _).
'$clpfd_lex_le'([A], [B], L) :- !,
    L #<==> (A #=< B).
'$clpfd_lex_le'([A|As], [B|Bs], L) :-
    L #==> (A #=< B),
    L #<==> (A #< B #\/ (A #= B #/\ L1)),
    '$clpfd_lex_le'(As, Bs, L1).

% disjoint2(+Rects) (ISS-2025-0764): F(X,W,Y,H) rectangles do not overlap.
disjoint2(Rs) :-
    must_be(list, Rs),
    '$clpfd_disjoint2'(Rs).

'$clpfd_disjoint2'([]).
'$clpfd_disjoint2'([R|Rs]) :-
    '$clpfd_disjoint2_'(Rs, R),
    '$clpfd_disjoint2'(Rs).

'$clpfd_disjoint2_'([], _).
'$clpfd_disjoint2_'([Q|Qs], R) :-
    '$clpfd_rect'(R, X1, W1, Y1, H1),
    '$clpfd_rect'(Q, X2, W2, Y2, H2),
    X1 + W1 #=< X2 #\/ X2 + W2 #=< X1 #\/ Y1 + H1 #=< Y2 #\/ Y2 + H2 #=< Y1,
    '$clpfd_disjoint2_'(Qs, R).

'$clpfd_rect'(R, X, W, Y, H) :-
    (   compound(R), functor(R, _, 4) -> arg(1, R, X), arg(2, R, W), arg(3, R, Y), arg(4, R, H)
    ;   var(R) -> throw(error(instantiation_error, context(disjoint2/1, _)))
    ;   throw(error(type_error(rectangle, R), context(disjoint2/1, _)))
    ).

% automaton/3,8 (ISS-2025-0765): SWI semantics. The signature is the sequence
% the automaton reads; nodes are arbitrary terms (numbered here); a counter
% transition arc(N0, L, N1, Exprs) updates the counters, where the Counters
% variables stand for the previous values and the Template variables for the
% current element of the Sequence.
automaton(Sigs, Ns, As) :-
    automaton(_, _, Sigs, Ns, As, [], [], _).

automaton(Seqs, Template, Sigs, Ns, As, Cs, Is, Fs) :-
    must_be(list, Sigs), must_be(list, Ns), must_be(list, As),
    must_be(list, Cs), must_be(list, Is),
    (   var(Seqs) -> Seqs = Sigs ; must_be(list, Seqs) ),
    '$clpfd_aut_nodes'(Ns, As, [], Nodes),
    '$clpfd_aut_kind'(Ns, source, Nodes, Sources),
    '$clpfd_aut_kind'(Ns, sink, Nodes, Sinks),
    '$clpfd_aut_arcs'(As, 1, Nodes, Cs, Rows, Updates),
    length(Sigs, N),
    N1 is N + 1,
    length(States, N1),
    States = [S0|_],
    last(States, Sn),
    tuples_in([[S0]], Sources),
    tuples_in([[Sn]], Sinks),
    length(Cs, NC),
    length(Is, NC),
    '$clpfd_aut_steps'(Sigs, Seqs, States, Template, Cs, Is, Final, Rows, Updates),
    Fs = Final.

'$clpfd_aut_nodes'([], As, Acc, Nodes) :- '$clpfd_aut_arc_nodes'(As, Acc, Nodes).
'$clpfd_aut_nodes'([N|Ns], As, Acc, Nodes) :-
    (   N = source(X) -> true
    ;   N = sink(X) -> true
    ;   throw(error(domain_error(automaton_node, N), context(automaton/8, _)))
    ),
    '$clpfd_aut_add'(X, Acc, Acc1),
    '$clpfd_aut_nodes'(Ns, As, Acc1, Nodes).

'$clpfd_aut_arc_nodes'([], Acc, Acc).
'$clpfd_aut_arc_nodes'([A|As], Acc, Nodes) :-
    (   A = arc(X, _, Y) -> true
    ;   A = arc(X, _, Y, _) -> true
    ;   throw(error(domain_error(automaton_arc, A), context(automaton/8, _)))
    ),
    '$clpfd_aut_add'(X, Acc, Acc1),
    '$clpfd_aut_add'(Y, Acc1, Acc2),
    '$clpfd_aut_arc_nodes'(As, Acc2, Nodes).

'$clpfd_aut_add'(X, Acc, Acc) :- '$clpfd_aut_num'(X, Acc, _), !.
'$clpfd_aut_add'(X, Acc, Acc1) :- append(Acc, [X], Acc1).

'$clpfd_aut_num'(X, Nodes, I) :- '$clpfd_aut_num'(Nodes, X, 1, I).
'$clpfd_aut_num'([Y|Ys], X, I0, I) :-
    (   Y == X -> I = I0 ; I1 is I0 + 1, '$clpfd_aut_num'(Ys, X, I1, I) ).

'$clpfd_aut_kind'([], _, _, []).
'$clpfd_aut_kind'([N|Ns], Kind, Nodes, Out) :-
    (   functor(N, Kind, 1) -> arg(1, N, X), '$clpfd_aut_num'(X, Nodes, I), Out = [[I]|Out1]
    ;   Out = Out1
    ),
    '$clpfd_aut_kind'(Ns, Kind, Nodes, Out1).

% Rows: [From, Label, Arc, To]; Updates: Arc-Exprs (Exprs = none: unchanged)
'$clpfd_aut_arcs'([], _, _, _, [], []).
'$clpfd_aut_arcs'([A|As], K, Nodes, Cs, [[F, L, K, T]|Rows], [K-E|Us]) :-
    (   A = arc(X, L, Y) -> E = none
    ;   A = arc(X, L, Y, E0),
        must_be(list, E0),
        (   length(Cs, NC), length(E0, NC) -> E = E0
        ;   throw(error(domain_error(automaton_counter_expressions, A), context(automaton/8, _)))
        )
    ),
    '$clpfd_aut_num'(X, Nodes, F),
    '$clpfd_aut_num'(Y, Nodes, T),
    K1 is K + 1,
    '$clpfd_aut_arcs'(As, K1, Nodes, Cs, Rows, Us).

'$clpfd_aut_steps'([], _, [_], _, _, Cur, Cur, _, _).
'$clpfd_aut_steps'([Sig|Sigs], Seqs, [S, S1|States], Template, Cs, Cur, Final, Rows, Updates) :-
    (   Seqs = [Seq|Seqs1] -> true ; Seq = Sig, Seqs1 = [] ),
    (   Cs == [] ->
        '$clpfd_aut_rows3'(Rows, Rows3),
        tuples_in([[S, Sig, S1]], Rows3),
        Next = Cur
    ;   tuples_in([[S, Sig, Arc, S1]], Rows),
        length(Cs, NC), length(Next, NC),
        '$clpfd_aut_updates'(Updates, Arc, Seq, Template, Cs, Cur, Next)
    ),
    '$clpfd_aut_steps'(Sigs, Seqs1, [S1|States], Template, Cs, Next, Final, Rows, Updates).

'$clpfd_aut_rows3'([], []).
'$clpfd_aut_rows3'([[F, L, _, T]|Rs], [[F, L, T]|Ts]) :- '$clpfd_aut_rows3'(Rs, Ts).

'$clpfd_aut_updates'([], _, _, _, _, _, _).
'$clpfd_aut_updates'([K-E|Us], Arc, Seq, Template, Cs, Cur, Next) :-
    (   E == none -> copy_term(Cs, Cs1), Es = Cs1
    ;   copy_term(Template-Cs-E, T1-Cs1-Es), T1 = Seq
    ),
    Cs1 = Cur,
    '$clpfd_aut_update'(Es, Next, Arc, K),
    '$clpfd_aut_updates'(Us, Arc, Seq, Template, Cs, Cur, Next).

'$clpfd_aut_update'([], [], _, _).
'$clpfd_aut_update'([E|Es], [N|Ns], Arc, K) :-
    (Arc #= K) #==> (N #= E),
    '$clpfd_aut_update'(Es, Ns, Arc, K).

% zcompare(?Order, ?A, ?B) (ISS-2025-0766): Order is the standard order of the
% integers A and B, reified: it is bound as soon as the domains decide it, and
% binding it posts the corresponding constraint.
zcompare(Order, A, B) :-
    nonvar(Order), !,
    '$clpfd_zcompare'(Order, A, B).
zcompare(Order, A, B) :-
    integer(A), integer(B), !,
    compare(Order, A, B).
zcompare(Order, A, B) :-
    L #<==> (A #< B),
    E #<==> (A #= B),
    G #<==> (A #> B),
    L + E + G #= 1,
    '$clpfd_zc_watch'(L, <, Order),
    '$clpfd_zc_watch'(E, =, Order),
    '$clpfd_zc_watch'(G, >, Order),
    freeze(Order, clpfd:'$clpfd_zcompare'(Order, A, B)).

'$clpfd_zc_watch'(B, Sym, Order) :-
    (   B == 1 -> Order = Sym
    ;   B == 0 -> true
    ;   freeze(B, (B =:= 1 -> Order = Sym ; true))
    ).

'$clpfd_zcompare'(O, A, B) :-
    (   O == (<) -> A #< B
    ;   O == (=) -> A #= B
    ;   O == (>) -> A #> B
    ;   throw(error(domain_error(order, O), context(zcompare/3, _)))
    ).

% global_cardinality(+Vs, +Pairs, +Options) (ISS-2025-0767): consistency(value)
% (the propagation global_cardinality/2 always does) and cost(Cost, Matrix):
% Matrix has a row per variable and a column per key; Cost is the sum of the
% entries the assignment selects.
global_cardinality(Vs, Pairs, Options) :-
    must_be(list, Options),
    global_cardinality(Vs, Pairs),
    '$clpfd_gcc_options'(Options, Vs, Pairs).

'$clpfd_gcc_options'([], _, _).
'$clpfd_gcc_options'([O|Os], Vs, Pairs) :-
    (   var(O) -> throw(error(instantiation_error, context(global_cardinality/3, _)))
    ;   O = consistency(value) -> true
    ;   O = cost(Cost, Matrix) ->
        must_be(list, Matrix),
        '$clpfd_gcc_keys'(Pairs, Keys),
        '$clpfd_gcc_cost'(Vs, Matrix, Keys, Costs),
        sum(Costs, #=, Cost)
    ;   throw(error(domain_error(global_cardinality_option, O), context(global_cardinality/3, _)))
    ),
    '$clpfd_gcc_options'(Os, Vs, Pairs).

'$clpfd_gcc_keys'([], []).
'$clpfd_gcc_keys'([K-_|Ps], [K|Ks]) :- '$clpfd_gcc_keys'(Ps, Ks).

'$clpfd_gcc_cost'([], [], _, []).
'$clpfd_gcc_cost'([V|Vs], [Row|Rows], Keys, [C|Cs]) :-
    element(I, Keys, V),
    element(I, Row, C),
    '$clpfd_gcc_cost'(Vs, Rows, Keys, Cs).
% END_CHANGE: ISS-2025-0760..0767
