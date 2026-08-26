% ---------------------------------------------------------------------------
% JProlog engine v4 prelude — coroutining (library(freeze), library(dif),
% library(when))
%
% ISS-2025-0459 (design B.9): freeze/2, frozen/2, when/2 and dif/2 are Prolog
% clauses on top of put_attr/3, get_attr/3 and the attr_unify_hook protocol,
% not Java built-ins. Binding an attributed variable pushes a wake goal onto
% the machine's queue; the drive loop runs it before the next goal, in the
% current binding context, so the bindings a woken goal makes are ordinary
% bindings that propagate (ISS-2025-0336 on the v2/legacy engines: only the
% side effects of a when/2-woken goal survived).
%
% The Java built-ins builtin.control.{Freeze,When,Dif} stay registered for the
% legacy and v2 engines; on v4 the clauses below win, because the machine
% consults the library layer before the legacy registry.
%
% Loaded into the v4 ClauseStore's LIBRARY layer, so a user definition of
% freeze/2 (or of any '$'-prefixed helper) replaces it, the KnowledgeBase is
% untouched, and the legacy/v2 engines never see any of this.
% ---------------------------------------------------------------------------

% ISS-2025-0468: wave W6 turned the prelude into autoloadable library modules;
% these clauses now live in the module `coroutining`, loaded on the first
% reference to freeze/2, dif/2, when/2, frozen/2, ?=/2 or the machine's
% '$attr_hook'/4 dispatcher. The '$'-prefixed helpers below are NOT exported,
% so they are private to this module.
% ---------------------------------------------------------------------------

:- module(coroutining, [freeze/2, frozen/2, dif/2, when/2, (?=)/2,
                        '$attr_hook'/4]).

% ---------------------------------------------------------------- dispatcher
% '$attr_hook'(+Module, +AttValue, +Other, +VarName) is called by the machine's
% wake queue for every attribute of a variable that has just been bound, unless
% the user defined Module:attr_unify_hook/2 (which then wins). The last clause
% makes an attribute of an unknown module inert data rather than an error —
% the behaviour the legacy engines had for a module they did not recognise.

'$attr_hook'(freeze, Goal, Other, _) :- !,
    (   var(Other)
    ->  (   get_attr(Other, freeze, Pending)
        ->  put_attr(Other, freeze, (Pending, Goal))
        ;   put_attr(Other, freeze, Goal)
        )
    ;   call(Goal)
    ).
'$attr_hook'(dif, Constraints, _, _) :- !,
    '$dif_check'(Constraints).
'$attr_hook'(when, Suspensions, _, _) :- !,
    '$when_wake'(Suspensions).
% ISS-2025-0486 (wave W9): clpfd no longer routes through this dispatcher. The machine wakes
% '$clpfd_unify_hook'('$attvar_cell'(Cell), Other) directly, because the Java store needs the CELL
% and this Prolog clause could only carry a name.
'$attr_hook'(_, _, _, _).

% -------------------------------------------------------------------- freeze
freeze(X, Goal) :-
    (   var(X)
    ->  (   get_attr(X, freeze, Pending)
        ->  put_attr(X, freeze, (Pending, Goal))
        ;   put_attr(X, freeze, Goal)
        )
    ;   call(Goal)
    ).

frozen(X, Goal) :-
    (   var(X),
        get_attr(X, freeze, Pending)
    ->  Goal = Pending
    ;   Goal = true
    ).

% ----------------------------------------------------------------------- dif
% dif(X, Y) succeeds when X and Y cannot be made identical. Ground and
% non-unifiable pairs are decided at once; anything else suspends on the
% variables of the REMAINING unifier (unifiable/3), so a partially
% instantiated pair re-suspends on exactly the variables that could still make
% it identical.
dif(X, Y) :-
    X \== Y,
    (   unifiable(X, Y, Unifier)
    ->  '$dif_suspend'(Unifier, X, Y)
    ;   true
    ).

'$dif_suspend'([], _, _).
'$dif_suspend'([Binding|Rest], X, Y) :-
    '$dif_attach'(Binding, X, Y),
    '$dif_suspend'(Rest, X, Y).

'$dif_attach'(V = _, X, Y) :-
    (   var(V)
    ->  (   get_attr(V, dif, Constraints)
        ->  put_attr(V, dif, ['$dif'(X, Y)|Constraints])
        ;   put_attr(V, dif, ['$dif'(X, Y)])
        )
    ;   true
    ).

'$dif_check'([]).
'$dif_check'(['$dif'(X, Y)|Rest]) :-
    dif(X, Y),
    '$dif_check'(Rest).

% ---------------------------------------------------------------------- when
% when(+Condition, :Goal). Conditions: nonvar/1, ground/1, ?=/2, (C1, C2),
% (C1 ; C2). The suspension carries a shared Fired flag, so a disjunctive
% condition attached to several variables still runs Goal exactly once (the
% flag is an ordinary binding, so backtracking re-arms it).
when(Condition, Goal) :-
    (   var(Condition)
    ->  throw(error(instantiation_error, when/2))
    ;   '$when_valid'(Condition)
    ->  (   '$when_ready'(Condition)
        ->  call(Goal)
        ;   term_variables(Condition, Vars),
            '$when_attach'(Vars, '$when'(_Fired, Condition, Goal))
        )
    ;   throw(error(domain_error(when_condition, Condition), when/2))
    ).

'$when_valid'(C) :- var(C), !, fail.
'$when_valid'(nonvar(_)) :- !.
'$when_valid'(ground(_)) :- !.
'$when_valid'(?=(_, _)) :- !.
'$when_valid'((A, B)) :- !, '$when_valid'(A), '$when_valid'(B).
'$when_valid'((A ; B)) :- !, '$when_valid'(A), '$when_valid'(B).

'$when_ready'(nonvar(X)) :- nonvar(X).
'$when_ready'(ground(X)) :- ground(X).
'$when_ready'(?=(X, Y)) :- ?=(X, Y).
'$when_ready'((A, B)) :- '$when_ready'(A), '$when_ready'(B).
'$when_ready'((A ; B)) :- ( '$when_ready'(A) -> true ; '$when_ready'(B) ).

'$when_attach'([], _).
'$when_attach'([V|Vs], Suspension) :-
    (   get_attr(V, when, Suspensions)
    ->  put_attr(V, when, [Suspension|Suspensions])
    ;   put_attr(V, when, [Suspension])
    ),
    '$when_attach'(Vs, Suspension).

'$when_wake'([]).
'$when_wake'([Suspension|Rest]) :-
    '$when_fire'(Suspension),
    '$when_wake'(Rest).

'$when_fire'('$when'(Fired, Condition, Goal)) :-
    (   nonvar(Fired)
    ->  true
    ;   '$when_ready'(Condition)
    ->  Fired = fired,
        call(Goal)
    ;   term_variables(Condition, Vars),
        '$when_attach'(Vars, '$when'(Fired, Condition, Goal))
    ).

% ?=(X, Y) is true when X and Y are identical or cannot unify — i.e. when their
% (dis)equality is already decided.
?=(X, Y) :-
    (   X == Y
    ->  true
    ;   X \= Y
    ).
