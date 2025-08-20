% ===================================================================
% TEST 08: Term Manipulation
% ===================================================================
% Tests: functor/3, arg/3, =../2, copy_term/2, term comparison

% Term construction and deconstruction
build_term(Functor, Args, Term) :-
    length(Args, Arity),
    functor(Term, Functor, Arity),
    fill_args(Term, Args, 1).

fill_args(_, [], _).
fill_args(Term, [Arg|Rest], N) :-
    arg(N, Term, Arg),
    N1 is N + 1,
    fill_args(Term, Rest, N1).

% Term analysis
analyze_term(Term, analysis(Functor, Arity, Args)) :-
    compound(Term), !,
    functor(Term, Functor, Arity),
    Term =.. [_|Args].
analyze_term(Term, analysis(Term, 0, [])) :-
    atomic(Term).

% Term transformation using =..
transform_term(Old, New) :-
    Old =.. [Functor|Args],
    append(Args, [transformed], NewArgs),
    New =.. [Functor|NewArgs].

% Term copying
safe_copy(Original, Copy) :-
    copy_term(Original, Copy).

% Term comparison
compare_terms(X, Y, equal) :- X == Y, !.
compare_terms(X, Y, equivalent) :- X = Y, !.
compare_terms(X, Y, before) :- X @< Y, !.
compare_terms(X, Y, after) :- X @> Y, !.
compare_terms(_, _, different).

% Complex term manipulation
swap_args(Term, SwappedTerm) :-
    compound(Term),
    functor(Term, F, 2), !,
    arg(1, Term, Arg1),
    arg(2, Term, Arg2),
    functor(SwappedTerm, F, 2),
    arg(1, SwappedTerm, Arg2),
    arg(2, SwappedTerm, Arg1).

% Test queries:
% ?- build_term(person, [john, 25], Term).
% ?- analyze_term(person(john, 25), Analysis).
% ?- transform_term(likes(mary, pizza), New).
% ?- safe_copy(f(X, X), Copy).
% ?- compare_terms(f(a), f(b), Result).
% ?- swap_args(loves(john, mary), Swapped).