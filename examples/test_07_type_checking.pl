% ===================================================================
% TEST 07: Type Checking Predicates
% ===================================================================
% Tests: var/1, nonvar/1, atom/1, number/1, compound/1, etc.

% Type checking predicates
check_type(X, variable) :- var(X), !.
check_type(X, atom) :- atom(X), !.
check_type(X, number) :- number(X), !.
check_type(X, integer) :- integer(X), !.
check_type(X, float) :- float(X), !.
check_type(X, compound) :- compound(X), !.
check_type(X, list) :- is_list(X), !.
check_type(X, atomic) :- atomic(X), !.
check_type(_, unknown).

% Safe operations based on types
safe_add(X, Y, Result) :-
    number(X),
    number(Y),
    Result is X + Y.

safe_concat(X, Y, Result) :-
    atom(X),
    atom(Y),
    atom_concat(X, Y, Result).

% Generic processing based on type
process_data(X, processed(atom, X)) :- atom(X), !.
process_data(X, processed(number, X)) :- number(X), !.
process_data(X, processed(list, Length)) :- 
    is_list(X), !, 
    length(X, Length).
process_data(X, processed(compound, functor_info(Functor, Arity))) :- 
    compound(X), !,
    functor(X, Functor, Arity).
process_data(X, processed(unknown, X)).

% Instantiation checking
must_be_instantiated(X) :-
    nonvar(X).

must_be_variable(X) :-
    var(X).

% Ground term checking
all_ground([]).
all_ground([H|T]) :-
    ground(H),
    all_ground(T).

% Test queries:
% ?- check_type(hello, Type).
% ?- check_type(42, Type).
% ?- check_type([a, b, c], Type).
% ?- check_type(f(a, b), Type).
% ?- safe_add(3, 4, R).
% ?- safe_concat(hello, world, R).
% ?- process_data([1, 2, 3], Result).
% ?- all_ground([a, 1, f(b)]).