% ===================================================================
% TEST 35: Attributed Variables and Constraint Handling
% ===================================================================
% Tests: put_atts/2, get_atts/2, attributed variable unification

% Domain constraint attribute
domain_constraint(Var, Min, Max) :-
    put_atts(Var, domain(Min, Max)).

% Type constraint attribute
type_constraint(Var, Type) :-
    put_atts(Var, type(Type)).

% Constraint propagation on unification
attr_unify_hook(domain(Min, Max), Value) :-
    number(Value),
    Value >= Min,
    Value =< Max.

attr_unify_hook(type(integer), Value) :-
    integer(Value).

attr_unify_hook(type(atom), Value) :-
    atom(Value).

% Multiple constraints on single variable
multi_constraint(Var, Type, Min, Max) :-
    put_atts(Var, [type(Type), domain(Min, Max)]).

% Constraint checking
check_constraints(Var) :-
    get_atts(Var, Attributes),
    validate_attributes(Attributes, Var).

validate_attributes([], _).
validate_attributes([Attr|Attrs], Var) :-
    validate_single_attribute(Attr, Var),
    validate_attributes(Attrs, Var).

validate_single_attribute(domain(Min, Max), Var) :-
    (   var(Var)
    ->  true
    ;   number(Var),
        Var >= Min,
        Var =< Max
    ).

validate_single_attribute(type(Type), Var) :-
    (   var(Var)
    ->  true
    ;   check_type(Type, Var)
    ).

check_type(integer, Value) :- integer(Value).
check_type(atom, Value) :- atom(Value).
check_type(number, Value) :- number(Value).

% Constraint propagation between variables
relate_variables(X, Y, Relation) :-
    put_atts(X, related(Y, Relation)),
    put_atts(Y, related(X, inverse_relation(Relation))).

inverse_relation(less_than, greater_than).
inverse_relation(greater_than, less_than).
inverse_relation(equal, equal).

% Advanced constraint: all_different
all_different_constraint(Variables) :-
    mark_all_different(Variables).

mark_all_different([]).
mark_all_different([Var|Vars]) :-
    put_atts(Var, all_different(Vars)),
    mark_all_different(Vars).

% Constraint solver interface
solve_constraints(Variables) :-
    propagate_constraints(Variables),
    label_variables(Variables).

propagate_constraints([]).
propagate_constraints([Var|Vars]) :-
    get_atts(Var, Attributes),
    apply_constraints(Attributes, Var),
    propagate_constraints(Vars).

apply_constraints([], _).
apply_constraints([Attr|Attrs], Var) :-
    apply_single_constraint(Attr, Var),
    apply_constraints(Attrs, Var).

apply_single_constraint(domain(Min, Max), Var) :-
    (   var(Var)
    ->  between(Min, Max, Var)
    ;   true
    ).

apply_single_constraint(all_different(Others), Var) :-
    (   var(Var)
    ->  true
    ;   check_all_different(Var, Others)
    ).

check_all_different(_, []).
check_all_different(Value, [Var|Vars]) :-
    (   var(Var)
    ->  true
    ;   Value \= Var
    ),
    check_all_different(Value, Vars).

label_variables([]).
label_variables([Var|Vars]) :-
    (   var(Var)
    ->  get_atts(Var, Attributes),
        choose_value(Attributes, Var)
    ;   true
    ),
    label_variables(Vars).

choose_value(Attributes, Var) :-
    (   member(domain(Min, Max), Attributes)
    ->  between(Min, Max, Var)
    ;   true
    ).

% Test constraint system
test_constraints :-
    domain_constraint(X, 1, 10),
    type_constraint(X, integer),
    X = 5,
    check_constraints(X).

test_all_different :-
    all_different_constraint([A, B, C]),
    A = 1,
    B = 2,
    C = 3,
    format('A=~w, B=~w, C=~w~n', [A, B, C]).

% Constraint-based puzzle solver
sudoku_cell(Cell, Row, Col, Block) :-
    domain_constraint(Cell, 1, 9),
    put_atts(Cell, [position(Row, Col), block(Block)]).

create_sudoku_grid(Grid) :-
    length(Grid, 9),
    maplist(create_row, Grid).

create_row(Row) :-
    length(Row, 9).

% Finite domain constraint system
fd_domain(Var, List) :-
    put_atts(Var, fd_domain(List)).

fd_in(Var, Min, Max) :-
    numlist(Min, Max, Domain),
    fd_domain(Var, Domain).

% Test queries:
% ?- test_constraints.
% ?- test_all_different.
% ?- domain_constraint(X, 1, 5), X = 3.
% ?- multi_constraint(Y, integer, 0, 100), Y = 50.
% ?- fd_in(Z, 1, 10), member(Z, [5,6,7]).