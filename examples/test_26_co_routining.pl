% ===================================================================
% TEST 26: Co-routining and Constraint Logic Programming
% ===================================================================
% Tests: when/2, freeze/2, dif/2, attributed variables

% Constraint satisfaction with delayed goals
test_delayed_goals :-
    when(ground(X), (X > 5, write('X is greater than 5'))),
    X = 10.

% Finite domain constraints
domain(X, Min, Max) :-
    when(nonvar(X), (X >= Min, X =< Max)).

% Mutual constraints
mutual_constraint(X, Y) :-
    when(ground(X), Y is X * 2),
    when(ground(Y), X is Y / 2).

% Disequality constraints
test_disequality :-
    dif(X, Y),
    X = a,
    Y = b.

% Freeze constraint (delay until instantiation)
test_freeze :-
    freeze(X, (X > 0, write('X is positive'))),
    X = 5.

% Constraint propagation
propagate_constraints(X, Y, Z) :-
    X + Y #= Z,
    X in 1..10,
    Y in 1..10,
    Z in 15..20.

% Coroutining with attributed variables
attr_var_test(X) :-
    put_atts(X, +type(integer)),
    put_atts(X, +domain(1..100)).

% Constraint-based scheduling
schedule_tasks(Tasks, Schedule) :-
    length(Tasks, N),
    length(Schedule, N),
    constrain_schedule(Tasks, Schedule, 0).

constrain_schedule([], [], _).
constrain_schedule([Task|Tasks], [Start|Schedule], Time) :-
    Start #>= Time,
    End #= Start + Task,
    constrain_schedule(Tasks, Schedule, End).

% Test queries:
% ?- test_delayed_goals.
% ?- domain(X, 1, 10), X = 5.
% ?- mutual_constraint(X, Y), X = 3.
% ?- test_disequality.
% ?- test_freeze.