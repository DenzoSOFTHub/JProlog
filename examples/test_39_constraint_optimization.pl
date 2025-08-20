% ===================================================================
% TEST 39: Constraint Logic Programming and Optimization
% ===================================================================
% Tests: CLP(FD), optimization, constraint solving over finite domains

% Basic constraint domain setup
:- use_module(library(clpfd)).

% N-Queens with optimization
nqueens_optimized(N, Queens, Conflicts) :-
    length(Queens, N),
    Queens ins 1..N,
    safe_queens(Queens, 1),
    count_conflicts(Queens, Conflicts),
    labeling([minimize(Conflicts)], Queens).

safe_queens([], _).
safe_queens([Q|Qs], Col) :-
    safe_queen(Q, Qs, Col, 1),
    Col1 is Col + 1,
    safe_queens(Qs, Col1).

safe_queen(_, [], _, _).
safe_queen(Q, [Q1|Qs], Col, Dist) :-
    Q #\= Q1,
    Q #\= Q1 + Dist,
    Q #\= Q1 - Dist,
    Dist1 is Dist + 1,
    safe_queen(Q, Qs, Col, Dist1).

count_conflicts(Queens, Conflicts) :-
    findall(1, conflict_pair(Queens), ConflictList),
    length(ConflictList, Conflicts).

conflict_pair(Queens) :-
    nth1(I, Queens, Qi),
    nth1(J, Queens, Qj),
    I < J,
    (   Qi =:= Qj
    ;   abs(Qi - Qj) =:= abs(I - J)
    ).

% Scheduling optimization problem
schedule_tasks(Tasks, Machines, Schedule, Makespan) :-
    length(Tasks, NumTasks),
    length(Machines, NumMachines),
    create_schedule_vars(NumTasks, NumMachines, Schedule, Durations),
    setup_schedule_constraints(Schedule, Durations, Tasks, Machines),
    calculate_makespan(Schedule, Durations, Makespan),
    labeling([minimize(Makespan)], [Makespan|Schedule]).

create_schedule_vars(0, _, [], []) :- !.
create_schedule_vars(N, NumMachines, [Start|Starts], [Duration|Durations]) :-
    N > 0,
    Start in 0..1000,
    Duration in 1..100,
    N1 is N - 1,
    create_schedule_vars(N1, NumMachines, Starts, Durations).

setup_schedule_constraints([], [], [], []).
setup_schedule_constraints([Start|Starts], [Duration|Durations], [Task|Tasks], Machines) :-
    task_duration(Task, Duration),
    constrain_machine_capacity(Start, Duration, Machines),
    setup_schedule_constraints(Starts, Durations, Tasks, Machines).

task_duration(task(_, Duration), Duration).

constrain_machine_capacity(Start, Duration, Machines) :-
    length(Machines, NumMachines),
    Machine in 1..NumMachines,
    End #= Start + Duration.

calculate_makespan([], [], 0).
calculate_makespan([Start|Starts], [Duration|Durations], Makespan) :-
    calculate_makespan(Starts, Durations, RestMakespan),
    End #= Start + Duration,
    Makespan #= max(End, RestMakespan).

% Bin packing optimization
bin_packing(Items, BinCapacity, Assignment, NumBins) :-
    length(Items, NumItems),
    max_bins(NumItems, MaxBins),
    length(Assignment, NumItems),
    Assignment ins 1..MaxBins,
    constrain_bin_capacities(Items, Assignment, BinCapacity, MaxBins),
    count_used_bins(Assignment, NumBins),
    labeling([minimize(NumBins)], [NumBins|Assignment]).

max_bins(NumItems, MaxBins) :-
    MaxBins is NumItems.

constrain_bin_capacities(Items, Assignment, BinCapacity, MaxBins) :-
    numlist(1, MaxBins, Bins),
    maplist(constrain_single_bin(Items, Assignment, BinCapacity), Bins).

constrain_single_bin(Items, Assignment, BinCapacity, Bin) :-
    findall(Size, (nth1(I, Items, item(Size)), nth1(I, Assignment, Bin)), BinItems),
    sum_list(BinItems, BinLoad),
    BinLoad #=< BinCapacity.

count_used_bins(Assignment, NumBins) :-
    max_list(Assignment, NumBins).

% Knapsack optimization
knapsack(Items, Capacity, Selection, Value) :-
    length(Items, NumItems),
    length(Selection, NumItems),
    Selection ins 0..1,
    knapsack_constraints(Items, Selection, Capacity),
    knapsack_value(Items, Selection, Value),
    labeling([maximize(Value)], [Value|Selection]).

knapsack_constraints(Items, Selection, Capacity) :-
    maplist(item_weight, Items, Weights),
    scalar_product(Weights, Selection, #=<, Capacity).

item_weight(item(Weight, _), Weight).

knapsack_value(Items, Selection, Value) :-
    maplist(item_value, Items, Values),
    scalar_product(Values, Selection, #=, Value).

item_value(item(_, Value), Value).

% Graph coloring optimization
graph_coloring(Vertices, Edges, Colors, NumColors) :-
    length(Vertices, NumVertices),
    length(Colors, NumVertices),
    max_colors(NumVertices, MaxColors),
    Colors ins 1..MaxColors,
    constrain_edges(Edges, Vertices, Colors),
    count_colors_used(Colors, NumColors),
    labeling([minimize(NumColors)], [NumColors|Colors]).

max_colors(NumVertices, MaxColors) :-
    MaxColors is NumVertices.

constrain_edges([], _, _).
constrain_edges([edge(V1, V2)|Edges], Vertices, Colors) :-
    nth1(I1, Vertices, V1),
    nth1(I2, Vertices, V2),
    nth1(I1, Colors, C1),
    nth1(I2, Colors, C2),
    C1 #\= C2,
    constrain_edges(Edges, Vertices, Colors).

count_colors_used(Colors, NumColors) :-
    max_list(Colors, NumColors).

% Traveling salesman problem (simplified)
tsp(Cities, Tour, Distance) :-
    length(Cities, NumCities),
    length(Tour, NumCities),
    Tour ins 1..NumCities,
    all_different(Tour),
    calculate_tour_distance(Tour, Cities, Distance),
    labeling([minimize(Distance)], [Distance|Tour]).

calculate_tour_distance(Tour, Cities, Distance) :-
    tour_distances(Tour, Cities, Distances),
    sum_list(Distances, Distance).

tour_distances([Last], Cities, [D]) :-
    nth1(Last, Cities, LastCity),
    nth1(1, Cities, FirstCity),
    city_distance(LastCity, FirstCity, D).
tour_distances([C1, C2|Rest], Cities, [D|Distances]) :-
    nth1(C1, Cities, City1),
    nth1(C2, Cities, City2),
    city_distance(City1, City2, D),
    tour_distances([C2|Rest], Cities, Distances).

city_distance(city(X1, Y1), city(X2, Y2), Distance) :-
    DX is X2 - X1,
    DY is Y2 - Y1,
    Distance is round(sqrt(DX*DX + DY*DY)).

% Multi-objective optimization
pareto_optimal(Objectives, Variables, Solutions) :-
    findall(Obj-Vars, (
        labeling([], Variables),
        evaluate_objectives(Objectives, Variables, Obj)
    ), AllSolutions),
    filter_pareto_optimal(AllSolutions, Solutions).

evaluate_objectives([], _, []).
evaluate_objectives([Obj|Objs], Variables, [Value|Values]) :-
    call(Obj, Variables, Value),
    evaluate_objectives(Objs, Variables, Values).

filter_pareto_optimal(Solutions, ParetoSolutions) :-
    findall(Sol, (
        member(Sol, Solutions),
        is_pareto_optimal(Sol, Solutions)
    ), ParetoSolutions).

is_pareto_optimal(Obj1-_, Solutions) :-
    \+ (
        member(Obj2-_, Solutions),
        dominates(Obj2, Obj1)
    ).

dominates([], []).
dominates([O1|Os1], [O2|Os2]) :-
    O1 =< O2,
    dominates(Os1, Os2).

% Test optimization problems
test_nqueens_optimization :-
    nqueens_optimized(8, Queens, Conflicts),
    format('8-Queens solution with ~w conflicts: ~w~n', [Conflicts, Queens]).

test_bin_packing :-
    Items = [item(10), item(20), item(30), item(15), item(25)],
    bin_packing(Items, 50, Assignment, NumBins),
    format('Bin packing: ~w bins, assignment: ~w~n', [NumBins, Assignment]).

test_knapsack :-
    Items = [item(10, 60), item(20, 100), item(30, 120)],
    knapsack(Items, 50, Selection, Value),
    format('Knapsack: value ~w, selection ~w~n', [Value, Selection]).

test_graph_coloring :-
    Vertices = [a, b, c, d],
    Edges = [edge(a, b), edge(b, c), edge(c, d), edge(d, a)],
    graph_coloring(Vertices, Edges, Colors, NumColors),
    format('Graph coloring: ~w colors, assignment ~w~n', [NumColors, Colors]).

test_tsp :-
    Cities = [city(0, 0), city(1, 1), city(2, 0), city(1, -1)],
    tsp(Cities, Tour, Distance),
    format('TSP: distance ~w, tour ~w~n', [Distance, Tour]).

% Test queries:
% ?- test_nqueens_optimization.
% ?- test_bin_packing.
% ?- test_knapsack.
% ?- test_graph_coloring.
% ?- test_tsp.
% ?- nqueens_optimized(4, Queens, Conflicts).
% ?- schedule_tasks([task(a, 10), task(b, 20)], [machine1, machine2], Schedule, Makespan).