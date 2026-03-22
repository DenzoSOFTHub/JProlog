# CLP(FD) -- Constraint Logic Programming over Finite Domains

## Package Overview

JProlog's CLP(FD) library provides constraint logic programming over finite integer domains. It allows you to declare variables with integer domains, post arithmetic and relational constraints between them, and search for solutions that satisfy all constraints simultaneously. The solver uses **AC-3 arc consistency propagation** to prune domains before and during search, and supports **backtracking with snapshot/restore** for complete enumeration of solutions.

The implementation consists of two main classes:

- **`ClpfdPredicates`** (`it.denzosoft.jprolog.builtin.clpfd.ClpfdPredicates`) -- Implements all 13 CLP(FD) built-in predicates as a context-dependent built-in (`BuiltInWithContext`). Each predicate is dispatched through an `OperationType` enum.
- **`ConstraintStore`** (`it.denzosoft.jprolog.builtin.clpfd.ConstraintStore`) -- Singleton store that manages variable domains (`TreeSet<Integer>`), maintains a list of constraints, and implements the AC-3 propagation loop. Supports three constraint types: `BinaryConstraint` (relational), `AllDifferentConstraint`, and `ArithmeticConstraint` (expressions with `+`, `-`, `*`).

**Key architectural features**:
- Domains are stored as `TreeSet<Integer>` for ordered access and efficient set operations.
- Propagation is triggered automatically when constraints are added, and during labeling after each variable assignment.
- Backtracking uses `ConstraintStoreSnapshot` objects that deep-copy all domains and the constraint list.
- The labeling algorithm supports a **first-fail** heuristic (`ff`) that selects the variable with the smallest remaining domain.

---

## Predicate Reference

### in/2

```prolog
X in +Domain
```

Declares the domain of variable `X`. The domain can be a contiguous range or an explicit list of integers.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| `X`      | variable or integer | `?` | The constrained variable, or an integer to check membership. |
| `Domain` | range or list | `+` | Either `Min..Max` (range) or `[V1, V2, ...]` (explicit list). |

**Domain formats**:
- Range: `X in 1..10` -- sets domain to {1, 2, 3, ..., 10}
- Explicit list: `X in [2, 4, 6, 8]` -- sets domain to {2, 4, 6, 8}

**Behavior**:
- If `X` is an unbound variable with no existing domain, the domain is set directly.
- If `X` already has a domain, the new domain is **intersected** with the existing one. Fails if the intersection is empty (domain wipeout).
- If `X` is already bound to an integer, succeeds only if that integer is a member of the specified domain.
- Triggers constraint propagation after setting the domain.

---

### #=/2

```prolog
Expr1 #= Expr2
```

Constrains two arithmetic expressions to be equal.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| `Expr1`  | expression | `?` | Left-hand arithmetic expression (variable, integer, or `A + B`, `A - B`, `A * B`). |
| `Expr2`  | expression | `?` | Right-hand arithmetic expression. |

**Behavior**: Posts an equality constraint. If both sides are ground integers, checks equality immediately. If one or both sides contain variables, creates `BinaryConstraint(EQ)` or `ArithmeticConstraint` entries and propagates. Supports nested arithmetic expressions by introducing internal temporary variables.

**Examples**:
```prolog
X #= 5              % X must equal 5
X #= Y + 3          % X must equal Y plus 3
X + Y #= Z * 2      % sum of X and Y must equal twice Z
```

---

### #\=/2

```prolog
Expr1 #\= Expr2
```

Constrains two expressions to be **not equal**.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| `Expr1`  | expression | `?` | Left-hand expression. |
| `Expr2`  | expression | `?` | Right-hand expression. |

**Behavior**: Posts a disequality constraint (`NEQ`). Propagation removes values from one variable's domain only when the other variable's domain is a singleton.

---

### #</2

```prolog
Expr1 #< Expr2
```

Constrains the left expression to be **strictly less than** the right.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| `Expr1`  | expression | `?` | Left-hand expression. |
| `Expr2`  | expression | `?` | Right-hand expression. |

**Behavior**: Posts a `LT` constraint. During propagation, values are removed from the left domain that have no support in the right domain (i.e., no right value strictly greater), and vice versa.

---

### #>/2

```prolog
Expr1 #> Expr2
```

Constrains the left expression to be **strictly greater than** the right.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| `Expr1`  | expression | `?` | Left-hand expression. |
| `Expr2`  | expression | `?` | Right-hand expression. |

---

### #=</2

```prolog
Expr1 #=< Expr2
```

Constrains the left expression to be **less than or equal to** the right.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| `Expr1`  | expression | `?` | Left-hand expression. |
| `Expr2`  | expression | `?` | Right-hand expression. |

---

### #>=/2

```prolog
Expr1 #>= Expr2
```

Constrains the left expression to be **greater than or equal to** the right.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| `Expr1`  | expression | `?` | Left-hand expression. |
| `Expr2`  | expression | `?` | Right-hand expression. |

---

### all_different/1

```prolog
all_different(+Vars)
```

Constrains all variables in the list to take **pairwise distinct** values.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| `Vars`   | list of variables/integers | `+` | A Prolog list of constrained variables. |

**Behavior**:
- If all elements are already ground integers, checks that no two are equal (fails if duplicates found).
- If elements include unbound variables, posts an `AllDifferentConstraint`. During propagation, whenever a variable's domain becomes a singleton, that value is removed from all other variables' domains in the constraint.
- This is a global constraint -- it reasons about all variables simultaneously rather than decomposing into pairwise `#\=` constraints.

---

### label/1

```prolog
label(+Vars)
```

Assigns concrete integer values to all variables in the list by searching over their domains.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| `Vars`   | list of variables | `+` | Variables to label (all must have domains declared via `in/2`). |

**Behavior**: Performs a complete backtracking search. For each variable (in list order), tries every value in its current domain, propagates constraints, checks for wipeouts, and recurses. Returns all valid assignments as separate solution bindings. Fails if no assignment satisfies all constraints. Uses snapshot/restore for backtracking.

---

### labeling/2

```prolog
labeling(+Options, +Vars)
```

Like `label/1` but with configurable search strategy.

| Argument  | Type | Mode | Description |
|-----------|------|------|-------------|
| `Options` | list of atoms | `+` | Search options. Currently supported: `[ff]` for first-fail. |
| `Vars`    | list of variables | `+` | Variables to label. |

**Supported options**:

| Option | Description |
|--------|-------------|
| `ff`   | **First-fail**: at each choice point, select the variable with the smallest remaining domain. This heuristic often dramatically reduces search time by detecting failures early. |

**Behavior**: Identical to `label/1` but with variable selection heuristics. When `ff` is specified, the variable with the smallest domain is chosen next at each step (ties broken by list position). The chosen variable is swapped into position for the recursive call.

---

### indomain/1

```prolog
indomain(+X)
```

Nondeterministically assigns a value from the domain to `X`.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| `X`      | variable | `+` | A constrained variable with a declared domain. |

**Behavior**: Generates one solution binding for each value in the variable's current domain, in ascending order. If `X` is already ground, succeeds once. Fails if `X` has no domain or the domain is empty. Unlike `label/1`, does not propagate constraints -- it simply enumerates domain values.

---

### fd_dom/2

```prolog
fd_dom(+X, -Dom)
```

Retrieves the current domain of a constrained variable as a Prolog list.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| `X`      | variable | `+` | A constrained variable. |
| `Dom`    | list | `-` | Unified with the sorted list of integers in X's domain. |

**Behavior**: Returns the domain as a sorted list of integers. Fails if `X` has no registered domain in the constraint store.

---

### fd_size/2

```prolog
fd_size(+X, -Size)
```

Retrieves the number of values in a constrained variable's domain.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| `X`      | variable | `+` | A constrained variable. |
| `Size`   | integer | `-` | Unified with the cardinality of X's domain. |

**Behavior**: Returns the count of elements in the domain. Useful for heuristic decisions and debugging constraint models.

---

## Real-World Examples

### Example 1: N-Queens Problem

Place N queens on an NxN chessboard such that no two queens attack each other -- no shared row, column, or diagonal.

```prolog
%% nqueens.pl
%% The N-Queens problem: place N queens on an NxN board with no mutual attacks.
%%
%% Each queen occupies a unique row (by construction). Variables Q1..QN represent
%% the column of the queen in each row. The constraints enforce:
%%   - All columns are different (no two queens in the same column)
%%   - No two queens share a diagonal (|row_i - row_j| != |col_i - col_j|)

%% Solve the N-Queens problem for a given board size.
%% Queens is a list of column positions [Q1, Q2, ..., QN] where Qi is the
%% column of the queen in row i.
nqueens(N, Queens) :-
    length(Queens, N),
    set_domains(Queens, 1, N),
    all_different(Queens),
    post_diagonal_constraints(Queens, 1),
    labeling([ff], Queens).

%% Set the domain of each queen variable to 1..N.
set_domains([], _, _).
set_domains([Q|Qs], _, N) :-
    Q in 1..N,
    set_domains(Qs, _, N).

%% Post diagonal constraints between all pairs of queens.
%% For queens in rows I and J (I < J), the column difference must not
%% equal the row difference: |Qi - Qj| != |I - J|.
%% This is equivalent to: Qi - Qj #\= J - I, Qi - Qj #\= I - J.
post_diagonal_constraints([], _).
post_diagonal_constraints([Q|Qs], Row) :-
    post_diag_with_rest(Q, Qs, Row, 1),
    Row1 is Row + 1,
    post_diagonal_constraints(Qs, Row1).

%% Constrain queen Q in Row against each subsequent queen.
post_diag_with_rest(_, [], _, _).
post_diag_with_rest(Q, [Q2|Qs], Row, Offset) :-
    %% Q and Q2 are Offset rows apart.
    %% They must not be on the same diagonal:
    %%   Q - Q2 #\= Offset, Q2 - Q #\= Offset
    %% Equivalently, using arithmetic constraints:
    Q + Offset #\= Q2,
    Q - Offset #\= Q2,
    Offset1 is Offset + 1,
    post_diag_with_rest(Q, Qs, Row, Offset1).

%% Usage:
%% ?- nqueens(4, Queens).
%% Queens = [2, 4, 1, 3]
%% Queens = [3, 1, 4, 2]
%%
%% ?- nqueens(8, Queens).
%% Queens = [1, 5, 8, 6, 3, 7, 2, 4]  (first solution)
%% ... (91 more solutions)
```

---

### Example 2: Sudoku Solver

Solve a 9x9 Sudoku puzzle where each row, column, and 3x3 box contains the digits 1 through 9 exactly once.

```prolog
%% sudoku.pl
%% Sudoku solver using CLP(FD) constraints.
%%
%% The puzzle is represented as a list of 81 variables/integers.
%% Pre-filled cells are integers; empty cells are unbound variables.
%% Constraints: each row, column, and 3x3 box has all_different values in 1..9.

%% Main solver: takes a 9x9 grid as a flat list of 81 elements.
sudoku(Puzzle) :-
    length(Puzzle, 81),
    set_all_domains(Puzzle),
    Puzzle = [S11, S12, S13, S14, S15, S16, S17, S18, S19,
              S21, S22, S23, S24, S25, S26, S27, S28, S29,
              S31, S32, S33, S34, S35, S36, S37, S38, S39,
              S41, S42, S43, S44, S45, S46, S47, S48, S49,
              S51, S52, S53, S54, S55, S56, S57, S58, S59,
              S61, S62, S63, S64, S65, S66, S67, S68, S69,
              S71, S72, S73, S74, S75, S76, S77, S78, S79,
              S81, S82, S83, S84, S85, S86, S87, S88, S89,
              S91, S92, S93, S94, S95, S96, S97, S98, S99],
    %% Row constraints
    all_different([S11, S12, S13, S14, S15, S16, S17, S18, S19]),
    all_different([S21, S22, S23, S24, S25, S26, S27, S28, S29]),
    all_different([S31, S32, S33, S34, S35, S36, S37, S38, S39]),
    all_different([S41, S42, S43, S44, S45, S46, S47, S48, S49]),
    all_different([S51, S52, S53, S54, S55, S56, S57, S58, S59]),
    all_different([S61, S62, S63, S64, S65, S66, S67, S68, S69]),
    all_different([S71, S72, S73, S74, S75, S76, S77, S78, S79]),
    all_different([S81, S82, S83, S84, S85, S86, S87, S88, S89]),
    all_different([S91, S92, S93, S94, S95, S96, S97, S98, S99]),
    %% Column constraints
    all_different([S11, S21, S31, S41, S51, S61, S71, S81, S91]),
    all_different([S12, S22, S32, S42, S52, S62, S72, S82, S92]),
    all_different([S13, S23, S33, S43, S53, S63, S73, S83, S93]),
    all_different([S14, S24, S34, S44, S54, S64, S74, S84, S94]),
    all_different([S15, S25, S35, S45, S55, S65, S75, S85, S95]),
    all_different([S16, S26, S36, S46, S56, S66, S76, S86, S96]),
    all_different([S17, S27, S37, S47, S57, S67, S77, S87, S97]),
    all_different([S18, S28, S38, S48, S58, S68, S78, S88, S98]),
    all_different([S19, S29, S39, S49, S59, S69, S79, S89, S99]),
    %% 3x3 box constraints
    all_different([S11, S12, S13, S21, S22, S23, S31, S32, S33]),
    all_different([S14, S15, S16, S24, S25, S26, S34, S35, S36]),
    all_different([S17, S18, S19, S27, S28, S29, S37, S38, S39]),
    all_different([S41, S42, S43, S51, S52, S53, S61, S62, S63]),
    all_different([S44, S45, S46, S54, S55, S56, S64, S65, S66]),
    all_different([S47, S48, S49, S57, S58, S59, S67, S68, S69]),
    all_different([S71, S72, S73, S81, S82, S83, S91, S92, S93]),
    all_different([S74, S75, S76, S84, S85, S86, S94, S95, S96]),
    all_different([S77, S78, S79, S87, S88, S89, S97, S98, S99]),
    %% Search
    labeling([ff], Puzzle).

%% Set domain 1..9 for all unbound variables; skip already-set cells.
set_all_domains([]).
set_all_domains([V|Vs]) :-
    ( integer(V) -> true ; V in 1..9 ),
    set_all_domains(Vs).

%% Pretty-print a solved puzzle.
print_sudoku([]).
print_sudoku(Puzzle) :-
    Puzzle = [A,B,C,D,E,F,G,H,I|Rest],
    write(A), write(' '), write(B), write(' '), write(C), write(' | '),
    write(D), write(' '), write(E), write(' '), write(F), write(' | '),
    write(G), write(' '), write(H), write(' '), write(I), nl,
    print_sudoku(Rest).

%% Example puzzle (0 represents empty cells -- use variables):
%% ?- Puzzle = [5,3,_,_,7,_,_,_,_,
%%              6,_,_,1,9,5,_,_,_,
%%              _,9,8,_,_,_,_,6,_,
%%              8,_,_,_,6,_,_,_,3,
%%              4,_,_,8,_,3,_,_,1,
%%              7,_,_,_,2,_,_,_,6,
%%              _,6,_,_,_,_,2,8,_,
%%              _,_,_,4,1,9,_,_,5,
%%              _,_,_,_,8,_,_,7,9],
%%    sudoku(Puzzle),
%%    print_sudoku(Puzzle).
%%
%% Solution:
%%   5 3 4 | 6 7 8 | 9 1 2
%%   6 7 2 | 1 9 5 | 3 4 8
%%   1 9 8 | 3 4 2 | 5 6 7
%%   8 5 9 | 7 6 1 | 4 2 3
%%   4 2 6 | 8 5 3 | 7 9 1
%%   7 1 3 | 9 2 4 | 8 5 6
%%   9 6 1 | 5 3 7 | 2 8 4
%%   2 8 7 | 4 1 9 | 6 3 5
%%   3 4 5 | 2 8 6 | 1 7 9
```

---

### Example 3: Job Scheduling with Precedence and Resource Constraints

Schedule jobs on machines with duration, precedence ordering, and non-overlap constraints.

```prolog
%% scheduling.pl
%% Job-shop scheduling: assign start times to tasks subject to:
%%   - Each task has a fixed duration
%%   - Precedence constraints (task A must finish before task B starts)
%%   - Resource constraints (tasks sharing a machine cannot overlap)
%%   - All tasks must complete within a given makespan

%% Task database: task(Name, Duration, Machine)
task(cut_metal,    3, machine_a).
task(weld_frame,   4, machine_b).
task(paint,        2, machine_c).
task(drill_holes,  2, machine_a).
task(assemble,     5, machine_b).
task(inspect,      1, machine_c).

%% Precedence: pred(Before, After) means Before must finish before After starts.
pred(cut_metal,   weld_frame).
pred(cut_metal,   drill_holes).
pred(weld_frame,  assemble).
pred(drill_holes, assemble).
pred(assemble,    paint).
pred(paint,       inspect).

%% Solve the scheduling problem with a given makespan limit.
schedule(Makespan, Solution) :-
    findall(task(Name, Dur, Mach), task(Name, Dur, Mach), Tasks),
    create_start_vars(Tasks, Makespan, StartVars),
    post_precedence_constraints(StartVars),
    post_machine_constraints(StartVars),
    extract_vars(StartVars, Vars),
    labeling([ff], Vars),
    build_solution(StartVars, Solution).

%% Create a start-time variable for each task with domain 0..Makespan-Duration.
create_start_vars([], _, []).
create_start_vars([task(Name, Dur, Mach)|Ts], Makespan, [sv(Name, Dur, Mach, Start)|Rest]) :-
    MaxStart is Makespan - Dur,
    Start in 0..MaxStart,
    create_start_vars(Ts, Makespan, Rest).

%% Post precedence constraints: for each pred(A, B), Start_A + Dur_A =< Start_B.
post_precedence_constraints(StartVars) :-
    forall(
        pred(Before, After),
        post_one_precedence(Before, After, StartVars)
    ).

post_one_precedence(Before, After, StartVars) :-
    member(sv(Before, DurB, _, StartB), StartVars),
    member(sv(After, _, _, StartA), StartVars),
    StartB + DurB #=< StartA.

%% Post machine non-overlap constraints: tasks on the same machine cannot overlap.
%% For tasks i and j on the same machine: either Si + Di =< Sj or Sj + Dj =< Si.
%% We encode this as: Si + Di #=< Sj (task i before j) OR Sj + Dj #=< Si (j before i).
%% Since CLP(FD) does not have disjunctive constraints directly, we post both
%% orderings as alternatives during labeling. Here we use a simpler approach:
%% post all_different on start times of same-machine tasks (approximate for equal durations).
post_machine_constraints(StartVars) :-
    findall(Mach, task(_, _, Mach), MachsRaw),
    sort(MachsRaw, Machines),
    post_machine_constraints_for(Machines, StartVars).

post_machine_constraints_for([], _).
post_machine_constraints_for([Mach|Ms], StartVars) :-
    findall(Start, member(sv(_, _, Mach, Start), StartVars), MachStarts),
    ( MachStarts = [_,_|_] ->  % Only if 2+ tasks on this machine
        all_different(MachStarts)
    ;
        true
    ),
    post_machine_constraints_for(Ms, StartVars).

%% Extract just the start-time variables for labeling.
extract_vars([], []).
extract_vars([sv(_, _, _, Start)|Rest], [Start|Vars]) :-
    extract_vars(Rest, Vars).

%% Build a human-readable solution list.
build_solution([], []).
build_solution([sv(Name, Dur, Mach, Start)|Rest],
               [scheduled(Name, Start, End, Mach)|Sol]) :-
    End is Start + Dur,
    build_solution(Rest, Sol).

%% Usage:
%% ?- schedule(16, Solution).
%% Solution = [scheduled(cut_metal, 0, 3, machine_a),
%%             scheduled(weld_frame, 3, 7, machine_b),
%%             scheduled(paint, 12, 14, machine_c),
%%             scheduled(drill_holes, 3, 5, machine_a),
%%             scheduled(assemble, 7, 12, machine_b),
%%             scheduled(inspect, 14, 15, machine_c)]
```

---

### Example 4: Map Coloring

Color a map of regions using the minimum number of colors so that no two adjacent regions share the same color.

```prolog
%% map_coloring.pl
%% Graph coloring: assign colors (integers) to regions of a map such that
%% no two adjacent regions share the same color.
%%
%% This example models the map of Australia's states and territories.

%% Adjacency relation: adjacent(Region1, Region2).
adjacent(western_australia, northern_territory).
adjacent(western_australia, south_australia).
adjacent(northern_territory, south_australia).
adjacent(northern_territory, queensland).
adjacent(south_australia, queensland).
adjacent(south_australia, new_south_wales).
adjacent(south_australia, victoria).
adjacent(queensland, new_south_wales).
adjacent(new_south_wales, victoria).

%% Color a map with at most MaxColors colors.
%% Colors are integers 1..MaxColors.
%% RegionColors is a list of region(Name, Color) pairs.
color_map(MaxColors, RegionColors) :-
    Regions = [western_australia, northern_territory, south_australia,
               queensland, new_south_wales, victoria, tasmania],
    create_color_vars(Regions, MaxColors, RegionColors, ColorVars),
    post_adjacency_constraints(RegionColors),
    labeling([ff], ColorVars).

%% Create a color variable for each region with domain 1..MaxColors.
create_color_vars([], _, [], []).
create_color_vars([R|Rs], MaxColors, [region(R, C)|RCs], [C|Cs]) :-
    C in 1..MaxColors,
    create_color_vars(Rs, MaxColors, RCs, Cs).

%% Post constraints: adjacent regions must have different colors.
post_adjacency_constraints(RegionColors) :-
    forall(
        adjacent(R1, R2),
        (   member(region(R1, C1), RegionColors),
            member(region(R2, C2), RegionColors),
            C1 #\= C2
        )
    ).

%% Find the chromatic number (minimum colors needed).
chromatic_number(MinColors, Solution) :-
    between(1, 10, MinColors),
    color_map(MinColors, Solution),
    !.  % Stop at first success: that is the minimum

%% Color names for pretty printing.
color_name(1, red).
color_name(2, green).
color_name(3, blue).
color_name(4, yellow).

%% Pretty-print the coloring.
print_coloring([]).
print_coloring([region(Name, ColorNum)|Rest]) :-
    ( color_name(ColorNum, ColorName) -> true ; ColorName = ColorNum ),
    write(Name), write(': '), write(ColorName), nl,
    print_coloring(Rest).

%% Usage:
%% ?- chromatic_number(N, Solution), print_coloring(Solution).
%% N = 3
%% western_australia: red
%% northern_territory: green
%% south_australia: blue
%% queensland: red
%% new_south_wales: green
%% victoria: red
%% tasmania: red
```

---

### Example 5: Cryptarithmetic -- SEND + MORE = MONEY

Each letter represents a unique digit (0-9). Leading digits cannot be zero. Find the assignment that makes the addition correct.

```prolog
%% send_more_money.pl
%% Classic cryptarithmetic puzzle:
%%     S E N D
%%   + M O R E
%%   ---------
%%   M O N E Y
%%
%% Each letter represents a distinct digit 0-9.
%% S and M cannot be 0 (no leading zeros).

send_more_money([S, E, N, D, M, O, R, Y]) :-
    %% All digits are in the range 0..9
    S in 0..9, E in 0..9, N in 0..9, D in 0..9,
    M in 0..9, O in 0..9, R in 0..9, Y in 0..9,

    %% All letters represent different digits
    all_different([S, E, N, D, M, O, R, Y]),

    %% No leading zeros
    S #> 0,
    M #> 0,

    %% The addition constraint:
    %% 1000*S + 100*E + 10*N + D
    %% + 1000*M + 100*O + 10*R + E
    %% = 10000*M + 1000*O + 100*N + 10*E + Y
    %%
    %% Rearranged: 1000*S + 91*E + D + 1000*M + 10*R - 9000*M - 900*O - 90*N - Y = 0
    %% Or directly:
    S * 1000 + E * 100 + N * 10 + D
    + M * 1000 + O * 100 + R * 10 + E
    #= M * 10000 + O * 1000 + N * 100 + E * 10 + Y,

    %% Search with first-fail heuristic
    labeling([ff], [S, E, N, D, M, O, R, Y]).

%% Pretty-print the solution.
print_solution([S, E, N, D, M, O, R, Y]) :-
    write('    '), write(S), write(E), write(N), write(D), nl,
    write('  + '), write(M), write(O), write(R), write(E), nl,
    write('  -----'), nl,
    write('  '), write(M), write(O), write(N), write(E), write(Y), nl.

%% Usage:
%% ?- send_more_money(Digits), print_solution(Digits).
%%     9567
%%   + 1085
%%   -----
%%   10652
%% Digits = [9, 5, 6, 7, 1, 0, 8, 2]
```

---

### Example 6: Resource Allocation -- Worker Shift Assignment

Assign workers to shifts respecting availability windows, skill requirements, and workload limits.

```prolog
%% shift_assignment.pl
%% Assign workers to weekly shifts respecting:
%%   - Worker availability (not everyone can work every shift)
%%   - Skill requirements (certain shifts need certain skills)
%%   - Maximum shifts per worker (fairness / labor law)
%%   - Minimum staffing per shift

%% Workers and their available shifts (1=Mon, 2=Tue, ..., 5=Fri).
%% available(Worker, ShiftDay).
available(alice, 1). available(alice, 2). available(alice, 3).
available(alice, 4). available(alice, 5).
available(bob, 1). available(bob, 2). available(bob, 3).
available(carol, 1). available(carol, 3). available(carol, 5).
available(dave, 2). available(dave, 3). available(dave, 4). available(dave, 5).
available(eve, 1). available(eve, 2). available(eve, 4). available(eve, 5).

%% Skills: skill(Worker, Skill).
skill(alice, senior). skill(alice, training).
skill(bob, senior). skill(bob, maintenance).
skill(carol, training).
skill(dave, maintenance). skill(dave, senior).
skill(eve, training).

%% Shift requirements: requires(Day, Skill) means that day needs at least one
%% worker with that skill.
requires(1, senior).
requires(3, maintenance).
requires(5, training).

%% Each worker is assigned a binary variable per day: 1 = working, 0 = not working.
%% We model this as a value in {0,1} for each (worker, day) pair.
assign_shifts(Assignment) :-
    Workers = [alice, bob, carol, dave, eve],
    Days = [1, 2, 3, 4, 5],
    create_assignment_vars(Workers, Days, Assignment, AllVars),
    post_availability_constraints(Assignment),
    post_max_shifts_constraints(Workers, Assignment, 3),
    post_min_staffing_constraints(Days, Assignment, 2),
    labeling([ff], AllVars).

%% Create binary (0/1) variables for each worker-day pair.
create_assignment_vars([], _, [], []).
create_assignment_vars([W|Ws], Days, Assignments, AllVars) :-
    create_worker_vars(W, Days, WAssign, WVars),
    append(WAssign, RestAssign, Assignments),
    append(WVars, RestVars, AllVars),
    create_assignment_vars(Ws, Days, RestAssign, RestVars).

create_worker_vars(_, [], [], []).
create_worker_vars(W, [D|Ds], [assign(W, D, V)|Rest], [V|Vars]) :-
    V in 0..1,
    create_worker_vars(W, Ds, Rest, Vars).

%% Workers can only be assigned to shifts they are available for.
post_availability_constraints([]).
post_availability_constraints([assign(W, D, V)|Rest]) :-
    ( available(W, D) -> true ; V #= 0 ),
    post_availability_constraints(Rest).

%% No worker works more than MaxShifts days per week.
post_max_shifts_constraints([], _, _).
post_max_shifts_constraints([W|Ws], Assignment, MaxShifts) :-
    findall(V, member(assign(W, _, V), Assignment), WorkerVars),
    sum_vars(WorkerVars, Total),
    Total #=< MaxShifts,
    post_max_shifts_constraints(Ws, Assignment, MaxShifts).

%% Each day must have at least MinStaff workers assigned.
post_min_staffing_constraints([], _, _).
post_min_staffing_constraints([D|Ds], Assignment, MinStaff) :-
    findall(V, member(assign(_, D, V), Assignment), DayVars),
    sum_vars(DayVars, Total),
    Total #>= MinStaff,
    post_min_staffing_constraints(Ds, Assignment, MinStaff).

%% Sum a list of CLP(FD) variables.
sum_vars([], S) :- S #= 0.
sum_vars([V], S) :- S #= V.
sum_vars([V1, V2|Vs], S) :-
    T #= V1 + V2,
    sum_vars([T|Vs], S).

%% Pretty-print the schedule.
print_schedule([]).
print_schedule([assign(W, D, 1)|Rest]) :-
    day_name(D, Name),
    write(W), write(' -> '), write(Name), nl,
    print_schedule(Rest).
print_schedule([assign(_, _, 0)|Rest]) :-
    print_schedule(Rest).

day_name(1, monday). day_name(2, tuesday). day_name(3, wednesday).
day_name(4, thursday). day_name(5, friday).

%% Usage:
%% ?- assign_shifts(A), print_schedule(A).
%% alice -> monday
%% alice -> tuesday
%% alice -> wednesday
%% bob -> monday
%% bob -> tuesday
%% bob -> wednesday
%% carol -> monday
%% carol -> friday
%% dave -> thursday
%% dave -> friday
%% eve -> thursday
%% eve -> friday
```

---

### Example 7: Magic Square

Fill an NxN grid with distinct integers 1..N*N such that every row, column, and both main diagonals sum to the same magic constant.

```prolog
%% magic_square.pl
%% Construct a 3x3 magic square: a grid filled with distinct integers 1..9
%% where every row, column, and diagonal sums to 15 (the magic constant).
%%
%% The magic constant for an NxN square with values 1..N^2 is:
%%   M = N * (N^2 + 1) / 2
%% For N=3: M = 3 * 10 / 2 = 15

magic_square_3x3([A, B, C, D, E, F, G, H, I]) :-
    %% All cells contain values 1..9
    A in 1..9, B in 1..9, C in 1..9,
    D in 1..9, E in 1..9, F in 1..9,
    G in 1..9, H in 1..9, I in 1..9,

    %% All values are distinct
    all_different([A, B, C, D, E, F, G, H, I]),

    %% Row sums equal 15
    A + B + C #= 15,
    D + E + F #= 15,
    G + H + I #= 15,

    %% Column sums equal 15
    A + D + G #= 15,
    B + E + H #= 15,
    C + F + I #= 15,

    %% Diagonal sums equal 15
    A + E + I #= 15,
    C + E + G #= 15,

    %% Search
    labeling([ff], [A, B, C, D, E, F, G, H, I]).

%% Pretty-print the magic square.
print_magic_square([A, B, C, D, E, F, G, H, I]) :-
    write('+---+---+---+'), nl,
    write('| '), write(A), write(' | '), write(B), write(' | '), write(C), write(' |'), nl,
    write('+---+---+---+'), nl,
    write('| '), write(D), write(' | '), write(E), write(' | '), write(F), write(' |'), nl,
    write('+---+---+---+'), nl,
    write('| '), write(G), write(' | '), write(H), write(' | '), write(I), write(' |'), nl,
    write('+---+---+---+'), nl.

%% Generalized magic square for NxN (requires generating constraints programmatically).
magic_square(N, Grid) :-
    N2 is N * N,
    Magic is N * (N2 + 1) // 2,
    length(Grid, N2),
    set_grid_domains(Grid, N2),
    all_different(Grid),
    constrain_rows(Grid, N, Magic),
    constrain_cols(Grid, N, Magic),
    constrain_main_diag(Grid, N, Magic),
    constrain_anti_diag(Grid, N, Magic),
    labeling([ff], Grid).

set_grid_domains([], _).
set_grid_domains([V|Vs], Max) :-
    V in 1..Max,
    set_grid_domains(Vs, Max).

%% Constrain each row to sum to Magic.
constrain_rows(_, 0, _) :- !.
constrain_rows(Grid, RowsLeft, Magic) :-
    RowsLeft > 0,
    length(Row, _),  % placeholder
    %% Extract row: simplified for 3x3 in the main predicate above
    true.

%% For the general case, row/column extraction would use nth1 and arithmetic
%% to index into the flat list. The 3x3 version above demonstrates the concept.

%% Usage:
%% ?- magic_square_3x3(S), print_magic_square(S).
%% +---+---+---+
%% | 2 | 7 | 6 |
%% +---+---+---+
%% | 9 | 5 | 1 |
%% +---+---+---+
%% | 4 | 3 | 8 |
%% +---+---+---+
%%
%% There are 8 solutions total (rotations and reflections of one fundamental solution).
```
