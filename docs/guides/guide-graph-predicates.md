# Graph Predicates Guide

## Package Overview

The `it.denzosoft.jprolog.builtin.graph` package provides a comprehensive set of graph algorithm predicates for JProlog. It supports both weighted and unweighted directed graphs, enabling Prolog programs to perform classic graph computations including path finding, shortest path (BFS and Dijkstra), topological sorting (Kahn's algorithm), connected component detection, minimum spanning tree (Kruskal's algorithm), cycle detection, and reachability analysis.

### Graph Representation

Graphs are represented as lists of edge terms wrapped in an optional `graph/1` functor:

- **Unweighted edges**: `edge(From, To)` -- default weight of 1.0
- **Weighted edges**: `edge(From, To, Weight)` -- explicit numeric weight

Both formats can be passed either bare as a list or wrapped:

```prolog
% Bare list
[edge(a, b), edge(b, c, 5), edge(a, c, 2)]

% Wrapped in graph/1 functor
graph([edge(a, b), edge(b, c, 5), edge(a, c, 2)])
```

Vertices are atoms or numbers. The graph is treated as **directed** for all predicates except `graph_components/2`, which treats edges as undirected.

**Algorithms used**:
- Path finding: depth-first search with cycle detection
- Shortest path: BFS for unweighted graphs, Dijkstra for weighted graphs
- Topological sort: Kahn's algorithm with deterministic (sorted) output
- Connected components: BFS on an undirected interpretation of the graph
- Minimum spanning tree: Kruskal's algorithm with Union-Find (path compression)
- Cycle detection: DFS with three-color (white/gray/black) marking

**Source file**: `src/main/java/it/denzosoft/jprolog/builtin/graph/GraphPredicates.java`

---

## Predicate Reference

### graph_path(+Graph, +Start, +End, -Path)

Find a path from Start to End using depth-first search with cycle detection.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Graph | list/compound | input | Graph as a list of `edge/2` or `edge/3` terms |
| Start | atom/number | input | Starting vertex |
| End | atom/number | input | Target vertex |
| Path | list | output | List of vertices from Start to End (inclusive) |

Returns the first path found by DFS. Backtracks through the graph avoiding cycles. Fails if no path exists.

```prolog
?- graph_path([edge(a,b), edge(b,c), edge(a,c)], a, c, Path).
Path = [a, b, c].
```

---

### shortest_path(+Graph, +Start, +End, -Path)

Find the shortest path from Start to End. Uses BFS for unweighted graphs (all weights 1.0) and Dijkstra's algorithm for weighted graphs.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Graph | list/compound | input | Graph as a list of `edge/2` or `edge/3` terms |
| Start | atom/number | input | Starting vertex |
| End | atom/number | input | Target vertex |
| Path | list | output | Shortest path as a list of vertices |

For weighted graphs, "shortest" means the path with minimum total weight. Fails if no path exists.

```prolog
?- shortest_path([edge(a,b,1), edge(b,c,1), edge(a,c,5)], a, c, Path).
Path = [a, b, c].   % Total weight 2, not the direct edge of weight 5
```

---

### graph_connected(+Graph, +Start)

Test whether all vertices in the graph are reachable from Start.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Graph | list/compound | input | Graph as a list of edge terms |
| Start | atom/number | input | Starting vertex for reachability check |

Succeeds if every vertex in the graph can be reached from Start by following directed edges. Fails otherwise.

```prolog
?- graph_connected([edge(a,b), edge(b,c), edge(c,a)], a).
true.

?- graph_connected([edge(a,b), edge(c,d)], a).
false.   % c and d not reachable from a
```

---

### graph_vertices(+Graph, -Vertices)

Extract a sorted list of all unique vertices in the graph.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Graph | list/compound | input | Graph as a list of edge terms |
| Vertices | list | output | Sorted list of all vertex names |

Collects both source and target vertices from all edges and returns them in sorted (alphabetical/numerical) order.

```prolog
?- graph_vertices([edge(c,a), edge(b,d), edge(a,b)], V).
V = [a, b, c, d].
```

---

### graph_edges(+Graph, -Edges)

Extract the list of edges from the graph.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Graph | list/compound | input | Graph (possibly wrapped in `graph/1`) |
| Edges | list | output | List of `edge/2` or `edge/3` terms |

Unwraps a `graph/1` wrapper if present and returns the raw edge list. Unweighted edges are returned as `edge(From, To)`, weighted as `edge(From, To, Weight)`.

```prolog
?- graph_edges(graph([edge(a,b,3), edge(b,c)]), E).
E = [edge(a, b, 3), edge(b, c)].
```

---

### graph_neighbors(+Graph, +Node, -Neighbors)

Get all direct neighbors (successors) of a node, sorted and deduplicated.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Graph | list/compound | input | Graph as a list of edge terms |
| Node | atom/number | input | Vertex to find neighbors of |
| Neighbors | list | output | Sorted list of vertices reachable in one step |

Only considers outgoing edges from Node (directed graph).

```prolog
?- graph_neighbors([edge(a,b), edge(a,c), edge(a,b), edge(b,d)], a, N).
N = [b, c].
```

---

### topological_sort(+Graph, -Sorted)

Compute a topological ordering of the vertices using Kahn's algorithm.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Graph | list/compound | input | Directed acyclic graph (DAG) |
| Sorted | list | output | Topologically sorted list of vertices |

Vertices with the same in-degree are ordered alphabetically for deterministic output. **Fails if the graph contains a cycle** (not all vertices can be processed).

```prolog
?- topological_sort([edge(a,b), edge(a,c), edge(b,d), edge(c,d)], S).
S = [a, b, c, d].
```

---

### graph_components(+Graph, -Components)

Find connected components treating the graph as undirected.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Graph | list/compound | input | Graph as a list of edge terms |
| Components | list of lists | output | Each inner list is a sorted list of vertices in one component |

Edges are treated bidirectionally. Components are returned in the order they are discovered by iterating over the sorted vertex set.

```prolog
?- graph_components([edge(a,b), edge(c,d), edge(d,e)], C).
C = [[a, b], [c, d, e]].
```

---

### minimum_spanning_tree(+Graph, -MST)

Compute the minimum spanning tree using Kruskal's algorithm with Union-Find.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Graph | list/compound | input | Weighted graph (edges with numeric weights) |
| MST | list | output | List of edge terms forming the MST |

Edges in the result are ordered by weight (ascending, as selected by Kruskal's). Uses path compression and union-by-rank for efficient Union-Find. For unweighted graphs, all edges have weight 1.0.

```prolog
?- minimum_spanning_tree(
       [edge(a,b,4), edge(a,c,2), edge(b,c,1), edge(b,d,5), edge(c,d,3)],
       MST).
MST = [edge(b, c, 1), edge(a, c, 2), edge(c, d, 3)].
```

---

### graph_degree(+Graph, +Node, -Degree)

Count the total degree of a node (sum of incoming and outgoing edges).

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Graph | list/compound | input | Graph as a list of edge terms |
| Node | atom/number | input | Vertex to compute degree for |
| Degree | integer | output | Total number of edges incident to Node |

Counts each edge where Node appears as source or target. A self-loop `edge(a, a)` contributes 2 to the degree.

```prolog
?- graph_degree([edge(a,b), edge(c,a), edge(a,d)], a, D).
D = 3.   % 2 outgoing (a->b, a->d) + 1 incoming (c->a)
```

---

### graph_has_cycle(+Graph)

Test whether a directed graph contains a cycle.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Graph | list/compound | input | Directed graph as a list of edge terms |

Succeeds if the graph contains at least one directed cycle. Uses DFS with three-color marking (white = unvisited, gray = in current path, black = fully processed). A back edge to a gray node indicates a cycle.

```prolog
?- graph_has_cycle([edge(a,b), edge(b,c), edge(c,a)]).
true.

?- graph_has_cycle([edge(a,b), edge(b,c)]).
false.
```

---

### graph_reachable(+Graph, +Start, -Reachable)

Find all vertices reachable from Start via BFS, including Start itself.

| Argument | Type | Mode | Description |
|----------|------|------|-------------|
| Graph | list/compound | input | Graph as a list of edge terms |
| Start | atom/number | input | Starting vertex |
| Reachable | list | output | Sorted list of all reachable vertices |

Uses breadth-first search following directed edges. The result always includes Start itself.

```prolog
?- graph_reachable([edge(a,b), edge(b,c), edge(d,e)], a, R).
R = [a, b, c].   % d and e not reachable from a
```

---

## Real-World Examples

### Example 1: Social Network Analysis

Model a social network as a directed graph (follower relationships), find connections between people, detect communities, and identify influential users.

```prolog
% ============================================================
% Social Network Analyzer
% Models a social network where edge(A, B) means "A follows B".
% Provides analysis tools: mutual connections, shortest path
% between people, community detection, and influence ranking.
% ============================================================

% ---- Network definition ----
% A directed graph: edge(Follower, Followed)

social_network(graph([
    % Tech community cluster
    edge(alice, bob),
    edge(bob, alice),      % mutual follow
    edge(alice, carol),
    edge(carol, alice),
    edge(bob, carol),
    edge(carol, dave),
    edge(dave, carol),

    % Art community cluster
    edge(eve, frank),
    edge(frank, eve),
    edge(frank, grace),
    edge(grace, frank),
    edge(eve, grace),

    % Bridge connections (cross-community)
    edge(carol, eve),      % Carol follows Eve, bridging tech and art
    edge(grace, alice),    % Grace follows Alice

    % Peripheral users
    edge(henry, alice),
    edge(henry, eve),
    edge(iris, bob)
])).

% ---- Find the shortest connection path between two people ----

find_connection(Person1, Person2) :-
    social_network(G),
    (   shortest_path(G, Person1, Person2, Path)
    ->  write('Connection from '), write(Person1),
        write(' to '), write(Person2), write(':'), nl,
        print_path(Path),
        length(Path, Len),
        Degrees is Len - 1,
        write('Degrees of separation: '), write(Degrees), nl
    ;   write('No connection from '), write(Person1),
        write(' to '), write(Person2), nl
    ).

print_path([]).
print_path([X]) :- write('  '), write(X), nl.
print_path([X,Y|Rest]) :-
    write('  '), write(X), write(' -> '),
    print_path([Y|Rest]).

% ---- Detect communities (connected components) ----

detect_communities :-
    social_network(G),
    graph_components(G, Components),
    write('=== Community Detection ==='), nl,
    number_communities(Components, 1).

number_communities([], _).
number_communities([C|Rest], N) :-
    write('Community '), write(N), write(': '),
    write(C), nl,
    length(C, Size),
    write('  Size: '), write(Size), write(' members'), nl,
    N1 is N + 1,
    number_communities(Rest, N1).

% ---- Find mutual followers (bidirectional edges) ----

mutual_followers :-
    social_network(G),
    graph_edges(G, Edges),
    write('=== Mutual Followers ==='), nl,
    find_mutuals(Edges, Edges, []).

find_mutuals([], _, _).
find_mutuals([edge(A, B)|Rest], AllEdges, Seen) :-
    A @< B,  % Avoid duplicates
    member(edge(B, A), AllEdges),
    \+ member(A-B, Seen),
    !,
    write('  '), write(A), write(' <-> '), write(B), nl,
    find_mutuals(Rest, AllEdges, [A-B|Seen]).
find_mutuals([_|Rest], AllEdges, Seen) :-
    find_mutuals(Rest, AllEdges, Seen).

% ---- Influence ranking by degree (in-degree = follower count) ----

influence_ranking :-
    social_network(G),
    graph_vertices(G, Vertices),
    graph_edges(G, Edges),
    write('=== Influence Ranking (by follower count) ==='), nl,
    compute_in_degrees(Vertices, Edges, Scores),
    sort_by_score(Scores, Sorted),
    print_ranking(Sorted, 1).

compute_in_degrees([], _, []).
compute_in_degrees([V|Vs], Edges, [score(V, InDeg)|Rest]) :-
    count_incoming(V, Edges, InDeg),
    compute_in_degrees(Vs, Edges, Rest).

count_incoming(_, [], 0).
count_incoming(V, [edge(_, V)|Rest], N) :-
    !, count_incoming(V, Rest, N1), N is N1 + 1.
count_incoming(V, [edge(_, V, _)|Rest], N) :-
    !, count_incoming(V, Rest, N1), N is N1 + 1.
count_incoming(V, [_|Rest], N) :-
    count_incoming(V, Rest, N).

sort_by_score(Scores, Sorted) :-
    % Simple insertion sort by score descending
    isort(Scores, Sorted).

isort([], []).
isort([X|Xs], Sorted) :-
    isort(Xs, SortedXs),
    insert_score(X, SortedXs, Sorted).

insert_score(X, [], [X]).
insert_score(score(N1, S1), [score(N2, S2)|Rest], [score(N1, S1), score(N2, S2)|Rest]) :-
    S1 >= S2, !.
insert_score(X, [Y|Rest], [Y|NewRest]) :-
    insert_score(X, Rest, NewRest).

print_ranking([], _).
print_ranking([score(Name, Followers)|Rest], Rank) :-
    write('  #'), write(Rank), write(' '),
    write(Name), write(' - '), write(Followers),
    write(' followers'), nl,
    Rank1 is Rank + 1,
    print_ranking(Rest, Rank1).

% ---- Who can person X reach? (potential audience) ----

audience_reach(Person) :-
    social_network(G),
    graph_reachable(G, Person, Reachable),
    length(Reachable, Size),
    Audience is Size - 1,  % Exclude self
    write(Person), write(' can reach '), write(Audience),
    write(' people:'), nl,
    exclude_self(Person, Reachable, Others),
    write('  '), write(Others), nl.

exclude_self(_, [], []).
exclude_self(P, [P|Rest], Result) :- !, exclude_self(P, Rest, Result).
exclude_self(P, [X|Rest], [X|Result]) :- exclude_self(P, Rest, Result).

% Usage:
% ?- find_connection(henry, grace).
% Connection from henry to grace:
%   henry -> alice -> carol -> eve -> grace
% Degrees of separation: 4
%
% ?- detect_communities.
% === Community Detection ===
% Community 1: [alice, bob, carol, dave, eve, frank, grace, henry, iris]
%   Size: 9 members
%
% ?- mutual_followers.
% === Mutual Followers ===
%   alice <-> bob
%   alice <-> carol
%   carol <-> dave
%   eve <-> frank
%   frank <-> grace
%
% ?- influence_ranking.
% === Influence Ranking (by follower count) ===
%   #1 alice - 3 followers
%   #2 carol - 2 followers
%   #3 eve - 2 followers
%   #4 bob - 2 followers
%   ...
%
% ?- audience_reach(carol).
% carol can reach 6 people:
%   [alice, bob, dave, eve, frank, grace]
```

---

### Example 2: Build Dependency Resolver

Model software package dependencies as a directed graph, resolve build order via topological sort, and detect circular dependencies.

```prolog
% ============================================================
% Package Dependency Resolver
% Models package dependencies, resolves build order using
% topological sort, and detects circular dependencies.
% ============================================================

:- dynamic package_dep/2.  % package_dep(Package, DependsOn)
:- dynamic package_info/3. % package_info(Name, Version, Description)

% ---- Define a project's dependencies ----

setup_web_project :-
    % Package metadata
    assert(package_info(express, '4.18', 'Web framework')),
    assert(package_info(lodash, '4.17', 'Utility library')),
    assert(package_info(mongoose, '7.0', 'MongoDB ODM')),
    assert(package_info(passport, '0.6', 'Authentication')),
    assert(package_info(helmet, '7.0', 'Security headers')),
    assert(package_info(cors, '2.8', 'CORS middleware')),
    assert(package_info(dotenv, '16.0', 'Environment vars')),
    assert(package_info(winston, '3.8', 'Logging')),
    assert(package_info(jest, '29.0', 'Test framework')),
    assert(package_info(supertest, '6.3', 'HTTP testing')),
    assert(package_info(app_server, '1.0', 'Main application')),
    assert(package_info(app_auth, '1.0', 'Auth module')),
    assert(package_info(app_db, '1.0', 'Database module')),
    assert(package_info(app_api, '1.0', 'API routes')),
    assert(package_info(app_tests, '1.0', 'Test suite')),

    % Dependencies: edge(A, B) means A depends on B (B must be built first)
    assert(package_dep(app_server, express)),
    assert(package_dep(app_server, helmet)),
    assert(package_dep(app_server, cors)),
    assert(package_dep(app_server, dotenv)),
    assert(package_dep(app_server, winston)),
    assert(package_dep(app_server, app_auth)),
    assert(package_dep(app_server, app_api)),
    assert(package_dep(app_server, app_db)),
    assert(package_dep(app_auth, passport)),
    assert(package_dep(app_auth, express)),
    assert(package_dep(app_db, mongoose)),
    assert(package_dep(app_db, dotenv)),
    assert(package_dep(app_api, express)),
    assert(package_dep(app_api, lodash)),
    assert(package_dep(app_api, app_auth)),
    assert(package_dep(app_api, app_db)),
    assert(package_dep(app_tests, jest)),
    assert(package_dep(app_tests, supertest)),
    assert(package_dep(app_tests, app_server)).

% ---- Build the dependency graph from asserted facts ----

build_dep_graph(Graph) :-
    findall(edge(Pkg, Dep), package_dep(Pkg, Dep), Edges),
    Graph = graph(Edges).

% ---- Resolve build order ----

resolve_build_order :-
    build_dep_graph(G),
    write('=== Resolving Build Order ==='), nl,
    (   topological_sort(G, Order)
    ->  write('Build order (install in this sequence):'), nl,
        print_build_steps(Order, 1),
        length(Order, Total),
        write('Total packages: '), write(Total), nl
    ;   write('ERROR: Circular dependency detected!'), nl,
        write('Cannot determine a valid build order.'), nl,
        write('Checking for cycles...'), nl,
        find_cycle_info(G)
    ).

print_build_steps([], _).
print_build_steps([Pkg|Rest], Step) :-
    (   package_info(Pkg, Version, Desc)
    ->  write('  '), write(Step), write('. '), write(Pkg),
        write('@'), write(Version),
        write(' ('), write(Desc), write(')'), nl
    ;   write('  '), write(Step), write('. '), write(Pkg), nl
    ),
    Step1 is Step + 1,
    print_build_steps(Rest, Step1).

% ---- Check for circular dependencies ----

check_circular_deps :-
    build_dep_graph(G),
    write('=== Circular Dependency Check ==='), nl,
    (   graph_has_cycle(G)
    ->  write('WARNING: Circular dependencies found!'), nl,
        find_cycle_info(G)
    ;   write('OK: No circular dependencies.'), nl
    ).

find_cycle_info(G) :-
    graph_vertices(G, Vertices),
    find_cycle_in_vertices(G, Vertices).

find_cycle_in_vertices(_, []).
find_cycle_in_vertices(G, [V|Rest]) :-
    graph_reachable(G, V, Reachable),
    (   member(V, Reachable),
        % Check if V can reach itself through dependencies
        graph_path(G, V, V, CyclePath)
    ->  write('  Cycle involving: '), write(V), nl,
        write('    Path: '), write(CyclePath), nl
    ;   true
    ),
    find_cycle_in_vertices(G, Rest).

% ---- Find all dependencies of a package (transitive) ----

all_dependencies(Package) :-
    build_dep_graph(G),
    graph_reachable(G, Package, AllReachable),
    exclude_self(Package, AllReachable, Deps),
    write('All dependencies of '), write(Package), write(':'), nl,
    print_dep_tree(Deps).

exclude_self(_, [], []).
exclude_self(P, [P|R], Result) :- !, exclude_self(P, R, Result).
exclude_self(P, [X|R], [X|Result]) :- exclude_self(P, R, Result).

print_dep_tree([]) :- write('  (none)'), nl.
print_dep_tree(Deps) :-
    Deps \= [],
    classify_deps(Deps, Direct, Transitive),
    write('  Direct: '), write(Direct), nl,
    write('  Transitive: '), write(Transitive), nl,
    length(Deps, Total),
    write('  Total: '), write(Total), nl.

classify_deps([], [], []).
classify_deps([D|Rest], [D|Direct], Transitive) :-
    package_dep(_, D), !,  % D is a direct dependency of something
    classify_deps(Rest, Direct, Transitive).
classify_deps([D|Rest], Direct, [D|Transitive]) :-
    classify_deps(Rest, Direct, Transitive).

% ---- Analyze what would break if a package is removed ----

impact_analysis(Package) :-
    write('=== Impact Analysis for removing '), write(Package), write(' ==='), nl,
    build_dep_graph(graph(Edges)),
    % Find all packages that (transitively) depend on this one
    % Build reverse graph
    findall(edge(Dep, Pkg), member(edge(Pkg, Dep), Edges), ReverseEdges),
    ReverseGraph = graph(ReverseEdges),
    graph_reachable(ReverseGraph, Package, Dependents),
    exclude_self(Package, Dependents, AffectedPkgs),
    (   AffectedPkgs = []
    ->  write('  No packages depend on '), write(Package), nl
    ;   write('  Affected packages: '), write(AffectedPkgs), nl,
        length(AffectedPkgs, Count),
        write('  Total affected: '), write(Count), nl
    ).

% Usage:
% ?- setup_web_project.
% ?- resolve_build_order.
% === Resolving Build Order ===
% Build order (install in this sequence):
%   1. cors@2.8 (CORS middleware)
%   2. dotenv@16.0 (Environment vars)
%   3. express@4.18 (Web framework)
%   4. helmet@7.0 (Security headers)
%   5. jest@29.0 (Test framework)
%   6. lodash@4.17 (Utility library)
%   7. mongoose@7.0 (MongoDB ODM)
%   8. passport@0.6 (Authentication)
%   9. supertest@6.3 (HTTP testing)
%   10. winston@3.8 (Logging)
%   11. app_auth@1.0 (Auth module)
%   12. app_db@1.0 (Database module)
%   13. app_api@1.0 (API routes)
%   14. app_server@1.0 (Main application)
%   15. app_tests@1.0 (Test suite)
% Total packages: 15
%
% ?- check_circular_deps.
% === Circular Dependency Check ===
% OK: No circular dependencies.
%
% ?- all_dependencies(app_api).
% All dependencies of app_api:
%   Direct: [express, lodash, passport, mongoose, dotenv]
%   Transitive: [app_auth, app_db]
%   Total: 7
%
% ?- impact_analysis(express).
% === Impact Analysis for removing express ===
%   Affected packages: [app_api, app_auth, app_server, app_tests]
%   Total affected: 4
```

---

### Example 3: Network Infrastructure -- MST for Cable Layout and Critical Nodes

Model a physical network of buildings, compute the minimum spanning tree for optimal cabling, and identify critical connection nodes.

```prolog
% ============================================================
% Network Infrastructure Planner
% Uses weighted graph to model physical distances between
% buildings. Computes MST for optimal cable layout and
% identifies critical nodes whose failure disconnects the network.
% ============================================================

% ---- Campus network: buildings with cable distances in meters ----

campus_network(graph([
    edge(admin,     library,    120),
    edge(admin,     science,    200),
    edge(library,   science,    150),
    edge(library,   arts,       180),
    edge(science,   lab,        80),
    edge(science,   datacenter, 100),
    edge(lab,       datacenter, 60),
    edge(arts,      dormitory,  250),
    edge(dormitory, cafeteria,  90),
    edge(cafeteria, gym,        110),
    edge(gym,       admin,      300),
    edge(datacenter,cafeteria,  220),
    edge(arts,      gym,        170)
])).

% ---- Compute optimal cable layout (MST) ----

plan_cable_layout :-
    campus_network(G),
    write('=== Optimal Cable Layout (Minimum Spanning Tree) ==='), nl,
    minimum_spanning_tree(G, MST),
    graph_edges(graph(MST), Edges),
    total_cable_length(Edges, 0, Total),
    nl,
    write('Total cable needed: '), write(Total), write(' meters'), nl,
    nl,
    % Compare with total of all possible connections
    graph_edges(G, AllEdges),
    total_cable_length(AllEdges, 0, AllTotal),
    Savings is AllTotal - Total,
    write('Cable saved vs full mesh: '), write(Savings), write(' meters'), nl.

total_cable_length([], Acc, Acc).
total_cable_length([edge(A, B, W)|Rest], Acc, Total) :-
    write('  '), write(A), write(' <-> '), write(B),
    write(': '), write(W), write('m'), nl,
    NewAcc is Acc + W,
    total_cable_length(Rest, NewAcc, Total).
total_cable_length([edge(A, B)|Rest], Acc, Total) :-
    write('  '), write(A), write(' <-> '), write(B), nl,
    NewAcc is Acc + 1,
    total_cable_length(Rest, NewAcc, Total).

% ---- Identify critical nodes ----
% A critical node is one whose removal disconnects the network.

find_critical_nodes :-
    campus_network(G),
    graph_vertices(G, Vertices),
    write('=== Critical Node Analysis ==='), nl,
    find_critical(G, Vertices, CriticalNodes),
    (   CriticalNodes = []
    ->  write('No single point of failure found. Network is robust.'), nl
    ;   write('Critical nodes (single points of failure):'), nl,
        print_critical_nodes(CriticalNodes)
    ).

find_critical(_, [], []).
find_critical(G, [V|Rest], Result) :-
    remove_vertex_edges(G, V, ReducedG),
    graph_vertices(ReducedG, RemainingVerts),
    (   RemainingVerts = []
    ->  find_critical(G, Rest, Result)
    ;   RemainingVerts = [First|_],
        graph_components(ReducedG, Components),
        length(Components, NumComponents),
        (   NumComponents > 1
        ->  Result = [critical(V, NumComponents)|RestResult],
            find_critical(G, Rest, RestResult)
        ;   find_critical(G, Rest, Result)
        )
    ).

remove_vertex_edges(graph(Edges), V, graph(Filtered)) :-
    exclude_vertex(Edges, V, Filtered).

exclude_vertex([], _, []).
exclude_vertex([edge(A, B)|Rest], V, Filtered) :-
    (A = V ; B = V), !,
    exclude_vertex(Rest, V, Filtered).
exclude_vertex([edge(A, B, W)|Rest], V, Filtered) :-
    (A = V ; B = V), !,
    exclude_vertex(Rest, V, Filtered).
exclude_vertex([E|Rest], V, [E|Filtered]) :-
    exclude_vertex(Rest, V, Filtered).

print_critical_nodes([]).
print_critical_nodes([critical(Node, Parts)|Rest]) :-
    write('  '), write(Node),
    write(' - removal creates '), write(Parts),
    write(' disconnected segments'), nl,
    print_critical_nodes(Rest).

% ---- Find the shortest network path between two buildings ----

network_route(From, To) :-
    campus_network(G),
    (   shortest_path(G, From, To, Path)
    ->  write('Shortest route from '), write(From),
        write(' to '), write(To), write(':'), nl,
        write('  Path: '), write(Path), nl,
        compute_path_cost(G, Path, Cost),
        write('  Total distance: '), write(Cost), write(' meters'), nl
    ;   write('No route from '), write(From),
        write(' to '), write(To), nl
    ).

compute_path_cost(_, [_], 0).
compute_path_cost(G, [A, B|Rest], Cost) :-
    graph_edges(G, Edges),
    (   member(edge(A, B, W), Edges)
    ->  true
    ;   member(edge(B, A, W), Edges)
    ->  true
    ;   W = 0
    ),
    compute_path_cost(G, [B|Rest], RestCost),
    Cost is W + RestCost.

% ---- Network coverage report ----

network_report :-
    campus_network(G),
    graph_vertices(G, Vertices),
    graph_edges(G, Edges),
    length(Vertices, VCount),
    length(Edges, ECount),
    write('=== Network Report ==='), nl,
    write('Buildings: '), write(VCount), nl,
    write('Connections: '), write(ECount), nl,
    nl,
    write('Connectivity per building:'), nl,
    report_degrees(G, Vertices),
    nl,
    (   graph_has_cycle(G)
    ->  write('Redundant paths: YES (network has cycles)'), nl
    ;   write('Redundant paths: NO (tree topology)'), nl
    ).

report_degrees(_, []).
report_degrees(G, [V|Rest]) :-
    graph_degree(G, V, Deg),
    graph_neighbors(G, V, Neighbors),
    write('  '), write(V), write(': degree='), write(Deg),
    write(', connects to '), write(Neighbors), nl,
    report_degrees(G, Rest).

% Usage:
% ?- plan_cable_layout.
% === Optimal Cable Layout (Minimum Spanning Tree) ===
%   lab <-> datacenter: 60m
%   science <-> lab: 80m
%   dormitory <-> cafeteria: 90m
%   science <-> datacenter: 100m
%   admin <-> library: 120m
%   library <-> science: 150m
%   arts <-> gym: 170m
%   library <-> arts: 180m
%
% Total cable needed: 950 meters
% Cable saved vs full mesh: 1080 meters
%
% ?- find_critical_nodes.
% === Critical Node Analysis ===
% Critical nodes (single points of failure):
%   library - removal creates 2 disconnected segments
%   science - removal creates 2 disconnected segments
%
% ?- network_route(admin, cafeteria).
% Shortest route from admin to cafeteria:
%   Path: [admin, library, science, datacenter, cafeteria]
%   Total distance: 590 meters
```

---

### Example 4: Route Planner -- City Road Network with Distances

A complete route planner for a city road network that finds shortest paths, alternative routes, and travel time estimates.

```prolog
% ============================================================
% City Route Planner
% Models a city road network with distances in km.
% Provides shortest path, alternative routes, and travel time
% estimates based on average speed per road type.
% ============================================================

% ---- Road network: intersections connected by roads ----
% edge(From, To, DistanceKm)

city_roads(graph([
    % Downtown grid
    edge(central_station, city_hall,    1.2),
    edge(city_hall,       market,       0.8),
    edge(market,          harbor,       1.5),
    edge(central_station, museum,       0.9),
    edge(museum,          park,         1.1),
    edge(park,            harbor,       2.0),
    edge(city_hall,       park,         1.4),

    % Suburban roads
    edge(central_station, north_suburb, 5.5),
    edge(north_suburb,    university,   2.3),
    edge(university,      tech_park,    1.8),
    edge(north_suburb,    mall,         3.1),
    edge(mall,            airport,      8.0),

    % Ring road (bypass)
    edge(harbor,          south_bridge, 3.2),
    edge(south_bridge,    airport,      12.0),
    edge(central_station, ring_north,   4.0),
    edge(ring_north,      ring_east,    6.5),
    edge(ring_east,       airport,      7.0),
    edge(ring_north,      north_suburb, 2.8),

    % Hospital and emergency routes
    edge(city_hall,       hospital,     1.0),
    edge(hospital,        university,   3.5),
    edge(park,            hospital,     2.2)
])).

% ---- Find the shortest route ----

find_route(From, To) :-
    city_roads(G),
    write('=== Route: '), write(From), write(' -> '), write(To),
    write(' ==='), nl,
    (   shortest_path(G, From, To, Path)
    ->  write('Shortest path: '), write(Path), nl,
        calculate_distance(G, Path, Dist),
        write('Distance: '), write(Dist), write(' km'), nl,
        EstTime is Dist / 40.0 * 60.0,  % 40 km/h average
        write('Estimated time: '), write_minutes(EstTime), nl
    ;   write('No route found.'), nl
    ).

calculate_distance(_, [_], 0.0).
calculate_distance(G, [A, B|Rest], Dist) :-
    graph_edges(G, Edges),
    find_edge_weight(A, B, Edges, W),
    calculate_distance(G, [B|Rest], RestDist),
    Dist is W + RestDist.

find_edge_weight(A, B, [edge(A, B, W)|_], W) :- !.
find_edge_weight(A, B, [edge(B, A, W)|_], W) :- !.
find_edge_weight(A, B, [_|Rest], W) :- find_edge_weight(A, B, Rest, W).

write_minutes(Minutes) :-
    Mins is truncate(Minutes),
    Secs is truncate((Minutes - Mins) * 60),
    write(Mins), write(' min '), write(Secs), write(' sec').

% ---- Find all reachable destinations from a location ----

reachable_from(Location) :-
    city_roads(G),
    graph_reachable(G, Location, Reachable),
    exclude_self(Location, Reachable, Destinations),
    length(Destinations, Count),
    write('From '), write(Location), write(', you can reach '),
    write(Count), write(' locations:'), nl,
    print_destinations(G, Location, Destinations).

exclude_self(_, [], []).
exclude_self(P, [P|R], Res) :- !, exclude_self(P, R, Res).
exclude_self(P, [X|R], [X|Res]) :- exclude_self(P, R, Res).

print_destinations(_, _, []).
print_destinations(G, From, [Dest|Rest]) :-
    (   shortest_path(G, From, Dest, Path)
    ->  calculate_distance(G, Path, Dist),
        write('  '), write(Dest), write(' - '),
        write(Dist), write(' km'), nl
    ;   write('  '), write(Dest), write(' - unreachable directly'), nl
    ),
    print_destinations(G, From, Rest).

% ---- Find nearest hospital ----

nearest_hospital(Location) :-
    city_roads(G),
    findall(
        dist(Dist, Path),
        (   shortest_path(G, Location, hospital, Path),
            calculate_distance(G, Path, Dist)
        ),
        Routes
    ),
    (   Routes = [dist(Dist, Path)|_]
    ->  write('Nearest hospital from '), write(Location), write(':'), nl,
        write('  Route: '), write(Path), nl,
        write('  Distance: '), write(Dist), write(' km'), nl,
        EmergTime is Dist / 60.0 * 60.0,  % 60 km/h emergency speed
        write('  Emergency response time: '),
        write_minutes(EmergTime), nl
    ;   write('No route to hospital from '), write(Location), nl
    ).

% ---- Network analysis ----

road_network_analysis :-
    city_roads(G),
    graph_vertices(G, Vertices),
    graph_edges(G, Edges),
    length(Vertices, VCount),
    length(Edges, ECount),
    write('=== City Road Network Analysis ==='), nl,
    write('Intersections: '), write(VCount), nl,
    write('Road segments: '), write(ECount), nl,
    nl,
    write('Hub analysis (by connectivity):'), nl,
    find_hubs(G, Vertices),
    nl,
    (   graph_has_cycle(G)
    ->  write('Network has alternative routes (cycles present).'), nl
    ;   write('WARNING: No alternative routes. Single path network!'), nl
    ).

find_hubs(_, []).
find_hubs(G, [V|Rest]) :-
    graph_degree(G, V, Deg),
    (   Deg >= 4
    ->  write('  MAJOR HUB: '), write(V),
        write(' ('), write(Deg), write(' connections)'), nl
    ;   Deg >= 3
    ->  write('  Minor hub: '), write(V),
        write(' ('), write(Deg), write(' connections)'), nl
    ;   true
    ),
    find_hubs(G, Rest).

% Usage:
% ?- find_route(central_station, airport).
% === Route: central_station -> airport ===
% Shortest path: [central_station, north_suburb, mall, airport]
% Distance: 16.6 km
% Estimated time: 24 min 54 sec
%
% ?- nearest_hospital(harbor).
% Nearest hospital from harbor:
%   Route: [harbor, park, hospital]
%   Distance: 4.2 km
%   Emergency response time: 4 min 12 sec
%
% ?- road_network_analysis.
% === City Road Network Analysis ===
% Intersections: 13
% Road segments: 21
% Hub analysis (by connectivity):
%   MAJOR HUB: central_station (4 connections)
%   MAJOR HUB: north_suburb (4 connections)
%   Minor hub: city_hall (3 connections)
%   Minor hub: park (3 connections)
% Network has alternative routes (cycles present).
```

---

### Example 5: Course Prerequisite Checker

Model university courses as a directed acyclic graph, compute valid study plans via topological sort, and detect impossible prerequisite chains.

```prolog
% ============================================================
% Course Prerequisite Checker
% Models university course dependencies as a DAG.
% Provides: valid study order, prerequisite checking,
% semester planning, and detection of circular prerequisites.
% ============================================================

:- dynamic course/3.      % course(Code, Name, Credits)
:- dynamic prerequisite/2. % prerequisite(Course, RequiredCourse)

% ---- Computer Science curriculum ----

setup_cs_curriculum :-
    % Course catalog
    assert(course(cs101, 'Intro to CS', 3)),
    assert(course(cs102, 'Programming I', 3)),
    assert(course(cs201, 'Data Structures', 4)),
    assert(course(cs202, 'Programming II', 3)),
    assert(course(cs210, 'Computer Architecture', 3)),
    assert(course(cs301, 'Algorithms', 4)),
    assert(course(cs302, 'Operating Systems', 3)),
    assert(course(cs310, 'Databases', 3)),
    assert(course(cs320, 'Software Engineering', 3)),
    assert(course(cs401, 'Machine Learning', 4)),
    assert(course(cs410, 'Distributed Systems', 3)),
    assert(course(cs420, 'Compilers', 4)),
    assert(course(cs499, 'Senior Project', 6)),
    assert(course(math101, 'Calculus I', 4)),
    assert(course(math201, 'Linear Algebra', 3)),
    assert(course(math301, 'Probability & Stats', 3)),

    % Prerequisites: prerequisite(Course, Required)
    % "Course requires Required to be completed first"
    assert(prerequisite(cs102, cs101)),
    assert(prerequisite(cs201, cs102)),
    assert(prerequisite(cs202, cs102)),
    assert(prerequisite(cs210, cs101)),
    assert(prerequisite(cs301, cs201)),
    assert(prerequisite(cs301, math201)),
    assert(prerequisite(cs302, cs210)),
    assert(prerequisite(cs302, cs201)),
    assert(prerequisite(cs310, cs201)),
    assert(prerequisite(cs320, cs202)),
    assert(prerequisite(cs320, cs201)),
    assert(prerequisite(cs401, cs301)),
    assert(prerequisite(cs401, math301)),
    assert(prerequisite(cs410, cs302)),
    assert(prerequisite(cs410, cs310)),
    assert(prerequisite(cs420, cs301)),
    assert(prerequisite(cs420, cs210)),
    assert(prerequisite(cs499, cs320)),
    assert(prerequisite(cs499, cs301)),
    assert(prerequisite(math201, math101)),
    assert(prerequisite(math301, math201)).

% ---- Build prerequisite graph ----

prereq_graph(Graph) :-
    findall(edge(Course, Prereq), prerequisite(Course, Prereq), Edges),
    Graph = graph(Edges).

% ---- Check if prerequisites are satisfiable (no cycles) ----

check_curriculum_validity :-
    prereq_graph(G),
    write('=== Curriculum Validity Check ==='), nl,
    (   graph_has_cycle(G)
    ->  write('ERROR: Circular prerequisites detected!'), nl,
        write('The curriculum has impossible requirements.'), nl
    ;   write('OK: No circular prerequisites.'), nl,
        write('All courses can be completed.'), nl
    ),
    nl,
    topological_sort(G, Order),
    write('Valid completion order (one possible sequence):'), nl,
    print_course_sequence(Order, 1).

print_course_sequence([], _).
print_course_sequence([Code|Rest], N) :-
    (   course(Code, Name, Credits)
    ->  write('  '), write(N), write('. '),
        write(Code), write(' - '), write(Name),
        write(' ('), write(Credits), write(' credits)'), nl
    ;   write('  '), write(N), write('. '), write(Code), nl
    ),
    N1 is N + 1,
    print_course_sequence(Rest, N1).

% ---- Check if a student can take a specific course ----

can_take(Course, CompletedCourses) :-
    findall(Prereq, prerequisite(Course, Prereq), Prerequisites),
    (   Prerequisites = []
    ->  write(Course), write(' has no prerequisites. You can take it!'), nl
    ;   check_prerequisites(Course, Prerequisites, CompletedCourses)
    ).

check_prerequisites(Course, Prerequisites, Completed) :-
    subtract(Prerequisites, Completed, Missing),
    (   Missing = []
    ->  write('You meet all prerequisites for '), write(Course), write('.'), nl
    ;   write('Cannot take '), write(Course),
        write('. Missing prerequisites:'), nl,
        print_missing(Missing)
    ).

print_missing([]).
print_missing([Code|Rest]) :-
    (   course(Code, Name, _)
    ->  write('  - '), write(Code), write(': '), write(Name), nl
    ;   write('  - '), write(Code), nl
    ),
    print_missing(Rest).

subtract([], _, []).
subtract([H|T], L2, Result) :-
    (   member(H, L2)
    ->  subtract(T, L2, Result)
    ;   Result = [H|Rest], subtract(T, L2, Rest)
    ).

% ---- Generate a semester plan ----
% Assign courses to semesters, respecting prerequisites and a
% maximum credit load per semester.

generate_study_plan(MaxCreditsPerSem) :-
    prereq_graph(G),
    (   topological_sort(G, Order)
    ->  write('=== Study Plan (max '), write(MaxCreditsPerSem),
        write(' credits/semester) ==='), nl,
        assign_semesters(Order, MaxCreditsPerSem, 1, [])
    ;   write('Cannot generate plan: circular prerequisites.'), nl
    ).

assign_semesters([], _, _, _) :- nl.
assign_semesters(Remaining, MaxCred, SemNum, Completed) :-
    Remaining \= [],
    write('Semester '), write(SemNum), write(':'), nl,
    select_courses(Remaining, MaxCred, Completed, Selected, Rest, 0),
    (   Selected = []
    ->  write('  ERROR: deadlock in scheduling.'), nl
    ;   print_semester_courses(Selected, 0, TotalCred),
        write('  Credits: '), write(TotalCred), nl, nl,
        append(Completed, Selected, NewCompleted),
        SemNum1 is SemNum + 1,
        assign_semesters(Rest, MaxCred, SemNum1, NewCompleted)
    ).

select_courses([], _, _, [], [], _).
select_courses([C|Rest], MaxCred, Completed, [C|Selected], Remaining, AccCred) :-
    % Check if all prerequisites are completed
    findall(P, prerequisite(C, P), Prereqs),
    all_completed(Prereqs, Completed),
    % Check credit limit
    (   course(C, _, Credits) -> true ; Credits = 3),
    NewAcc is AccCred + Credits,
    NewAcc =< MaxCred,
    !,
    select_courses(Rest, MaxCred, Completed, Selected, Remaining, NewAcc).
select_courses([C|Rest], MaxCred, Completed, Selected, [C|Remaining], AccCred) :-
    select_courses(Rest, MaxCred, Completed, Selected, Remaining, AccCred).

all_completed([], _).
all_completed([P|Rest], Completed) :-
    member(P, Completed),
    all_completed(Rest, Completed).

print_semester_courses([], Acc, Acc).
print_semester_courses([Code|Rest], Acc, Total) :-
    (   course(Code, Name, Credits)
    ->  write('  '), write(Code), write(' - '), write(Name),
        write(' ('), write(Credits), write(' cr)'), nl,
        NewAcc is Acc + Credits
    ;   write('  '), write(Code), nl, NewAcc is Acc + 3
    ),
    print_semester_courses(Rest, NewAcc, Total).

% ---- What courses does this course unlock? ----

unlocks(Course) :-
    prereq_graph(graph(Edges)),
    % Build reverse graph: edge(Prereq, Course) means completing
    % Prereq moves you closer to taking Course
    findall(edge(Req, Crs), member(edge(Crs, Req), Edges), RevEdges),
    RevGraph = graph(RevEdges),
    graph_reachable(RevGraph, Course, Reachable),
    exclude_self_list(Course, Reachable, Unlocked),
    (   Unlocked = []
    ->  write(Course), write(' is a terminal course (unlocks nothing).'), nl
    ;   write('Completing '), write(Course),
        write(' contributes to unlocking:'), nl,
        print_unlocked(Unlocked)
    ).

exclude_self_list(_, [], []).
exclude_self_list(X, [X|R], Res) :- !, exclude_self_list(X, R, Res).
exclude_self_list(X, [Y|R], [Y|Res]) :- exclude_self_list(X, R, Res).

print_unlocked([]).
print_unlocked([Code|Rest]) :-
    (   course(Code, Name, _)
    ->  write('  -> '), write(Code), write(': '), write(Name), nl
    ;   write('  -> '), write(Code), nl
    ),
    print_unlocked(Rest).

% Usage:
% ?- setup_cs_curriculum.
%
% ?- check_curriculum_validity.
% === Curriculum Validity Check ===
% OK: No circular prerequisites.
% All courses can be completed.
% Valid completion order (one possible sequence):
%   1. cs101 - Intro to CS (3 credits)
%   2. math101 - Calculus I (4 credits)
%   3. cs102 - Programming I (3 credits)
%   4. cs210 - Computer Architecture (3 credits)
%   5. math201 - Linear Algebra (3 credits)
%   ...
%
% ?- can_take(cs301, [cs101, cs102, cs201]).
% Cannot take cs301. Missing prerequisites:
%   - math201: Linear Algebra
%
% ?- can_take(cs301, [cs101, cs102, cs201, math101, math201]).
% You meet all prerequisites for cs301.
%
% ?- generate_study_plan(15).
% === Study Plan (max 15 credits/semester) ===
% Semester 1:
%   cs101 - Intro to CS (3 cr)
%   math101 - Calculus I (4 cr)
%   Credits: 7
%
% Semester 2:
%   cs102 - Programming I (3 cr)
%   cs210 - Computer Architecture (3 cr)
%   math201 - Linear Algebra (3 cr)
%   Credits: 9
%
% Semester 3:
%   cs201 - Data Structures (4 cr)
%   cs202 - Programming II (3 cr)
%   math301 - Probability & Stats (3 cr)
%   Credits: 10
% ...
%
% ?- unlocks(cs201).
% Completing cs201 contributes to unlocking:
%   -> cs301: Algorithms
%   -> cs302: Operating Systems
%   -> cs310: Databases
%   -> cs320: Software Engineering
%   -> cs401: Machine Learning
%   -> cs410: Distributed Systems
%   -> cs420: Compilers
%   -> cs499: Senior Project
```
