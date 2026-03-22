% Test 45: Graph Algorithm Predicates
% Tests: graph_vertices/2, graph_edges/2, graph_neighbors/3, graph_degree/3,
%        graph_path/4, shortest_path/4, graph_connected/2, graph_reachable/3,
%        topological_sort/2, graph_components/2, minimum_spanning_tree/2,
%        graph_has_cycle/1

% ============================================================
% Test framework
% ============================================================
:- dynamic(test_passed/1).
:- dynamic(test_failed/2).
:- dynamic(test_count/1).
:- assert(test_count(0)).

run_test(Name, Goal) :-
    retract(test_count(N)), N1 is N + 1, assert(test_count(N1)),
    copy_term(Goal, GoalCopy),
    ( catch(call(GoalCopy), E, (assert(test_failed(Name, E)), fail))
    -> assert(test_passed(Name)),
       write('  PASS: '), write(Name), nl
    ;  ( \+ test_failed(Name, _) -> assert(test_failed(Name, failed)) ; true ),
       write('  FAIL: '), write(Name), nl
    ).

% Test graphs
simple_graph(graph([edge(a,b), edge(b,c), edge(c,d), edge(a,d)])).
weighted_graph(graph([edge(a,b,1), edge(b,c,2), edge(a,c,5), edge(c,d,1)])).
dag_graph(graph([edge(a,b), edge(a,c), edge(b,d), edge(c,d)])).
cyclic_graph(graph([edge(a,b), edge(b,c), edge(c,a)])).
disconnected_graph(graph([edge(a,b), edge(b,a), edge(c,d), edge(d,c)])).

run_all_tests :-
    write('=== Test 45: Graph Algorithm Predicates ==='), nl, nl,
    test_vertices_edges,
    test_neighbors_degree,
    test_paths,
    test_connectivity,
    test_topological,
    test_components,
    test_mst,
    test_cycles,
    nl, write('--- Results ---'), nl,
    aggregate_all(count, test_passed(_), Passed),
    aggregate_all(count, test_failed(_, _), Failed),
    test_count(Total),
    write('Passed: '), write(Passed), write('/'), write(Total), nl,
    write('Failed: '), write(Failed), nl,
    ( Failed > 0
    -> forall(test_failed(N, R), (write('  '), write(N), write(': '), write(R), nl))
    ; true
    ).

% ============================================================
% 1. Vertices and Edges
% ============================================================
test_vertices_edges :-
    write('--- Vertices/Edges ---'), nl,
    run_test(vertices_simple,
        (simple_graph(G), graph_vertices(G, V), is_list(V), length(V, 4))),
    run_test(vertices_sorted,
        (simple_graph(G), graph_vertices(G, V), V == [a, b, c, d])),
    run_test(edges_simple,
        (simple_graph(G), graph_edges(G, E), is_list(E), length(E, 4))),
    run_test(edges_weighted,
        (weighted_graph(G), graph_edges(G, E), length(E, 4))).

% ============================================================
% 2. Neighbors and Degree
% ============================================================
test_neighbors_degree :-
    write('--- Neighbors/Degree ---'), nl,
    run_test(neighbors_a,
        (simple_graph(G), graph_neighbors(G, a, N), is_list(N))),
    run_test(neighbors_a_has_b,
        (simple_graph(G), graph_neighbors(G, a, N), member(b, N))),
    run_test(degree_a,
        (simple_graph(G), graph_degree(G, a, D), D == 2)),
    run_test(degree_b,
        (simple_graph(G), graph_degree(G, b, D), number(D), D >= 1)).

% ============================================================
% 3. Path finding
% ============================================================
test_paths :-
    write('--- Paths ---'), nl,
    run_test(path_direct,
        (simple_graph(G), graph_path(G, a, b, P), is_list(P))),
    run_test(path_transitive,
        (simple_graph(G), graph_path(G, a, d, P), is_list(P))),
    run_test(path_includes_endpoints,
        (simple_graph(G), graph_path(G, a, b, P),
         P = [a|_], last(P, b))),
    run_test(shortest_path_weighted,
        (weighted_graph(G), shortest_path(G, a, d, P), is_list(P))),
    run_test(shortest_path_direct_vs_indirect,
        (weighted_graph(G), shortest_path(G, a, c, P),
         is_list(P))).

% ============================================================
% 4. Connectivity and Reachability
% ============================================================
test_connectivity :-
    write('--- Connectivity ---'), nl,
    run_test(reachable_from_a,
        (simple_graph(G), graph_reachable(G, a, R), is_list(R))),
    run_test(reachable_includes_start,
        (simple_graph(G), graph_reachable(G, a, R), member(a, R))),
    run_test(reachable_all_from_a,
        (simple_graph(G), graph_reachable(G, a, R), length(R, Len), Len >= 2)).

% ============================================================
% 5. Topological Sort
% ============================================================
test_topological :-
    write('--- Topological Sort ---'), nl,
    run_test(topo_sort_dag,
        (dag_graph(G), topological_sort(G, S), is_list(S))),
    run_test(topo_sort_length,
        (dag_graph(G), topological_sort(G, S), length(S, 4))),
    run_test(topo_sort_a_first,
        (dag_graph(G), topological_sort(G, S),
         S = [a|_])),
    run_test(topo_sort_cyclic_fails,
        (cyclic_graph(G), \+ topological_sort(G, _))).

% ============================================================
% 6. Connected Components
% ============================================================
test_components :-
    write('--- Components ---'), nl,
    run_test(components_connected,
        (simple_graph(G), graph_components(G, C), is_list(C))),
    run_test(components_disconnected,
        (disconnected_graph(G), graph_components(G, C),
         is_list(C), length(C, 2))).

% ============================================================
% 7. Minimum Spanning Tree
% ============================================================
test_mst :-
    write('--- MST ---'), nl,
    run_test(mst_weighted,
        (weighted_graph(G), minimum_spanning_tree(G, MST), is_list(MST))),
    run_test(mst_edge_count,
        (weighted_graph(G), minimum_spanning_tree(G, MST),
         graph_vertices(G, V), length(V, NV), length(MST, NE),
         NE =:= NV - 1)).

% ============================================================
% 8. Cycle Detection
% ============================================================
test_cycles :-
    write('--- Cycles ---'), nl,
    run_test(has_cycle_true,
        (cyclic_graph(G), graph_has_cycle(G))),
    run_test(has_cycle_false,
        (dag_graph(G), \+ graph_has_cycle(G))).

:- run_all_tests.
