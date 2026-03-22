// START_CHANGE: ISS-2025-0127 - Graph algorithms package
package it.denzosoft.jprolog.builtin.graph;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Graph algorithm predicates for JProlog.
 * <p>
 * Graphs are represented as adjacency lists:
 * {@code graph([edge(a,b,1), edge(b,c,2), edge(a,c,5)])} for weighted graphs,
 * or {@code graph([edge(a,b), edge(b,c)])} for unweighted graphs.
 */
public class GraphPredicates implements BuiltIn {

    public enum OperationType {
        GRAPH_PATH, SHORTEST_PATH, GRAPH_CONNECTED, GRAPH_VERTICES,
        GRAPH_EDGES, GRAPH_NEIGHBORS, TOPOLOGICAL_SORT, GRAPH_COMPONENTS,
        MINIMUM_SPANNING_TREE, GRAPH_DEGREE, GRAPH_HAS_CYCLE, GRAPH_REACHABLE,
        // START_CHANGE: ISS-2025-0176 - Add SCC (Strongly Connected Components) via Tarjan's algorithm
        GRAPH_SCC
        // END_CHANGE: ISS-2025-0176
    }

    private final OperationType operationType;

    public GraphPredicates(OperationType operationType) {
        this.operationType = operationType;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        switch (operationType) {
            case GRAPH_PATH:
                return executeGraphPath(query, bindings, solutions);
            case SHORTEST_PATH:
                return executeShortestPath(query, bindings, solutions);
            case GRAPH_CONNECTED:
                return executeGraphConnected(query, bindings, solutions);
            case GRAPH_VERTICES:
                return executeGraphVertices(query, bindings, solutions);
            case GRAPH_EDGES:
                return executeGraphEdges(query, bindings, solutions);
            case GRAPH_NEIGHBORS:
                return executeGraphNeighbors(query, bindings, solutions);
            case TOPOLOGICAL_SORT:
                return executeTopologicalSort(query, bindings, solutions);
            case GRAPH_COMPONENTS:
                return executeGraphComponents(query, bindings, solutions);
            case MINIMUM_SPANNING_TREE:
                return executeMinimumSpanningTree(query, bindings, solutions);
            case GRAPH_DEGREE:
                return executeGraphDegree(query, bindings, solutions);
            case GRAPH_HAS_CYCLE:
                return executeGraphHasCycle(query, bindings, solutions);
            case GRAPH_REACHABLE:
                return executeGraphReachable(query, bindings, solutions);
            // START_CHANGE: ISS-2025-0176 - SCC dispatch
            case GRAPH_SCC:
                return executeGraphScc(query, bindings, solutions);
            // END_CHANGE: ISS-2025-0176
            default:
                return false;
        }
    }

    // ===== Internal Edge representation =====

    private static class Edge {
        final String from;
        final String to;
        final double weight;

        Edge(String from, String to, double weight) {
            this.from = from;
            this.to = to;
            this.weight = weight;
        }
    }

    // ===== Graph parsing helpers =====

    /**
     * Parse a graph term into an adjacency list.
     * Accepts: graph([edge(a,b), edge(a,b,3), ...]) or just [edge(a,b), ...]
     */
    private List<Edge> parseGraph(Term graphTerm, Map<String, Term> bindings) {
        Term resolved = graphTerm.resolveBindings(bindings);

        // Unwrap graph(...) functor if present
        if (resolved instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) resolved;
            if (ct.getName().equals("graph") && ct.getArguments().size() == 1) {
                resolved = ct.getArguments().get(0).resolveBindings(bindings);
            }
        }

        List<Edge> edges = new ArrayList<>();
        Term current = resolved;
        while (current instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) current;
            if (ct.getName().equals(".") && ct.getArguments().size() == 2) {
                Term head = ct.getArguments().get(0).resolveBindings(bindings);
                edges.add(parseEdge(head));
                current = ct.getArguments().get(1).resolveBindings(bindings);
            } else {
                break;
            }
        }
        return edges;
    }

    private Edge parseEdge(Term edgeTerm) {
        if (!(edgeTerm instanceof CompoundTerm)) {
            throw new PrologEvaluationException("Expected edge/2 or edge/3 term, got: " + edgeTerm);
        }
        CompoundTerm ct = (CompoundTerm) edgeTerm;
        if (!ct.getName().equals("edge")) {
            throw new PrologEvaluationException("Expected edge functor, got: " + ct.getName());
        }
        int arity = ct.getArguments().size();
        if (arity == 2) {
            String from = termToNodeName(ct.getArguments().get(0));
            String to = termToNodeName(ct.getArguments().get(1));
            return new Edge(from, to, 1.0);
        } else if (arity == 3) {
            String from = termToNodeName(ct.getArguments().get(0));
            String to = termToNodeName(ct.getArguments().get(1));
            double weight = termToWeight(ct.getArguments().get(2));
            return new Edge(from, to, weight);
        } else {
            throw new PrologEvaluationException("edge term must have arity 2 or 3, got: " + arity);
        }
    }

    private String termToNodeName(Term t) {
        if (t instanceof Atom) {
            return ((Atom) t).getName();
        } else if (t instanceof Number) {
            double v = ((Number) t).getValue();
            if (v == Math.floor(v) && !Double.isInfinite(v)) {
                return String.valueOf((long) v);
            }
            return String.valueOf(v);
        }
        return t.toString();
    }

    private double termToWeight(Term t) {
        if (t instanceof Number) {
            return ((Number) t).getValue();
        }
        throw new PrologEvaluationException("Edge weight must be a number, got: " + t);
    }

    private Map<String, List<Edge>> buildAdjacencyList(List<Edge> edges) {
        Map<String, List<Edge>> adj = new LinkedHashMap<>();
        for (Edge e : edges) {
            adj.computeIfAbsent(e.from, k -> new ArrayList<>()).add(e);
            adj.computeIfAbsent(e.to, k -> new ArrayList<>());
        }
        return adj;
    }

    private Set<String> getAllVertices(List<Edge> edges) {
        Set<String> vertices = new TreeSet<>();
        for (Edge e : edges) {
            vertices.add(e.from);
            vertices.add(e.to);
        }
        return vertices;
    }

    // ===== Term construction helpers =====

    private Term nodesToList(List<String> nodes) {
        Term list = new Atom("[]");
        for (int i = nodes.size() - 1; i >= 0; i--) {
            list = new CompoundTerm(new Atom("."), Arrays.asList(nodeNameToTerm(nodes.get(i)), list));
        }
        return list;
    }

    private Term nodeNameToTerm(String name) {
        // Try to parse as number
        try {
            long l = Long.parseLong(name);
            return new Number(l);
        } catch (NumberFormatException e1) {
            try {
                double d = Double.parseDouble(name);
                return new Number(d);
            } catch (NumberFormatException e2) {
                return new Atom(name);
            }
        }
    }

    private Term edgesToList(List<Edge> edges) {
        Term list = new Atom("[]");
        for (int i = edges.size() - 1; i >= 0; i--) {
            Edge e = edges.get(i);
            Term edgeTerm;
            if (e.weight == 1.0) {
                edgeTerm = new CompoundTerm(new Atom("edge"),
                        Arrays.asList(nodeNameToTerm(e.from), nodeNameToTerm(e.to)));
            } else {
                edgeTerm = new CompoundTerm(new Atom("edge"),
                        Arrays.asList(nodeNameToTerm(e.from), nodeNameToTerm(e.to), new Number(e.weight)));
            }
            list = new CompoundTerm(new Atom("."), Arrays.asList(edgeTerm, list));
        }
        return list;
    }

    // ===== Predicate implementations =====

    /**
     * graph_path(Graph, Start, End, Path) — DFS path finding with cycle detection.
     */
    private boolean executeGraphPath(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 4) {
            throw new PrologEvaluationException("graph_path/4 requires exactly 4 arguments.");
        }
        Term graphTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term startTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term endTerm = query.getArguments().get(2).resolveBindings(bindings);
        Term pathTerm = query.getArguments().get(3);

        String start = termToNodeName(startTerm);
        String end = termToNodeName(endTerm);
        List<Edge> edges = parseGraph(graphTerm, bindings);
        Map<String, List<Edge>> adj = buildAdjacencyList(edges);

        List<String> path = new ArrayList<>();
        Set<String> visited = new HashSet<>();
        if (dfsPath(adj, start, end, path, visited)) {
            Term pathList = nodesToList(path);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (pathTerm.resolveBindings(bindings).unify(pathList, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
        }
        return false;
    }

    private boolean dfsPath(Map<String, List<Edge>> adj, String current, String end,
                            List<String> path, Set<String> visited) {
        path.add(current);
        visited.add(current);
        if (current.equals(end)) {
            return true;
        }
        List<Edge> neighbors = adj.getOrDefault(current, Collections.emptyList());
        for (Edge e : neighbors) {
            if (!visited.contains(e.to)) {
                if (dfsPath(adj, e.to, end, path, visited)) {
                    return true;
                }
            }
        }
        path.remove(path.size() - 1);
        visited.remove(current);
        return false;
    }

    /**
     * shortest_path(Graph, Start, End, Path) — Dijkstra for weighted, BFS for unweighted.
     */
    private boolean executeShortestPath(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 4) {
            throw new PrologEvaluationException("shortest_path/4 requires exactly 4 arguments.");
        }
        Term graphTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term startTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term endTerm = query.getArguments().get(2).resolveBindings(bindings);
        Term pathTerm = query.getArguments().get(3);

        String start = termToNodeName(startTerm);
        String end = termToNodeName(endTerm);
        List<Edge> edges = parseGraph(graphTerm, bindings);
        Map<String, List<Edge>> adj = buildAdjacencyList(edges);

        // Check if all weights are 1.0 (unweighted) — use BFS
        boolean allUnweighted = edges.stream().allMatch(e -> e.weight == 1.0);

        List<String> path;
        if (allUnweighted) {
            path = bfsShortestPath(adj, start, end);
        } else {
            path = dijkstraShortestPath(adj, start, end);
        }

        if (path != null) {
            Term pathList = nodesToList(path);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (pathTerm.resolveBindings(bindings).unify(pathList, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
        }
        return false;
    }

    private List<String> bfsShortestPath(Map<String, List<Edge>> adj, String start, String end) {
        if (start.equals(end)) {
            return Collections.singletonList(start);
        }
        Queue<String> queue = new LinkedList<>();
        Map<String, String> parent = new HashMap<>();
        queue.add(start);
        parent.put(start, null);

        while (!queue.isEmpty()) {
            String current = queue.poll();
            for (Edge e : adj.getOrDefault(current, Collections.emptyList())) {
                if (!parent.containsKey(e.to)) {
                    parent.put(e.to, current);
                    if (e.to.equals(end)) {
                        return reconstructPath(parent, end);
                    }
                    queue.add(e.to);
                }
            }
        }
        return null;
    }

    private List<String> dijkstraShortestPath(Map<String, List<Edge>> adj, String start, String end) {
        Map<String, Double> dist = new HashMap<>();
        Map<String, String> parent = new HashMap<>();
        PriorityQueue<String> pq = new PriorityQueue<>(Comparator.comparingDouble(n -> dist.getOrDefault(n, Double.MAX_VALUE)));

        dist.put(start, 0.0);
        parent.put(start, null);
        pq.add(start);

        while (!pq.isEmpty()) {
            String current = pq.poll();
            double currentDist = dist.getOrDefault(current, Double.MAX_VALUE);

            if (current.equals(end)) {
                return reconstructPath(parent, end);
            }

            for (Edge e : adj.getOrDefault(current, Collections.emptyList())) {
                double newDist = currentDist + e.weight;
                if (newDist < dist.getOrDefault(e.to, Double.MAX_VALUE)) {
                    dist.put(e.to, newDist);
                    parent.put(e.to, current);
                    pq.add(e.to);
                }
            }
        }
        return null;
    }

    private List<String> reconstructPath(Map<String, String> parent, String end) {
        List<String> path = new ArrayList<>();
        String node = end;
        while (node != null) {
            path.add(node);
            node = parent.get(node);
        }
        Collections.reverse(path);
        return path;
    }

    /**
     * graph_connected(Graph, Start) — All nodes reachable from Start.
     */
    private boolean executeGraphConnected(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("graph_connected/2 requires exactly 2 arguments.");
        }
        Term graphTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term startTerm = query.getArguments().get(1).resolveBindings(bindings);

        String start = termToNodeName(startTerm);
        List<Edge> edges = parseGraph(graphTerm, bindings);
        Map<String, List<Edge>> adj = buildAdjacencyList(edges);
        Set<String> allVertices = getAllVertices(edges);

        Set<String> reachable = bfsReachable(adj, start);

        if (reachable.containsAll(allVertices)) {
            solutions.add(new HashMap<>(bindings));
            return true;
        }
        return false;
    }

    private Set<String> bfsReachable(Map<String, List<Edge>> adj, String start) {
        Set<String> visited = new LinkedHashSet<>();
        Queue<String> queue = new LinkedList<>();
        queue.add(start);
        visited.add(start);
        while (!queue.isEmpty()) {
            String current = queue.poll();
            for (Edge e : adj.getOrDefault(current, Collections.emptyList())) {
                if (!visited.contains(e.to)) {
                    visited.add(e.to);
                    queue.add(e.to);
                }
            }
        }
        return visited;
    }

    /**
     * graph_vertices(Graph, Vertices) — Extract sorted list of unique vertices.
     */
    private boolean executeGraphVertices(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("graph_vertices/2 requires exactly 2 arguments.");
        }
        Term graphTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term verticesTerm = query.getArguments().get(1);

        List<Edge> edges = parseGraph(graphTerm, bindings);
        Set<String> vertices = getAllVertices(edges);
        List<String> sorted = new ArrayList<>(vertices); // TreeSet already sorted

        Term verticesList = nodesToList(sorted);
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (verticesTerm.resolveBindings(bindings).unify(verticesList, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    /**
     * graph_edges(Graph, Edges) — Extract edges from the graph.
     */
    private boolean executeGraphEdges(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("graph_edges/2 requires exactly 2 arguments.");
        }
        Term graphTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term edgesTerm = query.getArguments().get(1);

        List<Edge> edges = parseGraph(graphTerm, bindings);

        Term edgesList = edgesToList(edges);
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (edgesTerm.resolveBindings(bindings).unify(edgesList, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    /**
     * graph_neighbors(Graph, Node, Neighbors) — Get all neighbors of a node.
     */
    private boolean executeGraphNeighbors(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("graph_neighbors/3 requires exactly 3 arguments.");
        }
        Term graphTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term nodeTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term neighborsTerm = query.getArguments().get(2);

        String node = termToNodeName(nodeTerm);
        List<Edge> edges = parseGraph(graphTerm, bindings);
        Map<String, List<Edge>> adj = buildAdjacencyList(edges);

        List<String> neighbors = adj.getOrDefault(node, Collections.emptyList())
                .stream()
                .map(e -> e.to)
                .distinct()
                .sorted()
                .collect(Collectors.toList());

        Term neighborsList = nodesToList(neighbors);
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (neighborsTerm.resolveBindings(bindings).unify(neighborsList, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    /**
     * topological_sort(Graph, Sorted) — Kahn's algorithm. Fails if graph has a cycle.
     */
    private boolean executeTopologicalSort(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("topological_sort/2 requires exactly 2 arguments.");
        }
        Term graphTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term sortedTerm = query.getArguments().get(1);

        List<Edge> edges = parseGraph(graphTerm, bindings);
        Set<String> allVertices = getAllVertices(edges);

        // Kahn's algorithm
        Map<String, Integer> inDegree = new LinkedHashMap<>();
        Map<String, List<String>> adj = new LinkedHashMap<>();
        for (String v : allVertices) {
            inDegree.put(v, 0);
            adj.put(v, new ArrayList<>());
        }
        for (Edge e : edges) {
            adj.get(e.from).add(e.to);
            inDegree.put(e.to, inDegree.get(e.to) + 1);
        }

        Queue<String> queue = new PriorityQueue<>(); // sorted for deterministic output
        for (Map.Entry<String, Integer> entry : inDegree.entrySet()) {
            if (entry.getValue() == 0) {
                queue.add(entry.getKey());
            }
        }

        List<String> result = new ArrayList<>();
        while (!queue.isEmpty()) {
            String node = queue.poll();
            result.add(node);
            for (String neighbor : adj.get(node)) {
                int newDegree = inDegree.get(neighbor) - 1;
                inDegree.put(neighbor, newDegree);
                if (newDegree == 0) {
                    queue.add(neighbor);
                }
            }
        }

        // If not all vertices are in result, there's a cycle
        if (result.size() != allVertices.size()) {
            return false;
        }

        Term sortedList = nodesToList(result);
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (sortedTerm.resolveBindings(bindings).unify(sortedList, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    /**
     * graph_components(Graph, Components) — Find connected components (undirected).
     * Returns list of lists of nodes.
     */
    private boolean executeGraphComponents(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("graph_components/2 requires exactly 2 arguments.");
        }
        Term graphTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term componentsTerm = query.getArguments().get(1);

        List<Edge> edges = parseGraph(graphTerm, bindings);
        Set<String> allVertices = getAllVertices(edges);

        // Build undirected adjacency list
        Map<String, List<String>> undirectedAdj = new LinkedHashMap<>();
        for (String v : allVertices) {
            undirectedAdj.put(v, new ArrayList<>());
        }
        for (Edge e : edges) {
            undirectedAdj.get(e.from).add(e.to);
            undirectedAdj.get(e.to).add(e.from);
        }

        Set<String> visited = new HashSet<>();
        List<List<String>> components = new ArrayList<>();
        for (String v : allVertices) {
            if (!visited.contains(v)) {
                List<String> component = new ArrayList<>();
                Queue<String> queue = new LinkedList<>();
                queue.add(v);
                visited.add(v);
                while (!queue.isEmpty()) {
                    String current = queue.poll();
                    component.add(current);
                    for (String neighbor : undirectedAdj.get(current)) {
                        if (!visited.contains(neighbor)) {
                            visited.add(neighbor);
                            queue.add(neighbor);
                        }
                    }
                }
                Collections.sort(component);
                components.add(component);
            }
        }

        // Build list of lists
        Term componentsList = new Atom("[]");
        for (int i = components.size() - 1; i >= 0; i--) {
            Term compList = nodesToList(components.get(i));
            componentsList = new CompoundTerm(new Atom("."), Arrays.asList(compList, componentsList));
        }

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (componentsTerm.resolveBindings(bindings).unify(componentsList, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    /**
     * minimum_spanning_tree(Graph, MST) — Kruskal's algorithm.
     */
    private boolean executeMinimumSpanningTree(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("minimum_spanning_tree/2 requires exactly 2 arguments.");
        }
        Term graphTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term mstTerm = query.getArguments().get(1);

        List<Edge> edges = parseGraph(graphTerm, bindings);
        Set<String> allVertices = getAllVertices(edges);

        // Kruskal's algorithm with Union-Find
        List<Edge> sortedEdges = new ArrayList<>(edges);
        sortedEdges.sort(Comparator.comparingDouble(e -> e.weight));

        Map<String, String> parent = new HashMap<>();
        Map<String, Integer> rank = new HashMap<>();
        for (String v : allVertices) {
            parent.put(v, v);
            rank.put(v, 0);
        }

        List<Edge> mst = new ArrayList<>();
        for (Edge e : sortedEdges) {
            String rootFrom = find(parent, e.from);
            String rootTo = find(parent, e.to);
            if (!rootFrom.equals(rootTo)) {
                mst.add(e);
                union(parent, rank, rootFrom, rootTo);
                if (mst.size() == allVertices.size() - 1) {
                    break;
                }
            }
        }

        Term mstList = edgesToList(mst);
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (mstTerm.resolveBindings(bindings).unify(mstList, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    private String find(Map<String, String> parent, String node) {
        while (!parent.get(node).equals(node)) {
            parent.put(node, parent.get(parent.get(node))); // path compression
            node = parent.get(node);
        }
        return node;
    }

    private void union(Map<String, String> parent, Map<String, Integer> rank, String a, String b) {
        int rankA = rank.get(a);
        int rankB = rank.get(b);
        if (rankA < rankB) {
            parent.put(a, b);
        } else if (rankA > rankB) {
            parent.put(b, a);
        } else {
            parent.put(b, a);
            rank.put(a, rankA + 1);
        }
    }

    /**
     * graph_degree(Graph, Node, Degree) — Number of edges incident to a node.
     */
    private boolean executeGraphDegree(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("graph_degree/3 requires exactly 3 arguments.");
        }
        Term graphTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term nodeTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term degreeTerm = query.getArguments().get(2);

        String node = termToNodeName(nodeTerm);
        List<Edge> edges = parseGraph(graphTerm, bindings);

        int degree = 0;
        for (Edge e : edges) {
            if (e.from.equals(node)) degree++;
            if (e.to.equals(node)) degree++;
        }

        Term degreeNumber = new Number(degree);
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (degreeTerm.resolveBindings(bindings).unify(degreeNumber, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    /**
     * graph_has_cycle(Graph) — Succeeds if the directed graph contains a cycle.
     */
    private boolean executeGraphHasCycle(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("graph_has_cycle/1 requires exactly 1 argument.");
        }
        Term graphTerm = query.getArguments().get(0).resolveBindings(bindings);

        List<Edge> edges = parseGraph(graphTerm, bindings);
        Set<String> allVertices = getAllVertices(edges);
        Map<String, List<Edge>> adj = buildAdjacencyList(edges);

        // DFS-based cycle detection for directed graph
        Set<String> white = new HashSet<>(allVertices); // unvisited
        Set<String> gray = new HashSet<>(); // in current DFS path
        Set<String> black = new HashSet<>(); // fully processed

        for (String v : allVertices) {
            if (white.contains(v)) {
                if (hasCycleDFS(adj, v, white, gray, black)) {
                    solutions.add(new HashMap<>(bindings));
                    return true;
                }
            }
        }
        return false;
    }

    private boolean hasCycleDFS(Map<String, List<Edge>> adj, String node,
                                Set<String> white, Set<String> gray, Set<String> black) {
        white.remove(node);
        gray.add(node);

        for (Edge e : adj.getOrDefault(node, Collections.emptyList())) {
            if (gray.contains(e.to)) {
                return true; // back edge = cycle
            }
            if (white.contains(e.to)) {
                if (hasCycleDFS(adj, e.to, white, gray, black)) {
                    return true;
                }
            }
        }

        gray.remove(node);
        black.add(node);
        return false;
    }

    /**
     * graph_reachable(Graph, Start, Reachable) — All nodes reachable from Start via BFS.
     */
    private boolean executeGraphReachable(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("graph_reachable/3 requires exactly 3 arguments.");
        }
        Term graphTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term startTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term reachableTerm = query.getArguments().get(2);

        String start = termToNodeName(startTerm);
        List<Edge> edges = parseGraph(graphTerm, bindings);
        Map<String, List<Edge>> adj = buildAdjacencyList(edges);

        Set<String> reachable = bfsReachable(adj, start);
        List<String> sorted = new ArrayList<>(reachable);
        Collections.sort(sorted);

        Term reachableList = nodesToList(sorted);
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (reachableTerm.resolveBindings(bindings).unify(reachableList, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }
    // START_CHANGE: ISS-2025-0176 - Tarjan's SCC algorithm
    /**
     * graph_scc(Edges, SCCs) - Find Strongly Connected Components using Tarjan's algorithm.
     * Edges is a list of edge(From, To) terms.
     * SCCs is a list of lists, where each inner list is one SCC.
     */
    private boolean executeGraphScc(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("graph_scc/2 requires exactly 2 arguments.");
        }
        Term graphTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term sccsTerm = query.getArguments().get(1);

        List<Edge> edges = parseGraph(graphTerm, bindings);
        Set<String> allVertices = getAllVertices(edges);
        Map<String, List<Edge>> adj = buildAdjacencyList(edges);

        // Tarjan's algorithm state
        Map<String, Integer> discovery = new HashMap<>();
        Map<String, Integer> lowLink = new HashMap<>();
        Set<String> onStack = new HashSet<>();
        Deque<String> stack = new ArrayDeque<>();
        List<List<String>> sccResult = new ArrayList<>();
        int[] indexCounter = {0}; // mutable counter

        for (String v : allVertices) {
            if (!discovery.containsKey(v)) {
                tarjanDfs(v, adj, discovery, lowLink, onStack, stack, sccResult, indexCounter);
            }
        }

        // Build list of lists term
        Term sccsList = new Atom("[]");
        for (int i = sccResult.size() - 1; i >= 0; i--) {
            List<String> component = sccResult.get(i);
            Collections.sort(component);
            Term compList = nodesToList(component);
            sccsList = new CompoundTerm(new Atom("."), Arrays.asList(compList, sccsList));
        }

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (sccsTerm.resolveBindings(bindings).unify(sccsList, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    private void tarjanDfs(String node, Map<String, List<Edge>> adj,
                           Map<String, Integer> discovery, Map<String, Integer> lowLink,
                           Set<String> onStack, Deque<String> stack,
                           List<List<String>> sccResult, int[] indexCounter) {
        int index = indexCounter[0]++;
        discovery.put(node, index);
        lowLink.put(node, index);
        stack.push(node);
        onStack.add(node);

        for (Edge e : adj.getOrDefault(node, Collections.emptyList())) {
            if (!discovery.containsKey(e.to)) {
                // Successor not yet visited
                tarjanDfs(e.to, adj, discovery, lowLink, onStack, stack, sccResult, indexCounter);
                lowLink.put(node, Math.min(lowLink.get(node), lowLink.get(e.to)));
            } else if (onStack.contains(e.to)) {
                // Successor is on the stack => part of current SCC
                lowLink.put(node, Math.min(lowLink.get(node), discovery.get(e.to)));
            }
        }

        // If node is a root of an SCC, pop the SCC from the stack
        if (lowLink.get(node).equals(discovery.get(node))) {
            List<String> scc = new ArrayList<>();
            String w;
            do {
                w = stack.pop();
                onStack.remove(w);
                scc.add(w);
            } while (!w.equals(node));
            sccResult.add(scc);
        }
    }
    // END_CHANGE: ISS-2025-0176
}
// END_CHANGE: ISS-2025-0127
