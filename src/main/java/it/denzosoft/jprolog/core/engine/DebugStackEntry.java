package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.core.terms.Term;
import java.util.HashMap;
import java.util.Map;

/**
 * Represents a single frame in the Prolog debug call stack.
 */
public class DebugStackEntry {

    private final Term goal;
    private final int depth;
    private final Map<String, Term> bindingsSnapshot;

    public DebugStackEntry(Term goal, int depth, Map<String, Term> bindings) {
        this.goal = goal;
        this.depth = depth;
        this.bindingsSnapshot = bindings != null ? new HashMap<>(bindings) : new HashMap<>();
    }

    public Term getGoal() { return goal; }
    public int getDepth() { return depth; }
    public Map<String, Term> getBindingsSnapshot() { return bindingsSnapshot; }

    @Override
    public String toString() {
        return "[" + depth + "] " + goal.toString();
    }
}
