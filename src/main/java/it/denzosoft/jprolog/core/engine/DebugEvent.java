package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.core.terms.Term;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Represents a debug event at a port in the Prolog box model.
 * Carries all information needed by the UI to display the current execution state.
 */
public class DebugEvent {

    /** Standard Prolog 4-port debug model */
    public enum Port {
        CALL, EXIT, FAIL, REDO;

        @Override
        public String toString() {
            return name().toLowerCase();
        }
    }

    /** Action the debugger should take after processing an event */
    public enum Action {
        STEP_INTO,
        STEP_OVER,
        STEP_OUT,
        CONTINUE,
        STOP
    }

    private final Port port;
    private final Term goal;
    private final int depth;
    private final Map<String, Term> bindings;
    private final List<DebugStackEntry> callStack;
    private final long timestamp;

    public DebugEvent(Port port, Term goal, int depth,
                      Map<String, Term> bindings, List<DebugStackEntry> callStack) {
        this.port = port;
        this.goal = goal;
        this.depth = depth;
        this.bindings = bindings != null ? new HashMap<>(bindings) : Collections.emptyMap();
        this.callStack = callStack;
        this.timestamp = System.currentTimeMillis();
    }

    public Port getPort() { return port; }
    public Term getGoal() { return goal; }
    public int getDepth() { return depth; }
    public Map<String, Term> getBindings() { return bindings; }
    public List<DebugStackEntry> getCallStack() { return callStack; }
    public long getTimestamp() { return timestamp; }

    /**
     * Format a trace line like:  [depth] PORT: goal
     */
    public String formatTraceLine() {
        StringBuilder sb = new StringBuilder();
        // Indentation based on depth
        for (int i = 0; i < depth; i++) {
            sb.append("   ");
        }
        sb.append("[").append(depth).append("] ");
        sb.append(port.toString().toUpperCase()).append(": ");
        sb.append(goal.toString());
        return sb.toString();
    }
}
