package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.Arrays;
import java.util.*;

/**
 * Controls debug execution by managing breakpoints, step modes,
 * and synchronization between the solver thread and the UI thread.
 *
 * Thread model:
 * - The solver runs on a background thread
 * - The UI (Swing EDT) calls resume methods
 * - Synchronization via wait/notify on pauseLock
 */
public class DebugController {

    /** Listener for debug events — called on solver thread, should post to EDT */
    public interface DebugListener {
        void onDebugPaused(DebugEvent event);
        void onTraceEvent(DebugEvent event);
        void onDebugFinished();
    }

    // START_CHANGE: ISS-2025-0281 - These collections are mutated on the Swing EDT
    // (addBreakpoint/removeBreakpoint/clear) and read on the solver thread (isBreakpointHit);
    // use concurrent collections to avoid races/corruption with the plain HashSet/HashMap.
    // Breakpoints (predicate/arity format, e.g., "parent/2")
    private final Set<String> breakpoints = java.util.concurrent.ConcurrentHashMap.newKeySet();

    // START_CHANGE: ISS-2025-0172 - Leash control: which ports trigger pausing
    private final Set<String> leashedPorts =
        java.util.concurrent.ConcurrentHashMap.newKeySet();
    {
        leashedPorts.addAll(Arrays.asList("CALL", "EXIT", "FAIL", "REDO"));
    }

    // Per-breakpoint port filters: breakpoint indicator -> set of ports
    private final Map<String, Set<String>> breakpointPorts = new java.util.concurrent.ConcurrentHashMap<>();
    // END_CHANGE: ISS-2025-0172
    // END_CHANGE: ISS-2025-0281

    // START_CHANGE: ISS-2025-0333 - conditional & hit-count breakpoints
    /** indicator -> Prolog goal that must succeed (against the current bindings) for the bp to fire. */
    private final Map<String, String> breakpointCondition = new java.util.concurrent.ConcurrentHashMap<>();
    /** indicator -> number of initial hits to ignore before pausing. */
    private final Map<String, Integer> breakpointIgnore = new java.util.concurrent.ConcurrentHashMap<>();
    /** indicator -> running hit count (reset per debug session). */
    private final Map<String, Integer> breakpointHits = new java.util.concurrent.ConcurrentHashMap<>();

    /** Evaluates a breakpoint condition goal against the paused bindings (set by the IDE). It must run a
     *  side-effect-free sub-solve WITHOUT this controller attached (no re-entrant debug events). */
    public interface ConditionEvaluator { boolean holds(String goal, Map<String, Term> bindings); }
    private volatile ConditionEvaluator conditionEvaluator;
    public void setConditionEvaluator(ConditionEvaluator e) { this.conditionEvaluator = e; }

    /** Register a conditional / hit-count breakpoint. condition or ignoreCount may be null/0. */
    public void addBreakpoint(String indicator, Set<String> ports, String condition, int ignoreCount) {
        breakpoints.add(indicator);
        if (ports != null && !ports.isEmpty()) breakpointPorts.put(indicator, ports);
        if (condition != null && !condition.trim().isEmpty()) breakpointCondition.put(indicator, condition.trim());
        if (ignoreCount > 0) breakpointIgnore.put(indicator, ignoreCount);
    }
    public String getBreakpointCondition(String indicator) { return breakpointCondition.get(indicator); }
    public int getBreakpointIgnore(String indicator) { Integer i = breakpointIgnore.get(indicator); return i == null ? 0 : i; }
    // END_CHANGE: ISS-2025-0333

    // Step mode
    private volatile DebugEvent.Action currentMode = DebugEvent.Action.STEP_INTO;
    private volatile int stepOverTargetDepth = -1;
    private volatile int stepOutTargetDepth = -1;

    // Call stack maintained by CALL/EXIT/FAIL hooks
    private final List<DebugStackEntry> callStack = new ArrayList<>();

    // Thread synchronization
    private final Object pauseLock = new Object();
    private volatile DebugEvent.Action pendingAction = null;
    private volatile boolean stopped = false;
    private volatile boolean paused = false;

    // Listener (typically the DebugPanel on EDT)
    private DebugListener listener;

    // Trace all events (even when not pausing)
    private boolean traceEnabled = true;

    public DebugController() {
    }

    public void setListener(DebugListener listener) {
        this.listener = listener;
    }

    public void setTraceEnabled(boolean traceEnabled) {
        this.traceEnabled = traceEnabled;
    }

    // START_CHANGE: ISS-2025-0481 - wave W8: the machine now emits ports for its inline built-ins
    // too, so a controller that is merely RUNNING (CONTINUE, no listener, no breakpoints) must not
    // make the engine snapshot every goal. The snapshot is a full term copy; it is needed only when
    // somebody will look at the term later — a trace listener (the IDE renders it on the EDT, after
    // the bindings have moved on) or a breakpoint/step decision.
    // START_CHANGE: ISS-2025-0494 - 4.1 wave A: and a controller that can OBSERVE nothing must not
    // make the engine emit ports at all. `notifyPort` maintains the call stack, decides whether to
    // pause and — only then — builds an event; with no listener, no breakpoint, CONTINUE mode, no
    // trace and no pending Stop, every one of those is a no-op, so the machine skips the port
    // entirely (and with it the depth bookkeeping and the choice-point trace fields).
    // NOTE: the DEFAULT mode of a fresh controller is STEP_INTO, which pauses — an idle controller
    // is one the embedder put in CONTINUE. `stopped` must keep ports alive: it is what turns the
    // next port into a DebugStopException.
    /** True when a port reported to this controller can have any effect. */
    public boolean needsPorts() {
        return stopped                                   // the next port must raise DebugStopException
            || listener != null                          // somebody renders the trace / the pause
            || !breakpoints.isEmpty()                    // a port may hit a breakpoint
            || currentMode != DebugEvent.Action.CONTINUE;  // stepping pauses on the next port
        // `traceEnabled` alone is NOT an observer: every use of it in this class is guarded by
        // `listener != null` (it is the IDE's per-session trace toggle, not an output switch), and
        // it defaults to true, so including it here would make every controller need ports.
    }
    // END_CHANGE: ISS-2025-0494

    /** True when the port's goal has to be resolved into a stable snapshot before being reported. */
    public boolean needsGoalSnapshot() {
        boolean need = (traceEnabled && listener != null)
            || currentMode != DebugEvent.Action.CONTINUE
            || !breakpoints.isEmpty();
        if (need) goalSnapshots.incrementAndGet();                   // ISS-2025-0529
        return need;
    }
    // END_CHANGE: ISS-2025-0481

    // START_CHANGE: ISS-2025-0529 - wave P1.16: "any breakpoint exists" is not a reason to snapshot
    // EVERY port. The snapshot is a full resolve of the goal, so with one unrelated breakpoint a
    // deterministic recursion over a 40 000-element list resolved the list at every port (47.9 s
    // against 21 ms). A breakpoint can only fire for the goal's own indicator, so that is checked
    // first — a string lookup instead of a term copy. A listener that renders the trace and the
    // stepping modes (which may pause on this very port) still get a snapshot every time.
    private final java.util.concurrent.atomic.AtomicLong goalSnapshots = new java.util.concurrent.atomic.AtomicLong();

    /**
     * True when the port's goal ({@code goal}, not yet resolved) has to be snapshotted before being
     * reported: a trace listener or a stepping mode needs it always, a breakpoint only when it is
     * set on this goal's indicator (or bare name).
     */
    public boolean needsGoalSnapshot(Term goal) {
        boolean need = (traceEnabled && listener != null)
            || currentMode != DebugEvent.Action.CONTINUE
            || (!breakpoints.isEmpty() && breakpointMayMatch(goal));
        if (need) goalSnapshots.incrementAndGet();
        return need;
    }

    private boolean breakpointMayMatch(Term goal) {
        String name = (goal == null) ? null : goal.getName();
        if (name == null) return false;
        int arity = (goal.getArguments() == null) ? 0 : goal.getArguments().size();
        return breakpoints.contains(name + "/" + arity) || breakpoints.contains(name);
    }

    /** Test hook: how many port goals this controller asked the engine to snapshot. */
    public long getGoalSnapshotCount() { return goalSnapshots.get(); }
    // END_CHANGE: ISS-2025-0529

    public boolean isTraceEnabled() {
        return traceEnabled;
    }

    // ===================== BREAKPOINT MANAGEMENT =====================

    public void addBreakpoint(String predicateIndicator) {
        breakpoints.add(predicateIndicator);
    }

    public void removeBreakpoint(String predicateIndicator) {
        breakpoints.remove(predicateIndicator);
        // START_CHANGE: ISS-2025-0172 - Clean up per-breakpoint port filters
        breakpointPorts.remove(predicateIndicator);
        // END_CHANGE: ISS-2025-0172
    }

    public void clearBreakpoints() {
        breakpoints.clear();
        // START_CHANGE: ISS-2025-0172 - Clean up per-breakpoint port filters
        breakpointPorts.clear();
        // END_CHANGE: ISS-2025-0172
    }

    public Set<String> getBreakpoints() {
        return Collections.unmodifiableSet(breakpoints);
    }

    // START_CHANGE: ISS-2025-0172 - Leash control methods
    /**
     * Set which ports trigger pausing during debugging.
     * @param ports set of port names (e.g., "CALL", "EXIT", "FAIL", "REDO")
     */
    public void setLeash(Set<String> ports) {
        leashedPorts.clear();
        for (String p : ports) {
            leashedPorts.add(p.toUpperCase());
        }
    }

    /**
     * Set which ports trigger pausing (varargs convenience).
     */
    public void setLeash(String... ports) {
        leashedPorts.clear();
        for (String p : ports) {
            leashedPorts.add(p.toUpperCase());
        }
    }

    /**
     * Return current leash set.
     */
    public Set<String> getLeash() {
        return Collections.unmodifiableSet(leashedPorts);
    }

    /**
     * Add a breakpoint with specific port filters.
     */
    public void addBreakpoint(String predicateIndicator, Set<String> ports) {
        breakpoints.add(predicateIndicator);
        if (ports != null && !ports.isEmpty()) {
            Set<String> upperPorts = new HashSet<>();
            for (String p : ports) {
                upperPorts.add(p.toUpperCase());
            }
            breakpointPorts.put(predicateIndicator, upperPorts);
        }
    }

    /**
     * Get port filters for a specific breakpoint, or null if no filter (all ports).
     */
    public Set<String> getBreakpointPorts(String predicateIndicator) {
        return breakpointPorts.get(predicateIndicator);
    }
    // END_CHANGE: ISS-2025-0172

    // ===================== SOLVER HOOKS (called on solver thread) =====================

    /**
     * Called by the engine at each debug port.
     * May block the solver thread if stepping or breakpoint hit.
     *
     * @throws DebugStopException if the user requested stop
     */
    public void notifyPort(DebugEvent.Port port, Term goal, Map<String, Term> bindings, int depth) {
        if (stopped) {
            throw new DebugStopException();
        }

        // Manage call stack
        switch (port) {
            case CALL:
                callStack.add(new DebugStackEntry(goal, depth, bindings));
                break;
            case EXIT:
            case FAIL:
                // Pop entries at this depth or deeper
                while (!callStack.isEmpty() &&
                       callStack.get(callStack.size() - 1).getDepth() >= depth) {
                    callStack.remove(callStack.size() - 1);
                }
                break;
            case REDO:
                // Pop and re-push for redo
                while (!callStack.isEmpty() &&
                       callStack.get(callStack.size() - 1).getDepth() >= depth) {
                    callStack.remove(callStack.size() - 1);
                }
                callStack.add(new DebugStackEntry(goal, depth, bindings));
                break;
        }

        // START_CHANGE: ISS-2025-0481 - build the event LAZILY. It copies the whole call stack,
        // and since wave W8 the machine emits ports for its inline built-ins too, so an attached
        // debugger that is only running (CONTINUE, no listener) would pay that copy per inference.
        DebugEvent event = null;

        // Always send trace if enabled
        if (traceEnabled && listener != null) {
            event = new DebugEvent(port, goal, depth, bindings, new ArrayList<>(callStack));
            listener.onTraceEvent(event);
        }
        // END_CHANGE: ISS-2025-0481

        // START_CHANGE: ISS-2025-0172 - Leash/port filtering for pause decisions
        // Determine whether to pause
        boolean shouldPause = false;
        String portName = port.name(); // e.g., "CALL", "EXIT", "FAIL", "REDO"

        if (port == DebugEvent.Port.CALL || port == DebugEvent.Port.REDO) {
            switch (currentMode) {
                case STEP_INTO:
                    shouldPause = leashedPorts.contains(portName);
                    break;
                case STEP_OVER:
                    shouldPause = (depth <= stepOverTargetDepth) && leashedPorts.contains(portName);
                    break;
                case STEP_OUT:
                    shouldPause = (depth < stepOutTargetDepth) && leashedPorts.contains(portName);
                    break;
                case CONTINUE:
                    shouldPause = isBreakpointHit(goal, portName, bindings);
                    break;
                case STOP:
                    throw new DebugStopException();
            }
        } else if (port == DebugEvent.Port.EXIT || port == DebugEvent.Port.FAIL) {
            // Also pause on EXIT/FAIL when stepping into, if port is leashed
            if (currentMode == DebugEvent.Action.STEP_INTO && leashedPorts.contains(portName)) {
                shouldPause = true;
            }
            // For step_out, pause when we reach the target depth on EXIT
            if (currentMode == DebugEvent.Action.STEP_OUT && depth <= stepOutTargetDepth && leashedPorts.contains(portName)) {
                shouldPause = true;
            }
        }
        // END_CHANGE: ISS-2025-0172

        if (shouldPause) {
            if (event == null) {   // ISS-2025-0481
                event = new DebugEvent(port, goal, depth, bindings, new ArrayList<>(callStack));
            }
            waitForUserAction(event);
        }
    }

    /**
     * Check if a breakpoint is set for the given goal and port.
     */
    // START_CHANGE: ISS-2025-0172 - Port-aware breakpoint checking
    private boolean isBreakpointHit(Term goal, String portName, Map<String, Term> bindings) {
        if (breakpoints.isEmpty()) return false;
        String name = goal.getName();
        if (name == null) return false;
        int arity = 0;
        if (goal.getArguments() != null) {
            arity = goal.getArguments().size();
        }
        String indicator = name + "/" + arity;
        String key = breakpoints.contains(indicator) ? indicator : (breakpoints.contains(name) ? name : null);
        if (key == null) return false;

        // Check per-breakpoint port filter (else the global leash)
        Set<String> bpPorts = breakpointPorts.get(key);
        boolean portOk = (bpPorts != null) ? bpPorts.contains(portName) : leashedPorts.contains(portName);
        if (!portOk) return false;

        // START_CHANGE: ISS-2025-0333 - hit-count then condition
        int hits = breakpointHits.merge(key, 1, Integer::sum);
        Integer ignore = breakpointIgnore.get(key);
        if (ignore != null && hits <= ignore) return false;            // still within the ignore count
        String cond = breakpointCondition.get(key);
        if (cond != null && conditionEvaluator != null) {
            try {
                if (!conditionEvaluator.holds(cond, bindings)) return false;   // condition not met -> skip
            } catch (RuntimeException e) {
                it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
                return false;                                          // a broken condition never pauses
            }
        }
        return true;
        // END_CHANGE: ISS-2025-0333
    }
    // END_CHANGE: ISS-2025-0172

    /**
     * Blocks the solver thread until the user chooses an action.
     */
    private void waitForUserAction(DebugEvent event) {
        paused = true;

        // Notify the listener (UI) that we are paused
        if (listener != null) {
            listener.onDebugPaused(event);
        }

        // Block until user action
        synchronized (pauseLock) {
            while (pendingAction == null && !stopped) {
                try {
                    pauseLock.wait();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    stopped = true;
                    throw new DebugStopException();
                }
            }
        }

        paused = false;

        if (stopped) {
            throw new DebugStopException();
        }

        // Apply the action
        DebugEvent.Action action = pendingAction;
        pendingAction = null;

        switch (action) {
            case STEP_INTO:
                currentMode = DebugEvent.Action.STEP_INTO;
                break;
            case STEP_OVER:
                currentMode = DebugEvent.Action.STEP_OVER;
                stepOverTargetDepth = event.getDepth();
                break;
            case STEP_OUT:
                currentMode = DebugEvent.Action.STEP_OUT;
                stepOutTargetDepth = event.getDepth();
                break;
            case CONTINUE:
                currentMode = DebugEvent.Action.CONTINUE;
                break;
            case STOP:
                stopped = true;
                throw new DebugStopException();
        }
    }

    // ===================== UI CONTROL (called on EDT) =====================

    /**
     * Resume execution with the given action. Called from Swing EDT.
     */
    public void resumeWithAction(DebugEvent.Action action) {
        synchronized (pauseLock) {
            pendingAction = action;
            pauseLock.notify();
        }
    }

    /**
     * Stop the debug session immediately.
     */
    public void stop() {
        stopped = true;
        synchronized (pauseLock) {
            pendingAction = DebugEvent.Action.STOP;
            pauseLock.notify();
        }
    }

    /**
     * Reset the controller for a new session.
     */
    public void reset() {
        stopped = false;
        paused = false;
        pendingAction = null;
        callStack.clear();
        currentMode = DebugEvent.Action.STEP_INTO;
        stepOverTargetDepth = -1;
        stepOutTargetDepth = -1;
        // START_CHANGE: ISS-2025-0172 - Reset leash to all ports
        leashedPorts.clear();
        leashedPorts.addAll(Arrays.asList("CALL", "EXIT", "FAIL", "REDO"));
        breakpointPorts.clear();
        breakpointHits.clear();   // ISS-2025-0333: reset hit counts per debug session
        // END_CHANGE: ISS-2025-0172
    }

    // START_CHANGE: ISS-2025-0186 - Debug, utility, and list predicate fixes
    /**
     * Handle an uncaught exception for a goal by popping its call stack entry
     * and firing a FAIL event so the stack doesn't leak entries on exceptions.
     *
     * @param goal  the goal that threw the exception
     * @param depth the depth at which the goal was called
     * @param bindings the current bindings at the time of the exception
     */
    public void handleException(Term goal, int depth, Map<String, Term> bindings) {
        if (stopped) return;

        // Pop entries at this depth or deeper (same as FAIL handling)
        while (!callStack.isEmpty() &&
               callStack.get(callStack.size() - 1).getDepth() >= depth) {
            callStack.remove(callStack.size() - 1);
        }

        // Fire a FAIL event so listeners are notified
        DebugEvent event = new DebugEvent(DebugEvent.Port.FAIL, goal, depth, bindings,
                new ArrayList<>(callStack));

        if (traceEnabled && listener != null) {
            listener.onTraceEvent(event);
        }
    }
    // END_CHANGE: ISS-2025-0186

    // ===================== STATE QUERIES =====================

    public boolean isStopped() { return stopped; }
    public boolean isPaused() { return paused; }
    public DebugEvent.Action getCurrentMode() { return currentMode; }

    public void setCurrentMode(DebugEvent.Action mode) {
        this.currentMode = mode;
    }

    public List<DebugStackEntry> getCallStack() {
        return new ArrayList<>(callStack);
    }

    /**
     * Exception thrown to unwind the solver stack when debug is stopped.
     */
    public static class DebugStopException extends RuntimeException {
        public DebugStopException() {
            super("Debug session stopped by user");
        }
    }
}
