// START_CHANGE: ISS-2025-0123 - CLP(FD) Constraint Logic Programming over Finite Domains
package it.denzosoft.jprolog.builtin.clpfd;

import java.util.*;

/**
 * Constraint store for CLP(FD) - manages variable domains and constraints,
 * implements AC-3 arc consistency propagation.
 */
public class ConstraintStore {
    private static final ConstraintStore INSTANCE = new ConstraintStore();

    private Map<String, TreeSet<Integer>> domains = new HashMap<>();
    private List<Constraint> constraints = new ArrayList<>();

    /**
     * Get the singleton instance.
     */
    public static ConstraintStore getInstance() {
        return INSTANCE;
    }

    // ---- Domain operations ----

    /**
     * Set the domain of a variable to a contiguous range [min..max].
     */
    public void setDomain(String var, int min, int max) {
        TreeSet<Integer> dom = new TreeSet<>();
        for (int i = min; i <= max; i++) {
            dom.add(i);
        }
        domains.put(var, dom);
    }

    /**
     * Set the domain of a variable to an explicit set of values.
     */
    public void setDomainValues(String var, Collection<Integer> values) {
        domains.put(var, new TreeSet<>(values));
    }

    /**
     * Get the current domain of a variable, or null if not constrained.
     */
    public TreeSet<Integer> getDomain(String var) {
        return domains.get(var);
    }

    /**
     * Check if a variable has a domain registered.
     */
    public boolean hasDomain(String var) {
        return domains.containsKey(var);
    }

    /**
     * Restrict a variable's domain by intersecting with allowed values.
     * Returns false if the domain becomes empty (wipeout).
     */
    public boolean restrictDomain(String var, Collection<Integer> allowed) {
        TreeSet<Integer> dom = domains.get(var);
        if (dom == null) {
            domains.put(var, new TreeSet<>(allowed));
            return !allowed.isEmpty();
        }
        dom.retainAll(allowed);
        return !dom.isEmpty();
    }

    /**
     * Remove a single value from a variable's domain.
     * Returns false if the domain becomes empty.
     */
    public boolean removeValue(String var, int value) {
        TreeSet<Integer> dom = domains.get(var);
        if (dom == null) {
            return true; // No domain means unconstrained
        }
        dom.remove(value);
        return !dom.isEmpty();
    }

    // ---- Constraint operations ----

    /**
     * Add a constraint and propagate.
     * Returns false if propagation leads to a domain wipeout.
     */
    public boolean addConstraint(Constraint c) {
        constraints.add(c);
        return propagate();
    }

    /**
     * Get all registered constraints.
     */
    public List<Constraint> getConstraints() {
        return Collections.unmodifiableList(constraints);
    }

    /**
     * AC-3 arc consistency propagation with bounds consistency pre-pass.
     * Returns false if any domain is wiped out.
     */
    public boolean propagate() {
        // START_CHANGE: ISS-2025-0175 - Bounds consistency pre-pass before AC-3 filtering
        // Run bounds inference to fixpoint before element-wise AC-3
        if (!boundsConsistencyPass()) {
            return false;
        }
        // END_CHANGE: ISS-2025-0175

        // Build work queue of constraint indices
        Deque<Integer> queue = new ArrayDeque<>();
        for (int i = 0; i < constraints.size(); i++) {
            queue.add(i);
        }

        while (!queue.isEmpty()) {
            int idx = queue.poll();
            Constraint c = constraints.get(idx);

            boolean changed = c.propagate(this);
            if (changed) {
                // Check for wipeout
                for (String var : c.getVariables()) {
                    TreeSet<Integer> dom = domains.get(var);
                    if (dom != null && dom.isEmpty()) {
                        return false; // Domain wipeout
                    }
                }
                // Re-enqueue all constraints that share variables with this one
                Set<String> vars = new HashSet<>(c.getVariables());
                for (int i = 0; i < constraints.size(); i++) {
                    if (i != idx && !queue.contains(i)) {
                        Constraint other = constraints.get(i);
                        for (String v : other.getVariables()) {
                            if (vars.contains(v)) {
                                queue.add(i);
                                break;
                            }
                        }
                    }
                }
            }
        }
        return true;
    }

    // START_CHANGE: ISS-2025-0175 - Bounds consistency inference for arithmetic constraints
    /**
     * Run bounds consistency inference to fixpoint.
     * For each constraint, compute the tightest bounds from related variable domains
     * using min/max interval arithmetic. This is much faster than element-wise AC-3
     * for large domains.
     * Returns false if any domain becomes empty (wipeout).
     */
    private boolean boundsConsistencyPass() {
        boolean changed = true;
        int maxIterations = 100; // Safety limit to prevent infinite loops
        int iteration = 0;

        while (changed && iteration < maxIterations) {
            changed = false;
            iteration++;

            for (Constraint c : constraints) {
                if (c instanceof BinaryConstraint) {
                    boolean result = propagateBinaryBounds((BinaryConstraint) c);
                    if (result) changed = true;
                    // Check for wipeout
                    for (String var : c.getVariables()) {
                        TreeSet<Integer> dom = domains.get(var);
                        if (dom != null && dom.isEmpty()) return false;
                    }
                } else if (c instanceof ArithmeticConstraint) {
                    boolean result = propagateArithmeticBounds((ArithmeticConstraint) c);
                    if (result) changed = true;
                    for (String var : c.getVariables()) {
                        TreeSet<Integer> dom = domains.get(var);
                        if (dom != null && dom.isEmpty()) return false;
                    }
                }
            }
        }
        return true;
    }

    /**
     * Bounds inference for binary constraints (#=, #\=, #<, #>, #=<, #>=).
     * Narrows domains using min/max bounds instead of element-wise filtering.
     * Returns true if any domain was modified.
     */
    private boolean propagateBinaryBounds(BinaryConstraint bc) {
        boolean changed = false;

        // Get bounds for left operand
        int leftMin, leftMax;
        if (bc.leftVar != null) {
            TreeSet<Integer> dom = domains.get(bc.leftVar);
            if (dom == null || dom.isEmpty()) return false;
            leftMin = dom.first();
            leftMax = dom.last();
        } else if (bc.leftConst != null) {
            leftMin = leftMax = bc.leftConst;
        } else {
            return false;
        }

        // Get bounds for right operand
        int rightMin, rightMax;
        if (bc.rightVar != null) {
            TreeSet<Integer> dom = domains.get(bc.rightVar);
            if (dom == null || dom.isEmpty()) return false;
            rightMin = dom.first();
            rightMax = dom.last();
        } else if (bc.rightConst != null) {
            rightMin = rightMax = bc.rightConst;
        } else {
            return false;
        }

        // Compute new bounds based on constraint type
        switch (bc.type) {
            case EQ:
                // X #= Y: X in [max(Xmin,Ymin)..min(Xmax,Ymax)]
                if (bc.leftVar != null) {
                    changed |= narrowDomain(bc.leftVar, Math.max(leftMin, rightMin), Math.min(leftMax, rightMax));
                }
                if (bc.rightVar != null) {
                    // Re-read left bounds after possible narrowing
                    if (bc.leftVar != null) {
                        TreeSet<Integer> ld = domains.get(bc.leftVar);
                        if (ld != null && !ld.isEmpty()) {
                            leftMin = ld.first();
                            leftMax = ld.last();
                        }
                    }
                    changed |= narrowDomain(bc.rightVar, Math.max(rightMin, leftMin), Math.min(rightMax, leftMax));
                }
                break;

            case LT:
                // X #< Y: X_max <= Y_max - 1, Y_min >= X_min + 1
                if (bc.leftVar != null) {
                    changed |= narrowDomain(bc.leftVar, leftMin, Math.min(leftMax, rightMax - 1));
                }
                if (bc.rightVar != null) {
                    if (bc.leftVar != null) {
                        TreeSet<Integer> ld = domains.get(bc.leftVar);
                        if (ld != null && !ld.isEmpty()) leftMin = ld.first();
                    }
                    changed |= narrowDomain(bc.rightVar, Math.max(rightMin, leftMin + 1), rightMax);
                }
                break;

            case GT:
                // X #> Y: X_min >= Y_min + 1, Y_max <= X_max - 1
                if (bc.leftVar != null) {
                    changed |= narrowDomain(bc.leftVar, Math.max(leftMin, rightMin + 1), leftMax);
                }
                if (bc.rightVar != null) {
                    if (bc.leftVar != null) {
                        TreeSet<Integer> ld = domains.get(bc.leftVar);
                        if (ld != null && !ld.isEmpty()) leftMax = ld.last();
                    }
                    changed |= narrowDomain(bc.rightVar, rightMin, Math.min(rightMax, leftMax - 1));
                }
                break;

            case LEQ:
                // X #=< Y: X_max <= Y_max, Y_min >= X_min
                if (bc.leftVar != null) {
                    changed |= narrowDomain(bc.leftVar, leftMin, Math.min(leftMax, rightMax));
                }
                if (bc.rightVar != null) {
                    if (bc.leftVar != null) {
                        TreeSet<Integer> ld = domains.get(bc.leftVar);
                        if (ld != null && !ld.isEmpty()) leftMin = ld.first();
                    }
                    changed |= narrowDomain(bc.rightVar, Math.max(rightMin, leftMin), rightMax);
                }
                break;

            case GEQ:
                // X #>= Y: X_min >= Y_min, Y_max <= X_max
                if (bc.leftVar != null) {
                    changed |= narrowDomain(bc.leftVar, Math.max(leftMin, rightMin), leftMax);
                }
                if (bc.rightVar != null) {
                    if (bc.leftVar != null) {
                        TreeSet<Integer> ld = domains.get(bc.leftVar);
                        if (ld != null && !ld.isEmpty()) leftMax = ld.last();
                    }
                    changed |= narrowDomain(bc.rightVar, rightMin, Math.min(rightMax, leftMax));
                }
                break;

            case NEQ:
                // X #\= Y: if Y is singleton {v}, remove v from X bounds (only if at boundary)
                if (bc.rightVar != null) {
                    TreeSet<Integer> rDom = domains.get(bc.rightVar);
                    if (rDom != null && rDom.size() == 1 && bc.leftVar != null) {
                        int val = rDom.first();
                        TreeSet<Integer> lDom = domains.get(bc.leftVar);
                        if (lDom != null && lDom.contains(val)) {
                            if (lDom.first() == val || lDom.last() == val) {
                                lDom.remove(val);
                                changed = true;
                            }
                        }
                    }
                }
                if (bc.leftVar != null) {
                    TreeSet<Integer> lDom = domains.get(bc.leftVar);
                    if (lDom != null && lDom.size() == 1 && bc.rightVar != null) {
                        int val = lDom.first();
                        TreeSet<Integer> rDom = domains.get(bc.rightVar);
                        if (rDom != null && rDom.contains(val)) {
                            if (rDom.first() == val || rDom.last() == val) {
                                rDom.remove(val);
                                changed = true;
                            }
                        }
                    }
                }
                // Also handle var #\= const
                if (bc.leftVar != null && bc.rightConst != null) {
                    TreeSet<Integer> lDom = domains.get(bc.leftVar);
                    if (lDom != null && lDom.contains(bc.rightConst)) {
                        if (lDom.first().equals(bc.rightConst) || lDom.last().equals(bc.rightConst)) {
                            lDom.remove((int) bc.rightConst);
                            changed = true;
                        }
                    }
                }
                if (bc.rightVar != null && bc.leftConst != null) {
                    TreeSet<Integer> rDom = domains.get(bc.rightVar);
                    if (rDom != null && rDom.contains(bc.leftConst)) {
                        if (rDom.first().equals(bc.leftConst) || rDom.last().equals(bc.leftConst)) {
                            rDom.remove((int) bc.leftConst);
                            changed = true;
                        }
                    }
                }
                break;
        }

        return changed;
    }

    /**
     * Bounds inference for arithmetic constraints (R = L op R).
     * For ADD: R in [Lmin+Rmin..Lmax+Rmax], L in [Rmin-Rmax..Rmax-Rmin], etc.
     * Returns true if any domain was modified.
     */
    private boolean propagateArithmeticBounds(ArithmeticConstraint ac) {
        boolean changed = false;

        // Get bounds for left operand of expression
        int exprLeftMin, exprLeftMax;
        if (ac.leftVar != null) {
            TreeSet<Integer> dom = domains.get(ac.leftVar);
            if (dom == null || dom.isEmpty()) return false;
            exprLeftMin = dom.first();
            exprLeftMax = dom.last();
        } else if (ac.leftConst != null) {
            exprLeftMin = exprLeftMax = ac.leftConst;
        } else {
            return false;
        }

        // Get bounds for right operand of expression
        int exprRightMin, exprRightMax;
        if (ac.rightVar != null) {
            TreeSet<Integer> dom = domains.get(ac.rightVar);
            if (dom == null || dom.isEmpty()) return false;
            exprRightMin = dom.first();
            exprRightMax = dom.last();
        } else if (ac.rightConst != null) {
            exprRightMin = exprRightMax = ac.rightConst;
        } else {
            return false;
        }

        // Get bounds for result variable
        if (ac.resultVar == null) return false;
        TreeSet<Integer> resDom = domains.get(ac.resultVar);
        if (resDom == null || resDom.isEmpty()) return false;
        int resMin = resDom.first();
        int resMax = resDom.last();

        switch (ac.op) {
            // START_CHANGE: ISS-2025-0262 - compute ADD/SUB bound combinations in long and clamp
            // to int range (mirroring the MUL case). Previously these used int arithmetic and
            // silently overflowed near Integer.MAX/MIN, producing bogus (unsound) bounds.
            case ADD:
                // result = left + right
                // result in [leftMin+rightMin .. leftMax+rightMax]
                changed |= narrowDomain(ac.resultVar,
                        Math.max(resMin, clampToInt((long) exprLeftMin + exprRightMin)),
                        Math.min(resMax, clampToInt((long) exprLeftMax + exprRightMax)));
                // Re-read result bounds
                resDom = domains.get(ac.resultVar);
                if (resDom != null && !resDom.isEmpty()) {
                    resMin = resDom.first();
                    resMax = resDom.last();
                }
                // left in [resMin-rightMax .. resMax-rightMin]
                if (ac.leftVar != null) {
                    changed |= narrowDomain(ac.leftVar,
                            Math.max(exprLeftMin, clampToInt((long) resMin - exprRightMax)),
                            Math.min(exprLeftMax, clampToInt((long) resMax - exprRightMin)));
                }
                // right in [resMin-leftMax .. resMax-leftMin]
                if (ac.rightVar != null) {
                    // Re-read left bounds after possible narrowing
                    if (ac.leftVar != null) {
                        TreeSet<Integer> ld = domains.get(ac.leftVar);
                        if (ld != null && !ld.isEmpty()) {
                            exprLeftMin = ld.first();
                            exprLeftMax = ld.last();
                        }
                    }
                    changed |= narrowDomain(ac.rightVar,
                            Math.max(exprRightMin, clampToInt((long) resMin - exprLeftMax)),
                            Math.min(exprRightMax, clampToInt((long) resMax - exprLeftMin)));
                }
                break;

            case SUB:
                // result = left - right
                // result in [leftMin-rightMax .. leftMax-rightMin]
                changed |= narrowDomain(ac.resultVar,
                        Math.max(resMin, clampToInt((long) exprLeftMin - exprRightMax)),
                        Math.min(resMax, clampToInt((long) exprLeftMax - exprRightMin)));
                resDom = domains.get(ac.resultVar);
                if (resDom != null && !resDom.isEmpty()) {
                    resMin = resDom.first();
                    resMax = resDom.last();
                }
                // left in [resMin+rightMin .. resMax+rightMax]
                if (ac.leftVar != null) {
                    changed |= narrowDomain(ac.leftVar,
                            Math.max(exprLeftMin, clampToInt((long) resMin + exprRightMin)),
                            Math.min(exprLeftMax, clampToInt((long) resMax + exprRightMax)));
                }
                // right in [leftMin-resMax .. leftMax-resMin]
                if (ac.rightVar != null) {
                    if (ac.leftVar != null) {
                        TreeSet<Integer> ld = domains.get(ac.leftVar);
                        if (ld != null && !ld.isEmpty()) {
                            exprLeftMin = ld.first();
                            exprLeftMax = ld.last();
                        }
                    }
                    changed |= narrowDomain(ac.rightVar,
                            Math.max(exprRightMin, clampToInt((long) exprLeftMin - resMax)),
                            Math.min(exprRightMax, clampToInt((long) exprLeftMax - resMin)));
                }
                break;
            // END_CHANGE: ISS-2025-0262

            case MUL:
                // result = left * right — interval multiplication
                // Need to consider all four combinations of bounds for correct interval
                long p1 = (long) exprLeftMin * exprRightMin;
                long p2 = (long) exprLeftMin * exprRightMax;
                long p3 = (long) exprLeftMax * exprRightMin;
                long p4 = (long) exprLeftMax * exprRightMax;
                long mulMin = Math.min(Math.min(p1, p2), Math.min(p3, p4));
                long mulMax = Math.max(Math.max(p1, p2), Math.max(p3, p4));
                // Clamp to int range
                int clampedMulMin = (int) Math.max(mulMin, Integer.MIN_VALUE);
                int clampedMulMax = (int) Math.min(mulMax, Integer.MAX_VALUE);
                changed |= narrowDomain(ac.resultVar,
                        Math.max(resMin, clampedMulMin),
                        Math.min(resMax, clampedMulMax));
                // For backward propagation of MUL, we need division which is more complex.
                // Only do simple cases: if right is a singleton constant > 0
                if (ac.rightVar == null && ac.rightConst != null && ac.rightConst != 0) {
                    resDom = domains.get(ac.resultVar);
                    if (resDom != null && !resDom.isEmpty()) {
                        resMin = resDom.first();
                        resMax = resDom.last();
                    }
                    int c = ac.rightConst;
                    if (c > 0 && ac.leftVar != null) {
                        // left in [ceil(resMin/c) .. floor(resMax/c)]
                        int newMin = ceilDiv(resMin, c);
                        int newMax = floorDiv(resMax, c);
                        changed |= narrowDomain(ac.leftVar, Math.max(exprLeftMin, newMin), Math.min(exprLeftMax, newMax));
                    } else if (c < 0 && ac.leftVar != null) {
                        // Division by negative flips bounds
                        int newMin = ceilDiv(resMax, c);
                        int newMax = floorDiv(resMin, c);
                        changed |= narrowDomain(ac.leftVar, Math.max(exprLeftMin, newMin), Math.min(exprLeftMax, newMax));
                    }
                }
                if (ac.leftVar == null && ac.leftConst != null && ac.leftConst != 0) {
                    resDom = domains.get(ac.resultVar);
                    if (resDom != null && !resDom.isEmpty()) {
                        resMin = resDom.first();
                        resMax = resDom.last();
                    }
                    int c = ac.leftConst;
                    if (c > 0 && ac.rightVar != null) {
                        int newMin = ceilDiv(resMin, c);
                        int newMax = floorDiv(resMax, c);
                        changed |= narrowDomain(ac.rightVar, Math.max(exprRightMin, newMin), Math.min(exprRightMax, newMax));
                    } else if (c < 0 && ac.rightVar != null) {
                        int newMin = ceilDiv(resMax, c);
                        int newMax = floorDiv(resMin, c);
                        changed |= narrowDomain(ac.rightVar, Math.max(exprRightMin, newMin), Math.min(exprRightMax, newMax));
                    }
                }
                break;
        }

        return changed;
    }

    /**
     * Narrow a variable's domain by removing values outside [newMin..newMax].
     * Returns true if the domain was modified.
     */
    private boolean narrowDomain(String var, int newMin, int newMax) {
        TreeSet<Integer> dom = domains.get(var);
        if (dom == null || dom.isEmpty()) return false;

        int oldSize = dom.size();
        int oldFirst = dom.first();
        int oldLast = dom.last();

        // Quick check: if bounds don't narrow anything, skip
        if (newMin <= oldFirst && newMax >= oldLast) return false;

        // If new bounds are inverted, domain becomes empty
        if (newMin > newMax) {
            dom.clear();
            return true;
        }

        // Remove values outside [newMin..newMax]
        if (newMin > oldFirst) {
            dom.headSet(newMin).clear();
        }
        if (newMax < oldLast) {
            dom.tailSet(newMax + 1).clear();
        }

        return dom.size() != oldSize;
    }

    // START_CHANGE: ISS-2025-0262 - saturating cast of a long bound to int range
    private static int clampToInt(long v) {
        if (v > Integer.MAX_VALUE) return Integer.MAX_VALUE;
        if (v < Integer.MIN_VALUE) return Integer.MIN_VALUE;
        return (int) v;
    }
    // END_CHANGE: ISS-2025-0262

    /**
     * Ceiling division for integers (rounds towards positive infinity).
     */
    private static int ceilDiv(int a, int b) {
        if (b > 0) {
            return (a + b - 1) / b;
        } else {
            return (a + b + 1) / b;
        }
    }

    /**
     * Floor division for integers (rounds towards negative infinity).
     */
    private static int floorDiv(int a, int b) {
        return Math.floorDiv(a, b);
    }
    // END_CHANGE: ISS-2025-0175

    // ---- Snapshot / restore for backtracking ----

    /**
     * Take a snapshot of the current state for backtracking.
     */
    public ConstraintStoreSnapshot snapshot() {
        Map<String, TreeSet<Integer>> domCopy = new HashMap<>();
        for (Map.Entry<String, TreeSet<Integer>> entry : domains.entrySet()) {
            domCopy.put(entry.getKey(), new TreeSet<>(entry.getValue()));
        }
        return new ConstraintStoreSnapshot(domCopy, new ArrayList<>(constraints));
    }

    /**
     * Restore a previously taken snapshot.
     */
    public void restore(ConstraintStoreSnapshot snapshot) {
        this.domains = new HashMap<>();
        for (Map.Entry<String, TreeSet<Integer>> entry : snapshot.getDomains().entrySet()) {
            this.domains.put(entry.getKey(), new TreeSet<>(entry.getValue()));
        }
        this.constraints = new ArrayList<>(snapshot.getConstraints());
    }

    /**
     * Clear all domains and constraints.
     */
    public void clear() {
        domains.clear();
        constraints.clear();
    }

    // ================================================================
    // Constraint types
    // ================================================================

    /**
     * Abstract constraint between variables and/or constants.
     */
    public static abstract class Constraint {
        /**
         * Propagate this constraint, filtering domains in the store.
         * Returns true if any domain was modified.
         */
        public abstract boolean propagate(ConstraintStore store);

        /**
         * Get all variable names involved in this constraint.
         */
        public abstract List<String> getVariables();
    }

    /**
     * Binary relational constraint between two operands.
     * Each operand is either a variable name (String) or a constant (Integer).
     */
    public static class BinaryConstraint extends Constraint {
        public enum Type { EQ, NEQ, LT, GT, LEQ, GEQ }

        // START_CHANGE: ISS-2025-0175 - Made package-private for bounds consistency access
        final String leftVar;   // null if left is constant
        final Integer leftConst;
        final String rightVar;  // null if right is constant
        final Integer rightConst;
        final Type type;
        // END_CHANGE: ISS-2025-0175

        public BinaryConstraint(String leftVar, Integer leftConst,
                                String rightVar, Integer rightConst, Type type) {
            this.leftVar = leftVar;
            this.leftConst = leftConst;
            this.rightVar = rightVar;
            this.rightConst = rightConst;
            this.type = type;
        }

        @Override
        public List<String> getVariables() {
            List<String> vars = new ArrayList<>();
            if (leftVar != null) vars.add(leftVar);
            if (rightVar != null) vars.add(rightVar);
            return vars;
        }

        @Override
        public boolean propagate(ConstraintStore store) {
            boolean changed = false;

            if (leftVar != null && rightVar != null) {
                // Both are variables
                TreeSet<Integer> leftDom = store.getDomain(leftVar);
                TreeSet<Integer> rightDom = store.getDomain(rightVar);
                if (leftDom == null || rightDom == null) return false;

                // Filter left domain based on right domain
                TreeSet<Integer> newLeft = new TreeSet<>();
                for (int lv : leftDom) {
                    for (int rv : rightDom) {
                        if (holds(lv, rv)) {
                            newLeft.add(lv);
                            break;
                        }
                    }
                }
                if (newLeft.size() < leftDom.size()) {
                    store.domains.put(leftVar, newLeft);
                    changed = true;
                }

                // Filter right domain based on (updated) left domain
                TreeSet<Integer> currentLeft = store.getDomain(leftVar);
                TreeSet<Integer> newRight = new TreeSet<>();
                for (int rv : rightDom) {
                    for (int lv : currentLeft) {
                        if (holds(lv, rv)) {
                            newRight.add(rv);
                            break;
                        }
                    }
                }
                if (newRight.size() < rightDom.size()) {
                    store.domains.put(rightVar, newRight);
                    changed = true;
                }
            } else if (leftVar != null && rightConst != null) {
                // Left is variable, right is constant
                TreeSet<Integer> leftDom = store.getDomain(leftVar);
                if (leftDom == null) return false;
                TreeSet<Integer> newDom = new TreeSet<>();
                for (int lv : leftDom) {
                    if (holds(lv, rightConst)) {
                        newDom.add(lv);
                    }
                }
                if (newDom.size() < leftDom.size()) {
                    store.domains.put(leftVar, newDom);
                    changed = true;
                }
            } else if (rightVar != null && leftConst != null) {
                // Left is constant, right is variable
                TreeSet<Integer> rightDom = store.getDomain(rightVar);
                if (rightDom == null) return false;
                TreeSet<Integer> newDom = new TreeSet<>();
                for (int rv : rightDom) {
                    if (holds(leftConst, rv)) {
                        newDom.add(rv);
                    }
                }
                if (newDom.size() < rightDom.size()) {
                    store.domains.put(rightVar, newDom);
                    changed = true;
                }
            }
            return changed;
        }

        private boolean holds(int left, int right) {
            switch (type) {
                case EQ:  return left == right;
                case NEQ: return left != right;
                case LT:  return left < right;
                case GT:  return left > right;
                case LEQ: return left <= right;
                case GEQ: return left >= right;
                default:  return false;
            }
        }
    }

    /**
     * All-different constraint: all variables must take distinct values.
     */
    public static class AllDifferentConstraint extends Constraint {
        private final List<String> variables;

        public AllDifferentConstraint(List<String> variables) {
            this.variables = new ArrayList<>(variables);
        }

        @Override
        public List<String> getVariables() {
            return variables;
        }

        @Override
        public boolean propagate(ConstraintStore store) {
            boolean changed = false;

            // If any variable has a singleton domain, remove that value from all others
            for (String var : variables) {
                TreeSet<Integer> dom = store.getDomain(var);
                if (dom != null && dom.size() == 1) {
                    int val = dom.first();
                    for (String other : variables) {
                        if (!other.equals(var)) {
                            TreeSet<Integer> otherDom = store.getDomain(other);
                            if (otherDom != null && otherDom.contains(val)) {
                                otherDom.remove(val);
                                changed = true;
                            }
                        }
                    }
                }
            }
            return changed;
        }
    }

    /**
     * Arithmetic constraint: leftVar = expr1 op expr2 where operands may be
     * variables or constants and op is +, -, *, etc.
     * This handles constraints like X #= Y + 3 by decomposing into
     * appropriate domain filtering.
     */
    public static class ArithmeticConstraint extends Constraint {
        public enum Op { ADD, SUB, MUL }

        // START_CHANGE: ISS-2025-0175 - Made package-private for bounds consistency access
        final String resultVar;
        final String leftVar;
        final Integer leftConst;
        final String rightVar;
        final Integer rightConst;
        final Op op;
        // END_CHANGE: ISS-2025-0175

        public ArithmeticConstraint(String resultVar,
                                     String leftVar, Integer leftConst,
                                     String rightVar, Integer rightConst,
                                     Op op) {
            this.resultVar = resultVar;
            this.leftVar = leftVar;
            this.leftConst = leftConst;
            this.rightVar = rightVar;
            this.rightConst = rightConst;
            this.op = op;
        }

        @Override
        public List<String> getVariables() {
            List<String> vars = new ArrayList<>();
            if (resultVar != null) vars.add(resultVar);
            if (leftVar != null) vars.add(leftVar);
            if (rightVar != null) vars.add(rightVar);
            return vars;
        }

        @Override
        public boolean propagate(ConstraintStore store) {
            boolean changed = false;

            // Compute the set of possible result values from left op right
            Set<Integer> possibleResults = new HashSet<>();
            Collection<Integer> leftVals = getValues(store, leftVar, leftConst);
            Collection<Integer> rightVals = getValues(store, rightVar, rightConst);

            if (leftVals == null || rightVals == null) return false;

            for (int lv : leftVals) {
                for (int rv : rightVals) {
                    Integer res = compute(lv, rv);
                    if (res != null) {
                        possibleResults.add(res);
                    }
                }
            }

            // Filter result domain
            if (resultVar != null) {
                TreeSet<Integer> resDom = store.getDomain(resultVar);
                if (resDom != null) {
                    int before = resDom.size();
                    resDom.retainAll(possibleResults);
                    if (resDom.size() < before) changed = true;
                }
            }

            // Backward propagation: filter left domain
            if (leftVar != null) {
                TreeSet<Integer> lDom = store.getDomain(leftVar);
                TreeSet<Integer> rDom = resultVar != null ? store.getDomain(resultVar) : null;
                Collection<Integer> resultVals = rDom != null ? rDom :
                        (resultVar == null ? Collections.emptySet() : null);
                if (lDom != null && resultVals != null && rightVals != null) {
                    TreeSet<Integer> newLeft = new TreeSet<>();
                    for (int lv : lDom) {
                        for (int rv : rightVals) {
                            Integer res = compute(lv, rv);
                            if (res != null && resultVals.contains(res)) {
                                newLeft.add(lv);
                                break;
                            }
                        }
                    }
                    if (newLeft.size() < lDom.size()) {
                        store.domains.put(leftVar, newLeft);
                        changed = true;
                    }
                }
            }

            // Backward propagation: filter right domain
            if (rightVar != null) {
                TreeSet<Integer> rDom = store.getDomain(rightVar);
                TreeSet<Integer> resDom2 = resultVar != null ? store.getDomain(resultVar) : null;
                Collection<Integer> resultVals2 = resDom2 != null ? resDom2 : null;
                Collection<Integer> leftVals2 = getValues(store, leftVar, leftConst);
                if (rDom != null && resultVals2 != null && leftVals2 != null) {
                    TreeSet<Integer> newRight = new TreeSet<>();
                    for (int rv : rDom) {
                        for (int lv : leftVals2) {
                            Integer res = compute(lv, rv);
                            if (res != null && resultVals2.contains(res)) {
                                newRight.add(rv);
                                break;
                            }
                        }
                    }
                    if (newRight.size() < rDom.size()) {
                        store.domains.put(rightVar, newRight);
                        changed = true;
                    }
                }
            }

            return changed;
        }

        private Collection<Integer> getValues(ConstraintStore store, String var, Integer constant) {
            if (constant != null) {
                return Collections.singleton(constant);
            }
            if (var != null) {
                return store.getDomain(var);
            }
            return null;
        }

        private Integer compute(int left, int right) {
            switch (op) {
                case ADD: return left + right;
                case SUB: return left - right;
                case MUL:
                    // Guard against overflow for very large domains
                    long result = (long) left * (long) right;
                    if (result > Integer.MAX_VALUE || result < Integer.MIN_VALUE) return null;
                    return (int) result;
                default: return null;
            }
        }
    }

    // ================================================================
    // Snapshot class
    // ================================================================

    /**
     * Immutable snapshot of constraint store state for backtracking.
     */
    public static class ConstraintStoreSnapshot {
        private final Map<String, TreeSet<Integer>> domains;
        private final List<Constraint> constraints;

        public ConstraintStoreSnapshot(Map<String, TreeSet<Integer>> domains,
                                        List<Constraint> constraints) {
            this.domains = domains;
            this.constraints = constraints;
        }

        public Map<String, TreeSet<Integer>> getDomains() {
            return domains;
        }

        public List<Constraint> getConstraints() {
            return constraints;
        }
    }
}
// END_CHANGE: ISS-2025-0123
