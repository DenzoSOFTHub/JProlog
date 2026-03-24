package it.denzosoft.jprolog.builtin.clpfd;

// START_CHANGE: LIM-010 - Basic Constraint Handling Rules (CHR) engine
import it.denzosoft.jprolog.core.terms.*;
import it.denzosoft.jprolog.core.engine.*;

import java.util.*;
import java.util.logging.Logger;

/**
 * Basic CHR (Constraint Handling Rules) store.
 * Supports simplification and propagation rules for user-defined constraints.
 *
 * CHR rules are of the form:
 *   - Simplification: constraint1 \ constraint2 <=> Guard | Body
 *   - Propagation: constraint1 , constraint2 ==> Guard | Body
 *   - Simpagation: constraint1 \ constraint2 <=> Guard | Body
 */
public class CHRStore {
    private static final Logger LOGGER = Logger.getLogger(CHRStore.class.getName());

    /** Active constraints in the store */
    private final List<CHRConstraint> constraints = new ArrayList<>();

    /** Registered CHR rules */
    private final List<CHRRule> rules = new ArrayList<>();

    /** Counter for constraint IDs */
    private int nextId = 0;

    /**
     * A constraint in the store with a unique ID and active flag.
     */
    public static class CHRConstraint {
        final int id;
        final Term term;
        boolean active = true;

        CHRConstraint(int id, Term term) {
            this.id = id;
            this.term = term;
        }

        @Override
        public String toString() {
            return (active ? "" : "~") + term.toString() + "#" + id;
        }
    }

    /**
     * A CHR rule (simplification or propagation).
     */
    public static class CHRRule {
        enum Type { SIMPLIFICATION, PROPAGATION, SIMPAGATION }

        final String name;
        final Type type;
        final List<Term> kept;      // Kept head constraints (for simpagation)
        final List<Term> removed;   // Removed head constraints
        final Term guard;           // Guard condition (null = true)
        final Term body;            // Body goals

        public CHRRule(String name, Type type, List<Term> kept, List<Term> removed, Term guard, Term body) {
            this.name = name;
            this.type = type;
            this.kept = kept != null ? kept : Collections.emptyList();
            this.removed = removed != null ? removed : Collections.emptyList();
            this.guard = guard;
            this.body = body;
        }
    }

    /**
     * Add a CHR rule to the store.
     */
    public void addRule(CHRRule rule) {
        rules.add(rule);
        LOGGER.fine("CHR rule added: " + rule.name);
    }

    /**
     * Add a constraint to the store and run the constraint solver.
     * @return true if the constraint was added successfully (no contradiction)
     */
    public boolean addConstraint(Term constraint, Map<String, Term> bindings) {
        CHRConstraint chr = new CHRConstraint(nextId++, constraint.resolveBindings(bindings));
        constraints.add(chr);
        return runSolver(chr, bindings);
    }

    /**
     * Run the CHR solver attempting to fire rules for the given constraint.
     */
    private boolean runSolver(CHRConstraint newConstraint, Map<String, Term> bindings) {
        boolean changed = true;
        int iterations = 0;
        int maxIterations = 1000;

        while (changed && iterations++ < maxIterations) {
            changed = false;
            for (CHRRule rule : rules) {
                if (!newConstraint.active) break;
                if (tryFireRule(rule, newConstraint, bindings)) {
                    changed = true;
                }
            }
        }
        return true;
    }

    /**
     * Try to fire a CHR rule with the given new constraint as one of the heads.
     */
    private boolean tryFireRule(CHRRule rule, CHRConstraint trigger, Map<String, Term> bindings) {
        List<Term> allHeads = new ArrayList<>();
        allHeads.addAll(rule.kept);
        allHeads.addAll(rule.removed);

        if (allHeads.isEmpty()) return false;

        // Try to match trigger against each head
        for (int i = 0; i < allHeads.size(); i++) {
            Map<String, Term> ruleBindings = new HashMap<>(bindings);
            if (trigger.active && allHeads.get(i).unify(trigger.term, ruleBindings)) {
                // Try to find matching constraints for remaining heads
                List<CHRConstraint> matched = new ArrayList<>();
                matched.add(trigger);

                boolean allMatched = true;
                for (int j = 0; j < allHeads.size(); j++) {
                    if (j == i) continue;
                    CHRConstraint found = findMatchingConstraint(allHeads.get(j), ruleBindings, matched);
                    if (found == null) {
                        allMatched = false;
                        break;
                    }
                    matched.add(found);
                }

                if (allMatched) {
                    // Check guard
                    if (rule.guard == null || checkGuard(rule.guard, ruleBindings)) {
                        // Fire the rule: remove constraints in 'removed' list
                        for (Term removed : rule.removed) {
                            for (CHRConstraint m : matched) {
                                if (m.active && removed.unify(m.term, new HashMap<>(ruleBindings))) {
                                    m.active = false;
                                    break;
                                }
                            }
                        }
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /**
     * Find an active constraint matching the pattern, excluding already matched constraints.
     */
    private CHRConstraint findMatchingConstraint(Term pattern, Map<String, Term> bindings, List<CHRConstraint> excluded) {
        for (CHRConstraint c : constraints) {
            if (!c.active) continue;
            if (excluded.contains(c)) continue;
            Map<String, Term> testBindings = new HashMap<>(bindings);
            if (pattern.unify(c.term, testBindings)) {
                bindings.putAll(testBindings);
                return c;
            }
        }
        return null;
    }

    /**
     * Check if a guard condition holds.
     */
    private boolean checkGuard(Term guard, Map<String, Term> bindings) {
        Term resolved = guard.resolveBindings(bindings);
        if (resolved instanceof Atom && "true".equals(((Atom) resolved).getName())) {
            return true;
        }
        // For simple ground checks
        return resolved.isGround();
    }

    /**
     * Get all active constraints.
     */
    public List<Term> getActiveConstraints() {
        List<Term> active = new ArrayList<>();
        for (CHRConstraint c : constraints) {
            if (c.active) active.add(c.term);
        }
        return active;
    }

    /**
     * Clear all constraints.
     */
    public void clear() {
        constraints.clear();
        nextId = 0;
    }

    /**
     * Remove inactive constraints (garbage collection).
     */
    public void gc() {
        constraints.removeIf(c -> !c.active);
    }
}
// END_CHANGE: LIM-010
