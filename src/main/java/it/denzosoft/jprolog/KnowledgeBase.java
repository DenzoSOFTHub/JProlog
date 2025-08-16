package it.denzosoft.jprolog;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.logging.Logger;

public class KnowledgeBase {
    private static final Logger LOGGER = Logger.getLogger(KnowledgeBase.class.getName());
    private final List<Rule> rules = new ArrayList<>();

    /**
     * Add a rule to the knowledge base.
     * 
     * @param rule The rule to add
     */
    public void addRule(Rule rule) {
        rules.add(Objects.requireNonNull(rule, "Rule cannot be null"));
        LOGGER.fine("Rule added: " + rule);
    }

    /**
     * Add multiple rules to the knowledge base.
     * 
     * @param rulesToAdd The rules to add
     */
    public void addRules(List<Rule> rulesToAdd) {
        if (rulesToAdd != null) {
            rules.addAll(rulesToAdd);
            LOGGER.fine(rulesToAdd.size() + " rules added.");
        }
    }

    /**
     * Get all rules in the knowledge base.
     * 
     * @return An immutable copy of the rules list
     */
    public List<Rule> getRules() {
        return Collections.unmodifiableList(new ArrayList<>(rules));
    }

    /**
     * Add a rule at the beginning of the knowledge base.
     * 
     * @param rule The rule to add
     */
    public void asserta(Rule rule) {
        rules.add(0, Objects.requireNonNull(rule, "Rule cannot be null"));
        LOGGER.fine("Rule asserted at the beginning: " + rule);
    }

    /**
     * Remove a rule from the knowledge base.
     * 
     * @param rule The rule to remove
     */
    public void retract(Rule rule) {
        boolean removed = rules.removeIf(r -> r.equals(rule));
        if (removed) {
            LOGGER.fine("Rule retracted: " + rule);
        } else {
            LOGGER.fine("Attempted to retract rule but it was not found: " + rule);
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("KnowledgeBase:\n");
        for (Rule rule : rules) {
            sb.append("  ").append(rule).append("\n");
        }
        return sb.toString();
    }
}
