package it.denzosoft.jprolog.core.engine;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.logging.Logger;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

public class KnowledgeBase {
    private static final Logger LOGGER = Logger.getLogger(KnowledgeBase.class.getName());
    private final List<Rule> rules = new ArrayList<>();
    // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
    private final Map<String, List<Rule>> ruleIndex = new HashMap<>();
    // END_CHANGE: ISS-2025-0075

    /**
     * Add a rule to the knowledge base.
     * 
     * @param rule The rule to add
     */
    public void addRule(Rule rule) {
        rules.add(Objects.requireNonNull(rule, "Rule cannot be null"));
        // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
        addToIndex(rule);
        // END_CHANGE: ISS-2025-0075
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
            // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
            for (Rule rule : rulesToAdd) {
                addToIndex(rule);
            }
            // END_CHANGE: ISS-2025-0075
            LOGGER.fine(rulesToAdd.size() + " rules added.");
        }
    }

    /**
     * Get all rules in the knowledge base.
     * 
     * @return An immutable copy of the rules list
     */
    // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
    public List<Rule> getRules() {
        return Collections.unmodifiableList(rules);
    }

    /**
     * Get rules matching a specific predicate functor and arity.
     * Uses the functor/arity index for O(1) lookup instead of scanning all rules.
     *
     * @param functor The predicate functor name
     * @param arity The predicate arity
     * @return An unmodifiable list of matching rules (empty if none found)
     */
    public List<Rule> getRulesForPredicate(String functor, int arity) {
        String key = functor + "/" + arity;
        List<Rule> indexed = ruleIndex.get(key);
        if (indexed == null) {
            return Collections.emptyList();
        }
        return Collections.unmodifiableList(indexed);
    }
    // END_CHANGE: ISS-2025-0075

    /**
     * Add a rule at the beginning of the knowledge base.
     * 
     * @param rule The rule to add
     */
    public void asserta(Rule rule) {
        rules.add(0, Objects.requireNonNull(rule, "Rule cannot be null"));
        // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
        addToIndexFirst(rule);
        // END_CHANGE: ISS-2025-0075
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
            // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
            removeFromIndex(rule);
            // END_CHANGE: ISS-2025-0075
            LOGGER.fine("Rule retracted: " + rule);
        } else {
            LOGGER.fine("Attempted to retract rule but it was not found: " + rule);
        }
    }

    /**
     * Add a clause to the beginning of the database.
     * 
     * @param clause The clause to add
     */
    public void addClauseFirst(Clause clause) {
        List<Term> bodyList = clause.getBody() != null ?
            java.util.Arrays.asList(clause.getBody()) :
            Collections.emptyList();
        Rule rule = new Rule(clause.getHead(), bodyList);
        rules.add(0, rule);
        // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
        addToIndexFirst(rule);
        // END_CHANGE: ISS-2025-0075
        LOGGER.fine("Clause added at beginning: " + clause);
    }
    
    /**
     * Add a clause to the end of the database.
     * 
     * @param clause The clause to add
     */
    public void addClauseLast(Clause clause) {
        List<Term> bodyList = clause.getBody() != null ?
            java.util.Arrays.asList(clause.getBody()) :
            Collections.emptyList();
        Rule rule = new Rule(clause.getHead(), bodyList);
        rules.add(rule);
        // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
        addToIndex(rule);
        // END_CHANGE: ISS-2025-0075
        LOGGER.fine("Clause added at end: " + clause);
    }
    
    /**
     * Remove clauses that match the given term.
     * 
     * @param term The term to match for retraction
     * @return true if any clauses were removed
     */
    public boolean retractClauses(Term term) {
        boolean removed = false;
        for (int i = rules.size() - 1; i >= 0; i--) {
            Rule rule = rules.get(i);
            if (unifiable(rule.getHead(), term)) {
                rules.remove(i);
                // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
                removeFromIndex(rule);
                // END_CHANGE: ISS-2025-0075
                removed = true;
                LOGGER.fine("Retracted clause: " + rule);
                break; // Only remove first match
            }
        }
        return removed;
    }
    
    /**
     * Remove all clauses that match the given term.
     * 
     * @param term The term to match for retraction
     * @return Number of clauses removed
     */
    public int retractAllClauses(Term term) {
        int count = 0;
        for (int i = rules.size() - 1; i >= 0; i--) {
            Rule rule = rules.get(i);
            if (unifiable(rule.getHead(), term)) {
                rules.remove(i);
                // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
                removeFromIndex(rule);
                // END_CHANGE: ISS-2025-0075
                count++;
                LOGGER.fine("Retracted clause: " + rule);
            }
        }
        return count;
    }
    
    /**
     * Remove all clauses for the given predicate.
     * 
     * @param functor The predicate functor
     * @param arity The predicate arity
     * @return Number of clauses removed
     */
    public int abolishPredicate(String functor, int arity) {
        int count = 0;
        for (int i = rules.size() - 1; i >= 0; i--) {
            Rule rule = rules.get(i);
            Term head = rule.getHead();

            if (matchesPredicate(head, functor, arity)) {
                rules.remove(i);
                count++;
                LOGGER.fine("Abolished clause: " + rule);
            }
        }
        // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
        if (count > 0) {
            String key = functor + "/" + arity;
            ruleIndex.remove(key);
        }
        // END_CHANGE: ISS-2025-0075
        return count;
    }
    
    /**
     * Get all predicate indicators in the knowledge base.
     * 
     * @return Set of predicate indicators (functor/arity)
     */
    // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
    public Set<String> getCurrentPredicates() {
        return new HashSet<>(ruleIndex.keySet());
    }
    // END_CHANGE: ISS-2025-0075
    
    private boolean unifiable(Term term1, Term term2) {
        // Simple unification check - could be more sophisticated
        try {
            java.util.Map<String, Term> bindings = new java.util.HashMap<>();
            return term1.unify(term2, bindings);
        } catch (Exception e) {
            return false;
        }
    }
    
    private boolean matchesPredicate(Term term, String functor, int arity) {
        if (term instanceof Atom) {
            return ((Atom) term).getName().equals(functor) && arity == 0;
        } else if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            return compound.getFunctor().getName().equals(functor) && 
                   compound.getArguments().size() == arity;
        }
        return false;
    }
    
    private String getPredicateIndicator(Term term) {
        if (term instanceof Atom) {
            return ((Atom) term).getName() + "/0";
        } else if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            return compound.getFunctor().getName() + "/" + compound.getArguments().size();
        }
        return "unknown/0";
    }

    // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
    /**
     * Add a rule to the end of the index list for its predicate indicator.
     */
    private void addToIndex(Rule rule) {
        String key = getPredicateIndicator(rule.getHead());
        ruleIndex.computeIfAbsent(key, k -> new ArrayList<>()).add(rule);
    }

    /**
     * Add a rule to the beginning of the index list for its predicate indicator.
     */
    private void addToIndexFirst(Rule rule) {
        String key = getPredicateIndicator(rule.getHead());
        ruleIndex.computeIfAbsent(key, k -> new ArrayList<>()).add(0, rule);
    }

    /**
     * Remove a rule from the index list for its predicate indicator.
     */
    private void removeFromIndex(Rule rule) {
        String key = getPredicateIndicator(rule.getHead());
        List<Rule> indexed = ruleIndex.get(key);
        if (indexed != null) {
            indexed.remove(rule);
            if (indexed.isEmpty()) {
                ruleIndex.remove(key);
            }
        }
    }
    // END_CHANGE: ISS-2025-0075

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("KnowledgeBase:\n");
        for (Rule rule : rules) {
            sb.append("  ").append(rule).append("\n");
        }
        return sb.toString();
    }
}
