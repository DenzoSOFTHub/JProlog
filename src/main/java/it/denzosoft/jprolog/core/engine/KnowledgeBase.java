package it.denzosoft.jprolog.core.engine;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
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
        return count;
    }
    
    /**
     * Get all predicate indicators in the knowledge base.
     * 
     * @return Set of predicate indicators (functor/arity)
     */
    public Set<String> getCurrentPredicates() {
        Set<String> predicates = new HashSet<>();
        for (Rule rule : rules) {
            Term head = rule.getHead();
            String indicator = getPredicateIndicator(head);
            predicates.add(indicator);
        }
        return predicates;
    }
    
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

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("KnowledgeBase:\n");
        for (Rule rule : rules) {
            sb.append("  ").append(rule).append("\n");
        }
        return sb.toString();
    }
}
