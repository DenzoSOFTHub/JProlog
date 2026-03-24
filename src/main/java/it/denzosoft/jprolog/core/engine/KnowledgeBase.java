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
    // START_CHANGE: ISS-2025-0093 - First-argument indexing for faster clause selection
    /** Two-level index: predicate indicator -> first-arg key -> rules */
    private final Map<String, Map<String, List<Rule>>> firstArgIndex = new HashMap<>();
    private static final String VAR_KEY = "_VAR";
    // END_CHANGE: ISS-2025-0093
    // START_CHANGE: LIM-014 - Multi-argument indexing (second argument)
    /** Three-level index: predicate indicator -> arg1 key -> arg2 key -> rules */
    private final Map<String, Map<String, Map<String, List<Rule>>>> multiArgIndex = new HashMap<>();
    // END_CHANGE: LIM-014

    /**
     * Add a rule to the knowledge base.
     *
     * @param rule The rule to add
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public void addRule(Rule rule) {
        synchronized (this) {
            rules.add(Objects.requireNonNull(rule, "Rule cannot be null"));
            // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
            addToIndex(rule);
            // END_CHANGE: ISS-2025-0075
            LOGGER.fine("Rule added: " + rule);
        }
    }
    // END_CHANGE: ISS-2025-0164

    /**
     * Add multiple rules to the knowledge base.
     *
     * @param rulesToAdd The rules to add
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public void addRules(List<Rule> rulesToAdd) {
        synchronized (this) {
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
    }
    // END_CHANGE: ISS-2025-0164

    /**
     * Get all rules in the knowledge base.
     *
     * @return An immutable copy of the rules list
     */
    // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public List<Rule> getRules() {
        synchronized (this) {
            return Collections.unmodifiableList(new ArrayList<>(rules));
        }
    }
    // END_CHANGE: ISS-2025-0164

    /**
     * Get rules matching a specific predicate functor and arity.
     * Uses the functor/arity index for O(1) lookup instead of scanning all rules.
     *
     * @param functor The predicate functor name
     * @param arity The predicate arity
     * @return An unmodifiable list of matching rules (empty if none found)
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public List<Rule> getRulesForPredicate(String functor, int arity) {
        synchronized (this) {
            String key = functor + "/" + arity;
            List<Rule> indexed = ruleIndex.get(key);
            if (indexed == null) {
                return Collections.emptyList();
            }
            return Collections.unmodifiableList(new ArrayList<>(indexed));
        }
    }
    // END_CHANGE: ISS-2025-0164
    // END_CHANGE: ISS-2025-0075

    // START_CHANGE: ISS-2025-0093 - First-argument indexing for faster clause selection
    /**
     * Get rules matching a specific predicate, filtered by first argument.
     * If the first argument of the query is ground (atom/number/ground compound),
     * returns only rules whose head's first argument matches or is a variable.
     * This dramatically reduces unification attempts for large predicate tables.
     *
     * @param functor The predicate functor name
     * @param arity The predicate arity
     * @param firstArg The resolved first argument of the query (null to skip filtering)
     * @return List of matching rules
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public List<Rule> getRulesWithFirstArgIndex(String functor, int arity, Term firstArg) {
        synchronized (this) {
            String predKey = functor + "/" + arity;
            Map<String, List<Rule>> argIndex = firstArgIndex.get(predKey);
            if (argIndex == null) {
                return Collections.emptyList();
            }

            // If first arg is null, variable, or non-indexable, return all rules for this predicate
            if (firstArg == null || firstArg instanceof Variable) {
                List<Rule> all = ruleIndex.get(predKey);
                return all != null ? Collections.unmodifiableList(new ArrayList<>(all)) : Collections.emptyList();
            }

            String argKey = getFirstArgKey(firstArg);
            List<Rule> matchingRules = argIndex.get(argKey);
            List<Rule> varRules = argIndex.get(VAR_KEY);

            if (matchingRules == null && varRules == null) {
                return Collections.emptyList();
            }
            if (matchingRules == null) {
                return Collections.unmodifiableList(new ArrayList<>(varRules));
            }
            if (varRules == null) {
                return Collections.unmodifiableList(new ArrayList<>(matchingRules));
            }

            // Merge matching + variable rules, preserving original order
            // We need to return them in the order they appear in ruleIndex
            List<Rule> allForPred = ruleIndex.get(predKey);
            if (allForPred == null) return Collections.emptyList();

            // Build a set of eligible rules for fast lookup
            Set<Rule> eligible = new HashSet<>(matchingRules.size() + varRules.size());
            eligible.addAll(matchingRules);
            eligible.addAll(varRules);

            List<Rule> result = new ArrayList<>(eligible.size());
            for (Rule r : allForPred) {
                if (eligible.contains(r)) {
                    result.add(r);
                }
            }
            return result;
        }
    }
    // END_CHANGE: ISS-2025-0164

    /**
     * Get the indexing key for the first argument of a term.
     * Atoms use their name, Numbers use their string value,
     * CompoundTerms use functor/arity, Variables use VAR_KEY.
     */
    private static String getFirstArgKey(Term arg) {
        if (arg instanceof Variable) {
            return VAR_KEY;
        } else if (arg instanceof Atom) {
            return "a:" + ((Atom) arg).getName();
        } else if (arg instanceof it.denzosoft.jprolog.core.terms.Number) {
            return "n:" + ((it.denzosoft.jprolog.core.terms.Number) arg).getValue();
        } else if (arg instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) arg;
            return "c:" + ct.getFunctor().getName() + "/" + ct.getArguments().size();
        }
        return VAR_KEY; // unknown term type, treat as variable
    }

    /**
     * Get the first argument of a rule's head, or null if arity 0.
     */
    private static Term getHeadFirstArg(Rule rule) {
        Term head = rule.getHead();
        if (head instanceof CompoundTerm) {
            List<Term> args = ((CompoundTerm) head).getArguments();
            if (args != null && !args.isEmpty()) {
                return args.get(0);
            }
        }
        return null;
    }

    private void addToFirstArgIndex(Rule rule) {
        String predKey = getPredicateIndicator(rule.getHead());
        Map<String, List<Rule>> argIndex = firstArgIndex.computeIfAbsent(predKey, k -> new HashMap<>());
        Term firstArg = getHeadFirstArg(rule);
        String argKey = (firstArg != null) ? getFirstArgKey(firstArg) : VAR_KEY;
        argIndex.computeIfAbsent(argKey, k -> new ArrayList<>()).add(rule);
        // START_CHANGE: LIM-014 - Multi-argument indexing
        addToMultiArgIndex(rule, predKey, argKey);
        // END_CHANGE: LIM-014
    }

    private void addToFirstArgIndexFirst(Rule rule) {
        String predKey = getPredicateIndicator(rule.getHead());
        Map<String, List<Rule>> argIndex = firstArgIndex.computeIfAbsent(predKey, k -> new HashMap<>());
        Term firstArg = getHeadFirstArg(rule);
        String argKey = (firstArg != null) ? getFirstArgKey(firstArg) : VAR_KEY;
        argIndex.computeIfAbsent(argKey, k -> new ArrayList<>()).add(0, rule);
        // START_CHANGE: LIM-014 - Multi-argument indexing
        addToMultiArgIndexFirst(rule, predKey, argKey);
        // END_CHANGE: LIM-014
    }

    // START_CHANGE: LIM-014 - Multi-argument indexing on second argument
    private void addToMultiArgIndex(Rule rule, String predKey, String arg1Key) {
        Term secondArg = getHeadNthArg(rule, 1);
        String arg2Key = (secondArg != null) ? getFirstArgKey(secondArg) : VAR_KEY;
        multiArgIndex
            .computeIfAbsent(predKey, k -> new HashMap<>())
            .computeIfAbsent(arg1Key, k -> new HashMap<>())
            .computeIfAbsent(arg2Key, k -> new ArrayList<>())
            .add(rule);
    }

    private void addToMultiArgIndexFirst(Rule rule, String predKey, String arg1Key) {
        Term secondArg = getHeadNthArg(rule, 1);
        String arg2Key = (secondArg != null) ? getFirstArgKey(secondArg) : VAR_KEY;
        multiArgIndex
            .computeIfAbsent(predKey, k -> new HashMap<>())
            .computeIfAbsent(arg1Key, k -> new HashMap<>())
            .computeIfAbsent(arg2Key, k -> new ArrayList<>())
            .add(0, rule);
    }

    private Term getHeadNthArg(Rule rule, int n) {
        Term head = rule.getHead();
        if (head instanceof CompoundTerm) {
            List<Term> args = ((CompoundTerm) head).getArguments();
            if (args != null && args.size() > n) {
                Term arg = args.get(n);
                if (arg instanceof Variable) return null; // Variable = no index
                return arg;
            }
        }
        return null;
    }

    /**
     * Get rules using multi-argument indexing (first + second argument).
     * Falls back to first-argument-only if second argument is a variable.
     */
    public List<Rule> getRulesWithMultiArgIndex(String functor, int arity, Term firstArg, Term secondArg) {
        synchronized (this) {
            String predKey = functor + "/" + arity;

            // If no first arg, fall through to basic lookup
            if (firstArg == null || firstArg instanceof Variable) {
                return getRulesForPredicate(functor, arity);
            }

            String arg1Key = getFirstArgKey(firstArg);

            // If no second arg index possible, use first-arg only
            if (secondArg == null || secondArg instanceof Variable) {
                return getRulesWithFirstArgIndex(functor, arity, firstArg);
            }

            String arg2Key = getFirstArgKey(secondArg);

            Map<String, Map<String, List<Rule>>> arg1Index = multiArgIndex.get(predKey);
            if (arg1Index == null) {
                return getRulesWithFirstArgIndex(functor, arity, firstArg);
            }

            // Collect: exact match on both args + variable matches
            List<Rule> result = new ArrayList<>();
            collectMultiArgRules(arg1Index, arg1Key, arg2Key, result);
            collectMultiArgRules(arg1Index, arg1Key, VAR_KEY, result);
            collectMultiArgRules(arg1Index, VAR_KEY, arg2Key, result);
            collectMultiArgRules(arg1Index, VAR_KEY, VAR_KEY, result);

            // Deduplicate while preserving order
            Set<Rule> seen = new HashSet<>();
            List<Rule> deduped = new ArrayList<>();
            for (Rule r : result) {
                if (seen.add(r)) deduped.add(r);
            }
            return Collections.unmodifiableList(deduped);
        }
    }

    private void collectMultiArgRules(Map<String, Map<String, List<Rule>>> arg1Index,
                                       String key1, String key2, List<Rule> result) {
        Map<String, List<Rule>> arg2Index = arg1Index.get(key1);
        if (arg2Index != null) {
            List<Rule> rules = arg2Index.get(key2);
            if (rules != null) result.addAll(rules);
        }
    }
    // END_CHANGE: LIM-014

    private void removeFromFirstArgIndex(Rule rule) {
        String predKey = getPredicateIndicator(rule.getHead());
        Map<String, List<Rule>> argIndex = firstArgIndex.get(predKey);
        if (argIndex != null) {
            Term firstArg = getHeadFirstArg(rule);
            String argKey = (firstArg != null) ? getFirstArgKey(firstArg) : VAR_KEY;
            List<Rule> list = argIndex.get(argKey);
            if (list != null) {
                list.remove(rule);
                if (list.isEmpty()) {
                    argIndex.remove(argKey);
                }
            }
            if (argIndex.isEmpty()) {
                firstArgIndex.remove(predKey);
            }
        }
        // START_CHANGE: ISS-2025-0180 - Core engine bug fixes
        Map<String, Map<String, List<Rule>>> arg1Index = multiArgIndex.get(predKey);
        if (arg1Index != null) {
            Term firstArg = getHeadFirstArg(rule);
            String arg1Key = (firstArg != null) ? getFirstArgKey(firstArg) : VAR_KEY;
            Map<String, List<Rule>> arg2Index = arg1Index.get(arg1Key);
            if (arg2Index != null) {
                Term secondArg = getHeadNthArg(rule, 1);
                String arg2Key = (secondArg != null) ? getFirstArgKey(secondArg) : VAR_KEY;
                List<Rule> list = arg2Index.get(arg2Key);
                if (list != null) {
                    list.remove(rule);
                    if (list.isEmpty()) {
                        arg2Index.remove(arg2Key);
                    }
                }
                if (arg2Index.isEmpty()) {
                    arg1Index.remove(arg1Key);
                }
            }
            if (arg1Index.isEmpty()) {
                multiArgIndex.remove(predKey);
            }
        }
        // END_CHANGE: ISS-2025-0180
    }
    // END_CHANGE: ISS-2025-0093

    /**
     * Add a rule at the beginning of the knowledge base.
     *
     * @param rule The rule to add
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public void asserta(Rule rule) {
        synchronized (this) {
            rules.add(0, Objects.requireNonNull(rule, "Rule cannot be null"));
            // START_CHANGE: ISS-2025-0075 - Add functor/arity indexing for O(1) rule lookup
            addToIndexFirst(rule);
            // END_CHANGE: ISS-2025-0075
            LOGGER.fine("Rule asserted at the beginning: " + rule);
        }
    }
    // END_CHANGE: ISS-2025-0164

    /**
     * Remove a rule from the knowledge base.
     *
     * @param rule The rule to remove
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public void retract(Rule rule) {
        synchronized (this) {
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
    }
    // END_CHANGE: ISS-2025-0164

    /**
     * Add a clause to the beginning of the database.
     *
     * @param clause The clause to add
     */
    // START_CHANGE: ISS-2025-0180 - Core engine bug fixes
    public void addClauseFirst(Clause clause) {
        synchronized (this) {
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
    }
    // END_CHANGE: ISS-2025-0180

    /**
     * Add a clause to the end of the database.
     *
     * @param clause The clause to add
     */
    // START_CHANGE: ISS-2025-0180 - Core engine bug fixes
    public void addClauseLast(Clause clause) {
        synchronized (this) {
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
    }
    // END_CHANGE: ISS-2025-0180

    /**
     * Remove clauses that match the given term.
     *
     * @param term The term to match for retraction
     * @return true if any clauses were removed
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public boolean retractClauses(Term term) {
        synchronized (this) {
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
    }
    // END_CHANGE: ISS-2025-0164

    // START_CHANGE: ISS-2025-0122 - Retract with unification bindings
    /**
     * Retract first matching clause and return unification bindings.
     * This is needed so retract(counter(N)) properly binds N to the value.
     *
     * @param term The term to match
     * @param bindings Current bindings to extend with unification result
     * @return New bindings map if a clause was retracted, null otherwise
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public java.util.Map<String, it.denzosoft.jprolog.core.terms.Term> retractClauseWithBindings(
            Term term, java.util.Map<String, it.denzosoft.jprolog.core.terms.Term> bindings) {
        synchronized (this) {
            for (int i = 0; i < rules.size(); i++) {
                Rule rule = rules.get(i);
                // Copy the rule head to get fresh variables
                Term freshHead = rule.getHead().copy();
                java.util.Map<String, it.denzosoft.jprolog.core.terms.Term> newBindings =
                    new java.util.HashMap<>(bindings);
                if (term.resolveBindings(bindings).unify(freshHead, newBindings)) {
                    rules.remove(i);
                    removeFromIndex(rule);
                    LOGGER.fine("Retracted clause with bindings: " + rule);
                    return newBindings;
                }
            }
            return null;
        }
    }
    // END_CHANGE: ISS-2025-0164
    // END_CHANGE: ISS-2025-0122

    // START_CHANGE: ISS-2025-0164 - Non-deterministic retract/1
    /**
     * Retract ALL matching clauses and return unification bindings for each.
     * This supports non-deterministic retract/1 which should return multiple
     * solutions on backtracking per ISO Prolog.
     *
     * @param term The term to match
     * @param bindings Current bindings to extend with unification result
     * @return List of binding maps, one per retracted clause; empty if none matched
     */
    public java.util.List<java.util.Map<String, it.denzosoft.jprolog.core.terms.Term>> retractAllClausesWithBindings(
            Term term, java.util.Map<String, it.denzosoft.jprolog.core.terms.Term> bindings) {
        synchronized (this) {
            java.util.List<java.util.Map<String, it.denzosoft.jprolog.core.terms.Term>> results =
                new java.util.ArrayList<>();
            for (int i = 0; i < rules.size(); ) {
                Rule rule = rules.get(i);
                Term freshHead = rule.getHead().copy();
                java.util.Map<String, it.denzosoft.jprolog.core.terms.Term> newBindings =
                    new java.util.HashMap<>(bindings);
                if (term.resolveBindings(bindings).unify(freshHead, newBindings)) {
                    rules.remove(i);
                    removeFromIndex(rule);
                    LOGGER.fine("Retracted clause with bindings: " + rule);
                    results.add(newBindings);
                } else {
                    i++;
                }
            }
            return results;
        }
    }
    // END_CHANGE: ISS-2025-0164

    /**
     * Remove all clauses that match the given term.
     *
     * @param term The term to match for retraction
     * @return Number of clauses removed
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public int retractAllClauses(Term term) {
        synchronized (this) {
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
    }
    // END_CHANGE: ISS-2025-0164

    /**
     * Remove all clauses for the given predicate.
     *
     * @param functor The predicate functor
     * @param arity The predicate arity
     * @return Number of clauses removed
     */
    // START_CHANGE: ISS-2025-0164 - Thread safety for KnowledgeBase
    public int abolishPredicate(String functor, int arity) {
        synchronized (this) {
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
                // START_CHANGE: ISS-2025-0093 - Clear first-argument index on abolish
                firstArgIndex.remove(key);
                // END_CHANGE: ISS-2025-0093
                // START_CHANGE: ISS-2025-0180 - Core engine bug fixes
                multiArgIndex.remove(key);
                // END_CHANGE: ISS-2025-0180
            }
            // END_CHANGE: ISS-2025-0075
            return count;
        }
    }
    // END_CHANGE: ISS-2025-0164

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
        // START_CHANGE: ISS-2025-0093 - Maintain first-argument index
        addToFirstArgIndex(rule);
        // END_CHANGE: ISS-2025-0093
    }

    /**
     * Add a rule to the beginning of the index list for its predicate indicator.
     */
    private void addToIndexFirst(Rule rule) {
        String key = getPredicateIndicator(rule.getHead());
        ruleIndex.computeIfAbsent(key, k -> new ArrayList<>()).add(0, rule);
        // START_CHANGE: ISS-2025-0093 - Maintain first-argument index
        addToFirstArgIndexFirst(rule);
        // END_CHANGE: ISS-2025-0093
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
        // START_CHANGE: ISS-2025-0093 - Maintain first-argument index
        removeFromFirstArgIndex(rule);
        // END_CHANGE: ISS-2025-0093
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
