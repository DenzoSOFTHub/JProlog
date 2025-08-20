package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.util.TermCopier;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;


public class QuerySolver {
    private static final Logger LOGGER = Logger.getLogger(QuerySolver.class.getName());
    
    private KnowledgeBase knowledgeBase;
    private BuiltInRegistry builtInRegistry;
    private boolean traceEnabled = false;
    private Prolog prologContext;

    /**
     * Create a query solver.
     * 
     * @param knowledgeBase The knowledge base to query
     * @param builtInRegistry The built-in registry
     */
    public QuerySolver(KnowledgeBase knowledgeBase, BuiltInRegistry builtInRegistry) {
        this.knowledgeBase = knowledgeBase;
        this.builtInRegistry = builtInRegistry;
    }

    /**
     * Enable or disable tracing.
     * 
     * @param traceEnabled true to enable tracing
     */
    public void setTraceEnabled(boolean traceEnabled) {
        this.traceEnabled = traceEnabled;
    }

    /**
     * Solve a query and return all solutions.
     * 
     * @param query The query to solve
     * @return List of all solutions
     */
    public List<Map<String, Term>> solve(Term query) {
        List<Map<String, Term>> solutions = new ArrayList<>();
        solve(query, new HashMap<>(), solutions, CutStatus.notOccurred());
        return solutions;
    }

    // START_CHANGE: ISS-2025-0013 - Add recursion depth limiting to prevent StackOverflowError
    private static final ThreadLocal<Integer> recursionDepth = new ThreadLocal<>();
    private static final int MAX_RECURSION_DEPTH = 100;
    
    /**
     * Solve a goal with current bindings.
     * 
     * @param goal The goal to solve
     * @param bindings Current variable bindings
     * @param solutions List to add successful solutions to
     * @param cutStatus Cut control status
     * @return true if any solutions were found
     */
    public boolean solve(Term goal, Map<String, Term> bindings, List<Map<String, Term>> solutions, CutStatus cutStatus) {
        // Recursion protection is now handled in solveInternal()
        return solveInternal(goal, bindings, solutions, cutStatus);
    }
    
    /**
     * Internal solve method with recursion protection.
     */
    private boolean solveInternal(Term goal, Map<String, Term> bindings, List<Map<String, Term>> solutions, CutStatus cutStatus) {
        // START_CHANGE: ISS-2025-0013 - Add recursion depth protection to solveInternal
        Integer depth = recursionDepth.get();
        if (depth == null) depth = 0;
        
        if (depth > MAX_RECURSION_DEPTH) {
            // Always log this warning, not just when traceEnabled
            System.err.println("WARNING: Maximum recursion depth " + MAX_RECURSION_DEPTH + " reached for goal: " + goal);
            return false; // Prevent infinite recursion
        }
        
        try {
            recursionDepth.set(depth + 1);
            return solveInternalProtected(goal, bindings, solutions, cutStatus);
        } finally {
            if (depth == 0) {
                recursionDepth.remove();
            } else {
                recursionDepth.set(depth);
            }
        }
    }
    
    /**
     * Internal solve method - actual implementation without recursion protection.
     */
    private boolean solveInternalProtected(Term goal, Map<String, Term> bindings, List<Map<String, Term>> solutions, CutStatus cutStatus) {
        // END_CHANGE: ISS-2025-0013
        
        if (traceEnabled) {
            LOGGER.info("Attempting to solve: " + goal + " with " + bindings);
        }
        
        // Check for cut
        if (cutStatus.isCutOccurred()) {
            return false;
        }

        // Base case: If the query is true, add the current bindings to solutions
        if (isTrueAtom(goal)) {
            solutions.add(new HashMap<>(bindings));
            return true;
        }

        // Special handling for conjunction operator ,(A,B)
        if (goal.getName() != null && ",".equals(goal.getName()) && 
            goal.getArguments() != null && goal.getArguments().size() == 2) {
            return handleConjunction(goal, bindings, solutions, cutStatus);
        }

        // Built-in predicate handling - check both functor and arity
        if (goal.getName() != null) {
            String functor = goal.getName();
            int arity = it.denzosoft.jprolog.util.TermUtils.getArity(goal);
            
            
            // Only route to built-in if both functor and arity match
            if (builtInRegistry.isBuiltIn(functor, arity)) {
                return handleBuiltIn(goal, bindings, solutions, cutStatus);
            }
        }

        // Attempt to match the query against the knowledge base
        return solveAgainstKnowledgeBase(goal, bindings, solutions, cutStatus);
    }

    private boolean isTrueAtom(Term goal) {
        return goal.getName() != null && goal.getName().equals("true") && 
               (goal.getArguments() == null || goal.getArguments().isEmpty());
    }

    private boolean handleBuiltIn(Term goal, Map<String, Term> bindings, List<Map<String, Term>> solutions, CutStatus cutStatus) {
        BuiltIn predicate = builtInRegistry.getBuiltIn(goal.getName());
        
        // Special handling for cut
        if (goal.getName().equals("cut") || goal.getName().equals("!")) {
            solutions.add(new HashMap<>(bindings));
            // The cut behavior is handled by returning a special CutStatus in the calling context
            return true;
        }
        
        // Handle context-dependent built-ins
        if (predicate instanceof BuiltInWithContext) {
            return ((BuiltInWithContext) predicate).executeWithContext(this, goal, bindings, solutions);
        }
        
        if (predicate != null) {
            return predicate.execute(goal, bindings, solutions);
        }
        
        return false;
    }

    private boolean solveAgainstKnowledgeBase(Term goal, Map<String, Term> bindings, 
                                              List<Map<String, Term>> solutions, CutStatus cutStatus) {
        boolean foundMatch = false;
        
        for (Rule rule : knowledgeBase.getRules()) {
            if(traceEnabled) {
                LOGGER.info("Trying rule: " + rule);
            }
            
            // Create fresh copies for this attempt, preserving variable sharing
            TermCopier.RuleCopy copiedRule = TermCopier.copyRule(rule.getHead(), rule.getBody());
            Term head = copiedRule.head;
            List<Term> body = copiedRule.body;
            
            // Create a working copy of bindings for this rule attempt
            Map<String, Term> attemptBindings = new HashMap<>(bindings);

            if (head.unify(goal, attemptBindings)) {
                if(traceEnabled) {
                    LOGGER.info("Head unified. New substitution: " + attemptBindings);
                }
                
                if (body.isEmpty()) {
                    // Fact case - add solution
                    solutions.add(new HashMap<>(attemptBindings));
                    foundMatch = true;
                    
                    // Check if cut occurred
                    if (cutStatus.isCutOccurred()) {
                        break;
                    }
                } else {
                    // Rule case - solve body goals
                    // Save the initial bindings after head unification (maps query vars to rule vars)
                    Map<String, Term> headUnificationBindings = new HashMap<>(attemptBindings);
                    
                    CutStatus newCutStatus = CutStatus.notOccurred();
                    List<Map<String, Term>> bodySolutions = new ArrayList<>();
                    if (solveBodyGoals(body, attemptBindings, bodySolutions, newCutStatus)) {
                        // Map rule variable bindings back to query variables
                        for (Map<String, Term> bodySolution : bodySolutions) {
                            Map<String, Term> mappedSolution = mapRuleVariablesToQueryVariables(
                                goal, head, headUnificationBindings, bodySolution);
                            solutions.add(mappedSolution);
                        }
                        foundMatch = true;
                    }
                    
                    // Check if cut occurred in body solving
                    if (newCutStatus.isCutOccurred()) {
                        // Break out of rule matching loop
                        break;
                    }
                }
            } else {
                if(traceEnabled) {
                    LOGGER.info("Unification failed.");
                }
            }
        }
        return foundMatch;
    }
    
    /**
     * Map rule variable bindings back to query variables.
     * When a rule head unifies with a query, query variables get mapped to rule variables.
     * After solving the body, rule variables have values, but we need to map these back
     * to the original query variable names.
     */
    private Map<String, Term> mapRuleVariablesToQueryVariables(
            Term query, Term ruleHead, 
            Map<String, Term> headUnificationBindings, 
            Map<String, Term> bodySolution) {
        
        if (traceEnabled) {
            LOGGER.info("=== MAPPING DEBUG ===");
            LOGGER.info("Query: " + query);
            LOGGER.info("Rule head: " + ruleHead);
            LOGGER.info("Head unification bindings: " + headUnificationBindings);
            LOGGER.info("Body solution: " + bodySolution);
        }
        
        Map<String, Term> result = new HashMap<>();
        
        // Extract variables from the query
        Map<String, Variable> queryVars = extractVariables(query);
        
        if (traceEnabled) {
            LOGGER.info("Query variables: " + queryVars.keySet());
            LOGGER.info("Query variables size: " + queryVars.size());
            LOGGER.info("Query variables isEmpty: " + queryVars.isEmpty());
            for (String key : queryVars.keySet()) {
                LOGGER.info("Key: '" + key + "'");
            }
        }
        
        // For each query variable, find its value by following the mapping chain
        for (String queryVarName : queryVars.keySet()) {
            if (traceEnabled) {
                LOGGER.info("Processing query variable: " + queryVarName);
            }
            
            // Simple approach: look through head bindings to find which rule var maps to this query var
            String ruleVarName = null;
            for (Map.Entry<String, Term> entry : headUnificationBindings.entrySet()) {
                if (entry.getValue() instanceof Variable) {
                    Variable mappedVar = (Variable) entry.getValue();
                    if (queryVarName.equals(mappedVar.getName())) {
                        ruleVarName = entry.getKey();
                        break;
                    }
                }
            }
            
            if (traceEnabled) {
                LOGGER.info("Query var " + queryVarName + " maps to rule var " + ruleVarName);
            }
            
            // If we found the rule variable, look up its value in body solution
            if (ruleVarName != null && bodySolution.containsKey(ruleVarName)) {
                Term value = bodySolution.get(ruleVarName);
                result.put(queryVarName, value);
                if (traceEnabled) {
                    LOGGER.info("Mapped " + queryVarName + " -> " + value);
                }
            }
        }
        
        if (traceEnabled) {
            LOGGER.info("Final mapped result: " + result);
            LOGGER.info("=== END MAPPING DEBUG ===");
        }
        
        return result;
    }
    
    /**
     * Extract all variables from a term and return them as a map.
     */
    private Map<String, Variable> extractVariables(Term term) {
        Map<String, Variable> vars = new HashMap<>();
        extractVariablesRecursive(term, vars);
        return vars;
    }
    
    private void extractVariablesRecursive(Term term, Map<String, Variable> vars) {
        if (term instanceof Variable) {
            Variable var = (Variable) term;
            vars.put(var.getName(), var);
        } else if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            for (Term arg : compound.getArguments()) {
                extractVariablesRecursive(arg, vars);
            }
        }
    }
    
    /**
     * Find the value of a query variable by following the mapping chain:
     * ruleVar -> queryVar (from head unification) -> value (from body solution)
     */
    private Term findVariableValue(String queryVarName, 
                                  Map<String, Term> headUnificationBindings, 
                                  Map<String, Term> bodySolution) {
        
        if (traceEnabled) {
            LOGGER.info("Finding value for query var: " + queryVarName);
        }
        
        // Look for a rule variable that maps to this query variable
        for (Map.Entry<String, Term> entry : headUnificationBindings.entrySet()) {
            String ruleVarName = entry.getKey();
            Term mappedValue = entry.getValue();
            
            if (traceEnabled) {
                LOGGER.info("Checking rule var " + ruleVarName + " -> " + mappedValue + 
                           " (type: " + mappedValue.getClass().getSimpleName() + ")");
            }
            
            // Check if this rule variable maps to our query variable
            if (mappedValue instanceof Variable && 
                queryVarName.equals(((Variable) mappedValue).getName())) {
                
                if (traceEnabled) {
                    LOGGER.info("Found mapping: " + ruleVarName + " -> " + queryVarName);
                    LOGGER.info("Looking for " + ruleVarName + " in body solution: " + bodySolution.containsKey(ruleVarName));
                }
                
                // Found the mapping: ruleVar -> queryVar
                // Now look up the rule variable's value in the body solution
                if (bodySolution.containsKey(ruleVarName)) {
                    Term value = bodySolution.get(ruleVarName);
                    if (traceEnabled) {
                        LOGGER.info("Found value: " + value);
                    }
                    return value;
                }
            }
        }
        
        // Also check if the query variable is directly in the body solution
        if (bodySolution.containsKey(queryVarName)) {
            return bodySolution.get(queryVarName);
        }
        
        if (traceEnabled) {
            LOGGER.info("No value found for " + queryVarName);
        }
        
        return null;
    }

    private boolean solveBodyGoals(List<Term> body, Map<String, Term> attemptBindings,
                                   List<Map<String, Term>> solutions, CutStatus cutStatus) {
        boolean bodySucceeded = true;
        List<Map<String, Term>> bodySolutions = new ArrayList<>();
        bodySolutions.add(new HashMap<>(attemptBindings));
        
        // For each body term, find all solutions
        for (Term bodyTerm : body) {
            List<Map<String, Term>> nextSolutions = new ArrayList<>();
            
            // Special handling for cut
            if (bodyTerm.getName() != null && 
                (bodyTerm.getName().equals("!") || bodyTerm.getName().equals("cut"))) {
                if (!bodySolutions.isEmpty()) {
                    // Add the first solution - cut commits to first choice
                    nextSolutions.add(bodySolutions.get(0));
                }
                bodySolutions = nextSolutions;
                // Cut prevents backtracking - stop processing remaining goals in normal order
                break;
            }
            
            for (Map<String, Term> currentBindings : bodySolutions) {
                List<Map<String, Term>> termSolutions = new ArrayList<>();
                CutStatus bodyCutStatus = CutStatus.notOccurred();
                
                if (solveInternal(bodyTerm, currentBindings, termSolutions, bodyCutStatus)) {
                    nextSolutions.addAll(termSolutions);
                    
                    // If cut occurred, stop processing
                    if (bodyCutStatus.isCutOccurred()) {
                        break;
                    }
                }
            }
            
            bodySolutions = nextSolutions;
            
            if (bodySolutions.isEmpty()) {
                bodySucceeded = false;
                break;
            }
        }
        
        if (bodySucceeded && !bodySolutions.isEmpty()) {
            solutions.addAll(bodySolutions);
            return true;
        }
        return false;
    }

    /**
     * Handle conjunction ,(A,B) - solve A, then solve B with the results from A.
     */
    private boolean handleConjunction(Term conjunction, Map<String, Term> bindings, 
                                     List<Map<String, Term>> solutions, CutStatus cutStatus) {
        List<Term> goals = new ArrayList<>();
        goals.add(conjunction.getArguments().get(0)); // A
        goals.add(conjunction.getArguments().get(1)); // B
        
        // Use the existing solveBodyGoals logic which handles conjunctions properly
        return solveBodyGoals(goals, bindings, solutions, cutStatus);
    }

    /**
     * Get the knowledge base.
     * 
     * @return The knowledge base
     */
    public KnowledgeBase getKnowledgeBase() {
        return knowledgeBase;
    }

    /**
     * Set the knowledge base.
     * 
     * @param knowledgeBase The knowledge base
     */
    public void setKnowledgeBase(KnowledgeBase knowledgeBase) {
        this.knowledgeBase = knowledgeBase;
    }

    /**
     * Get the built-in registry.
     * 
     * @return The built-in registry
     */
    public BuiltInRegistry getBuiltInRegistry() {
        return builtInRegistry;
    }

    /**
     * Set the built-in registry.
     * 
     * @param builtInRegistry The built-in registry
     */
    public void setBuiltInRegistry(BuiltInRegistry builtInRegistry) {
        this.builtInRegistry = builtInRegistry;
    }
    
    /**
     * Get the Prolog context.
     * 
     * @return The Prolog context
     */
    public Prolog getPrologContext() {
        return prologContext;
    }
    
    /**
     * Set the Prolog context.
     * 
     * @param prologContext The Prolog context
     */
    public void setPrologContext(Prolog prologContext) {
        this.prologContext = prologContext;
    }
}
