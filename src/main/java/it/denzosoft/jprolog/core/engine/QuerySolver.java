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
    // START_CHANGE: CR-2025-0002 - Track current module context for resolution
    private it.denzosoft.jprolog.core.module.Module currentModuleContext = null;
    // END_CHANGE: CR-2025-0002

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
        // START_CHANGE: ISS-2025-0069 - Deep-resolve variable chains in returned solutions
        // After solving, variable bindings may contain chains like X→Y→Z→hello.
        // Resolve all chains so the caller gets final values.
        for (Map<String, Term> solution : solutions) {
            deepResolveSolution(solution);
        }
        // END_CHANGE: ISS-2025-0069
        return solutions;
    }

    // START_CHANGE: ISS-2025-0078 - Optimize deepResolveSolution with two-pass approach
    /**
     * Resolve all variable chains in a solution map.
     * Uses a two-pass approach: first resolves Variables and CompoundTerms,
     * then a second pass catches any remaining Variable chains.
     */
    private void deepResolveSolution(Map<String, Term> solution) {
        // Single-pass resolution: resolve each value through the binding map
        for (Map.Entry<String, Term> entry : solution.entrySet()) {
            Term value = entry.getValue();
            if (value instanceof Variable || value instanceof CompoundTerm) {
                Term resolved = value.resolveBindings(solution);
                if (resolved != value) {
                    entry.setValue(resolved);
                }
            }
        }
        // Second pass for any remaining chains (e.g., X→Y→Z where Y was resolved after X)
        for (Map.Entry<String, Term> entry : solution.entrySet()) {
            Term value = entry.getValue();
            if (value instanceof Variable) {
                Term resolved = value.resolveBindings(solution);
                if (resolved != value) {
                    entry.setValue(resolved);
                }
            }
        }
    }
    // END_CHANGE: ISS-2025-0078

    // START_CHANGE: ISS-2025-0013 - Add recursion depth limiting to prevent StackOverflowError
    private static final ThreadLocal<Integer> recursionDepth = new ThreadLocal<>();
    // START_CHANGE: ISS-2025-0050 - Increase recursion depth for larger programs
    private static final int MAX_RECURSION_DEPTH = 1000;
    // END_CHANGE: ISS-2025-0050
    
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

        // START_CHANGE: ISS-2025-0085 - Handle module-qualified calls Module:Goal
        if (goal.getName() != null && ":".equals(goal.getName()) &&
            goal.getArguments() != null && goal.getArguments().size() == 2) {
            Term moduleTerm = goal.getArguments().get(0).resolveBindings(bindings);
            Term innerGoal = goal.getArguments().get(1).resolveBindings(bindings);

            // Try module-specific lookup if we have a Prolog context with ModuleManager
            if (moduleTerm instanceof Atom && prologContext != null) {
                String moduleName = ((Atom) moduleTerm).getName();
                it.denzosoft.jprolog.core.module.Module module =
                    prologContext.getModuleManager().getModule(moduleName);
                if (module != null) {
                    return solveInModuleContext(innerGoal, module, bindings, solutions, cutStatus);
                }
            }
            // Fallback: solve inner goal in global context
            return solveInternal(innerGoal, bindings, solutions, cutStatus);
        }
        // END_CHANGE: ISS-2025-0085

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

    // START_CHANGE: ISS-2025-0067 - Preserve input bindings in knowledge base solutions
    private boolean solveAgainstKnowledgeBase(Term goal, Map<String, Term> bindings,
                                              List<Map<String, Term>> solutions, CutStatus cutStatus) {
        boolean foundMatch = false;

        // START_CHANGE: CR-2025-0002 - Check module context first for rule lookup
        String functor = goal.getName();
        int arity = it.denzosoft.jprolog.util.TermUtils.getArity(goal);
        List<Rule> candidateRules;

        if (currentModuleContext != null && functor != null) {
            it.denzosoft.jprolog.core.module.PredicateSignature sig =
                new it.denzosoft.jprolog.core.module.PredicateSignature(functor, arity);
            candidateRules = currentModuleContext.getRulesForPredicate(sig);
            if (candidateRules.isEmpty()) {
                // Fall back to global KB
                candidateRules = knowledgeBase.getRulesForPredicate(functor, arity);
            }
        } else if (functor != null) {
            // START_CHANGE: ISS-2025-0075 - Use functor/arity indexing for O(1) rule lookup
            candidateRules = knowledgeBase.getRulesForPredicate(functor, arity);
            // END_CHANGE: ISS-2025-0075
            // START_CHANGE: CR-2025-0002 - Check imported module predicates for unqualified calls
            if (candidateRules.isEmpty() && prologContext != null) {
                it.denzosoft.jprolog.core.module.ModuleManager mm = prologContext.getModuleManager();
                it.denzosoft.jprolog.core.module.PredicateSignature sig =
                    new it.denzosoft.jprolog.core.module.PredicateSignature(functor, arity);
                it.denzosoft.jprolog.core.module.Module resolved = mm.getCurrentModule().resolvePredicate(sig);
                if (resolved != null) {
                    candidateRules = resolved.getRulesForPredicate(sig);
                }
            }
            // END_CHANGE: CR-2025-0002
        } else {
            candidateRules = knowledgeBase.getRules();
        }
        // END_CHANGE: CR-2025-0002

        for (Rule rule : candidateRules) {
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
                            // START_CHANGE: ISS-2025-0071 - Use bodySolution as base to preserve transitive bindings
                            // Use the full body solution (which contains all accumulated bindings
                            // including transitive variable mappings from recursive calls) as base,
                            // then overlay with the mapped query variables. This ensures that
                            // variable chains like X->_R1_G->_R2_G->4 are fully resolved.
                            Map<String, Term> mappedSolution = new HashMap<>(bodySolution);
                            mappedSolution.putAll(mapRuleVariablesToQueryVariables(
                                goal, head, headUnificationBindings, bodySolution));
                            solutions.add(mappedSolution);
                            // END_CHANGE: ISS-2025-0071
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

    // START_CHANGE: CR-2025-0002 - Module-isolated rule lookup
    /**
     * Solve a goal in the context of a specific module.
     * First tries built-ins, then looks up rules from the module's local rules.
     */
    private boolean solveInModuleContext(Term goal, it.denzosoft.jprolog.core.module.Module module,
                                         Map<String, Term> bindings, List<Map<String, Term>> solutions,
                                         CutStatus cutStatus) {
        // Set module context so that body goal resolution uses module rules
        it.denzosoft.jprolog.core.module.Module savedContext = currentModuleContext;
        currentModuleContext = module;
        try {
            return solveInternal(goal, bindings, solutions, cutStatus);
        } finally {
            currentModuleContext = savedContext;
        }
    }
    // END_CHANGE: CR-2025-0002

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
            
            // START_CHANGE: ISS-2025-0065 - Fix variable mapping to handle both unification directions
            // Look through head bindings to find which rule var maps to this query var.
            // Unification can produce either direction: ruleVar -> queryVar OR queryVar -> ruleVar.
            String ruleVarName = null;
            // Direction 1: ruleVar -> Variable(queryVar)
            for (Map.Entry<String, Term> entry : headUnificationBindings.entrySet()) {
                if (entry.getValue() instanceof Variable) {
                    Variable mappedVar = (Variable) entry.getValue();
                    if (queryVarName.equals(mappedVar.getName())) {
                        ruleVarName = entry.getKey();
                        break;
                    }
                }
            }
            // Direction 2: queryVar -> Variable(ruleVar)
            if (ruleVarName == null && headUnificationBindings.containsKey(queryVarName)) {
                Term mappedValue = headUnificationBindings.get(queryVarName);
                if (mappedValue instanceof Variable) {
                    ruleVarName = ((Variable) mappedValue).getName();
                } else {
                    // Query variable was directly unified to a value (not a variable)
                    // Deep-resolve using combined bindings (body solution + head unification)
                    // Body solution may not contain all variables (after recursive mapping),
                    // but headUnificationBindings has the complete set from the current rule level.
                    Map<String, Term> combinedBindings = new HashMap<>(headUnificationBindings);
                    combinedBindings.putAll(bodySolution);
                    Term resolved = mappedValue.resolveBindings(combinedBindings);
                    if (traceEnabled) {
                        LOGGER.info("Direction 2 non-var: " + queryVarName + " -> resolved=" + resolved);
                    }
                    result.put(queryVarName, resolved);
                }
            }
            // END_CHANGE: ISS-2025-0065
            
            if (traceEnabled) {
                LOGGER.info("Query var " + queryVarName + " maps to rule var " + ruleVarName);
            }
            
            // If we found the rule variable, look up its value in body solution or head bindings
            // START_CHANGE: ISS-2025-0065 - Deep-resolve value through combined bindings
            Map<String, Term> combinedForResolve = null;
            if (ruleVarName != null && (bodySolution.containsKey(ruleVarName) || headUnificationBindings.containsKey(ruleVarName))) {
                Term value = bodySolution.containsKey(ruleVarName) ? bodySolution.get(ruleVarName) : headUnificationBindings.get(ruleVarName);
                // Deep resolve using combined bindings
                if (combinedForResolve == null) {
                    combinedForResolve = new HashMap<>(headUnificationBindings);
                    combinedForResolve.putAll(bodySolution);
                }
                value = value.resolveBindings(combinedForResolve);
                result.put(queryVarName, value);
                if (traceEnabled) {
                    LOGGER.info("Mapped " + queryVarName + " -> " + value);
                }
            }
            // END_CHANGE: ISS-2025-0065
        }
        
        if (traceEnabled) {
            LOGGER.info("Final mapped result: " + result);
            LOGGER.info("=== END MAPPING DEBUG ===");
        }
        
        return result;
    }
    
    // START_CHANGE: ISS-2025-0065 - Follow binding chain to resolve variables
    /**
     * Follow a binding chain to resolve a term to its final value.
     * If the term is a Variable and it has a binding in the solution, follow it.
     * Uses a visited set to prevent infinite loops.
     */
    private Term resolveBindingChain(Term value, Map<String, Term> bindings) {
        java.util.Set<String> visited = new java.util.HashSet<>();
        while (value instanceof Variable) {
            String varName = ((Variable) value).getName();
            if (visited.contains(varName) || !bindings.containsKey(varName)) {
                break;
            }
            visited.add(varName);
            value = bindings.get(varName);
        }
        return value;
    }
    // END_CHANGE: ISS-2025-0065

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
            
            // START_CHANGE: ISS-2025-0054 - Fix cut semantics: continue body, prevent clause backtracking
            // ISO Prolog: cut (!) commits to the current clause choice and succeeds.
            // Remaining body goals AFTER cut must still be executed.
            // Cut only prevents backtracking to alternative clauses.
            if (bodyTerm.getName() != null &&
                (bodyTerm.getName().equals("!") || bodyTerm.getName().equals("cut"))) {
                if (!bodySolutions.isEmpty()) {
                    // Commit to first choice point only
                    nextSolutions.add(bodySolutions.get(0));
                }
                bodySolutions = nextSolutions;
                // Signal cut to prevent backtracking to alternative clauses
                cutStatus.setCutOccurred();
                // Continue executing remaining body goals (do NOT break)
                continue;
            }
            // END_CHANGE: ISS-2025-0054
            
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
