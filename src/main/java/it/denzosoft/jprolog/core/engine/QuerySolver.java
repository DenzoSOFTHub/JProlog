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
    // START_CHANGE: ISS-2025-0090 - Debug controller for step-by-step execution
    private DebugController debugController;
    // END_CHANGE: ISS-2025-0090
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

    // START_CHANGE: ISS-2025-0090 - Debug controller for interactive debugging
    /**
     * Set the debug controller for step-by-step execution.
     * When set, the solver will notify the controller at each debug port.
     * @param debugController the controller, or null to disable debugging
     */
    public void setDebugController(DebugController debugController) {
        this.debugController = debugController;
    }

    /**
     * Get the current debug controller.
     */
    public DebugController getDebugController() {
        return debugController;
    }
    // END_CHANGE: ISS-2025-0090

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

    // START_CHANGE: ISS-2025-0091 - Replace ThreadLocal with instance field for faster access
    private int recursionDepth = 0;
    private static final int MAX_RECURSION_DEPTH = 10000;
    // END_CHANGE: ISS-2025-0091
    
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
    // START_CHANGE: ISS-2025-0091 - Use instance field instead of ThreadLocal for recursion depth
    private boolean solveInternal(Term goal, Map<String, Term> bindings, List<Map<String, Term>> solutions, CutStatus cutStatus) {
        if (recursionDepth > MAX_RECURSION_DEPTH) {
            System.err.println("WARNING: Maximum recursion depth " + MAX_RECURSION_DEPTH + " reached for goal: " + goal);
            return false;
        }

        recursionDepth++;
        try {
            return solveInternalProtected(goal, bindings, solutions, cutStatus);
        } finally {
            recursionDepth--;
        }
    }
    // END_CHANGE: ISS-2025-0091
    
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

        // START_CHANGE: ISS-2025-0090 - Debug CALL port notification
        if (debugController != null && goal.getName() != null &&
            !",".equals(goal.getName()) && !":".equals(goal.getName())) {
            try {
                debugController.notifyPort(DebugEvent.Port.CALL, goal, bindings, recursionDepth);
            } catch (DebugController.DebugStopException e) {
                return false;
            }
        }
        // END_CHANGE: ISS-2025-0090

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
            return true;
        }

        boolean result;
        // Handle context-dependent built-ins
        if (predicate instanceof BuiltInWithContext) {
            result = ((BuiltInWithContext) predicate).executeWithContext(this, goal, bindings, solutions);
        } else if (predicate != null) {
            result = predicate.execute(goal, bindings, solutions);
        } else {
            result = false;
        }

        // START_CHANGE: ISS-2025-0090 - Debug EXIT/FAIL port for built-ins
        if (debugController != null && goal.getName() != null &&
            !",".equals(goal.getName()) && !":".equals(goal.getName())) {
            try {
                debugController.notifyPort(
                    result ? DebugEvent.Port.EXIT : DebugEvent.Port.FAIL,
                    goal, bindings, recursionDepth);
            } catch (DebugController.DebugStopException e) {
                return false;
            }
        }
        // END_CHANGE: ISS-2025-0090

        return result;
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

        // START_CHANGE: ISS-2025-0090 - Track rule attempts for REDO port
        boolean isFirstAttempt = true;
        // END_CHANGE: ISS-2025-0090

        for (Rule rule : candidateRules) {
            if(traceEnabled) {
                LOGGER.info("Trying rule: " + rule);
            }

            // START_CHANGE: ISS-2025-0090 - Emit REDO port on second+ rule attempts
            if (debugController != null && !isFirstAttempt && goal.getName() != null &&
                !",".equals(goal.getName()) && !":".equals(goal.getName())) {
                try {
                    debugController.notifyPort(DebugEvent.Port.REDO, goal, bindings, recursionDepth);
                } catch (DebugController.DebugStopException e) {
                    return false;
                }
            }
            isFirstAttempt = false;
            // END_CHANGE: ISS-2025-0090

            // START_CHANGE: ISS-2025-0092 - Skip TermCopier for ground facts (no variables to rename)
            Term head;
            List<Term> body;
            if (rule.isGroundFact()) {
                head = rule.getHead();
                body = rule.getBody();
            } else {
                TermCopier.RuleCopy copiedRule = TermCopier.copyRule(rule.getHead(), rule.getBody());
                head = copiedRule.head;
                body = copiedRule.body;
            }
            // END_CHANGE: ISS-2025-0092

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
                    Map<String, Term> headUnificationBindings = new HashMap<>(attemptBindings);

                    CutStatus newCutStatus = CutStatus.notOccurred();
                    List<Map<String, Term>> bodySolutions = new ArrayList<>();
                    if (solveBodyGoals(body, attemptBindings, bodySolutions, newCutStatus)) {
                        Map<String, Variable> queryVars = extractVariables(goal);
                        for (Map<String, Term> bodySolution : bodySolutions) {
                            Map<String, Term> mappedSolution = new HashMap<>(bodySolution);
                            mappedSolution.putAll(mapRuleVariablesToQueryVariablesCached(
                                queryVars, head, headUnificationBindings, bodySolution));
                            solutions.add(mappedSolution);
                        }
                        foundMatch = true;
                    }

                    if (newCutStatus.isCutOccurred()) {
                        break;
                    }
                }
            } else {
                if(traceEnabled) {
                    LOGGER.info("Unification failed.");
                }
            }
        }

        // START_CHANGE: ISS-2025-0090 - Debug EXIT/FAIL port for KB goals
        if (debugController != null && goal.getName() != null &&
            !",".equals(goal.getName()) && !":".equals(goal.getName())) {
            try {
                debugController.notifyPort(
                    foundMatch ? DebugEvent.Port.EXIT : DebugEvent.Port.FAIL,
                    goal, bindings, recursionDepth);
            } catch (DebugController.DebugStopException e) {
                return false;
            }
        }
        // END_CHANGE: ISS-2025-0090

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

    // START_CHANGE: ISS-2025-0091 - Optimized: reverse index O(N), lazy combinedBindings, no double lookups
    /**
     * Optimized version of mapRuleVariablesToQueryVariables that:
     * 1. Accepts pre-extracted query variables (no redundant extractVariables)
     * 2. Builds a reverse index (queryVarName -> ruleVarName) in O(N) instead of O(N²) linear scan
     * 3. Creates combined bindings map at most once (lazy)
     * 4. Uses get() + null check instead of containsKey() + get()
     */
    private Map<String, Term> mapRuleVariablesToQueryVariablesCached(
            Map<String, Variable> queryVars, Term ruleHead,
            Map<String, Term> headUnificationBindings,
            Map<String, Term> bodySolution) {

        Map<String, Term> result = new HashMap<>();

        // Build reverse index once: queryVarName -> ruleVarName (Direction 1)
        // This replaces O(N²) linear scan with O(N) build + O(1) lookups
        Map<String, String> reverseIndex = new HashMap<>();
        for (Map.Entry<String, Term> entry : headUnificationBindings.entrySet()) {
            if (entry.getValue() instanceof Variable) {
                reverseIndex.put(((Variable) entry.getValue()).getName(), entry.getKey());
            }
        }

        Map<String, Term> combinedBindings = null;

        for (String queryVarName : queryVars.keySet()) {
            // O(1) reverse lookup instead of O(N) linear scan
            String ruleVarName = reverseIndex.get(queryVarName);

            // Direction 2: queryVar -> Variable(ruleVar) or direct value
            if (ruleVarName == null) {
                Term mappedValue = headUnificationBindings.get(queryVarName);
                if (mappedValue != null) {
                    if (mappedValue instanceof Variable) {
                        ruleVarName = ((Variable) mappedValue).getName();
                    } else {
                        if (combinedBindings == null) {
                            combinedBindings = new HashMap<>(headUnificationBindings);
                            combinedBindings.putAll(bodySolution);
                        }
                        result.put(queryVarName, mappedValue.resolveBindings(combinedBindings));
                    }
                }
            }

            if (ruleVarName != null) {
                // Use get + null check instead of containsKey + get (avoids double hash)
                Term value = bodySolution.get(ruleVarName);
                if (value == null) {
                    value = headUnificationBindings.get(ruleVarName);
                }
                if (value != null) {
                    if (combinedBindings == null) {
                        combinedBindings = new HashMap<>(headUnificationBindings);
                        combinedBindings.putAll(bodySolution);
                    }
                    result.put(queryVarName, value.resolveBindings(combinedBindings));
                }
            }
        }

        return result;
    }
    // END_CHANGE: ISS-2025-0091

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
    
    private boolean solveBodyGoals(List<Term> body, Map<String, Term> attemptBindings,
                                   List<Map<String, Term>> solutions, CutStatus cutStatus) {
        boolean bodySucceeded = true;
        List<Map<String, Term>> bodySolutions = new ArrayList<>();
        bodySolutions.add(new HashMap<>(attemptBindings));

        // For each body term, find all solutions
        for (Term bodyTerm : body) {
            List<Map<String, Term>> nextSolutions = new ArrayList<>();

            // START_CHANGE: ISS-2025-0054 - Fix cut semantics: continue body, prevent clause backtracking
            if (bodyTerm.getName() != null &&
                (bodyTerm.getName().equals("!") || bodyTerm.getName().equals("cut"))) {
                if (!bodySolutions.isEmpty()) {
                    nextSolutions.add(bodySolutions.get(0));
                }
                bodySolutions = nextSolutions;
                cutStatus.setCutOccurred();
                continue;
            }
            // END_CHANGE: ISS-2025-0054

            for (Map<String, Term> currentBindings : bodySolutions) {
                List<Map<String, Term>> termSolutions = new ArrayList<>();
                CutStatus bodyCutStatus = CutStatus.notOccurred();

                if (solveInternal(bodyTerm, currentBindings, termSolutions, bodyCutStatus)) {
                    nextSolutions.addAll(termSolutions);

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
    // START_CHANGE: ISS-2025-0091 - Use Arrays.asList to avoid ArrayList allocation
    private boolean handleConjunction(Term conjunction, Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions, CutStatus cutStatus) {
        List<Term> goals = java.util.Arrays.asList(
            conjunction.getArguments().get(0),  // A
            conjunction.getArguments().get(1)   // B
        );
        return solveBodyGoals(goals, bindings, solutions, cutStatus);
    }
    // END_CHANGE: ISS-2025-0091

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
