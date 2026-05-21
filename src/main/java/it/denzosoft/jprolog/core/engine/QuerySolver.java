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
    // START_CHANGE: ISS-2025-0167 - Track caller module context for transparent predicates
    private it.denzosoft.jprolog.core.module.Module callerModuleContext = null;
    // END_CHANGE: ISS-2025-0167

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
        Variable.AttributeUnifyHook previousHook = Variable.getAttributeUnifyHook();
        Variable.setAttributeUnifyHook(this::handleAttributeUnification);
        Trail.clear();
        try {
            solve(query, new HashMap<>(), solutions, CutStatus.notOccurred());
            for (Map<String, Term> solution : solutions) {
                deepResolveSolution(solution);
            }
            return solutions;
        } finally {
            Variable.setAttributeUnifyHook(previousHook);
            Trail.clear();
        }
    }

    // START_CHANGE: ISS-2025-0098 - Single-pass deep resolve with path compression
    /**
     * Resolve all variable chains in a solution map in a single pass.
     * Uses path compression: when resolving X→Y→Z→hello, updates X directly to hello,
     * so subsequent lookups of X are O(1). This eliminates the need for a second pass.
     */
    private void deepResolveSolution(Map<String, Term> solution) {
        for (Map.Entry<String, Term> entry : solution.entrySet()) {
            Term value = entry.getValue();
            if (value instanceof Variable) {
                // Follow the chain with path compression
                Term resolved = resolveChainWithCompression(value, solution);
                if (resolved != value) {
                    entry.setValue(resolved);
                }
            } else if (value instanceof CompoundTerm) {
                Term resolved = value.resolveBindings(solution);
                if (resolved != value) {
                    entry.setValue(resolved);
                }
            }
        }
    }

    // START_CHANGE: ISS-2025-0166 - Circular variable binding detection with depth limit
    private static final int MAX_CHAIN_DEPTH = 64;
    // END_CHANGE: ISS-2025-0166

    /**
     * Resolve a variable chain and compress the path:
     * if X→Y→Z→hello, update Y→hello and X→hello so future lookups are O(1).
     */
    private Term resolveChainWithCompression(Term term, Map<String, Term> solution) {
        return resolveChainWithCompression(term, solution, 0);
    }

    private Term resolveChainWithCompression(Term term, Map<String, Term> solution, int depth) {
        if (!(term instanceof Variable)) return term;
        String varName = ((Variable) term).getName();
        Term next = solution.get(varName);
        if (next == null || next == term) return term;

        // START_CHANGE: ISS-2025-0166 - Detect circular variable bindings
        if (depth >= MAX_CHAIN_DEPTH) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.resourceError(
                    "circular_binding", "Circular variable binding detected (depth " + MAX_CHAIN_DEPTH + ") for variable: " + varName));
        }
        // END_CHANGE: ISS-2025-0166

        // Recurse to find the end of the chain
        Term resolved = resolveChainWithCompression(next, solution, depth + 1);

        // Path compression: update this link to point directly to the result
        if (resolved != next) {
            solution.put(varName, resolved);
        }
        return resolved;
    }
    // END_CHANGE: ISS-2025-0098

    // START_CHANGE: ISS-2025-0163 - Lower depth limit to prevent Java StackOverflow
    // Each Prolog recursion level uses ~4-5 Java stack frames, so 2000 Prolog levels
    // is ~8000-10000 Java frames, safely within default JVM stack size.
    private int recursionDepth = 0;
    private static final int MAX_RECURSION_DEPTH = 2000;
    // END_CHANGE: ISS-2025-0163

    // START_CHANGE: ISS-2025-0160 - Last Call Optimization via trampoline
    private static final int LCO_MAX_ITERATIONS = 100000;
    // END_CHANGE: ISS-2025-0160

    // START_CHANGE: ISS-2025-0163 - Expose parent cut status for built-ins like IfThenElse
    private CutStatus currentCutStatus;

    /**
     * Get the current parent cut status. Used by control built-ins (;, ->) to
     * propagate cut from branches back to the enclosing clause.
     */
    public CutStatus getCurrentCutStatus() {
        return currentCutStatus;
    }
    // END_CHANGE: ISS-2025-0163
    
    /**
     * Solve a goal with current bindings.
     * 
     * @param goal The goal to solve
     * @param bindings Current variable bindings
     * @param solutions List to add successful solutions to
     * @param cutStatus Cut control status
     * @return true if any solutions were found
     */
    // START_CHANGE: ISS-2025-0163 - Catch StackOverflowError and convert to ISO resource_error
    public boolean solve(Term goal, Map<String, Term> bindings, List<Map<String, Term>> solutions, CutStatus cutStatus) {
        try {
            return solveInternal(goal, bindings, solutions, cutStatus);
        } catch (StackOverflowError e) {
            recursionDepth = 0; // Reset for recovery
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.resourceError(
                    "stack_overflow", "Java stack overflow during goal: " + goal));
        }
    }
    // END_CHANGE: ISS-2025-0163
    
    /**
     * Internal solve method with recursion protection.
     */
    // START_CHANGE: ISS-2025-0091 - Use instance field instead of ThreadLocal for recursion depth
    // START_CHANGE: ISS-2025-0163 - Throw ISO resource_error instead of silent failure on depth limit
    private boolean solveInternal(Term goal, Map<String, Term> bindings, List<Map<String, Term>> solutions, CutStatus cutStatus) {
        if (recursionDepth > MAX_RECURSION_DEPTH) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.resourceError(
                    "max_recursion_depth", "Maximum recursion depth " + MAX_RECURSION_DEPTH + " exceeded for goal: " + goal));
        }
    // END_CHANGE: ISS-2025-0163

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

        // START_CHANGE: ISS-2025-0097 - Cache goal name and use early dispatch
        // Cache getName() to avoid repeated virtual dispatch (called 8-10 times per goal)
        String goalName = goal.getName();

        // Base case: If the query is true, add the current bindings to solutions
        if ("true".equals(goalName) && (goal.getArguments() == null || goal.getArguments().isEmpty())) {
            solutions.add(new HashMap<>(bindings));
            return true;
        }

        // Fast dispatch for special operators using cached goalName
        if (goalName != null) {
            // Debug CALL port notification (skip conjunction and module operators)
            if (debugController != null && !",".equals(goalName) && !":".equals(goalName)) {
                try {
                    debugController.notifyPort(DebugEvent.Port.CALL, goal, bindings, recursionDepth);
                } catch (DebugController.DebugStopException e) {
                    return false;
                }
            }

            // START_CHANGE: LIM-004 - Module-qualified calls Module:Goal
            if (":".equals(goalName) && goal.getArguments() != null && goal.getArguments().size() == 2) {
                Term moduleTerm = goal.getArguments().get(0).resolveBindings(bindings);
                Term innerGoal = goal.getArguments().get(1).resolveBindings(bindings);
                if (moduleTerm instanceof Atom && prologContext != null) {
                    String moduleName = ((Atom) moduleTerm).getName();
                    it.denzosoft.jprolog.core.module.Module module =
                        prologContext.getModuleManager().getModule(moduleName);
                    if (module != null) {
                        return solveInModuleContext(innerGoal, module, bindings, solutions, cutStatus);
                    }
                    // Module not found - throw existence_error(module, ModuleName)
                    throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                        it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.existenceError(
                            "module", new Atom(moduleName), ":/2",
                            "Module '" + moduleName + "' does not exist"));
                }
                if (moduleTerm instanceof it.denzosoft.jprolog.core.terms.Variable) {
                    throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                        it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError(
                            ":/2 - module argument must be instantiated"));
                }
                // Non-atom, non-variable module term - try to solve inner goal directly
                return solveInternal(innerGoal, bindings, solutions, cutStatus);
            }
            // END_CHANGE: LIM-004

            // Handle conjunction ,(A,B)
            if (",".equals(goalName) && goal.getArguments() != null && goal.getArguments().size() == 2) {
                return handleConjunction(goal, bindings, solutions, cutStatus);
            }

            // Built-in predicate handling
            int arity = it.denzosoft.jprolog.util.TermUtils.getArity(goal);
            if (builtInRegistry.isBuiltIn(goalName, arity)) {
                return handleBuiltIn(goal, bindings, solutions, cutStatus);
            }

            // Tabling interception
            if (prologContext != null) {
                TableStore tableStore = prologContext.getTableStore();
                if (tableStore.isTabled(goalName, arity)) {
                    return solveWithTabling(goal, bindings, solutions, cutStatus, tableStore);
                }
            }
        }
        // END_CHANGE: ISS-2025-0097

        // Attempt to match the query against the knowledge base
        return solveAgainstKnowledgeBase(goal, bindings, solutions, cutStatus);
    }

    // START_CHANGE: ISS-2025-0092 - Tabling (memoization) for recursive predicates
    /**
     * Solve a tabled goal: check cache first, detect loops, compute and cache if needed.
     * Uses variant tabling with canonical variable normalization.
     */
    private boolean solveWithTabling(Term goal, Map<String, Term> bindings,
            List<Map<String, Term>> solutions, CutStatus cutStatus, TableStore tableStore) {
        // Resolve the goal with current bindings
        Term resolvedGoal = goal.resolveBindings(bindings);

        // Normalize: replace unbound variables with canonical _TV0, _TV1, ...
        TableStore.NormalizedGoal normalized = tableStore.normalize(resolvedGoal);
        String cacheKey = normalized.cacheKey;

        // Check cache first
        List<Map<String, Term>> cached = tableStore.getCachedSolutions(cacheKey);
        if (cached != null) {
            // Replay cached solutions, mapping canonical variable names back to original
            for (Map<String, Term> cachedSol : cached) {
                Map<String, Term> replayed = new HashMap<>(bindings);
                for (Map.Entry<String, Term> entry : cachedSol.entrySet()) {
                    String canonicalName = entry.getKey();
                    String origName = normalized.canonicalToOrig.get(canonicalName);
                    if (origName != null) {
                        replayed.put(origName, entry.getValue());
                    }
                }
                solutions.add(replayed);
            }
            return !cached.isEmpty();
        }

        // START_CHANGE: R5 - tabling fixpoint iteration for left-recursion
        // If already in progress, return the partial cache built so far (variant tabling)
        if (tableStore.isInProgress(cacheKey)) {
            List<Map<String, Term>> partial = tableStore.getPartialCache(cacheKey);
            if (partial != null) {
                for (Map<String, Term> cachedSol : partial) {
                    Map<String, Term> replayed = new HashMap<>(bindings);
                    for (Map.Entry<String, Term> entry : cachedSol.entrySet()) {
                        String origName = normalized.canonicalToOrig.get(entry.getKey());
                        if (origName != null) replayed.put(origName, entry.getValue());
                    }
                    solutions.add(replayed);
                }
                return !partial.isEmpty();
            }
            return false;
        }

        tableStore.markInProgress(cacheKey);
        tableStore.setPartialCache(cacheKey, new ArrayList<>());
        try {
            // Fixpoint iteration: keep solving until no new canonical solutions appear
            List<Map<String, Term>> aggregated = new ArrayList<>();
            java.util.Set<String> seenKeys = new java.util.HashSet<>();
            int maxIters = 100;
            for (int iter = 0; iter < maxIters; iter++) {
                List<Map<String, Term>> computed = new ArrayList<>();
                boolean result = solveAgainstKnowledgeBase(goal, bindings, computed, cutStatus);
                boolean changed = false;
                for (Map<String, Term> sol : computed) {
                    Map<String, Term> canonicalSol = new HashMap<>();
                    for (Map.Entry<String, String> mapping : normalized.canonicalToOrig.entrySet()) {
                        String origName = mapping.getValue();
                        Term value = sol.get(origName);
                        if (value != null) canonicalSol.put(mapping.getKey(), value);
                    }
                    String key = canonicalSol.toString();
                    if (seenKeys.add(key)) {
                        aggregated.add(canonicalSol);
                        changed = true;
                    }
                }
                tableStore.setPartialCache(cacheKey, new ArrayList<>(aggregated));
                if (!changed) {
                    // Fixpoint reached
                    tableStore.cacheSolutions(cacheKey, aggregated);
                    solutions.addAll(computed);
                    return result;
                }
            }
            // Hit max iterations: cache what we have
            tableStore.cacheSolutions(cacheKey, aggregated);
            return !aggregated.isEmpty();
        } finally {
            tableStore.clearPartialCache(cacheKey);
            tableStore.unmarkInProgress(cacheKey);
        // END_CHANGE: R5
        }
    }
    // END_CHANGE: ISS-2025-0092

    private boolean isTrueAtom(Term goal) {
        return goal.getName() != null && goal.getName().equals("true") && 
               (goal.getArguments() == null || goal.getArguments().isEmpty());
    }

    private boolean handleBuiltIn(Term goal, Map<String, Term> bindings, List<Map<String, Term>> solutions, CutStatus cutStatus) {
        // START_CHANGE: ISS-2025-0097 - Cache getName() in handleBuiltIn
        String name = goal.getName();
        BuiltIn predicate = builtInRegistry.getBuiltIn(name);

        // START_CHANGE: ISS-2025-0194 - Fix cut propagation in handleBuiltIn
        // Special handling for cut
        if ("cut".equals(name) || "!".equals(name)) {
            solutions.add(new HashMap<>(bindings));
            cutStatus.setCutOccurred();
            return true;
        }
        // END_CHANGE: ISS-2025-0194
        // END_CHANGE: ISS-2025-0097

        boolean result;
        // Handle context-dependent built-ins
        if (predicate instanceof BuiltInWithContext) {
            // START_CHANGE: ISS-2025-0163 - Set current cut status for control built-ins
            CutStatus previousCutStatus = currentCutStatus;
            currentCutStatus = cutStatus;
            try {
                result = ((BuiltInWithContext) predicate).executeWithContext(this, goal, bindings, solutions);
            } finally {
                currentCutStatus = previousCutStatus;
            }
            // END_CHANGE: ISS-2025-0163
        } else if (predicate != null) {
            result = predicate.execute(goal, bindings, solutions);
        } else {
            result = false;
        }

        // START_CHANGE: ISS-2025-0090 - Debug EXIT/FAIL port for built-ins
        if (debugController != null && name != null &&
            !",".equals(name) && !":".equals(name)) {
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

        String functor = goal.getName();
        int arity = it.denzosoft.jprolog.util.TermUtils.getArity(goal);
        // START_CHANGE: CR-2025-0009 - record predicate call for profiler (zero overhead when disabled)
        if (Profiler.isEnabled()) {
            Profiler.recordCall(functor, arity);
        }
        // END_CHANGE: CR-2025-0009
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
            // START_CHANGE: ISS-2025-0093 - Use first-argument indexing for faster clause selection
            Term firstArg = null;
            if (goal instanceof CompoundTerm && goal.getArguments() != null && !goal.getArguments().isEmpty()) {
                firstArg = goal.getArguments().get(0).resolveBindings(bindings);
            }
            candidateRules = knowledgeBase.getRulesWithFirstArgIndex(functor, arity, firstArg);
            // END_CHANGE: ISS-2025-0093
            // START_CHANGE: ISS-2025-0165 - Enforce export visibility when resolving from imported modules
            // Only fall back to module lookup if the predicate doesn't exist at all in the KB
            // (not just empty for a specific first-argument value due to indexing)
            if (candidateRules.isEmpty() && knowledgeBase.getRulesForPredicate(functor, arity).isEmpty()
                && prologContext != null) {
                it.denzosoft.jprolog.core.module.ModuleManager mm = prologContext.getModuleManager();
                it.denzosoft.jprolog.core.module.PredicateSignature sig =
                    new it.denzosoft.jprolog.core.module.PredicateSignature(functor, arity);
                it.denzosoft.jprolog.core.module.Module currentMod = mm.getCurrentModule();
                // Use internal resolution for the current module itself (sees all local predicates),
                // but the resolved module must export the predicate if it's a different module
                it.denzosoft.jprolog.core.module.Module resolved = currentMod.resolvePredicate(sig);
                if (resolved != null && resolved != currentMod) {
                    // Predicate was found in an imported module - check export visibility
                    resolved = resolved.resolvePredicateForExternalAccess(sig);
                }
                if (resolved != null) {
                    candidateRules = resolved.getRulesForPredicate(sig);
                }
            }
            // END_CHANGE: ISS-2025-0165
        } else {
            candidateRules = knowledgeBase.getRules();
        }
        // END_CHANGE: CR-2025-0002

        // START_CHANGE: ISS-2025-0166 - Ensure iteration uses a local snapshot immune to concurrent modification
        // While KB methods already return copies, ensure we have our own mutable snapshot
        // so that assert/retract during backtracking cannot invalidate the iterator.
        List<Rule> iterationRules = new ArrayList<>(candidateRules);
        // END_CHANGE: ISS-2025-0166

        // START_CHANGE: ISS-2025-0090 - Track rule attempts for REDO port
        boolean isFirstAttempt = true;
        // END_CHANGE: ISS-2025-0090

        for (Rule rule : iterationRules) {
            if(traceEnabled) {
                LOGGER.info("Trying rule: " + rule);
            }

            // START_CHANGE: ISS-2025-0090 - Emit REDO port on second+ rule attempts
            if (debugController != null && !isFirstAttempt && functor != null &&
                !",".equals(functor) && !":".equals(functor)) {
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

            // START_CHANGE: ISS-2025-0095 - Use LayeredMap for structural sharing
            // Create a layered working copy instead of full HashMap copy
            LayeredMap attemptBindings = new LayeredMap(bindings);
            // END_CHANGE: ISS-2025-0095

            if (head.unify(goal, attemptBindings)) {
                if(traceEnabled) {
                    LOGGER.info("Head unified. New substitution: " + attemptBindings);
                }

                if (body.isEmpty()) {
                    // Fact case - add solution (materialize to HashMap for storage)
                    solutions.add(attemptBindings.flatten());
                    foundMatch = true;

                    // Check if cut occurred
                    if (cutStatus.isCutOccurred()) {
                        break;
                    }
                } else {
                    // Rule case - solve body goals
                    Map<String, Term> headUnificationBindings = attemptBindings.flatten();

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
        if (debugController != null && functor != null &&
            !",".equals(functor) && !":".equals(functor)) {
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
        // START_CHANGE: ISS-2025-0167 - module_transparent: use caller's module context
        String gName = goal.getName();
        int gArity = it.denzosoft.jprolog.util.TermUtils.getArity(goal);
        // START_CHANGE: Round5 - enforce export list for external module-qualified calls
        if (gName != null) {
            it.denzosoft.jprolog.core.module.PredicateSignature sig =
                new it.denzosoft.jprolog.core.module.PredicateSignature(gName, gArity);
            // If predicate is NOT exported by the module AND caller is in a different module,
            // the call must fail (no visibility). Built-ins always visible.
            if (!builtInRegistry.isBuiltIn(gName, gArity)
                && !",".equals(gName) && !";".equals(gName) && !"->".equals(gName)
                && !module.isExported(sig)
                && (currentModuleContext == null || !module.equals(currentModuleContext))) {
                return false;
            }
        }
        // END_CHANGE: Round5
        if (gName != null) {
            it.denzosoft.jprolog.core.module.PredicateSignature sig =
                new it.denzosoft.jprolog.core.module.PredicateSignature(gName, gArity);
            if (module.isTransparent(sig) && callerModuleContext != null) {
                // Transparent predicate: use caller's module context instead
                module = callerModuleContext;
            }
        }
        // END_CHANGE: ISS-2025-0167

        // Set module context so that body goal resolution uses module rules
        it.denzosoft.jprolog.core.module.Module savedContext = currentModuleContext;
        // START_CHANGE: ISS-2025-0167 - Track caller module for transparent predicates
        it.denzosoft.jprolog.core.module.Module savedCaller = callerModuleContext;
        callerModuleContext = savedContext;
        // END_CHANGE: ISS-2025-0167
        currentModuleContext = module;
        try {
            return solveInternal(goal, bindings, solutions, cutStatus);
        } finally {
            currentModuleContext = savedContext;
            // START_CHANGE: ISS-2025-0167 - Restore caller module context
            callerModuleContext = savedCaller;
            // END_CHANGE: ISS-2025-0167
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
    
    // START_CHANGE: ISS-2025-0099 - Avoid eager HashMap materialization in body solving
    private boolean solveBodyGoals(List<Term> body, Map<String, Term> attemptBindings,
                                   List<Map<String, Term>> solutions, CutStatus cutStatus) {
        boolean bodySucceeded = true;
        List<Map<String, Term>> bodySolutions = new ArrayList<>();
        // For single-goal bodies, pass directly (solveInternal creates LayeredMap per attempt)
        // For multi-goal bodies, must copy to avoid corruption between body goals
        if (body.size() == 1) {
            bodySolutions.add(attemptBindings);
        } else if (attemptBindings instanceof it.denzosoft.jprolog.core.engine.LayeredMap) {
            bodySolutions.add(((it.denzosoft.jprolog.core.engine.LayeredMap) attemptBindings).flatten());
        } else {
            bodySolutions.add(new HashMap<>(attemptBindings));
        }
    // END_CHANGE: ISS-2025-0099

        int bodySize = body.size();
        // For each body term, find all solutions
        for (int i = 0; i < bodySize; i++) {
            Term bodyTerm = body.get(i);
            List<Map<String, Term>> nextSolutions = new ArrayList<>();

            // START_CHANGE: ISS-2025-0054 - Fix cut semantics: continue body, prevent clause backtracking
            String bodyTermName = bodyTerm.getName();
            if (bodyTermName != null &&
                ("!".equals(bodyTermName) || "cut".equals(bodyTermName))) {
                if (!bodySolutions.isEmpty()) {
                    nextSolutions.add(bodySolutions.get(0));
                }
                bodySolutions = nextSolutions;
                cutStatus.setCutOccurred();
                continue;
            }
            // END_CHANGE: ISS-2025-0054

            // START_CHANGE: ISS-2025-0160 - Last Call Optimization with trampoline
            // For the last body goal with a single input solution, use trampoline
            // to avoid stack growth on tail-recursive predicates
            boolean isLastGoal = (i == bodySize - 1);
            if (isLastGoal && bodySolutions.size() == 1) {
                Map<String, Term> currentBindings = bodySolutions.get(0);
                // Trampoline loop: resolve the last goal iteratively when possible
                Term trampolineGoal = bodyTerm;
                Map<String, Term> trampolineBindings = currentBindings;
                int lcoIterations = 0;
                while (lcoIterations < LCO_MAX_ITERATIONS) {
                    lcoIterations++;
                    Term resolvedGoal = trampolineGoal.resolveBindings(trampolineBindings);
                    String gName = resolvedGoal.getName();
                    // Only apply trampoline to simple user-defined predicate calls
                    // (not conjunction, disjunction, built-in, cut, true, etc.)
                    if (gName == null || ",".equals(gName) || ";".equals(gName) ||
                        "->".equals(gName) || "*->".equals(gName) || "!".equals(gName) || "true".equals(gName) ||
                        ":".equals(gName) || "\\+".equals(gName)) {
                        break; // Fall through to normal solve
                    }
                    int gArity = it.denzosoft.jprolog.util.TermUtils.getArity(resolvedGoal);
                    if (builtInRegistry.isBuiltIn(gName, gArity)) {
                        break; // Built-in, fall through to normal solve
                    }
                    if (prologContext != null && prologContext.getTableStore().isTabled(gName, gArity)) {
                        break; // Tabled, fall through to normal solve
                    }
                    // Get candidate rules for this goal
                    Term firstArg = null;
                    if (resolvedGoal instanceof CompoundTerm && resolvedGoal.getArguments() != null
                        && !resolvedGoal.getArguments().isEmpty()) {
                        firstArg = resolvedGoal.getArguments().get(0).resolveBindings(trampolineBindings);
                    }
                    List<Rule> candidates = knowledgeBase.getRulesWithFirstArgIndex(gName, gArity, firstArg);
                    if (candidates.size() != 1) {
                        break; // Non-deterministic or no match, fall through
                    }
                    // Single candidate: try to unify and replace goal with body's last goal
                    Rule singleRule = candidates.get(0);
                    TermCopier.RuleCopy copy;
                    Term rHead;
                    List<Term> rBody;
                    if (singleRule.isGroundFact()) {
                        rHead = singleRule.getHead();
                        rBody = singleRule.getBody();
                    } else {
                        copy = TermCopier.copyRule(singleRule.getHead(), singleRule.getBody());
                        rHead = copy.head;
                        rBody = copy.body;
                    }
                    Map<String, Term> newBindings = new HashMap<>(trampolineBindings);
                    if (!rHead.unify(resolvedGoal, newBindings)) {
                        break; // Unification failed, fall through
                    }
                    if (rBody.isEmpty()) {
                        // Fact matched — success
                        solutions.add(newBindings);
                        return true;
                    }
                    if (rBody.size() == 1) {
                        // Single body goal — trampoline directly
                        trampolineGoal = rBody.get(0);
                        trampolineBindings = newBindings;
                        continue;
                    }
                    // Multiple body goals — solve all but last, then trampoline the last
                    List<Term> prefix = rBody.subList(0, rBody.size() - 1);
                    List<Map<String, Term>> prefixSolutions = new ArrayList<>();
                    CutStatus prefixCut = CutStatus.notOccurred();
                    prefixSolutions.add(newBindings);
                    boolean prefixOk = true;
                    for (Term prefixGoal : prefix) {
                        List<Map<String, Term>> nextPrefix = new ArrayList<>();
                        for (Map<String, Term> pb : prefixSolutions) {
                            List<Map<String, Term>> pSol = new ArrayList<>();
                            CutStatus pCut = CutStatus.notOccurred();
                            String pfName = prefixGoal.getName();
                            if (pfName != null && ("!".equals(pfName) || "cut".equals(pfName))) {
                                if (!prefixSolutions.isEmpty()) {
                                    nextPrefix.add(pb);
                                }
                                cutStatus.setCutOccurred();
                                break;
                            }
                            if (solveInternal(prefixGoal, pb, pSol, pCut)) {
                                nextPrefix.addAll(pSol);
                            }
                            // START_CHANGE: ISS-2025-0194 - Propagate cut from prefix goal to clause level
                            if (pCut.isCutOccurred()) {
                                cutStatus.setCutOccurred();
                                break;
                            }
                            // END_CHANGE: ISS-2025-0194
                        }
                        prefixSolutions = nextPrefix;
                        if (prefixSolutions.isEmpty()) {
                            prefixOk = false;
                            break;
                        }
                    }
                    if (!prefixOk || prefixSolutions.isEmpty()) {
                        return false;
                    }
                    if (prefixSolutions.size() == 1) {
                        // Deterministic prefix — trampoline the last body goal
                        trampolineGoal = rBody.get(rBody.size() - 1);
                        trampolineBindings = prefixSolutions.get(0);
                        continue;
                    }
                    // Non-deterministic prefix — solve last goal normally for each
                    boolean found = false;
                    Term lastBodyGoal = rBody.get(rBody.size() - 1);
                    for (Map<String, Term> ps : prefixSolutions) {
                        CutStatus lCut = CutStatus.notOccurred();
                        if (solveInternal(lastBodyGoal, ps, nextSolutions, lCut)) {
                            found = true;
                        }
                        if (lCut.isCutOccurred()) {
                            cutStatus.setCutOccurred();
                            break;
                        }
                    }
                    if (found) {
                        solutions.addAll(nextSolutions);
                    }
                    return found;
                }
                // START_CHANGE: ISS-2025-0180 - Core engine bug fixes
                if (lcoIterations >= LCO_MAX_ITERATIONS) {
                    LOGGER.warning("LCO trampoline exceeded " + LCO_MAX_ITERATIONS + " iterations for goal: " + trampolineGoal + " — falling back to normal solve");
                }
                // END_CHANGE: ISS-2025-0180
                // Trampoline exhausted or not applicable — fall back to normal solve
                CutStatus bodyCutStatus = CutStatus.notOccurred();
                if (solveInternal(trampolineGoal, trampolineBindings, nextSolutions, bodyCutStatus)) {
                    solutions.addAll(nextSolutions);
                    if (bodyCutStatus.isCutOccurred()) {
                        cutStatus.setCutOccurred();
                    }
                    return true;
                }
                return false;
            }
            // END_CHANGE: ISS-2025-0160

            for (Map<String, Term> currentBindings : bodySolutions) {
                List<Map<String, Term>> termSolutions = new ArrayList<>();
                CutStatus bodyCutStatus = CutStatus.notOccurred();

                // START_CHANGE: ISS-2025-0194 - Propagate cut from body goal to clause level
                if (solveInternal(bodyTerm, currentBindings, termSolutions, bodyCutStatus)) {
                    nextSolutions.addAll(termSolutions);

                    if (bodyCutStatus.isCutOccurred()) {
                        cutStatus.setCutOccurred();
                        break;
                    }
                }
                // END_CHANGE: ISS-2025-0194
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

    // START_CHANGE: LIM-002 - Attribute unification hook dispatcher
    /**
     * Handle attribute unification events. Called by Variable.unify() when an
     * attributed variable is bound to a non-variable term.
     * Dispatches to the appropriate module-specific handler for each attribute.
     *
     * @param variable the attributed variable being bound
     * @param value the non-variable term it was bound to
     * @param substitution the current substitution map
     * @return true if all hooks succeed, false if any hook fails (which fails unification)
     */
    private boolean handleAttributeUnification(Variable variable, Term value,
                                                java.util.Map<String, Term> substitution) {
        // Copy the attribute map since handlers may modify it
        java.util.Map<String, Term> attrs = new HashMap<>(variable.getAttributes());

        for (java.util.Map.Entry<String, Term> entry : attrs.entrySet()) {
            String module = entry.getKey();
            Term attrValue = entry.getValue();

            switch (module) {
                case it.denzosoft.jprolog.builtin.control.Freeze.FREEZE_MODULE:
                    // Execute the frozen goal
                    if (!it.denzosoft.jprolog.builtin.control.Freeze.executeFrozenGoal(
                            this, attrValue, substitution)) {
                        return false;
                    }
                    break;

                case it.denzosoft.jprolog.builtin.control.When.WHEN_MODULE:
                    // Re-check the when condition and potentially execute the goal
                    if (!it.denzosoft.jprolog.builtin.control.When.executeWhenGoal(
                            this, attrValue, substitution)) {
                        return false;
                    }
                    break;

                case it.denzosoft.jprolog.builtin.control.Dif.DIF_MODULE:
                    // Re-check the dif constraint
                    if (!it.denzosoft.jprolog.builtin.control.Dif.checkDifConstraint(
                            this, attrValue, substitution)) {
                        return false;
                    }
                    break;

                default:
                    // Unknown module — try to find attr_unify_hook/2 in knowledge base
                    // For now, just ignore unknown modules
                    break;
            }
        }
        return true;
    }
    // END_CHANGE: LIM-002
}
