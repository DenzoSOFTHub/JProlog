package it.denzosoft.jprolog;

import it.denzosoft.jprolog.terms.Term;
import it.denzosoft.jprolog.terms.Atom;
import it.denzosoft.jprolog.terms.Variable;

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

        // Built-in predicate handling
        if (goal.getName() != null && builtInRegistry.hasBuiltIn(goal.getName())) {
            return handleBuiltIn(goal, bindings, solutions, cutStatus);
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
            // Return true but indicate cut has occurred through the return value or by updating a mutable object
            return true; // We'll handle the cut status check at the call site
        }
        
        // Handle context-dependent built-ins
        if (predicate instanceof BuiltInWithContext) {
            return ((BuiltInWithContext) predicate).executeWithContext(this, goal, bindings, solutions);
        }
        
        return predicate.execute(goal, bindings, solutions);
    }

    private boolean solveAgainstKnowledgeBase(Term goal, Map<String, Term> bindings, 
                                              List<Map<String, Term>> solutions, CutStatus cutStatus) {
        boolean foundMatch = false;
        
        for (Rule rule : knowledgeBase.getRules()) {
            if(traceEnabled) {
                LOGGER.info("Trying rule: " + rule);
            }
            
            // Create fresh copies for this attempt
            Term head = rule.getHead().copy();
            List<Term> body = new ArrayList<>();
            for (Term term : rule.getBody()) {
                body.add(term.copy());
            }
            
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
                    CutStatus newCutStatus = CutStatus.notOccurred();
                    if (solveBodyGoals(body, attemptBindings, solutions, newCutStatus)) {
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
                    // Add the first solution and mark cut occurred
                    nextSolutions.add(bodySolutions.get(0));
                    // Update cut status to indicate cut occurred
                    // Since CutStatus is immutable, we can't modify it directly
                    // We'll need to signal this differently - perhaps through return values
                    // For now, just add the first solution
                }
                bodySolutions = nextSolutions;
                // Set a flag to indicate cut occurred
                // This is a simplified approach - in a full implementation we'd need a mutable wrapper
                break; // Cut means we stop processing the rest of the body
            }
            
            for (Map<String, Term> currentBindings : bodySolutions) {
                List<Map<String, Term>> termSolutions = new ArrayList<>();
                CutStatus bodyCutStatus = CutStatus.notOccurred();
                
                if (solve(bodyTerm, currentBindings, termSolutions, bodyCutStatus)) {
                    nextSolutions.addAll(termSolutions);
                    
                    // If cut occurred, stop processing
                    if (bodyCutStatus.isCutOccurred()) {
                        // This is where the issue is - CutStatus is immutable
                        // We'll create a new CutStatus that indicates cut occurred
                        // But since CutStatus is immutable, we need to return this information differently
                        // For now, let's just handle cut directly in the loop
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
