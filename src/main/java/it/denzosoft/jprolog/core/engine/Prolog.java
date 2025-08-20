package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.exceptions.PrologParserException;
import it.denzosoft.jprolog.core.parser.Parser;
import it.denzosoft.jprolog.core.parser.TermParser;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.module.ModuleManager;
import it.denzosoft.jprolog.core.dcg.DCGTransformer;
import it.denzosoft.jprolog.util.TermUtils;
import java.util.*;
import java.util.logging.Logger;
import java.util.logging.Level;


public class Prolog {
    private static final Logger LOGGER = Logger.getLogger(Prolog.class.getName());
    
    private final KnowledgeBase knowledgeBase;
    private final BuiltInRegistry builtInRegistry;
    private final QuerySolver querySolver;
    private final Parser parser;
    private final ModuleManager moduleManager;
    private final DCGTransformer dcgTransformer;
    private boolean traceEnabled = false;

    /**
     * Create a new Prolog engine with default components.
     */
    public Prolog() {
        this.knowledgeBase = new KnowledgeBase();
        this.builtInRegistry = new BuiltInRegistry();
        this.moduleManager = new ModuleManager();
        this.dcgTransformer = new DCGTransformer();
        this.querySolver = new QuerySolver(knowledgeBase, builtInRegistry);
        this.querySolver.setPrologContext(this);
        this.parser = new Parser();
        registerBuiltInPredicates();
    }

    private void registerBuiltInPredicates() {
        try {
            // Register all standard built-ins using the factory
            BuiltInFactory.getFactoryMap().forEach((name, factory) -> {
                try {
                    BuiltIn builtIn = factory.get();
                    if (builtIn instanceof BuiltInWithContext && 
                        (name.equals("findall") || name.equals("bagof") || name.equals("setof") ||
                         name.equals("catch") || name.equals("call") || name.equals("once") || 
                         name.equals("ignore") || name.equals("forall") ||
                         name.equals("asserta") || name.equals("assertz") || name.equals("retract") ||
                         name.equals("retractall") || name.equals("abolish") || name.equals("current_predicate") ||
                         name.equals("clause") || name.equals("listing") || name.equals("\\+") || name.equals("phrase"))) {
                        // Special handling for context-dependent predicates
                        builtInRegistry.registerBuiltIn(name, new CollectionBuiltInAdapter((BuiltInWithContext) builtIn, querySolver));
                    } else if (name.equals("listing")) {
                        // Handle overloaded listing predicate
                        if (builtIn instanceof it.denzosoft.jprolog.builtin.database.Listing0) {
                            builtInRegistry.registerBuiltIn("listing/0", builtIn);
                        } else {
                            builtInRegistry.registerBuiltIn("listing/1", builtIn);
                        }
                    } else {
                        builtInRegistry.registerBuiltIn(name, builtIn);
                    }
                } catch (Exception e) {
                    LOGGER.log(Level.WARNING, "Failed to register built-in predicate: " + name, e);
                }
            });
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Error registering built-in predicates", e);
            throw new PrologException("Error registering built-in predicates: " + e.getMessage(), e);
        }
    }
    
    /**
     * Consults a Prolog program from a string.
     *
     * @param program The Prolog program as a string.
     */
    public void consult(String program) {
        try {
            List<Rule> rules = parser.parse(program);
            for (Rule rule : rules) {
                // Check if this is a directive (starts with :-)
                if (isDirective(rule)) {
                    processDirective(rule);
                } else if (isDCGRule(rule)) {
                    // Transform DCG rule to standard Prolog rule
                    Rule transformedRule = transformDCGRule(rule);
                    
                    // Check if the transformed rule would conflict with a built-in
                    checkBuiltInConflict(transformedRule);
                    
                    moduleManager.addRule(transformedRule);
                    knowledgeBase.addRule(transformedRule);
                    LOGGER.log(Level.INFO, "DCG rule transformed: " + rule.getHead() + " --> " + transformedRule.getHead());
                } else {
                    // Check if this rule would conflict with a built-in
                    checkBuiltInConflict(rule);
                    
                    // Add regular rule to current module
                    moduleManager.addRule(rule);
                    knowledgeBase.addRule(rule);
                }
            }
        } catch (PrologParserException e) {
            throw new PrologException("Error parsing program: " + e.getMessage(), e);
        }
    }
    
    /**
     * Check if a rule conflicts with a built-in predicate.
     * Throws an exception if there's a conflict.
     */
    private void checkBuiltInConflict(Rule rule) {
        Term head = rule.getHead();
        String functor = TermUtils.getFunctorName(head);
        int arity = TermUtils.getArity(head);
        
        // Check if this functor/arity combination is a built-in
        if (builtInRegistry.isBuiltIn(functor, arity)) {
            throw new PrologException(
                String.format("Cannot redefine built-in predicate %s/%d", functor, arity)
            );
        }
    }
    
    /**
     * Check if a rule is a directive (starts with :-).
     */
    private boolean isDirective(Rule rule) {
        Term head = rule.getHead();
        return head instanceof CompoundTerm && ":-".equals(TermUtils.getFunctorName(head)) && TermUtils.getArity(head) == 1;
    }
    
    /**
     * Process a directive (e.g., :- module(...), :- use_module(...)).
     */
    private void processDirective(Rule rule) {
        Term head = rule.getHead();
        if (head instanceof CompoundTerm && ":-".equals(TermUtils.getFunctorName(head))) {
            Term directive = TermUtils.getArgument((CompoundTerm) head, 0);
            
            if (directive instanceof CompoundTerm) {
                String functor = TermUtils.getFunctorName(directive);
                
                switch (functor) {
                    case "module":
                        if (moduleManager.parseModuleDirective(directive)) {
                            LOGGER.log(Level.INFO, "Module directive processed: " + directive);
                        } else {
                            LOGGER.log(Level.WARNING, "Failed to process module directive: " + directive);
                        }
                        break;
                    case "use_module":
                        processUseModuleDirective(directive);
                        break;
                    default:
                        LOGGER.log(Level.INFO, "Unknown directive ignored: " + directive);
                }
            }
        }
    }
    
    /**
     * Process a use_module directive.
     */
    private void processUseModuleDirective(Term directive) {
        if (directive instanceof CompoundTerm && TermUtils.getArity(directive) >= 1) {
            Term moduleTerm = TermUtils.getArgument((CompoundTerm) directive, 0);
            if (moduleTerm instanceof Atom) {
                String moduleName = ((Atom) moduleTerm).getName();
                try {
                    moduleManager.importModule(moduleName);
                    LOGGER.log(Level.INFO, "Module imported: " + moduleName);
                } catch (IllegalArgumentException e) {
                    LOGGER.log(Level.WARNING, "Failed to import module: " + moduleName + " - " + e.getMessage());
                }
            }
        }
    }
    
    /**
     * Check if a rule is a DCG rule (uses --> operator).
     */
    private boolean isDCGRule(Rule rule) {
        Term head = rule.getHead();
        return DCGTransformer.isDCGRule(head);
    }
    
    /**
     * Transform a DCG rule to a standard Prolog rule.
     */
    private Rule transformDCGRule(Rule rule) {
        Term head = rule.getHead();
        if (head instanceof CompoundTerm) {
            return dcgTransformer.transformDCGRule((CompoundTerm) head);
        }
        throw new IllegalArgumentException("Invalid DCG rule: " + rule);
    }
    
    /**
     * Assert a fact or rule at the beginning of the knowledge base.
     * 
     * @param clauseString The clause as a string
     */
    public void asserta(String clauseString) {
        try {
            List<Rule> rules = parser.parse(clauseString);
            for (Rule rule : rules) {
                knowledgeBase.asserta(rule);
            }
        } catch (PrologParserException e) {
            throw new PrologException("Error parsing clause: " + e.getMessage(), e);
        }
    }
    
    /**
     * Retract a fact or rule from the knowledge base.
     * 
     * @param clauseString The clause as a string
     */
    public void retract(String clauseString) {
        try {
            List<Rule> rules = parser.parse(clauseString);
            for (Rule rule : rules) {
                knowledgeBase.retract(rule);
            }
        } catch (PrologParserException e) {
            throw new PrologException("Error parsing clause: " + e.getMessage(), e);
        }
    }
    
    /**
     * Solve a query and return all solutions.
     * 
     * @param queryString The query as a string
     * @return List of all solutions
     */
    public List<Map<String, Term>> solve(String queryString) {
        try {
            // Remove trailing period if present
            if (queryString.endsWith(".")) {
                queryString = queryString.substring(0, queryString.length() - 1);
            }
            
            Term query = parser.parseTerm(queryString);
            List<Map<String, Term>> solutions = querySolver.solve(query);
            
            // START_CHANGE: ISS-2025-0010 - Fix variable name mapping
            // Post-process solutions to map internal variable names back to query variables
            return mapInternalVariablesToQueryVariables(query, solutions);
            // END_CHANGE: ISS-2025-0010
        } catch (PrologParserException e) {
            throw new PrologException("Error parsing query: " + e.getMessage(), e);
        }
    }
    
    /**
     * Map internal variable names (created by TermCopier) back to original query variable names.
     * This fixes the issue where predicates return solutions with internal names like "_R123456_N"
     * instead of the original query variable names like "N".
     */
    private List<Map<String, Term>> mapInternalVariablesToQueryVariables(Term query, List<Map<String, Term>> solutions) {
        // Extract variables from the query
        Map<String, Variable> queryVars = extractQueryVariables(query);
        
        List<Map<String, Term>> mappedSolutions = new ArrayList<>();
        
        for (Map<String, Term> solution : solutions) {
            Map<String, Term> mappedSolution = new HashMap<>();
            
            // For each query variable, try to find its value in the solution
            for (String queryVarName : queryVars.keySet()) {
                // First, check if the variable is directly in the solution
                if (solution.containsKey(queryVarName)) {
                    mappedSolution.put(queryVarName, solution.get(queryVarName));
                } else {
                    // Look for internal variable names that might correspond to this query variable
                    for (Map.Entry<String, Term> entry : solution.entrySet()) {
                        String solutionKey = entry.getKey();
                        // Check if this is an internal variable name for our query variable
                        if (solutionKey.contains("_" + queryVarName) && solutionKey.startsWith("_R")) {
                            mappedSolution.put(queryVarName, entry.getValue());
                            break;
                        }
                    }
                }
            }
            
            // If we didn't map any query variables, use the original solution
            if (mappedSolution.isEmpty() && !solution.isEmpty()) {
                mappedSolutions.add(solution);
            } else {
                mappedSolutions.add(mappedSolution);
            }
        }
        
        return mappedSolutions;
    }
    
    /**
     * Extract all variables from a query term.
     */
    private Map<String, Variable> extractQueryVariables(Term term) {
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
     * Solve a query term and return all solutions.
     * 
     * @param query The query term
     * @return List of all solutions
     */
    public List<Map<String, Term>> solve(Term query) {
        return querySolver.solve(query);
    }
    
    /**
     * Register a built-in predicate.
     * 
     * @param name The predicate name
     * @param builtIn The built-in implementation
     */
    public void registerBuiltInPredicate(String name, BuiltIn builtIn) {
        builtInRegistry.registerBuiltIn(name, builtIn);
    }
    
    /**
     * Enable or disable tracing.
     * 
     * @param traceEnabled true to enable tracing
     */
    public void setTraceEnabled(boolean traceEnabled) {
        this.traceEnabled = traceEnabled;
        querySolver.setTraceEnabled(traceEnabled);
    }
    
    /**
     * Get all rules in the knowledge base.
     * 
     * @return List of rules
     */
    public List<Rule> getRules() {
        return knowledgeBase.getRules();
    }
    
    /**
     * Get the term parser.
     * 
     * @return The term parser
     */
    public TermParser getTermParser() {
        return parser.getTermParser();
    }
    
    /**
     * Get the parser.
     * 
     * @return The parser
     */
    public Parser getParser() {
        return parser;
    }
    
    /**
     * List all predicates in the knowledge base.
     */
    public void listing() {
        String listing = getListingOutput();
        System.out.println(listing);
        // Also store for retrieval by IDE
        lastListingOutput = listing;
    }
    
    /**
     * List a specific predicate in the knowledge base.
     * 
     * @param predicateIndicator The predicate indicator (e.g., "parent/2")
     */
    public void listing(String predicateIndicator) {
        String listing = getListingOutput(predicateIndicator);
        System.out.println(listing);
        // Also store for retrieval by IDE
        lastListingOutput = listing;
    }
    
    /**
     * Get listing output as string without printing.
     */
    public String getListingOutput() {
        StringBuilder sb = new StringBuilder();
        List<Rule> rules = knowledgeBase.getRules();
        
        if (rules.isEmpty()) {
            sb.append("% Knowledge base is empty\n");
        } else {
            sb.append("% Knowledge base contains " + rules.size() + " clauses:\n\n");
            for (Rule rule : rules) {
                sb.append(rule.toString()).append(".\n");
            }
        }
        
        return sb.toString();
    }
    
    /**
     * Get listing output for specific predicate as string without printing.
     */
    public String getListingOutput(String predicateIndicator) {
        StringBuilder sb = new StringBuilder();
        List<Rule> rules = knowledgeBase.getRules();
        
        // Parse predicate indicator (e.g. "parent/2")
        String[] parts = predicateIndicator.split("/");
        if (parts.length != 2) {
            sb.append("% Invalid predicate indicator: " + predicateIndicator + "\n");
            return sb.toString();
        }
        
        String functor = parts[0];
        int arity;
        try {
            arity = Integer.parseInt(parts[1]);
        } catch (NumberFormatException e) {
            sb.append("% Invalid arity in predicate indicator: " + predicateIndicator + "\n");
            return sb.toString();
        }
        
        sb.append("% Listing for " + predicateIndicator + ":\n\n");
        
        boolean found = false;
        for (Rule rule : rules) {
            Term head = rule.getHead();
            if (matchesPredicate(head, functor, arity)) {
                sb.append(rule.toString()).append(".\n");
                found = true;
            }
        }
        
        if (!found) {
            sb.append("% No clauses found for " + predicateIndicator + "\n");
        }
        
        return sb.toString();
    }
    
    private boolean matchesPredicate(Term term, String functor, int arity) {
        if (term instanceof it.denzosoft.jprolog.core.terms.Atom) {
            return ((it.denzosoft.jprolog.core.terms.Atom) term).getName().equals(functor) && arity == 0;
        } else if (term instanceof it.denzosoft.jprolog.core.terms.CompoundTerm) {
            it.denzosoft.jprolog.core.terms.CompoundTerm compound = (it.denzosoft.jprolog.core.terms.CompoundTerm) term;
            return compound.getFunctor().getName().equals(functor) && 
                   compound.getArguments().size() == arity;
        }
        return false;
    }
    
    // Storage for last listing output for IDE retrieval
    private String lastListingOutput = "";
    
    /**
     * Get the last listing output (for IDE integration).
     */
    public String getLastListingOutput() {
        return lastListingOutput;
    }
    
    // Type checking methods that seem to be referenced in tests
    public boolean isAtom(Term term) {
        return term instanceof it.denzosoft.jprolog.core.terms.Atom;
    }
    
    public boolean isInteger(Term term) {
        if (term instanceof it.denzosoft.jprolog.core.terms.Number) {
            double value = ((it.denzosoft.jprolog.core.terms.Number) term).getValue();
            return value == Math.floor(value) && !Double.isInfinite(value);
        }
        return false;
    }
    
    public boolean isFloat(Term term) {
        if (term instanceof it.denzosoft.jprolog.core.terms.Number) {
            double value = ((it.denzosoft.jprolog.core.terms.Number) term).getValue();
            return !isInteger(term) && !Double.isInfinite(value);
        }
        return false;
    }
    
    /**
     * Add a clause to the beginning of the database (asserta).
     * 
     * @param clause The clause to add
     */
    public void addClauseFirst(Clause clause) {
        knowledgeBase.addClauseFirst(clause);
    }
    
    /**
     * Add a clause to the end of the database (assertz).
     * 
     * @param clause The clause to add
     */
    public void addClauseLast(Clause clause) {
        knowledgeBase.addClauseLast(clause);
    }
    
    /**
     * Remove clauses that match the given term.
     * 
     * @param term The term to match for retraction
     * @return true if any clauses were removed
     */
    public boolean retractClauses(Term term) {
        return knowledgeBase.retractClauses(term);
    }
    
    /**
     * Remove all clauses that match the given term.
     * 
     * @param term The term to match for retraction
     * @return Number of clauses removed
     */
    public int retractAllClauses(Term term) {
        return knowledgeBase.retractAllClauses(term);
    }
    
    /**
     * Remove all clauses for the given predicate.
     * 
     * @param functor The predicate functor
     * @param arity The predicate arity
     * @return Number of clauses removed
     */
    public int abolishPredicate(String functor, int arity) {
        return knowledgeBase.abolishPredicate(functor, arity);
    }
    
    /**
     * Get all predicate indicators in the knowledge base.
     * 
     * @return Set of predicate indicators (functor/arity)
     */
    public Set<String> getCurrentPredicates() {
        return knowledgeBase.getCurrentPredicates();
    }
    
    /**
     * Get the module manager.
     * 
     * @return The module manager
     */
    public ModuleManager getModuleManager() {
        return moduleManager;
    }
}
