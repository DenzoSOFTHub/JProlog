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
import it.denzosoft.jprolog.core.operator.OperatorTable;
import it.denzosoft.jprolog.core.dcg.DCGTransformer;
import it.denzosoft.jprolog.builtin.system.OperatorDefinition;
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
    // START_CHANGE: ISS-2025-0085 - Shared operator table for parser/predicate integration
    private final OperatorTable operatorTable;
    // END_CHANGE: ISS-2025-0085
    private boolean traceEnabled = false;

    /**
     * Create a new Prolog engine with default components.
     */
    public Prolog() {
        this.knowledgeBase = new KnowledgeBase();
        this.builtInRegistry = new BuiltInRegistry();
        this.moduleManager = new ModuleManager();
        this.dcgTransformer = new DCGTransformer();
        // START_CHANGE: ISS-2025-0085 - Create shared OperatorTable
        this.operatorTable = new OperatorTable();
        this.parser = new Parser(operatorTable);
        OperatorDefinition.setSharedOperatorTable(operatorTable);
        // END_CHANGE: ISS-2025-0085
        this.querySolver = new QuerySolver(knowledgeBase, builtInRegistry);
        this.querySolver.setPrologContext(this);
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
                         name.equals("clause") || name.equals("listing") || name.equals("\\+") || name.equals("phrase") ||
                         name.equals("maplist") || name.equals("include") || name.equals("exclude") ||
                         name.equals("foldl") || name.equals("with_output_to"))) {
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
            // START_CHANGE: ISS-2025-0085 - Parse clauses incrementally so op directives
            // take effect before subsequent clauses are parsed
            List<java.lang.String> clauses = parser.extractClauses(program);
            for (java.lang.String clause : clauses) {
                java.lang.String trimmed = clause.trim();
                if (trimmed.isEmpty()) continue;

                Rule rule;
                try {
                    rule = parser.parseRule(trimmed);
                } catch (PrologParserException e) {
                    throw new PrologParserException("Error parsing clause: " + e.getMessage(), e);
                }

                if (isDirective(rule)) {
                    processDirective(rule);
                } else if (isDCGRule(rule)) {
                    Rule transformedRule = transformDCGRule(rule);
                    checkBuiltInConflict(transformedRule);
                    moduleManager.addRule(transformedRule);
                    // START_CHANGE: CR-2025-0002 - Module-isolated rule storage
                    if ("user".equals(moduleManager.getCurrentModule().getName())) {
                        knowledgeBase.addRule(transformedRule);
                    }
                    // END_CHANGE: CR-2025-0002
                } else {
                    checkBuiltInConflict(rule);
                    moduleManager.addRule(rule);
                    // START_CHANGE: CR-2025-0002 - Module-isolated rule storage
                    if ("user".equals(moduleManager.getCurrentModule().getName())) {
                        knowledgeBase.addRule(rule);
                    }
                    // END_CHANGE: CR-2025-0002
                }
            }
            // END_CHANGE: ISS-2025-0085
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
                    // START_CHANGE: ISS-2025-0085 - Handle op/3 directives during consult
                    case "op":
                        processOpDirective(directive);
                        break;
                    // END_CHANGE: ISS-2025-0085
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
    
    // START_CHANGE: ISS-2025-0085 - Process op/3 directives to update shared OperatorTable
    /**
     * Process an op/3 directive during consult.
     * This immediately updates the shared OperatorTable so subsequent
     * parsing can use the new operator.
     */
    private void processOpDirective(Term directive) {
        if (directive instanceof CompoundTerm && TermUtils.getArity(directive) == 3) {
            CompoundTerm ct = (CompoundTerm) directive;
            Term precTerm = ct.getArguments().get(0);
            Term typeTerm = ct.getArguments().get(1);
            Term nameTerm = ct.getArguments().get(2);

            if (precTerm instanceof it.denzosoft.jprolog.core.terms.Number &&
                typeTerm instanceof Atom && nameTerm instanceof Atom) {
                int precedence = (int) Math.round(((it.denzosoft.jprolog.core.terms.Number) precTerm).getValue());
                java.lang.String type = ((Atom) typeTerm).getName();
                java.lang.String name = ((Atom) nameTerm).getName();

                try {
                    if (precedence == 0) {
                        // Remove operator — remove all definitions with this name
                        for (it.denzosoft.jprolog.core.operator.Operator op : operatorTable.getOperators(name)) {
                            operatorTable.removeOperator(op.getPrecedence(), op.getType(), name);
                        }
                    } else {
                        it.denzosoft.jprolog.core.operator.Operator.Type opType =
                            it.denzosoft.jprolog.core.operator.Operator.parseType(type);
                        operatorTable.defineOperator(precedence, opType, name);
                    }
                    LOGGER.log(Level.INFO, "Operator directive processed: op(" + precedence + ", " + type + ", " + name + ")");
                } catch (Exception e) {
                    LOGGER.log(Level.WARNING, "Failed to process op directive: " + e.getMessage());
                }
            }
        }
    }
    // END_CHANGE: ISS-2025-0085

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
    // START_CHANGE: ISS-2025-0085 - Process directives and DCG rules in asserta
    public void asserta(String clauseString) {
        try {
            List<java.lang.String> clauses = parser.extractClauses(clauseString);
            for (java.lang.String clause : clauses) {
                java.lang.String trimmed = clause.trim();
                if (trimmed.isEmpty()) continue;

                Rule rule = parser.parseRule(trimmed);

                if (isDirective(rule)) {
                    processDirective(rule);
                } else if (isDCGRule(rule)) {
                    Rule transformedRule = transformDCGRule(rule);
                    checkBuiltInConflict(transformedRule);
                    moduleManager.addRule(transformedRule);
                    // START_CHANGE: CR-2025-0002 - Module-isolated rule storage
                    // Only add to global KB if in user module (default)
                    if ("user".equals(moduleManager.getCurrentModule().getName())) {
                        knowledgeBase.asserta(transformedRule);
                    }
                    // END_CHANGE: CR-2025-0002
                } else {
                    // START_CHANGE: CR-2025-0002 - Module-isolated rule storage
                    if ("user".equals(moduleManager.getCurrentModule().getName())) {
                        knowledgeBase.asserta(rule);
                    } else {
                        moduleManager.addRule(rule);
                    }
                    // END_CHANGE: CR-2025-0002
                }
            }
        } catch (PrologParserException e) {
            throw new PrologException("Error parsing clause: " + e.getMessage(), e);
        }
    }
    // END_CHANGE: ISS-2025-0085
    
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
        } catch (DebugController.DebugStopException e) {
            // Re-throw debug stop so DebugPanel can catch it
            throw e;
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
     * Get the query solver (for debug controller integration).
     *
     * @return The query solver
     */
    public QuerySolver getQuerySolver() {
        return querySolver;
    }

    /**
     * Get the module manager.
     *
     * @return The module manager
     */
    public ModuleManager getModuleManager() {
        return moduleManager;
    }

    // START_CHANGE: ISS-2025-0085 - Expose shared OperatorTable
    /**
     * Get the shared operator table.
     */
    public OperatorTable getOperatorTable() {
        return operatorTable;
    }
    // END_CHANGE: ISS-2025-0085

    // START_CHANGE: ISS-2025-0090 - Compilation with diagnostics for IDE error reporting
    /**
     * Result of compiling a Prolog source with diagnostic information.
     */
    public static class CompilationResult {
        public final boolean success;
        public final List<CompilationError> errors;
        public final int totalClauses;

        public CompilationResult(boolean success, List<CompilationError> errors, int totalClauses) {
            this.success = success;
            this.errors = errors;
            this.totalClauses = totalClauses;
        }
    }

    /**
     * Represents a compilation error with location information.
     */
    public static class CompilationError {
        public final String file;
        public final int lineNumber;
        public final String message;
        public final String severity; // "error" or "warning"

        public CompilationError(String file, int lineNumber, String message, String severity) {
            this.file = file;
            this.lineNumber = lineNumber;
            this.message = message;
            this.severity = severity;
        }
    }

    /**
     * Consult a Prolog program with error collection instead of throwing.
     * Returns all errors found, allowing the IDE to display them inline.
     *
     * @param program The Prolog source
     * @param filename The source file name (for error reporting)
     * @return CompilationResult with success flag and error list
     */
    public CompilationResult consultWithDiagnostics(String program, String filename) {
        List<CompilationError> errors = new ArrayList<>();
        int clauseCount = 0;

        try {
            List<java.lang.String> clauses = parser.extractClauses(program);
            int lineEstimate = 1;

            for (java.lang.String clause : clauses) {
                java.lang.String trimmed = clause.trim();
                if (trimmed.isEmpty()) {
                    // Count newlines in skipped content for line tracking
                    for (char c : clause.toCharArray()) {
                        if (c == '\n') lineEstimate++;
                    }
                    continue;
                }

                try {
                    Rule rule = parser.parseRule(trimmed);

                    if (isDirective(rule)) {
                        processDirective(rule);
                    } else if (isDCGRule(rule)) {
                        Rule transformedRule = transformDCGRule(rule);
                        checkBuiltInConflict(transformedRule);
                        moduleManager.addRule(transformedRule);
                        if ("user".equals(moduleManager.getCurrentModule().getName())) {
                            knowledgeBase.addRule(transformedRule);
                        }
                    } else {
                        checkBuiltInConflict(rule);
                        moduleManager.addRule(rule);
                        if ("user".equals(moduleManager.getCurrentModule().getName())) {
                            knowledgeBase.addRule(rule);
                        }
                    }
                    clauseCount++;
                } catch (Exception e) {
                    errors.add(new CompilationError(filename, lineEstimate, e.getMessage(), "error"));
                }

                // Estimate line number from clause content
                for (char c : clause.toCharArray()) {
                    if (c == '\n') lineEstimate++;
                }
            }
        } catch (Exception e) {
            errors.add(new CompilationError(filename, 1, e.getMessage(), "error"));
        }

        return new CompilationResult(errors.isEmpty(), errors, clauseCount);
    }
    // END_CHANGE: ISS-2025-0090

    // START_CHANGE: ISS-2025-0085 - Compiled binary format support
    /**
     * Compile a Prolog source string to binary JPC format.
     *
     * @param source the Prolog source text
     * @param out    output stream to write the compiled format
     */
    public void compile(String source, java.io.OutputStream out) throws java.io.IOException {
        try {
            List<Rule> rules = new ArrayList<>();
            List<java.lang.String> clauses = parser.extractClauses(source);
            for (java.lang.String clause : clauses) {
                java.lang.String trimmed = clause.trim();
                if (trimmed.isEmpty()) continue;
                Rule rule = parser.parseRule(trimmed);
                if (isDirective(rule)) {
                    processDirective(rule);
                }
                rules.add(rule);
            }
            long hash = it.denzosoft.jprolog.core.compiled.JpcWriter.computeSourceHash(source);
            new it.denzosoft.jprolog.core.compiled.JpcWriter()
                .write(rules, operatorTable, hash, out);
        } catch (PrologParserException e) {
            throw new java.io.IOException("Parse error during compilation: " + e.getMessage(), e);
        }
    }

    /**
     * Compile a Prolog source file to a .jpc file (same name, different extension).
     *
     * @param sourceFile path to the .pl source file
     * @return path to the generated .jpc file
     */
    public java.lang.String compileFile(java.lang.String sourceFile) throws java.io.IOException {
        java.lang.String source = new java.lang.String(
            java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(sourceFile)),
            java.nio.charset.StandardCharsets.UTF_8);
        java.lang.String jpcFile = sourceFile.replaceAll("\\.[^.]+$", "") +
            it.denzosoft.jprolog.core.compiled.JpcFormat.EXTENSION;
        try (java.io.FileOutputStream fos = new java.io.FileOutputStream(jpcFile)) {
            compile(source, fos);
        }
        return jpcFile;
    }

    /**
     * Load a compiled JPC file into the engine.
     * Falls back to source consult if the JPC is stale or missing.
     *
     * @param jpcFile path to the .jpc file
     */
    public void consultCompiled(java.lang.String jpcFile) throws java.io.IOException {
        try (java.io.FileInputStream fis = new java.io.FileInputStream(jpcFile)) {
            consultCompiled(fis);
        }
    }

    /**
     * Load compiled rules from an input stream.
     *
     * @param in input stream containing JPC data
     */
    public void consultCompiled(java.io.InputStream in) throws java.io.IOException {
        it.denzosoft.jprolog.core.compiled.JpcReader reader = new it.denzosoft.jprolog.core.compiled.JpcReader();
        it.denzosoft.jprolog.core.compiled.JpcReader.CompiledProgram program = reader.read(in);
        // Register operators
        for (it.denzosoft.jprolog.core.operator.Operator op : program.operators) {
            operatorTable.defineOperator(op.getPrecedence(), op.getType(), op.getName());
        }
        // Load rules
        for (Rule rule : program.rules) {
            if (isDirective(rule)) {
                processDirective(rule);
            } else if (isDCGRule(rule)) {
                checkBuiltInConflict(rule);
                moduleManager.addRule(rule);
                knowledgeBase.addRule(rule);
            } else {
                checkBuiltInConflict(rule);
                moduleManager.addRule(rule);
                knowledgeBase.addRule(rule);
            }
        }
    }

    /**
     * Smart consult: uses compiled .jpc if available and up-to-date, otherwise
     * parses from source and optionally compiles for next time.
     *
     * @param sourceFile path to the .pl source file
     */
    public void consultSmart(java.lang.String sourceFile) throws java.io.IOException {
        java.lang.String jpcFile = sourceFile.replaceAll("\\.[^.]+$", "") +
            it.denzosoft.jprolog.core.compiled.JpcFormat.EXTENSION;
        java.io.File jpc = new java.io.File(jpcFile);
        java.io.File src = new java.io.File(sourceFile);

        if (jpc.exists() && jpc.lastModified() >= src.lastModified()) {
            // Try compiled version
            try {
                java.lang.String source = new java.lang.String(
                    java.nio.file.Files.readAllBytes(src.toPath()),
                    java.nio.charset.StandardCharsets.UTF_8);
                long currentHash = it.denzosoft.jprolog.core.compiled.JpcWriter.computeSourceHash(source);

                it.denzosoft.jprolog.core.compiled.JpcReader reader = new it.denzosoft.jprolog.core.compiled.JpcReader();
                it.denzosoft.jprolog.core.compiled.JpcReader.CompiledProgram program;
                try (java.io.FileInputStream fis = new java.io.FileInputStream(jpcFile)) {
                    program = reader.read(fis);
                }
                if (program.sourceHash == currentHash) {
                    // Hash matches — load compiled
                    for (it.denzosoft.jprolog.core.operator.Operator op : program.operators) {
                        operatorTable.defineOperator(op.getPrecedence(), op.getType(), op.getName());
                    }
                    for (Rule rule : program.rules) {
                        if (isDirective(rule)) {
                            processDirective(rule);
                        } else {
                            checkBuiltInConflict(rule);
                            moduleManager.addRule(rule);
                            knowledgeBase.addRule(rule);
                        }
                    }
                    LOGGER.log(Level.INFO, "Loaded compiled: " + jpcFile);
                    return;
                }
            } catch (Exception e) {
                LOGGER.log(Level.WARNING, "Failed to load compiled file, falling back to source: " + e.getMessage());
            }
        }

        // Fall back to source consult + compile for next time
        java.lang.String source = new java.lang.String(
            java.nio.file.Files.readAllBytes(src.toPath()),
            java.nio.charset.StandardCharsets.UTF_8);
        consult(source);
        try {
            compileFile(sourceFile);
            LOGGER.log(Level.INFO, "Compiled: " + jpcFile);
        } catch (Exception e) {
            LOGGER.log(Level.WARNING, "Failed to compile: " + e.getMessage());
        }
    }
    // END_CHANGE: ISS-2025-0085
}
