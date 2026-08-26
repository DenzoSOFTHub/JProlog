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
    private final EngineContext engineContext;   // ISS-2025-0484
    private final Parser parser;
    private final ModuleManager moduleManager;
    private final DCGTransformer dcgTransformer;
    // START_CHANGE: ISS-2025-0085 - Shared operator table for parser/predicate integration
    private final OperatorTable operatorTable;
    // END_CHANGE: ISS-2025-0085
    // START_CHANGE: ISS-2025-0092 - Tabling (memoization) support
    private final TableStore tableStore;
    // END_CHANGE: ISS-2025-0092
    // START_CHANGE: LIM-003 - Global non-backtrackable variables
    private final java.util.concurrent.ConcurrentHashMap<String, it.denzosoft.jprolog.core.terms.Term> globalVariables =
        new java.util.concurrent.ConcurrentHashMap<>();
    // END_CHANGE: LIM-003
    // START_CHANGE: ISS-2025-0279 - initialization/1 goals deferred until after the file is loaded
    private final List<Term> pendingInitializationGoals = new ArrayList<>();
    // END_CHANGE: ISS-2025-0279

    // START_CHANGE: ISS-2025-0293 - make the clean-room v2 parser the default for consult+queries.
    // Toggle off with -Djprolog.parser=legacy (or Prolog.setUseV2Parser(false)) to fall back.
    private static volatile boolean USE_V2_PARSER =
        !"legacy".equalsIgnoreCase(System.getProperty("jprolog.parser", "v2"));
    public static void setUseV2Parser(boolean v2) { USE_V2_PARSER = v2; }
    public static boolean isUsingV2Parser() { return USE_V2_PARSER; }
    // END_CHANGE: ISS-2025-0293

    // START_CHANGE: ISS-2025-0294 - flag to enable the v2 CLP(FD) solver at engine creation.
    // Default on: the v2 solver is sound (interval domains, real #\= propagation, trail-backtracked
    // labeling). Fall back to the legacy store with -Djprolog.clpfd=legacy / Prolog.setUseV2Clpfd(false).
    private static volatile boolean USE_V2_CLPFD =
        !"legacy".equalsIgnoreCase(System.getProperty("jprolog.clpfd", "v2"));
    public static void setUseV2Clpfd(boolean v2) { USE_V2_CLPFD = v2; }
    public static boolean isUsingV2Clpfd() { return USE_V2_CLPFD; }
    // END_CHANGE: ISS-2025-0294

    // START_CHANGE: ISS-2025-0304 - clean-room v2 DCG translator (default; -Djprolog.dcg=legacy to fall back)
    private static volatile boolean USE_V2_DCG =
        !"legacy".equalsIgnoreCase(System.getProperty("jprolog.dcg", "v2"));
    public static void setUseV2Dcg(boolean v2) { USE_V2_DCG = v2; }
    public static boolean isUsingV2Dcg() { return USE_V2_DCG; }
    // END_CHANGE: ISS-2025-0304

    // START_CHANGE: ISS-2025-0491 - 4.1 wave A: there is ONE resolution engine. The v2 machine
    // (default 3.1.0..3.14.0, one-release fallback in 4.0.0) is DELETED, and with it
    // `jprolog.engine=v2`, the four static engine-selection accessors and the `engine-v2` Maven
    // profile. The property is still READ so that a build script that
    // still passes `-Djprolog.engine=v2` gets a loud warning instead of silently running a
    // different engine than it asked for; any value other than `v4` warns once and runs v4.
    static {
        String engineProperty = System.getProperty("jprolog.engine");
        if (engineProperty != null && !"v4".equalsIgnoreCase(engineProperty)) {
            Logger.getLogger(Prolog.class.getName()).warning(
                "-Djprolog.engine=" + engineProperty + " is obsolete: the v2 engine was removed in "
                + "4.1.0 and core.engine.v4 is the only engine. Running v4.");
        }
    }
    // END_CHANGE: ISS-2025-0491

    /** This engine's v4 context (clause store, built-in tables); created on first v4 query. */
    private volatile it.denzosoft.jprolog.core.engine.v4.Engine v4Engine;

    /** The v4 engine context of this {@code Prolog} instance (created on demand). */
    public it.denzosoft.jprolog.core.engine.v4.Engine getV4Engine() {
        it.denzosoft.jprolog.core.engine.v4.Engine e = v4Engine;
        if (e == null) {
            synchronized (this) {
                e = v4Engine;
                if (e == null) {
                    e = new it.denzosoft.jprolog.core.engine.v4.Engine(
                        this, knowledgeBase, builtInRegistry, engineContext, moduleManager, tableStore);
                    v4Engine = e;
                }
            }
        }
        return e;
    }

    /** Run one query on the v4 machine (a fresh machine per query). */
    private List<Map<String, Term>> solveWithV4Engine(Term query) {
        final List<Map<String, Term>> out = new ArrayList<>();
        solveStreamWithV4Engine(query, sol -> { out.add(sol); return true; });
        return out;
    }

    private void solveStreamWithV4Engine(Term query, java.util.function.Predicate<Map<String, Term>> sink) {
        // START_CHANGE: ISS-2025-0491 - 4.1 wave A: the legacy Variable.AttributeUnifyHook was the
        // v2 engine's coroutining entry point and is deleted with it; v4 has its own wake queue
        // (core.engine.v4.Coroutining), so there is nothing to uninstall around a query any more.
        try {
            it.denzosoft.jprolog.core.engine.v4.Machine m =
                new it.denzosoft.jprolog.core.engine.v4.Machine(getV4Engine(), new ResourceGuard(inferenceBudget));
            m.solve(query, sink::test);
        } catch (StackOverflowError e) {
            // The v4 core is iterative; this can only come from a legacy built-in deep in a term.
            throw new PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.resourceError("stack_overflow", "solve"));
        }
        // END_CHANGE: ISS-2025-0491
    }
    // END_CHANGE: ISS-2025-0444
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
        // START_CHANGE: ISS-2025-0474 - wave W7 (design B.12): the operator table is the engine's
        // own store, not a process-global one handed to OperatorDefinition. op/3, current_op/3, the
        // parser, write_term/2,3 and the .jpc writer all read this one object (LIM-034).
        this.operatorTable = engineState.ops().globalTable();
        this.parser = new Parser(operatorTable);
        // END_CHANGE: ISS-2025-0474
        // END_CHANGE: ISS-2025-0085
        // START_CHANGE: ISS-2025-0092 - Initialize table store
        this.tableStore = new TableStore();
        // END_CHANGE: ISS-2025-0092
        this.engineContext = new EngineContext(this, knowledgeBase, builtInRegistry);   // ISS-2025-0484
        registerBuiltInPredicates();
        // START_CHANGE: ISS-2025-0294 - optionally make the clean-room v2 CLP(FD) the default
        // (enable with -Djprolog.clpfd=v2 or Prolog.setUseV2Clpfd(true)).
        if (USE_V2_CLPFD) {
            enableV2Clpfd();
        }
        // END_CHANGE: ISS-2025-0294
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
                         name.equals("foldl") || name.equals("with_output_to") ||
                         name.equals("table") || name.equals("abolish_all_tables") ||
                         name.equals("abolish_table") ||
                         name.equals("aggregate_all") ||
                         // START_CHANGE: LIM-005 - predicate_property/2 context-dependent predicate
                         name.equals("predicate_property") ||
                         // END_CHANGE: LIM-005
                         // START_CHANGE: ISS-2025-0123 - CLP(FD) context-dependent predicates
                         name.equals("in") || name.equals("#=") || name.equals("#\\=") ||
                         name.equals("#<") || name.equals("#>") || name.equals("#=<") || name.equals("#>=") ||
                         name.equals("all_different") || name.equals("label") || name.equals("labeling") ||
                         name.equals("indomain") || name.equals("fd_dom") || name.equals("fd_size") ||
                         // END_CHANGE: ISS-2025-0123
                         // START_CHANGE: ISS-2025-0126 - Persistence context-dependent predicates
                         name.equals("db_save") || name.equals("db_load") || name.equals("db_save_predicate") ||
                         name.equals("persist") || name.equals("unpersist") ||
                         name.equals("db_export_json") || name.equals("db_import_json") ||
                         name.equals("db_snapshot") || name.equals("db_restore") || name.equals("db_clear") ||
                         // END_CHANGE: ISS-2025-0126
                         // START_CHANGE: ISS-2025-0139 - Concurrent execution predicates
                         // START_CHANGE: ISS-2025-0480 - wave W8: the concurrency predicates are
                         // NO LONGER wrapped in a CollectionBuiltInAdapter. The adapter pinned one
                         // solver at registration time, which is exactly the object a worker must
                         // not use: the built-in has to receive the per-query SolverFacade so its
                         // solveInWorker runs the goal on a fresh Machine over the same Engine
                         // (LIM-024). The dispatcher (LegacyBuiltinAdapter) already handles a
                         // BuiltInWithContext directly and passes the right context.
                         // END_CHANGE: ISS-2025-0480
                         // START_CHANGE: LIM-003 - Global variable predicates (context-dependent)
                         name.equals("nb_setval") || name.equals("nb_getval") ||
                         name.equals("nb_current") || name.equals("nb_delete") ||
                         name.equals("b_setval") || name.equals("b_getval")
                         // END_CHANGE: LIM-003
                         // ISS-2025-0491 (4.1 wave A): the attributed-variable (LIM-002) and
                         // coroutining (LIM-001) names left this list with the legacy classes that
                         // implemented them — put_attr/get_attr/del_attr/attvar are v4 natives and
                         // freeze/when/dif are prelude clauses.
                         )) {
                        // START_CHANGE: ISS-2025-0485 - wave W9: CollectionBuiltInAdapter is
                        // DELETED. It wrapped a BuiltInWithContext so it could be called through
                        // the plain BuiltIn interface, pinning ONE solver at registration time —
                        // which is exactly the object a per-query context must not be. The
                        // dispatcher (LegacyBuiltinAdapter) handles a BuiltInWithContext directly
                        // and passes the RIGHT context, so the built-in is registered unwrapped.
                        // Invariant 43 becomes structural.
                        builtInRegistry.registerBuiltIn(name, builtIn);
                        // END_CHANGE: ISS-2025-0485
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
                    it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
                    LOGGER.log(Level.WARNING, "Failed to register built-in predicate: " + name, e);
                }
            });
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            LOGGER.log(Level.SEVERE, "Error registering built-in predicates", e);
            throw new PrologException("Error registering built-in predicates: " + e.getMessage(), e);
        }
    }
    
    /**
     * Consults a Prolog program from a string.
     *
     * @param program The Prolog program as a string.
     */
    // START_CHANGE: ISS-2025-0168 - Multi-error parser recovery: collect all errors instead of stopping at first
    // ISS-2025-0437 - ENG-06
    public void consult(String program) {
        State prev = enterState();
        try { consultGuarded(program); } finally { exitState(prev); }
    }

        private void consultGuarded(String program) {
        // START_CHANGE: ISS-2025-0293 - default to the v2 parser
        if (USE_V2_PARSER) { consultV2(program); return; }
        // END_CHANGE: ISS-2025-0293
        List<java.lang.String> errors = new ArrayList<>();
        try {
            // START_CHANGE: ISS-2025-0085 - Parse clauses incrementally so op directives
            // take effect before subsequent clauses are parsed
            List<java.lang.String> clauses = parser.extractClauses(program);
            for (java.lang.String clause : clauses) {
                java.lang.String trimmed = clause.trim();
                if (trimmed.isEmpty()) continue;

                try {
                    Rule rule;
                    try {
                        rule = parser.parseRule(trimmed);
                    } catch (PrologParserException e) {
                        errors.add("Error parsing clause: '" + trimmed + "' - " + e.getMessage());
                        continue; // Skip this clause and continue with the next
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
                // START_CHANGE: ISS-2025-0346 - a halt raised by a directive aborts the load
                } catch (PrologException pe) {
                    if (pe.isHalt()) {
                        throw pe;
                    }
                    errors.add("Error processing clause: '" + trimmed + "' - " + pe.getMessage());
                // END_CHANGE: ISS-2025-0346
                } catch (Exception e) {
                    it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
                    errors.add("Error processing clause: '" + trimmed + "' - " + e.getMessage());
                    // Continue with next clause
                }
            }
            // END_CHANGE: ISS-2025-0085
            // START_CHANGE: ISS-2025-0279 - run deferred initialization/1 goals now that the file is loaded
            runPendingInitializationGoals();
            // END_CHANGE: ISS-2025-0279
        // START_CHANGE: ISS-2025-0346 - propagate halt to the embedder (CLI/IDE terminate the session)
        } catch (PrologException pe) {
            if (pe.isHalt()) {
                throw pe;
            }
            errors.add("Error extracting clauses: " + pe.getMessage());
        // END_CHANGE: ISS-2025-0346
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            errors.add("Error extracting clauses: " + e.getMessage());
        }

        // If there were errors, report them all
        if (!errors.isEmpty()) {
            StringBuilder sb = new StringBuilder("Errors encountered during consult (")
                .append(errors.size()).append(" error(s)):\n");
            for (int i = 0; i < errors.size(); i++) {
                sb.append("  ").append(i + 1).append(". ").append(errors.get(i));
                if (i < errors.size() - 1) sb.append("\n");
            }
            LOGGER.log(Level.WARNING, sb.toString());
            throw new PrologException(sb.toString());
        }
    }
    // END_CHANGE: ISS-2025-0168

    // START_CHANGE: ISS-2025-0292 - opt-in consult driven by the clean-room v2 parser
    // (core.parser.v2). Same clause handling as consult(String) — directives, DCG, facts, rules —
    // but parsing goes through the new ISO tokenizer + operator-precedence parser. The legacy
    // consult() path is unchanged; this lets the v2 parser be validated end-to-end through the
    // engine (and adopted once it passes a full regression).
    public void consultV2(String program) {
        List<java.lang.String> errors = new ArrayList<>();
        try {
            it.denzosoft.jprolog.core.parser.v2.TermReader reader =
                new it.denzosoft.jprolog.core.parser.v2.TermReader(
                    it.denzosoft.jprolog.core.parser.v2.Lexer.tokenize(program), engineState.ops().table());
            // START_CHANGE: ISS-2025-0295 - per-clause error recovery: a parse error on one clause
            // must NOT drop the rest of the file (matches the legacy consult). Resync to the next '.'.
            for (;;) {
                Term clauseTerm;
                try {
                    clauseTerm = reader.nextClause();
                } catch (RuntimeException pe) {
                    it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(pe);   // ISS-2025-0431
                    errors.add("Parse error (v2): " + pe.getMessage());
                    reader.recover();
                    if (reader.atEof()) break;
                    continue;
                }
                if (clauseTerm == null) break;
                // END_CHANGE: ISS-2025-0295
                try {
                    Rule rule = clauseTermToRule(clauseTerm);
                    if (isDirective(rule)) {
                        processDirective(rule);
                    } else if (isDCGRule(rule)) {
                        Rule transformed = transformDCGRule(rule);
                        checkBuiltInConflict(transformed);
                        moduleManager.addRule(transformed);
                        if ("user".equals(moduleManager.getCurrentModule().getName())) {
                            knowledgeBase.addRule(transformed);
                        }
                    } else {
                        checkBuiltInConflict(rule);
                        moduleManager.addRule(rule);
                        if ("user".equals(moduleManager.getCurrentModule().getName())) {
                            knowledgeBase.addRule(rule);
                        }
                    }
                // START_CHANGE: ISS-2025-0346 - a halt raised by a directive aborts the load
                } catch (PrologException pe) {
                    if (pe.isHalt()) {
                        throw pe;
                    }
                    errors.add("Error processing clause '" + clauseTerm + "': " + pe.getMessage());
                // END_CHANGE: ISS-2025-0346
                } catch (Exception e) {
                    it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
                    errors.add("Error processing clause '" + clauseTerm + "': " + e.getMessage());
                }
            }
            runPendingInitializationGoals();
        // START_CHANGE: ISS-2025-0346 - propagate halt to the embedder (CLI/IDE terminate the session)
        } catch (PrologException pe) {
            if (pe.isHalt()) {
                throw pe;
            }
            errors.add("Parse error (v2): " + pe.getMessage());
        // END_CHANGE: ISS-2025-0346
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            errors.add("Parse error (v2): " + e.getMessage());
        }
        if (!errors.isEmpty()) {
            StringBuilder sb = new StringBuilder("Errors during consultV2 (")
                .append(errors.size()).append("):\n");
            for (java.lang.String er : errors) sb.append("  ").append(er).append("\n");
            throw new PrologException(sb.toString());
        }
    }

    /** Convert a v2-parsed clause term into a {@link Rule} (directive / DCG / rule / fact). */
    private Rule clauseTermToRule(Term clause) {
        if (clause instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) clause;
            String f = c.getName();
            int ar = c.getArguments().size();
            if (":-".equals(f) && ar == 2) {                 // Head :- Body
                return new Rule(c.getArguments().get(0), flattenConjunction(c.getArguments().get(1)));
            }
            if (":-".equals(f) && ar == 1) {                 // :- Directive
                return new Rule(clause, new ArrayList<>());
            }
            if ("-->".equals(f) && ar == 2) {                // DCG rule (head is the --> term)
                return new Rule(clause, new ArrayList<>());
            }
        }
        return new Rule(clause, new ArrayList<>());          // fact
    }

    /** Flatten a (right-nested) ','/2 conjunction into a list of goals. */
    private List<Term> flattenConjunction(Term body) {
        List<Term> goals = new ArrayList<>();
        Term cur = body;
        while (cur instanceof CompoundTerm
                && ",".equals(((CompoundTerm) cur).getName())
                && ((CompoundTerm) cur).getArguments().size() == 2) {
            goals.add(((CompoundTerm) cur).getArguments().get(0));
            cur = ((CompoundTerm) cur).getArguments().get(1);
        }
        goals.add(cur);
        return goals;
    }
    // END_CHANGE: ISS-2025-0292
    
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
                            // START_CHANGE: R2 - publish current module name for op/3 visibility
                            // ISS-2025-0474: the operator store is per engine now (design B.12)
                            engineState.ops().setModuleContext(
                                moduleManager.getCurrentModule() != null
                                    ? moduleManager.getCurrentModule().getName()
                                    : "user");
                            // END_CHANGE: R2
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
                    // START_CHANGE: ISS-2025-0092 - Handle table/1 directive during consult
                    case "table":
                        processTableDirective(directive);
                        break;
                    // END_CHANGE: ISS-2025-0092
                    // START_CHANGE: ISS-2025-0167 - Handle meta_predicate/1 directive
                    case "meta_predicate":
                        processMetaPredicateDirective(directive);
                        break;
                    // END_CHANGE: ISS-2025-0167
                    // START_CHANGE: ISS-2025-0167 - Handle module_transparent/1 directive
                    case "module_transparent":
                        processModuleTransparentDirective(directive);
                        break;
                    // END_CHANGE: ISS-2025-0167
                    // START_CHANGE: ISS-2025-0122 - Handle dynamic directive and execute goal directives
                    // START_CHANGE: ISS-2025-0347 - ':- dynamic PI' marks the procedure(s) dynamic in
                    // the KnowledgeBase so an empty dynamic predicate fails instead of raising
                    // existence_error under the 'unknown' flag (was a logged no-op).
                    case "dynamic":
                        processDynamicDirective(directive);
                        break;
                    // END_CHANGE: ISS-2025-0347
                    case "discontiguous":
                    case "ensure_loaded":
                        // These are declaration directives - acknowledge and continue
                        LOGGER.log(Level.FINE, "Declaration directive processed: " + directive);
                        break;
                    // START_CHANGE: ISS-2025-0279 - initialization(Goal): run Goal AFTER the whole
                    // file has been loaded (so it may reference predicates defined later in the file).
                    case "initialization":
                        if (TermUtils.getArity((CompoundTerm) directive) >= 1) {
                            pendingInitializationGoals.add(TermUtils.getArgument((CompoundTerm) directive, 0));
                        }
                        break;
                    // END_CHANGE: ISS-2025-0279
                    default:
                        // ISO Prolog: unknown directives are executed as goals
                        executeGoalDirective(directive);
                    // END_CHANGE: ISS-2025-0122
                }
            } else if (directive instanceof Atom) {
                // START_CHANGE: ISS-2025-0122 - Execute atom directives as goals (e.g., :- run_all_tests.)
                executeGoalDirective(directive);
                // END_CHANGE: ISS-2025-0122
            }
        }
    }

    // START_CHANGE: ISS-2025-0347 - parse ':- dynamic(PI)' (PI = Name/Arity, a ','-sequence of
    // indicators, or a list) and mark each procedure dynamic in the KnowledgeBase.
    private void processDynamicDirective(Term directive) {
        if (directive instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) directive;
            for (Term arg : c.getArguments()) {
                markDynamicIndicators(arg);
            }
        }
        LOGGER.log(Level.FINE, "Dynamic directive processed: " + directive);
    }

    private void markDynamicIndicators(Term spec) {
        if (spec instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) spec;
            String f = c.getName();
            int ar = c.getArguments().size();
            if ((",".equals(f) || ".".equals(f)) && ar == 2) {       // ','-sequence or list of PIs
                markDynamicIndicators(c.getArguments().get(0));
                markDynamicIndicators(c.getArguments().get(1));
                return;
            }
            if ("/".equals(f) && ar == 2) {                          // Name/Arity
                Term name = c.getArguments().get(0);
                Term arity = c.getArguments().get(1);
                if (name instanceof Atom && arity instanceof it.denzosoft.jprolog.core.terms.Number) {
                    knowledgeBase.markDynamic(((Atom) name).getName(),
                        (int) Math.round(((it.denzosoft.jprolog.core.terms.Number) arity).getValue()));
                    return;
                }
            }
        }
        if (spec instanceof Atom && !"[]".equals(((Atom) spec).getName())) {
            // bare ':- dynamic foo.' (SWI extension): mark the arity-0 procedure
            knowledgeBase.markDynamic(((Atom) spec).getName(), 0);
        }
    }
    // END_CHANGE: ISS-2025-0347

    // START_CHANGE: ISS-2025-0279 - Run initialization/1 goals collected during consult, once the
    // whole file is loaded. Snapshot + clear first so a goal that itself consults is isolated.
    private void runPendingInitializationGoals() {
        if (pendingInitializationGoals.isEmpty()) return;
        List<Term> goals = new ArrayList<>(pendingInitializationGoals);
        pendingInitializationGoals.clear();
        for (Term g : goals) {
            executeGoalDirective(g);
        }
    }
    // END_CHANGE: ISS-2025-0279
    
    // START_CHANGE: ISS-2025-0122 - Execute goal directives during consult (ISO Prolog behavior)
    /**
     * Execute a directive as a Prolog goal during consult.
     * This implements ISO Prolog behavior where :- Goal. directives
     * are executed at load time (e.g., :- run_all_tests. or :- assert(fact).)
     */
    private void executeGoalDirective(Term goal) {
        try {
            List<Map<String, Term>> solutions = solve(goal);   // ISS-2025-0484: the selected engine
            if (solutions.isEmpty()) {
                // START_CHANGE: ISS-2025-0288 - surface a failed directive (was logged only at FINE)
                System.err.println("Warning: goal directive failed: " + goal);
                // END_CHANGE: ISS-2025-0288
                LOGGER.log(Level.FINE, "Goal directive failed (no solutions): " + goal);
            }
        // START_CHANGE: ISS-2025-0288 - do not swallow the debugger stop signal; surface errors
        } catch (DebugController.DebugStopException e) {
            throw e;
        // START_CHANGE: ISS-2025-0346 - ':- halt.' in a directive must stop the load and reach the
        // embedder (CLI/IDE), which terminates the session; it is not a directive failure.
        } catch (PrologException pe) {
            if (pe.isHalt()) {
                throw pe;
            }
            System.err.println("Warning: goal directive raised an error: " + goal + " - " + pe.getMessage());
            LOGGER.log(Level.WARNING, "Goal directive error: " + goal + " - " + pe.getMessage());
        // END_CHANGE: ISS-2025-0346
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            System.err.println("Warning: goal directive raised an error: " + goal + " - " + e.getMessage());
            LOGGER.log(Level.WARNING, "Goal directive error: " + goal + " - " + e.getMessage());
        }
        // END_CHANGE: ISS-2025-0288
    }
    // END_CHANGE: ISS-2025-0122

    // START_CHANGE: ISS-2025-0167 - Process meta_predicate/1 directive
    /**
     * Process a meta_predicate/1 directive during consult.
     * Parses the meta-predicate specification and registers it in the current module.
     * Example: :- meta_predicate maplist(2, +, -).
     */
    private void processMetaPredicateDirective(Term directive) {
        if (directive instanceof CompoundTerm && TermUtils.getArity(directive) >= 1) {
            Term spec = TermUtils.getArgument((CompoundTerm) directive, 0);
            if (spec instanceof CompoundTerm) {
                String functor = TermUtils.getFunctorName(spec);
                int arity = TermUtils.getArity(spec);
                it.denzosoft.jprolog.core.module.PredicateSignature sig =
                    new it.denzosoft.jprolog.core.module.PredicateSignature(functor, arity);
                List<String> argSpecs = new ArrayList<>();
                for (int i = 0; i < arity; i++) {
                    Term arg = TermUtils.getArgument((CompoundTerm) spec, i);
                    argSpecs.add(arg.toString());
                }
                moduleManager.getCurrentModule().declareMetaPredicate(sig, argSpecs);
                LOGGER.log(Level.INFO, "Meta-predicate directive processed: " + functor + "/" + arity);
            } else if (spec instanceof Atom) {
                String functor = ((Atom) spec).getName();
                it.denzosoft.jprolog.core.module.PredicateSignature sig =
                    new it.denzosoft.jprolog.core.module.PredicateSignature(functor, 0);
                moduleManager.getCurrentModule().declareMetaPredicate(sig, new ArrayList<>());
                LOGGER.log(Level.INFO, "Meta-predicate directive processed: " + functor + "/0");
            }
        }
    }
    // END_CHANGE: ISS-2025-0167

    // START_CHANGE: ISS-2025-0167 - Process module_transparent/1 directive
    /**
     * Process a module_transparent/1 directive during consult.
     * Marks a predicate as transparent so it uses the caller's module context.
     * Example: :- module_transparent maplist/2.
     */
    private void processModuleTransparentDirective(Term directive) {
        if (directive instanceof CompoundTerm && TermUtils.getArity(directive) == 1) {
            Term arg = TermUtils.getArgument((CompoundTerm) directive, 0);
            if (arg instanceof CompoundTerm && "/".equals(TermUtils.getFunctorName(arg))
                && TermUtils.getArity(arg) == 2) {
                Term functorTerm = TermUtils.getArgument((CompoundTerm) arg, 0);
                Term arityTerm = TermUtils.getArgument((CompoundTerm) arg, 1);
                if (functorTerm instanceof Atom && arityTerm instanceof it.denzosoft.jprolog.core.terms.Number) {
                    String functor = ((Atom) functorTerm).getName();
                    int arity = (int) Math.round(((it.denzosoft.jprolog.core.terms.Number) arityTerm).getValue());
                    it.denzosoft.jprolog.core.module.PredicateSignature sig =
                        new it.denzosoft.jprolog.core.module.PredicateSignature(functor, arity);
                    moduleManager.getCurrentModule().declareTransparent(sig);
                    LOGGER.log(Level.INFO, "Module transparent directive processed: " + functor + "/" + arity);
                }
            }
        }
    }
    // END_CHANGE: ISS-2025-0167

    /**
     * Process a use_module directive.
     */
    private void processUseModuleDirective(Term directive) {
        if (directive instanceof CompoundTerm && TermUtils.getArity(directive) >= 1) {
            Term moduleTerm = TermUtils.getArgument((CompoundTerm) directive, 0);
            // START_CHANGE: Round5 minor - accept library(Name) form for SWI compatibility
            String moduleName = null;
            if (moduleTerm instanceof Atom) {
                moduleName = ((Atom) moduleTerm).getName();
            } else if (moduleTerm instanceof CompoundTerm
                       && "library".equals(TermUtils.getFunctorName(moduleTerm))
                       && TermUtils.getArity(moduleTerm) == 1) {
                Term inner = TermUtils.getArgument((CompoundTerm) moduleTerm, 0);
                if (inner instanceof Atom) {
                    moduleName = ((Atom) inner).getName();
                    // Known auto-loadable libraries: built-ins already registered, treat as no-op success
                    if ("clpfd".equals(moduleName) || "lists".equals(moduleName)
                        || "between".equals(moduleName) || "apply".equals(moduleName)
                        || "assoc".equals(moduleName) || "format".equals(moduleName)) {
                        LOGGER.log(Level.INFO, "Library auto-load (built-in): " + moduleName);
                        return;
                    }
                }
            }
            if (moduleName == null) {
                LOGGER.log(Level.WARNING, "use_module: unrecognized module spec: " + moduleTerm);
                return;
            }
            try {
                moduleManager.importModule(moduleName);
                LOGGER.log(Level.INFO, "Module imported: " + moduleName);
            } catch (IllegalArgumentException e) {
                LOGGER.log(Level.WARNING, "Failed to import module: " + moduleName + " - " + e.getMessage());
            }
            // END_CHANGE: Round5 minor
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
                    // START_CHANGE: ISS-2025-0474 - wave W7 (design B.12): go through the engine's
                    // operator store, so current_op/3 SEES an operator declared by a consulted
                    // `:- op/3` directive (it saw none before: this method only ever touched the
                    // parser's OperatorTable, which current_op/3 did not read) and so an operator
                    // declared inside a module file stays local to that module.
                    engineState.ops().define(precedence, type, name);
                    if (precedence != 0) {
                        // START_CHANGE: ISS-2025-0167 - Per-module operator scope
                        it.denzosoft.jprolog.core.module.Module currentMod = moduleManager.getCurrentModule();
                        if (currentMod != null && !"user".equals(currentMod.getName())) {
                            currentMod.defineOperator(precedence, type, name);
                        }
                        // END_CHANGE: ISS-2025-0167
                    }
                    // END_CHANGE: ISS-2025-0474
                    LOGGER.log(Level.INFO, "Operator directive processed: op(" + precedence + ", " + type + ", " + name + ")");
                } catch (Exception e) {
                    it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
                    LOGGER.log(Level.WARNING, "Failed to process op directive: " + e.getMessage());
                }
            }
        }
    }
    // END_CHANGE: ISS-2025-0085

    // START_CHANGE: ISS-2025-0092 - Process table/1 directive during consult
    /**
     * Process a table/1 directive to declare a predicate as tabled (memoized).
     * Usage: :- table Functor/Arity.
     */
    private void processTableDirective(Term directive) {
        if (directive instanceof CompoundTerm && TermUtils.getArity(directive) == 1) {
            Term arg = TermUtils.getArgument((CompoundTerm) directive, 0);
            if (arg instanceof CompoundTerm && "/".equals(TermUtils.getFunctorName(arg))
                && TermUtils.getArity(arg) == 2) {
                Term functorTerm = TermUtils.getArgument((CompoundTerm) arg, 0);
                Term arityTerm = TermUtils.getArgument((CompoundTerm) arg, 1);
                if (functorTerm instanceof Atom && arityTerm instanceof it.denzosoft.jprolog.core.terms.Number) {
                    String functor = ((Atom) functorTerm).getName();
                    int arity = (int) Math.round(((it.denzosoft.jprolog.core.terms.Number) arityTerm).getValue());
                    tableStore.declareTable(functor, arity);
                    LOGGER.log(Level.INFO, "Table directive processed: " + functor + "/" + arity);
                    return;
                }
            }
        }
        LOGGER.log(Level.WARNING, "Invalid table directive: " + directive);
    }

    /**
     * Get the table store for tabling/memoization support.
     */
    public TableStore getTableStore() {
        return tableStore;
    }
    // END_CHANGE: ISS-2025-0092

    // START_CHANGE: LIM-003 - Global non-backtrackable variable operations
    /**
     * Set a non-backtrackable global variable.
     * @param name The variable name (must be an atom name)
     * @param value The value to store
     */
    public void nbSetval(String name, Term value) {
        globalVariables.put(name, value);
    }

    /**
     * Get a non-backtrackable global variable.
     * @param name The variable name
     * @return The stored value, or null if not set
     */
    public Term nbGetval(String name) {
        return globalVariables.get(name);
    }

    /**
     * Delete a non-backtrackable global variable.
     * @param name The variable name
     */
    public void nbDelete(String name) {
        globalVariables.remove(name);
    }

    /**
     * Get all current global variables as a snapshot.
     * @return A copy of the global variables map
     */
    public Map<String, Term> nbCurrentAll() {
        return new HashMap<>(globalVariables);
    }
    // END_CHANGE: LIM-003

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
            // START_CHANGE: ISS-2025-0304 - default to the clean-room v2 DCG translator
            if (USE_V2_DCG) {
                Term clause = new it.denzosoft.jprolog.core.dcg.v2.DCGTranslator().translate((CompoundTerm) head);
                return clauseTermToRule(clause);
            }
            // END_CHANGE: ISS-2025-0304
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
                        // START_CHANGE: ISS-2025-0347 - assert implies dynamic (ISO 8.9.1)
                        markRuleDynamic(transformedRule);
                        // END_CHANGE: ISS-2025-0347
                    }
                    // END_CHANGE: CR-2025-0002
                } else {
                    // START_CHANGE: CR-2025-0002 - Module-isolated rule storage
                    if ("user".equals(moduleManager.getCurrentModule().getName())) {
                        knowledgeBase.asserta(rule);
                        // START_CHANGE: ISS-2025-0347 - assert implies dynamic (ISO 8.9.1)
                        markRuleDynamic(rule);
                        // END_CHANGE: ISS-2025-0347
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
    
    // START_CHANGE: ISS-2025-0347 - assert implies dynamic (ISO 8.9.1)
    private void markRuleDynamic(Rule rule) {
        Term head = rule.getHead();
        java.lang.String functor = TermUtils.getFunctorName(head);
        if (functor != null) {
            knowledgeBase.markDynamic(functor, TermUtils.getArity(head));
        }
    }
    // END_CHANGE: ISS-2025-0347

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
    // START_CHANGE: ISS-2025-0437 - ENG-06: this engine's OWN ISO flag store (unknown,
    // double_quotes, occurs_check, trace, ...). It is installed as the thread-current store around
    // every solve/consult entry point, so the static PrologFlags API used by the built-ins and the
    // parsers routes to the right engine; the previous store is restored afterwards, so an engine
    // invoked from inside another engine's built-in cannot leave its flags behind.
    private final it.denzosoft.jprolog.core.system.PrologFlags prologFlags = new it.denzosoft.jprolog.core.system.PrologFlags();

    /** This engine's ISO flag store. Flags set here affect only this {@code Prolog} instance. */
    public it.denzosoft.jprolog.core.system.PrologFlags getFlags() { return prologFlags; }

    /** Enable/disable four-port call tracing for THIS engine (safe to call from another thread,
     *  e.g. the IDE's Trace toggle or the CLI's {@code :trace} command). */
    public void setTracing(boolean enabled) { prologFlags.setTracing(enabled); }

    /** True when four-port call tracing is enabled for this engine. */
    public boolean isTracing() { return prologFlags.isTracing(); }

    // END_CHANGE: ISS-2025-0437

    // START_CHANGE: ISS-2025-0472 - engine v4 wave W7 (design B.11/B.12): this engine's OWN stream
    // table, operator store, spy points and profiler counters. Installed as the thread-current
    // state around every solve/consult entry point exactly as the flag store is, so the unchanged
    // static facades (StreamManager, OperatorDefinition, Spy, Profiler) used by the ~400 legacy
    // built-ins route to the right engine on both engines (design decision 1, B.17).
    private final it.denzosoft.jprolog.core.engine.v4.EngineState engineState =
        new it.denzosoft.jprolog.core.engine.v4.EngineState();

    /** This engine's per-engine state: streams, operators, spy points, profiler counters. */
    public it.denzosoft.jprolog.core.engine.v4.EngineState getEngineState() { return engineState; }

    /** This engine's stream table (design B.11). */
    public it.denzosoft.jprolog.core.engine.v4.Streams getStreams() { return engineState.streams(); }

    /** This engine's operator store (design B.12). */
    public it.denzosoft.jprolog.core.engine.v4.Ops getOps() { return engineState.ops(); }

    /** Enter this engine's state on the calling thread; the caller restores it in a finally. */
    private State enterState() {
        return new State(it.denzosoft.jprolog.core.system.PrologFlags.setCurrent(prologFlags),
                         it.denzosoft.jprolog.core.engine.v4.EngineState.setCurrent(engineState));
    }

    private void exitState(State previous) {
        it.denzosoft.jprolog.core.system.PrologFlags.setCurrent(previous.flags);
        it.denzosoft.jprolog.core.engine.v4.EngineState.setCurrent(previous.state);
    }

    /** The saved thread-current engine state of an enclosing engine. */
    private static final class State {
        final it.denzosoft.jprolog.core.system.PrologFlags flags;
        final it.denzosoft.jprolog.core.engine.v4.EngineState state;
        State(it.denzosoft.jprolog.core.system.PrologFlags f, it.denzosoft.jprolog.core.engine.v4.EngineState s) {
            this.flags = f; this.state = s;
        }
    }
    // END_CHANGE: ISS-2025-0472

    // ISS-2025-0437 - ENG-06: run under this engine's own flag store
    public List<Map<String, Term>> solve(String queryString) {
        State prev = enterState();
        try { return solveGuarded(queryString); } finally { exitState(prev); }
    }

        private List<Map<String, Term>> solveGuarded(String queryString) {
        try {
            // START_CHANGE: ISS-2025-0252 - reset transient per-query state (CLP(FD) store)
            resetTransientQueryState();
            // END_CHANGE: ISS-2025-0252
            if (queryString.endsWith(".")) {
                queryString = queryString.substring(0, queryString.length() - 1);
            }
            // START_CHANGE: ISS-2025-0293 - parse queries with the v2 parser by default
            Term query;
            if (USE_V2_PARSER) {
                try {
                    query = it.denzosoft.jprolog.core.parser.v2.TermReader.parseTerm(queryString, engineState.ops().table());
                } catch (StackOverflowError e) {   // ISS-2025-0341: deeply nested untrusted input
                    throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.resourceError("parser_nesting", "read"));
                } catch (RuntimeException e) {
                    it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
                    throw new PrologException("Error parsing query: " + e.getMessage(), e);
                }
            } else {
                query = parser.parseTerm(queryString);
            }
            // END_CHANGE: ISS-2025-0293
            // START_CHANGE: ISS-2025-0461 - wave W4, design decision 3 (B.17, approved): a query's
            // variables die with the query. No session-scoped attributed-variable splicing, so a
            // suspended goal of a FINISHED query can never fire in a later one.
            // ISS-2025-0491 - 4.1 wave A: and there is no other engine to route to.
            return solveWithV4Engine(query);
            // END_CHANGE: ISS-2025-0461
        } catch (DebugController.DebugStopException e) {
            throw e;
        } catch (PrologParserException e) {
            throw new PrologException("Error parsing query: " + e.getMessage(), e);
        }
    }

    // START_CHANGE: ISS-2025-0491 - 4.1 wave A: `solveWithV2Engine` is DELETED with the v2
    // machine, together with the attribute-unify hook install it needed and the cross-query
    // attributed-variable session (design decision 3 dropped it on v4).
    // END_CHANGE: ISS-2025-0491

    // START_CHANGE: ISS-2025-0321 - streaming solve: deliver solutions one at a time to a sink that
    // returns false to stop (lazy + bounded + cancellable). Lets the IDE cap result counts and avoid
    // buffering every solution of a high-/infinite-solution query. Mirrors the solve(String) setup.
    // START_CHANGE: ISS-2025-0484 - wave W9: `solveLegacy(String)`, `solveLegacyGuarded` and
    // `solveWithGuard` are DELETED with the recursive engine they drove. The IDE used
    // `solveLegacy` for detached breakpoint-condition sub-solves; it uses `solve` with the debug
    // controller temporarily nulled instead (DebugPanel).
    // END_CHANGE: ISS-2025-0484

    // ISS-2025-0437 - ENG-06
    public void solveStream(String queryString, java.util.function.Predicate<Map<String, Term>> sink) {
        State prev = enterState();
        try { solveStreamGuarded(queryString, sink); } finally { exitState(prev); }
    }

        private void solveStreamGuarded(String queryString, java.util.function.Predicate<Map<String, Term>> sink) {
        resetTransientQueryState();
        if (queryString.endsWith(".")) queryString = queryString.substring(0, queryString.length() - 1);
        Term query;
        try {
            query = USE_V2_PARSER
                ? it.denzosoft.jprolog.core.parser.v2.TermReader.parseTerm(queryString, engineState.ops().table())
                : parser.parseTerm(queryString);
        } catch (StackOverflowError e) {   // ISS-2025-0341: deeply nested untrusted input
            throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.resourceError("parser_nesting", "read"));
        } catch (RuntimeException e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw new PrologException("Error parsing query: " + e.getMessage(), e);
        }
        // ISS-2025-0444 - the v4 streaming path (lazy + cancellable)
        // ISS-2025-0461 - no cross-query coroutining (design decision 3)
        solveStreamWithV4Engine(query, sink);
    }
    // END_CHANGE: ISS-2025-0311

    // START_CHANGE: ISS-2025-0491 - 4.1 wave A: `spliceAttributedSessionVars` /
    // `refreshAttributedSessionVars` and the `attributedSessionVars` map are DELETED with the v2
    // engine that used them. Cross-query coroutining was dropped on v4 by design decision 3
    // (B.17): a query's variables — and the goals frozen on them — die with the query.
    /** Kept as a no-op for embedders written against 3.x: there is no cross-solve state to clear.
     *  Residual constraints of one answer are read with {@link #residualGoals(Map)}. */
    public void clearSession() {
        // nothing to clear on v4 (ISS-2025-0461)
    }
    // END_CHANGE: ISS-2025-0491
    // END_CHANGE: v2.9.4

    // START_CHANGE: ISS-2025-0462 - engine v4 wave W4 (design B.9 / B.12, limit L-11): the residual
    // goals of an answer. An answer whose variables are still constrained is only half printed
    // without them — SWI shows `freeze(X, Goal)`, `dif(X, Y)` and `X in 1..3` after the bindings.
    // This is the accessor the CLI and the IDE will print in wave W7; nothing prints it yet.
    /**
     * The residual goals still attached to the variables of {@code solution}: frozen goals
     * ({@code freeze/2}), pending {@code when/2} conditions, {@code dif/2} constraints and CLP(FD)
     * domains ({@code in/2}), plus {@code put_attr/3} for an attribute module with no known
     * rendering. Empty when the answer is unconstrained.
     *
     * <p>Call it right after the {@code solve} that produced the answer: the CLP(FD) part reads the
     * per-query constraint store, which the next top-level query resets. Meaningful on the v4
     * engine, where attributes live in the answer's variable cells; on the legacy and v2 engines an
     * answer is a name-keyed map of resolved terms and this returns whatever attributes those terms
     * still carry.
     */
    public List<Term> residualGoals(Map<String, Term> solution) {
        if (solution == null || solution.isEmpty()) return new ArrayList<>();
        return it.denzosoft.jprolog.core.engine.v4.Coroutining.residualGoals(
            new ArrayList<>(solution.values()));
    }
    // END_CHANGE: ISS-2025-0462
    
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
    // ISS-2025-0437 - ENG-06
    public List<Map<String, Term>> solve(Term query) {
        State prev = enterState();
        try { return solveTermGuarded(query); } finally { exitState(prev); }
    }

        private List<Map<String, Term>> solveTermGuarded(Term query) {
        // START_CHANGE: ISS-2025-0252 - reset transient per-query state (CLP(FD) store)
        resetTransientQueryState();
        // END_CHANGE: ISS-2025-0252
        // START_CHANGE: ISS-2025-0345 - the Term overload runs the same engine as solve(String),
        // with the inference budget applied and StackOverflowError converted to resource_error.
        // ISS-2025-0491 - 4.1 wave A: that engine is v4, and it is the only one.
        return solveWithV4Engine(query);
        // END_CHANGE: ISS-2025-0345
    }

    // START_CHANGE: ISS-2025-0252 - Reset process-wide transient state at the start of each
    // top-level query so it does not leak across independent queries (and engines). The CLP(FD)
    // ConstraintStore is currently a singleton keyed by variable name; a per-engine store keyed
    // by variable identity is the proper long-term fix (tracked separately).
    private void resetTransientQueryState() {
        it.denzosoft.jprolog.builtin.clpfd.ConstraintStore.getInstance().clear();
        // START_CHANGE: ISS-2025-0294 - reset the per-query v2 CLP(FD) store (no cross-query leak)
        it.denzosoft.jprolog.builtin.clpfd.v2.ClpfdV2Bridge.reset();
        // END_CHANGE: ISS-2025-0294
    }

    // START_CHANGE: ISS-2025-0294 - opt-in: route the CLP(FD) built-ins through the clean-room v2
    // solver (interval domains, real #\= propagation, sound labeling, per-query identity store).
    public void enableV2Clpfd() {
        it.denzosoft.jprolog.builtin.clpfd.v2.ClpfdV2Builtins.register(this);
    }
    // END_CHANGE: ISS-2025-0294
    // END_CHANGE: ISS-2025-0252
    
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
        // ISS-2025-0484: the recursive solver's LOGGER.info trace (an unrelated mechanism to
        // trace/0) went away with it; the four-port tracer is Prolog.setTracing / the engine flag.
        this.traceEnabled = traceEnabled;
    }

    // START_CHANGE: ISS-2025-0168 - Occurs check flag support
    /**
     * Set the occurs_check Prolog flag.
     * When true, the occurs check is performed during standard unification.
     * When false (default), the occurs check is skipped for performance.
     * The unify_with_occurs_check/2 built-in always performs the check
     * regardless of this flag.
     *
     * @param enabled true to enable occurs check
     */
    public void setOccursCheck(boolean enabled) {
        Variable.setOccursCheckEnabled(enabled);
    }

    /**
     * Get the current value of the occurs_check flag.
     *
     * @return true if occurs check is enabled
     */
    public boolean getOccursCheck() {
        return Variable.isOccursCheckEnabled();
    }
    // END_CHANGE: ISS-2025-0168

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
        // START_CHANGE: ISS-2025-0499 - 4.1 wave B: through StreamManager.out() (invariant 11), so
        // listing/0 is captured by with_output_to/2 and by the IDE's per-thread console override
        // instead of escaping to the process stdout.
        it.denzosoft.jprolog.builtin.io.StreamManager.out().println(listing);
        // END_CHANGE: ISS-2025-0499
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
        // START_CHANGE: ISS-2025-0499 - see listing() above (invariant 11)
        it.denzosoft.jprolog.builtin.io.StreamManager.out().println(listing);
        // END_CHANGE: ISS-2025-0499
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
                // ISS-2025-0499: Rule.toString() already ends in the full stop; appending one
                // printed every clause as "foo(a)..". Visible in listing/0 and, since listing/1
                // works at all, there too.
                sb.append(rule.toString()).append("\n");
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
                sb.append(rule.toString()).append("\n");   // ISS-2025-0499: no doubled full stop
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

    // START_CHANGE: ISS-2025-0122 - Retract with unification bindings
    public Map<String, Term> retractClauseWithBindings(Term term, Map<String, Term> bindings) {
        return knowledgeBase.retractClauseWithBindings(term, bindings);
    }
    // END_CHANGE: ISS-2025-0122

    // START_CHANGE: ISS-2025-0164 - Non-deterministic retract/1
    public List<Map<String, Term>> retractAllClausesWithBindings(Term term, Map<String, Term> bindings) {
        return knowledgeBase.retractAllClausesWithBindings(term, bindings);
    }
    // END_CHANGE: ISS-2025-0164

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
     * The durable per-engine execution context: this is where the IDE installs its
     * {@link DebugController} and where the running machine publishes the query's
     * {@link ResourceGuard}.
     *
     * <p>ISS-2025-0484 (wave W9) replaced the old solver accessor with this: the recursive
     * solver it used to return no longer exists.
     */
    public EngineContext getEngineContext() {
        return engineContext;
    }

    /** The built-in predicate registry (used by the v2 engine bridge). */
    public BuiltInRegistry getBuiltInRegistry() {
        return builtInRegistry;
    }

    // START_CHANGE: ISS-2025-0338 - sandbox / safe mode for untrusted programs
    /** Built-in packages that can touch the host (process exec, JVM reflection, filesystem, network,
     *  database, persistence). Removed by {@link #enableSafeMode()}. */
    private static final String[] UNSAFE_BUILTIN_PACKAGES = {
        ".builtin.os.", ".builtin.ffi.", ".builtin.filesystem.", ".builtin.network.",
        ".builtin.http.", ".builtin.jdbc.", ".builtin.persistence.",
        // START_CHANGE: ISS-2025-0479 - wave W8 made thread_create/2,3 and the concurrent_* family
        // run REAL goals on REAL threads. A JVM thread is a host resource exactly like a process or
        // a socket, and an untrusted program that can spawn them escapes the inference budget (each
        // worker gets its own counter). They were harmless before — thread_create did not run its
        // goal — so the package had never needed to be denied.
        ".builtin.threading."
        // END_CHANGE: ISS-2025-0479
    };
    private boolean safeMode = false;

    /**
     * Remove all host-touching built-ins (OS shell, Java FFI, filesystem, network, HTTP, JDBC,
     * persistence, threads) from THIS engine, so a subsequently consulted/queried (untrusted) program cannot
     * execute processes, reflect into the JVM, or read/write files, sockets or databases. Irreversible
     * for this instance. Returns the number of predicates removed. Use a fresh {@link Prolog} per
     * security domain. NOTE: this is a deny-by-package sandbox, not a full resource sandbox — combine
     * with an inference budget ({@link #setInferenceBudget(long)}) and a query timeout. (ISS-2025-0338)
     */
    public int enableSafeMode() {
        int removed = 0;
        for (String name : builtInRegistry.getBuiltInNames()) {
            BuiltIn b = builtInRegistry.getBuiltIn(name);
            if (b == null) continue;
            String cls = b.getClass().getName();
            for (String pkg : UNSAFE_BUILTIN_PACKAGES) {
                if (cls.contains(pkg)) { builtInRegistry.unregisterBuiltIn(name); removed++; break; }
            }
        }
        safeMode = true;
        return removed;
    }

    public boolean isSafeMode() { return safeMode; }

    // ISS-2025-0339: per-query inference (step) budget for the default v2 engine; 0 = unlimited.
    private long inferenceBudget = 0;
    /** Abort any subsequent query by throwing {@link InferenceLimitException} after this many
     *  resolution steps. Deliberately NOT a PrologException, so an untrusted {@code catch/3}
     *  cannot trap it — the Java embedder must catch it. Bounds CPU on untrusted/runaway
     *  queries (v2 engine only). 0 disables it.
     *
     *  <p>ISS-2025-0427 / ENG-08 — the unit is a MACHINE STEP (one drive-loop iteration), not a
     *  logical inference: conjunction splits, {@code true}, cut and the machine's internal action
     *  goals each consume one. Treat it as a runaway guard with a 2-4x safety factor over the
     *  predicate-call count, not as a metering device. */
    public void setInferenceBudget(long steps) { this.inferenceBudget = steps; }
    public long getInferenceBudget() { return inferenceBudget; }
    // END_CHANGE: ISS-2025-0338

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
    // ISS-2025-0437 - ENG-06
    public CompilationResult consultWithDiagnostics(String program, String filename) {
        State prev = enterState();
        try { return consultWithDiagnosticsGuarded(program, filename); } finally { exitState(prev); }
    }

        private CompilationResult consultWithDiagnosticsGuarded(String program, String filename) {
        // START_CHANGE: ISS-2025-0302 - honor the default v2 parser for IDE diagnostics too
        if (USE_V2_PARSER) return consultWithDiagnosticsV2(program, filename);
        // END_CHANGE: ISS-2025-0302
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
                    it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
                    errors.add(new CompilationError(filename, lineEstimate, e.getMessage(), "error"));
                }

                // Estimate line number from clause content
                for (char c : clause.toCharArray()) {
                    if (c == '\n') lineEstimate++;
                }
            }
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            errors.add(new CompilationError(filename, 1, e.getMessage(), "error"));
        }

        return new CompilationResult(errors.isEmpty(), errors, clauseCount);
    }

    // START_CHANGE: ISS-2025-0302 - per-clause diagnostics through the v2 parser (line-accurate,
    // resync on parse error so every clause is reported, not just the first).
    private CompilationResult consultWithDiagnosticsV2(String program, String filename) {
        List<CompilationError> errors = new ArrayList<>();
        int clauseCount = 0;
        try {
            it.denzosoft.jprolog.core.parser.v2.TermReader reader =
                new it.denzosoft.jprolog.core.parser.v2.TermReader(
                    it.denzosoft.jprolog.core.parser.v2.Lexer.tokenize(program), engineState.ops().table());
            for (;;) {
                int line = reader.peekLine();
                Term clauseTerm;
                try {
                    clauseTerm = reader.nextClause();
                } catch (RuntimeException pe) {
                    it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(pe);   // ISS-2025-0431
                    errors.add(new CompilationError(filename, line, pe.getMessage(), "error"));
                    reader.recover();
                    if (reader.atEof()) break;
                    continue;
                }
                if (clauseTerm == null) break;
                try {
                    Rule rule = clauseTermToRule(clauseTerm);
                    rule.setSourceLine(line);                     // ISS-2025-0322: for line breakpoints
                    if (isDirective(rule)) {
                        processDirective(rule);
                    } else if (isDCGRule(rule)) {
                        Rule tr = transformDCGRule(rule);
                        tr.setSourceLine(line);
                        checkBuiltInConflict(tr);
                        moduleManager.addRule(tr);
                        if ("user".equals(moduleManager.getCurrentModule().getName())) knowledgeBase.addRule(tr);
                    } else {
                        checkBuiltInConflict(rule);
                        moduleManager.addRule(rule);
                        if ("user".equals(moduleManager.getCurrentModule().getName())) knowledgeBase.addRule(rule);
                    }
                    clauseCount++;
                } catch (Exception e) {
                    it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
                    errors.add(new CompilationError(filename, line, e.getMessage(), "error"));
                }
            }
            runPendingInitializationGoals();
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            errors.add(new CompilationError(filename, 1, e.getMessage(), "error"));
        }
        return new CompilationResult(errors.isEmpty(), errors, clauseCount);
    }
    // END_CHANGE: ISS-2025-0302
    // END_CHANGE: ISS-2025-0090

    // START_CHANGE: ISS-2025-0322 - line -> predicate indicator, for line-accurate IDE breakpoints.
    /**
     * Return the predicate indicator ("functor/arity") of the clause whose head is on, or immediately
     * above, the given 1-based source line — i.e. the clause that "owns" that line. Returns null if no
     * clause maps to the line. Used by the IDE to turn a gutter click into an accurate spy point
     * instead of a fragile regex over the source text.
     */
    public String getPredicateIndicatorAtLine(int line) {
        Rule best = null;
        for (Rule r : knowledgeBase.getRules()) {
            int sl = r.getSourceLine();
            if (sl < 0 || sl > line) continue;
            if (best == null || sl > best.getSourceLine()) best = r;
        }
        if (best == null) return null;
        Term h = best.getHead();
        if (h instanceof it.denzosoft.jprolog.core.terms.Atom) {
            return ((it.denzosoft.jprolog.core.terms.Atom) h).getName() + "/0";
        }
        if (h instanceof it.denzosoft.jprolog.core.terms.CompoundTerm) {
            it.denzosoft.jprolog.core.terms.CompoundTerm c = (it.denzosoft.jprolog.core.terms.CompoundTerm) h;
            return c.getName() + "/" + c.getArguments().size();
        }
        return null;
    }
    // END_CHANGE: ISS-2025-0322

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
            // ISS-2025-0447 - stamp each clause with its source line so the .jpc file (format 0x03)
            // carries it and IDE line breakpoints work on compiled sources too.
            List<Integer> lines = parser.getLastClauseLines();
            for (int ci = 0; ci < clauses.size(); ci++) {
                java.lang.String trimmed = clauses.get(ci).trim();
                if (trimmed.isEmpty()) continue;
                Rule rule = parser.parseRule(trimmed);
                if (ci < lines.size() && lines.get(ci) != null) rule.setSourceLine(lines.get(ci));
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
                it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
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
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            LOGGER.log(Level.WARNING, "Failed to compile: " + e.getMessage());
        }
    }
    // END_CHANGE: ISS-2025-0085
}
