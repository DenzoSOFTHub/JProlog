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

    // START_CHANGE: ISS-2025-0636 - initialization(G, main) (SWI): the goal is the program's MAIN
    // goal — it runs after everything is loaded and then the process halts (0 on success, 1 on
    // failure or an uncaught error, N on halt(N)). Only a toplevel can do that: the CLI asks for the
    // goals to be deferred and runs them itself. An embedder that does not ask keeps the pre-4.5
    // behaviour (the goal runs after the load, like initialization/1, and nothing halts).
    private volatile boolean deferInitializationMain;
    private final List<Term> initializationMain = java.util.Collections.synchronizedList(new ArrayList<Term>());

    /** Collect {@code initialization(G, main)} goals instead of running them after the load. */
    public void setDeferInitializationMain(boolean defer) { this.deferInitializationMain = defer; }

    /** The collected {@code initialization(G, main)} goals, in load order; the list is emptied. */
    public List<Term> takeInitializationMain() {
        synchronized (initializationMain) {
            List<Term> out = new ArrayList<Term>(initializationMain);
            initializationMain.clear();
            return out;
        }
    }

    /**
     * Run {@code goal} once, as a toplevel does with a -g / main goal: true on success, false on
     * failure; an uncaught error or halt propagates as the PrologException.
     */
    public boolean runOnce(Term goal) {
        final boolean[] ok = {false};
        State prev = enterState();
        try {
            resetTransientQueryState();
            solveStreamWithV4Engine(goal, sol -> { ok[0] = true; return false; });
        } finally {
            exitState(prev);
        }
        return ok[0];
    }
    // END_CHANGE: ISS-2025-0636

    // START_CHANGE: ISS-2025-0736 - 4.6 wave Q3.6: initialization(G, main) for embedders. Call
    // setDeferInitializationMain(true) before loading, then runMain(): the collected goals run in
    // load order, as SWI's main does, and the exit code the process WOULD halt with is returned
    // (nothing halts the JVM).
    /**
     * Run the deferred {@code initialization(G, main)} goals.
     *
     * @return -1 when there is no main goal; 0 when every goal succeeded; 1 when one failed or
     *         raised an uncaught error (reported on {@code user_error}); N when one called
     *         {@code halt(N)}
     */
    public int runMain() {
        List<Term> mains = takeInitializationMain();
        if (mains.isEmpty()) return -1;
        for (Term g : mains) {
            try {
                if (!runOnce(g)) {
                    warn("goal (" + it.denzosoft.jprolog.core.engine.v4.Writer.format(g,
                        it.denzosoft.jprolog.core.engine.v4.Writer.Options.writeq()) + ") failed");
                    return 1;
                }
            } catch (PrologException pe) {
                if (pe.isHalt()) return pe.getExitCode();
                Term et = pe.getErrorTerm();
                warn("goal (" + it.denzosoft.jprolog.core.engine.v4.Writer.format(g,
                    it.denzosoft.jprolog.core.engine.v4.Writer.Options.writeq()) + ") raised "
                    + (et != null ? it.denzosoft.jprolog.core.engine.v4.Writer.format(et,
                        it.denzosoft.jprolog.core.engine.v4.Writer.Options.writeq()) : String.valueOf(pe.getMessage())));
                return 1;
            }
        }
        return 0;
    }
    // END_CHANGE: ISS-2025-0736

    // START_CHANGE: ISS-2025-0627 - wave P6.4: streaming with determinism, for a toplevel.
    /** One answer of {@link #solveStream(String, AnswerSink)}. */
    public interface AnswerSink {
        /**
         * @param answer the answer (a copy: later bindings cannot show through it)
         * @param more   false when no alternative is left, i.e. this is certainly the last answer
         * @return false to stop the search
         */
        boolean onAnswer(Map<String, Term> answer, boolean more);
    }

    /** Like {@link #solveStream(String, java.util.function.Predicate)}, also saying whether more may follow. */
    public void solveStream(String queryString, final AnswerSink sink) {
        final it.denzosoft.jprolog.core.engine.v4.Machine[] holder = new it.denzosoft.jprolog.core.engine.v4.Machine[1];
        State prev = enterState();
        it.denzosoft.jprolog.core.engine.v4.Machine[] prevStreaming = streamingMachine.get();   // ISS-2025-0755
        try {
            machineHolder.set(holder);
            streamingMachine.set(holder);
            solveStreamGuarded(queryString, sol -> sink.onAnswer(sol, holder[0] == null || holder[0].hasAlternatives()));
        } finally {
            machineHolder.remove();
            if (prevStreaming == null) streamingMachine.remove(); else streamingMachine.set(prevStreaming);
            exitState(prev);
        }
    }

    // START_CHANGE: ISS-2025-0755 - 4.6 wave Q4.5: a toplevel can tell a conditional answer.
    private final ThreadLocal<it.denzosoft.jprolog.core.engine.v4.Machine[]> streamingMachine =
        new ThreadLocal<it.denzosoft.jprolog.core.engine.v4.Machine[]>();

    /**
     * Inside an {@link AnswerSink} of {@link #solveStream(String, AnswerSink)}: the delayed
     * literals of the answer being delivered (a conjunction such as {@code tnot(p)}), or null when
     * the answer is unconditional. Non-null means the answer is <i>undefined</i> under the
     * well-founded semantics.
     */
    public Term currentAnswerDelays() {
        it.denzosoft.jprolog.core.engine.v4.Machine[] h = streamingMachine.get();
        return (h == null || h[0] == null) ? null : h[0].answerDelays();
    }
    // END_CHANGE: ISS-2025-0755

    /** Set by {@link #solveStream(String, AnswerSink)} so the machine can be handed back. */
    private final ThreadLocal<it.denzosoft.jprolog.core.engine.v4.Machine[]> machineHolder =
        new ThreadLocal<it.denzosoft.jprolog.core.engine.v4.Machine[]>();
    // END_CHANGE: ISS-2025-0627

    private void solveStreamWithV4Engine(Term query, java.util.function.Predicate<Map<String, Term>> sink) {
        // START_CHANGE: ISS-2025-0491 - 4.1 wave A: the legacy Variable.AttributeUnifyHook was the
        // v2 engine's coroutining entry point and is deleted with it; v4 has its own wake queue
        // (core.engine.v4.Coroutining), so there is nothing to uninstall around a query any more.
        try {
            it.denzosoft.jprolog.core.engine.v4.Machine m =
                new it.denzosoft.jprolog.core.engine.v4.Machine(getV4Engine(), new ResourceGuard(inferenceBudget));
            // ISS-2025-0627: hand the machine to solveStream(String, AnswerSink), once, for this query
            it.denzosoft.jprolog.core.engine.v4.Machine[] holder = machineHolder.get();
            if (holder != null && holder[0] == null) { holder[0] = m; machineHolder.remove(); }
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
        // START_CHANGE: ISS-2025-0737 - file_search_path/2 is a user dynamic multifile predicate
        // (defined, initially without clauses: the library/swi/foreign defaults are applied by
        // the file search itself, see FileSearch)
        knowledgeBase.markDynamic("file_search_path", 2);
        knowledgeBase.markMultifile("file_search_path", 2);
        // END_CHANGE: ISS-2025-0737
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
        try {
            // START_CHANGE: ISS-2025-0573 - every consult is one LOAD: its module scope ends with it
            if (USE_V2_PARSER) { consultV2(program); return; }
            LoadContext ctx = newLoadContext(null, null);
            beginLoad(ctx);                                                  // ISS-2025-0739
            try {
                consultGuarded(program);
            } finally {
                try { runInitGoals(ctx); } finally { finishLoad(ctx); }
            }
            // END_CHANGE: ISS-2025-0573
        } finally { exitState(prev); }
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
        // START_CHANGE: ISS-2025-0574 - 4.5 wave P3: the v2 consult is the loader core now (load
        // context stack, module scope per load, include/1, per-load initialization goals).
        LoadResult r = loadText(program, newLoadContext(null, null), true);
        throwIfErrors(r);
        // END_CHANGE: ISS-2025-0574
    }

    // =====================================================================================
    // START_CHANGE: ISS-2025-0574 - 4.5 wave P3.1/P3.2: loading files from Prolog.
    // One LOAD = one LoadContext on a per-engine stack: the file (null for text handed in from
    // Java), the directory relative names resolve against, the module that was current when the
    // load began, the initialization/1 goals it collected, and the modules it declared. When a
    // load ends the current (type-in) module is the one it started in again, and the exports of
    // every module the load declared are imported into it (P3.2: consulting m1.pl then m2.pl, or a
    // module file then a plain file, no longer leaves the second file inside the first module).
    // =====================================================================================

    /** One active load. */
    public static final class LoadContext {
        final String file;
        final String directory;
        final String source;
        final String startModule;
        final boolean include;
        final List<Term> initGoals;
        final List<String> modules;
        final Set<String> userPredicates;
        final int[] clauses;
        final List<LoadError> errors;
        final java.util.function.Function<Term, Boolean> runner;
        String diagnosticsName;
        int includeDepth;
        // START_CHANGE: ISS-2025-0730 - clause ownership and the discontiguous check
        /** The file that OWNS the clauses of this load (an include's clauses belong to the
         *  including file); null for text consulted from Java. */
        final String owner;
        /** Predicates of this file seen so far, and the one the previous clause belonged to. */
        final Set<String> seenKeys = new HashSet<>();
        String lastName, lastModule;
        int lastArity = -1;
        /** discontiguous/1 declarations of this load (shared with its includes). */
        final Set<String> discontiguous;
        /** Did this load store clauses for another module (flat M:Head clauses)? */
        final boolean[] qualifiedClauses;
        /** The files this load included (for make/0). */
        final List<String> includes;
        // END_CHANGE: ISS-2025-0730

        LoadContext(String file, String directory, String source, String startModule,
                    java.util.function.Function<Term, Boolean> runner) {
            this.file = file;
            this.directory = directory;
            this.source = source;
            this.startModule = startModule;
            this.include = false;
            this.initGoals = new ArrayList<>();
            this.modules = new ArrayList<>();
            this.userPredicates = new LinkedHashSet<>();
            this.clauses = new int[1];
            this.errors = new ArrayList<>();
            this.runner = runner;
            this.owner = file;                                                   // ISS-2025-0730
            this.discontiguous = new HashSet<>();
            this.qualifiedClauses = new boolean[1];
            this.includes = new ArrayList<>();
        }

        /** The context of an included file: same load, another file. */
        LoadContext(LoadContext parent, String file) {
            this.file = file;
            this.directory = new java.io.File(file).getParent();
            this.source = parent.source;
            this.startModule = parent.startModule;
            this.include = true;
            this.initGoals = parent.initGoals;
            this.modules = parent.modules;
            this.userPredicates = parent.userPredicates;
            this.clauses = parent.clauses;
            this.errors = parent.errors;
            this.runner = parent.runner;
            this.diagnosticsName = parent.diagnosticsName;
            this.includeDepth = parent.includeDepth + 1;
            this.owner = parent.owner;                                           // ISS-2025-0730
            this.discontiguous = parent.discontiguous;
            this.qualifiedClauses = parent.qualifiedClauses;
            this.includes = parent.includes;
        }

        public String file() { return file; }
        public String directory() { return directory; }
        public String source() { return source; }
    }

    /** One error found while loading. */
    public static final class LoadError {
        public final String file;
        public final int line;
        public final String message;
        LoadError(String file, int line, String message) { this.file = file; this.line = line; this.message = message; }
        @Override public String toString() {
            return (file != null ? file + ":" + line + ": " : "") + message;
        }
    }

    /** What a load did. */
    public static final class LoadResult {
        public final String file;
        public final int clauses;
        public final List<LoadError> errors;
        public final String module;
        LoadResult(String file, int clauses, List<LoadError> errors, String module) {
            this.file = file; this.clauses = clauses; this.errors = errors; this.module = module;
        }
    }

    /** A file loaded by this engine: when, what it defined, and the module it is (if any). */
    private static final class FileRecord {
        final String path;
        long modified;
        String module;
        Set<String> userPredicates = new LinkedHashSet<>();
        boolean qualifiedClauses;                                               // ISS-2025-0730
        /** Included files and their modification times when loaded (ISS-2025-0736, make/0). */
        Map<String, Long> includes = new LinkedHashMap<>();
        FileRecord(String path) { this.path = path; }
    }

    // START_CHANGE: ISS-2025-0739 - 4.6 wave Q3.8: the lock is per FILE and the load-context
    // stack is per THREAD (a load on another thread is another load); the loaded-file table has
    // its own monitor.
    private final LoadLock loadLock = new LoadLock();
    private final ThreadLocal<ArrayDeque<LoadContext>> loadStacks = ThreadLocal.withInitial(ArrayDeque::new);
    /** The lock key of text consulted from Java (such loads are serialised with each other). */
    private static final String TEXT_LOAD_KEY = "<text>";

    private ArrayDeque<LoadContext> loadStack() { return loadStacks.get(); }

    /** Enter a load: its file's lock, the thread's module scope, the context stack. */
    private void beginLoad(LoadContext ctx) {
        String key = ctx.file != null ? ctx.file : TEXT_LOAD_KEY;
        loadLock.lock(key);
        moduleManager.enterLoad();
        loadStack().push(ctx);
    }

    /** Leave a load entered with {@link #beginLoad}: end it (module scope, imports), then unlock. */
    private void finishLoad(LoadContext ctx) {
        String key = ctx.file != null ? ctx.file : TEXT_LOAD_KEY;
        try {
            loadStack().pop();
            endLoad(ctx);
        } finally {
            try { moduleManager.exitLoad(); } finally { loadLock.unlock(key); }
        }
    }

    /** Is {@code file} being loaded by THIS thread (a file that loads itself)? */
    private boolean loadingOnThisThread(String file) {
        for (LoadContext c : loadStack()) if (file.equals(c.file) && !c.include) return true;
        return false;
    }
    // END_CHANGE: ISS-2025-0739
    private final Map<String, FileRecord> loadedFiles = new LinkedHashMap<>();

    private LoadContext newLoadContext(String file, java.util.function.Function<Term, Boolean> runner) {
        String dir;
        if (file != null) {
            dir = it.denzosoft.jprolog.core.engine.v4.EngineState.file(file).getAbsoluteFile().getParent();   // ISS-2025-0745
        } else {
            LoadContext outer = loadStack().peek();
            dir = outer != null ? outer.directory : engineState.workingDirectory();   // ISS-2025-0745
        }
        LoadContext outer = loadStack().peek();
        String source = file != null ? file : (outer != null ? outer.source : null);
        String start = moduleManager.getCurrentModule() != null ? moduleManager.getCurrentModule().getName() : "user";
        return new LoadContext(file, dir, source, start, runner);
    }

    /** The innermost active load, or null. */
    public LoadContext currentLoad() {
        return loadStack().peek();                                          // ISS-2025-0739: per thread
    }

    /** Load program text as one load. Parse and clause errors are collected, never thrown. */
    private LoadResult loadText(String program, LoadContext ctx, boolean v2) {
        beginLoad(ctx);                                                      // ISS-2025-0739
        try {
            loadClauses(program, ctx);
            runInitGoals(ctx);
        } finally {
            finishLoad(ctx);
        }
        return new LoadResult(ctx.file, ctx.clauses[0], ctx.errors,
            ctx.modules.isEmpty() ? null : ctx.modules.get(0));
    }

    /** Read and handle every clause of {@code program} in {@code ctx} (the v2 reader). */
    private void loadClauses(String program, LoadContext ctx) {
        it.denzosoft.jprolog.core.parser.v2.TermReader reader;
        try {
            reader = new it.denzosoft.jprolog.core.parser.v2.TermReader(
                it.denzosoft.jprolog.core.parser.v2.Lexer.tokenize(program), engineState.ops().table());
        } catch (RuntimeException e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);
            ctx.errors.add(new LoadError(ctx.file, 1, "Parse error (v2): " + e.getMessage()));
            return;
        }
        for (;;) {
            int line = reader.peekLine();
            Term clauseTerm;
            try {
                clauseTerm = reader.nextClause();
            } catch (RuntimeException pe) {
                it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(pe);   // ISS-2025-0431
                ctx.errors.add(new LoadError(ctx.file, line, "Parse error (v2): " + messageOf(pe)));
                reader.recover();
                if (reader.atEof()) break;
                continue;
            }
            if (clauseTerm == null) break;
            handleClause(clauseTerm, line, ctx);
        }
    }

    private static String messageOf(Throwable e) {
        return e.getMessage();
    }

    /** One clause of a load: a directive, a DCG rule, a rule or a fact. */
    private void handleClause(Term clauseTerm, int line, LoadContext ctx) {
        // START_CHANGE: ISS-2025-0571 - a user term_expansion/2 rewrites each clause read (SWI)
        if (!!knowledgeBase.hasRules("term_expansion/2")) {
            List<Term> expanded;
            try {
                expanded = termExpansion(clauseTerm);
            } catch (PrologException pe) {
                if (pe.isHalt()) throw pe;
                ctx.errors.add(new LoadError(ctx.file, line, "term_expansion/2: " + messageOf(pe)));
                return;
            }
            if (expanded != null) {
                for (Term e : expanded) handleRule(null, e, line, ctx);
                return;
            }
        }
        // END_CHANGE: ISS-2025-0571
        handleRule(null, clauseTerm, line, ctx);
    }

    // START_CHANGE: ISS-2025-0731 - 4.6 wave Q3.2: goal_expansion/2 (SWI). When user or the module
    // being loaded defines goal_expansion/2, every goal of a clause body (and of a directive) is
    // expanded at load time, through the control constructs and the goal arguments of the common
    // meta-predicates, to a fixpoint — the module's own goal_expansion/2 first, then user's —
    // with a depth cap that turns a non-terminating expansion into a load error.
    static final int GOAL_EXPANSION_MAX_DEPTH = 100;

    private boolean goalExpansionDefined() {
        if (knowledgeBase.hasRules("goal_expansion/2")) return true;
        String cm = moduleManager.getCurrentModule().getName();
        return !"user".equals(cm) && moduleManager.getCurrentModule().definesGoalExpansion();
    }

    private Rule expandRuleGoals(Rule rule) {
        if (rule.getBody().isEmpty() || !goalExpansionDefined()) return rule;
        // one call per clause: the head and the body travel together, so the variables the
        // expansion shares with the rest of the clause stay shared
        Term body = rule.getBody().get(rule.getBody().size() - 1);
        for (int i = rule.getBody().size() - 2; i >= 0; i--) {
            body = new CompoundTerm(new Atom(","), Arrays.asList(rule.getBody().get(i), body));
        }
        Term whole = new CompoundTerm(new Atom(":-"), Arrays.asList(rule.getHead(), body));
        Term[] out = expandGoalsOf(whole, body);
        if (out == null) return rule;
        Rule r = new Rule(((CompoundTerm) out[0]).getArguments().get(0), flattenConjunction(out[1]));
        r.setSourceLine(rule.getSourceLine());
        return r;
    }

    /** Expand a directive's goal (null-safe: the goal itself when nothing applies). */
    private Term expandDirectiveGoal(Term goal) {
        if (!goalExpansionDefined()) return goal;
        Term[] out = expandGoalsOf(goal, goal);
        return out == null ? goal : out[1];
    }

    /**
     * Expand every goal of {@code goals} (a subterm of {@code context}); returns the context and
     * the goals AFTER the expansion (as they came back from the engine, variables shared), or
     * null when nothing changed.
     */
    private Term[] expandGoalsOf(Term context, Term goals) {
        final boolean[] changed = {false};
        final Map<Term, Term> done = new IdentityHashMap<>();
        Term expanded = expandGoal(goals, 0, changed);
        if (!changed[0]) return null;
        // re-read the context with the expanded goals put in place (identity substitution)
        done.put(goals, expanded);
        return new Term[] { substituteIdentity(context, done), expanded };
    }

    private static Term substituteIdentity(Term t, Map<Term, Term> map) {
        Term r = map.get(t);
        if (r != null) return r;
        if (t instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) t;
            List<Term> args = c.getArguments();
            List<Term> out = null;
            for (int i = 0; i < args.size(); i++) {
                Term a = args.get(i);
                Term b = substituteIdentity(a, map);
                if (b != a && out == null) { out = new ArrayList<>(args); }
                if (out != null) out.set(i, b);
            }
            return out == null ? t : new CompoundTerm(c.getFunctor(), out);
        }
        return t;
    }

    private Term expandGoal(Term g0, int depth, boolean[] changed) {
        Term g = it.denzosoft.jprolog.core.engine.v4.Unify.deref(g0);
        if (g instanceof Variable) return g;
        if (g instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) g;
            String f = c.getName();
            int n = c.getArguments().size();
            int[] goalArgs = metaGoalArgs(f, n);
            if (goalArgs != null) {
                List<Term> args = new ArrayList<>(c.getArguments());
                boolean any = false;
                for (int k : goalArgs) {
                    Term a = args.get(k);
                    Term b = expandMetaArg(a, depth, changed);
                    if (b != a) { args.set(k, b); any = true; }
                }
                return any ? new CompoundTerm(c.getFunctor(), args) : g;
            }
        } else if (!(g instanceof Atom)) {
            return g;
        }
        if (depth > GOAL_EXPANSION_MAX_DEPTH) {
            throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.resourceError(
                "goal_expansion_depth", "goal_expansion/2"));
        }
        Term r = callGoalExpansion(g);
        if (r == null || variantOf(r, g)) return g;
        changed[0] = true;
        return expandGoal(r, depth + 1, changed);
    }

    /** A meta-argument: `V^G` (bagof/setof) keeps the `^` prefix; `M:G` expands G. */
    private Term expandMetaArg(Term a0, int depth, boolean[] changed) {
        Term a = it.denzosoft.jprolog.core.engine.v4.Unify.deref(a0);
        if (a instanceof CompoundTerm && ((CompoundTerm) a).getArguments().size() == 2
                && ("^".equals(((CompoundTerm) a).getName()) || ":".equals(((CompoundTerm) a).getName()))) {
            Term inner = ((CompoundTerm) a).getArguments().get(1);
            Term e = expandMetaArg(inner, depth, changed);
            return e == inner ? a : new CompoundTerm(((CompoundTerm) a).getFunctor(),
                Arrays.asList(((CompoundTerm) a).getArguments().get(0), e));
        }
        return expandGoal(a, depth, changed);
    }

    /** The goal-argument positions of the control constructs and common meta-predicates. */
    private static int[] metaGoalArgs(String f, int n) {
        switch (n) {
            case 1:
                if ("\\+".equals(f) || "call".equals(f) || "once".equals(f) || "ignore".equals(f)
                        || "not".equals(f)) return new int[] {0};
                return null;
            case 2:
                if (",".equals(f) || ";".equals(f) || "->".equals(f) || "*->".equals(f) || "|".equals(f)
                        || "forall".equals(f)) return new int[] {0, 1};
                if (":".equals(f)) return new int[] {1};
                return null;
            case 3:
                if ("findall".equals(f) || "bagof".equals(f) || "setof".equals(f)
                        || "aggregate_all".equals(f)) return new int[] {1};
                if ("catch".equals(f)) return new int[] {0, 2};
                if ("setup_call_cleanup".equals(f)) return new int[] {0, 1, 2};
                return null;
            case 4:
                if ("findall".equals(f)) return new int[] {1};
                return null;
            default:
                return null;
        }
    }

    /** goal_expansion(G, X) in the loading module, then in user: X, or null. */
    private Term callGoalExpansion(Term g) {
        String cm = moduleManager.getCurrentModule().getName();
        if (!"user".equals(cm) && moduleManager.getCurrentModule().definesGoalExpansion()) {
            Term r = runGoalExpansion(g, cm);
            if (r != null) return r;
        }
        if (knowledgeBase.hasRules("goal_expansion/2")) return runGoalExpansion(g, null);
        return null;
    }

    private Term runGoalExpansion(Term g, String module) {
        Variable out = new Variable("GoalExpansionResult__");
        Term call = new CompoundTerm(new Atom("goal_expansion"), Arrays.asList(g, (Term) out));
        if (module != null) call = new CompoundTerm(new Atom(":"), Arrays.asList((Term) new Atom(module), call));
        List<Map<String, Term>> sols = solve(new CompoundTerm(new Atom("once"), Collections.singletonList(call)));
        if (sols.isEmpty()) return null;
        return sols.get(0).get("GoalExpansionResult__");
    }

    /** Are {@code a} and {@code b} variants (equal up to a consistent renaming of variables)? */
    private static boolean variantOf(Term a, Term b) {
        return variant(a, b, new IdentityHashMap<Variable, Variable>(), new IdentityHashMap<Variable, Variable>());
    }

    private static boolean variant(Term a0, Term b0, Map<Variable, Variable> ab, Map<Variable, Variable> ba) {
        Term a = it.denzosoft.jprolog.core.engine.v4.Unify.deref(a0);
        Term b = it.denzosoft.jprolog.core.engine.v4.Unify.deref(b0);
        if (a instanceof Variable || b instanceof Variable) {
            if (!(a instanceof Variable) || !(b instanceof Variable)) return false;
            Variable x = ab.get(a), y = ba.get(b);
            if (x == null && y == null) { ab.put((Variable) a, (Variable) b); ba.put((Variable) b, (Variable) a); return true; }
            return x == b && y == a;
        }
        if (a instanceof CompoundTerm) {
            if (!(b instanceof CompoundTerm)) return false;
            CompoundTerm x = (CompoundTerm) a, y = (CompoundTerm) b;
            if (!x.getName().equals(y.getName()) || x.getArguments().size() != y.getArguments().size()) return false;
            for (int i = 0; i < x.getArguments().size(); i++) {
                if (!variant(x.getArguments().get(i), y.getArguments().get(i), ab, ba)) return false;
            }
            return true;
        }
        return it.denzosoft.jprolog.core.engine.v4.Unify.equalTerms(a, b, null);
    }
    // END_CHANGE: ISS-2025-0731

    // START_CHANGE: ISS-2025-0571
    /** The clauses term_expansion/2 turns {@code t} into, or null when it does not apply. */
    private List<Term> termExpansion(Term t) {
        Variable out = new Variable("TermExpansionResult__");
        Term goal = new CompoundTerm(new Atom("once"), Collections.singletonList(
            new CompoundTerm(new Atom("term_expansion"), Arrays.asList(t, (Term) out))));
        List<Map<String, Term>> sols = solve(goal);
        if (sols.isEmpty()) return null;
        Term r = sols.get(0).get("TermExpansionResult__");
        if (r == null) return null;
        List<Term> outList = new ArrayList<>();
        Term cur = r;
        if (cur instanceof CompoundTerm && ".".equals(((CompoundTerm) cur).getName())
                && ((CompoundTerm) cur).getArguments().size() == 2) {
            while (cur instanceof CompoundTerm && ".".equals(((CompoundTerm) cur).getName())
                    && ((CompoundTerm) cur).getArguments().size() == 2) {
                outList.add(((CompoundTerm) cur).getArguments().get(0));
                cur = ((CompoundTerm) cur).getArguments().get(1);
            }
            return outList;
        }
        if (cur instanceof Atom && "[]".equals(((Atom) cur).getName())) return outList;
        outList.add(r);
        return outList;
    }
    // END_CHANGE: ISS-2025-0571

    /** One clause of a load, as a Rule (the .jpc path) or as the term read (the source path). */
    private void handleRule(Rule given, Term clauseTerm, int line, LoadContext ctx) {
        try {
            Rule rule = given != null ? given : clauseTermToRule(clauseTerm);
            if (clauseTerm == null) clauseTerm = rule.getHead();
            if (line > 0) rule.setSourceLine(line);                  // ISS-2025-0322
            if (isDirective(rule)) {
                Term d = TermUtils.getArgument((CompoundTerm) rule.getHead(), 0);
                if (d instanceof CompoundTerm && "include".equals(((CompoundTerm) d).getName())
                        && ((CompoundTerm) d).getArguments().size() == 1) {
                    includeFile(((CompoundTerm) d).getArguments().get(0), ctx);   // ISS-2025-0575
                } else {
                    processDirective(rule);
                }
            } else if (isDCGRule(rule)) {
                Rule transformed = transformDCGRule(rule);
                if (line > 0) transformed.setSourceLine(line);
                addLoadedClause(expandRuleGoals(transformed), ctx);           // ISS-2025-0731
            } else {
                addLoadedClause(expandRuleGoals(rule), ctx);                  // ISS-2025-0731
            }
        // START_CHANGE: ISS-2025-0346 - a halt raised by a directive aborts the load
        } catch (PrologException pe) {
            if (pe.isHalt()) throw pe;
            ctx.errors.add(new LoadError(ctx.file, line, "Error processing clause '" + clauseTerm + "': " + messageOf(pe)));
        // END_CHANGE: ISS-2025-0346
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            ctx.errors.add(new LoadError(ctx.file, line, "Error processing clause '" + clauseTerm + "': " + e.getMessage()));
        } catch (StackOverflowError so) {
            ctx.errors.add(new LoadError(ctx.file, line, "error(resource_error(stack_overflow),consult)"));
        }
    }

    private void addLoadedClause(Rule rule, LoadContext ctx) {
        // START_CHANGE: ISS-2025-0733 - a module-qualified head (`m:foo(1).`, `user:hook(X) :- B`)
        // defines the predicate in THAT module (SWI), not a flat ':'/2 predicate
        if (rule.getHead() instanceof CompoundTerm && ":".equals(((CompoundTerm) rule.getHead()).getName())
                && ((CompoundTerm) rule.getHead()).getArguments().size() == 2) {
            rule = placeQualified(rule, ctx);
            if (rule == null) { ctx.clauses[0]++; return; }
        }
        // END_CHANGE: ISS-2025-0733
        checkBuiltInConflict(rule);
        // START_CHANGE: ISS-2025-0730 - ownership + the SWI discontiguous warning
        if (ctx.owner != null) rule.setSourceFile(ctx.owner);
        it.denzosoft.jprolog.core.module.Module curMod = moduleManager.getCurrentModule();
        String cur = curMod.getName();
        boolean newPred = noteClauseKey(rule, cur, ctx);
        // END_CHANGE: ISS-2025-0730
        moduleManager.addRule(curMod, rule);
        if ("user".equals(cur)) {
            knowledgeBase.addRule(rule);
            // ISS-2025-0730: recorded once per run of clauses of one predicate, not per clause
            if (ctx.file != null && newPred) ctx.userPredicates.add(ctx.lastName + "/" + ctx.lastArity);
        }
        ctx.clauses[0]++;
    }

    // START_CHANGE: ISS-2025-0730 - 4.6 wave Q3.1: discontiguous/1 (SWI: a warning, not an error)
    /** Returns true when the clause starts a new run of clauses (another predicate than the last). */
    private boolean noteClauseKey(Rule rule, String module, LoadContext ctx) {
        Term h = rule.getHead();
        String name = TermUtils.getFunctorName(h);
        int arity = TermUtils.getArity(h);
        if (arity == ctx.lastArity && name.equals(ctx.lastName) && module.equals(ctx.lastModule)) return false;
        ctx.lastName = name;
        ctx.lastArity = arity;
        ctx.lastModule = module;
        String key = "user".equals(module) ? name + "/" + arity : module + ":" + name + "/" + arity;
        if (!ctx.seenKeys.add(key) && !ctx.discontiguous.contains(key)
                && !("user".equals(module) && knowledgeBase.isDynamic(TermUtils.getFunctorName(h), TermUtils.getArity(h)))) {
            warn((ctx.file != null ? ctx.file + ":" + rule.getSourceLine() + ": " : "")
                + "Clauses of " + key + " are not together in the source-file");
        }
        return true;
    }
    // END_CHANGE: ISS-2025-0730

    // START_CHANGE: ISS-2025-0733 - 4.6 wave Q3 (extra): clauses with a module-qualified head.
    // `user:H :- B` goes into user; `M:H` for the module being loaded is an ordinary clause of it;
    // any other module is created on demand and the clause is stored as the `M:H` clause every
    // other path (assertz(M:H), clause/2, retract/1, M:G calls) already uses. When the clause is
    // written inside another module S, its body runs in S (SWI: a clause added to another
    // module's multifile predicate keeps the source module's context), so the body's goals are
    // qualified with S.
    /** Store {@code rule} (head {@code M:H}); returns the unqualified rule to add normally, or null. */
    private Rule placeQualified(Rule rule, LoadContext ctx) {
        Term mt = ((CompoundTerm) rule.getHead()).getArguments().get(0);
        Term inner = ((CompoundTerm) rule.getHead()).getArguments().get(1);
        while (inner instanceof CompoundTerm && ":".equals(((CompoundTerm) inner).getName())
                && ((CompoundTerm) inner).getArguments().size() == 2) {
            mt = ((CompoundTerm) inner).getArguments().get(0);
            inner = ((CompoundTerm) inner).getArguments().get(1);
        }
        if (!(mt instanceof Atom) || !(inner instanceof Atom || inner instanceof CompoundTerm)) return rule;
        String target = ((Atom) mt).getName();
        String source = moduleManager.getCurrentModule().getName();
        if (target.equals(source)) {
            Rule r = new Rule(inner, rule.getBody());
            r.setSourceLine(rule.getSourceLine());
            return r;
        }
        List<Term> body = qualifyBody(rule.getBody(), source);
        Rule r;
        if ("user".equals(target)) {
            r = new Rule(inner, body);
            r.setSourceLine(rule.getSourceLine());
            checkBuiltInConflict(r);
            if (ctx.owner != null) r.setSourceFile(ctx.owner);
            noteClauseKey(r, "user", ctx);
            knowledgeBase.addRule(r);
            if (ctx.file != null) ctx.userPredicates.add(TermUtils.getFunctorName(inner) + "/" + TermUtils.getArity(inner));
            return null;
        }
        ensureModule(target);
        r = new Rule(new CompoundTerm(new Atom(":"), Arrays.asList((Term) new Atom(target), inner)), body);
        r.setSourceLine(rule.getSourceLine());
        if (ctx.owner != null) r.setSourceFile(ctx.owner);
        knowledgeBase.addRule(r);
        ctx.qualifiedClauses[0] = true;
        return null;
    }

    /** Create module {@code name} on demand (a clause or declaration names it). */
    public void ensureModule(String name) {
        if ("user".equals(name) || "system".equals(name)) return;
        if (getV4Engine().modules4().isModule(name)) return;
        moduleManager.getOrCreateModule(name);
    }

    /** The body goals, each run in module {@code m}: `m:G` for every goal outside a control construct. */
    private static List<Term> qualifyBody(List<Term> body, String m) {
        List<Term> out = new ArrayList<>(body.size());
        for (Term g : body) out.add(it.denzosoft.jprolog.core.engine.v4.Machine.qualifyGoal(g, m));
        return out;
    }
    // END_CHANGE: ISS-2025-0733

    private void runInitGoals(LoadContext ctx) {
        if (ctx.initGoals.isEmpty()) return;
        List<Term> goals = new ArrayList<>(ctx.initGoals);
        ctx.initGoals.clear();
        for (Term g : goals) executeGoalDirective(g);
    }

    /** End of a load: back to the module it started in, which imports the modules it declared. */
    private void endLoad(LoadContext ctx) {
        if (ctx.include) return;
        it.denzosoft.jprolog.core.module.Module cur = moduleManager.getCurrentModule();
        if (cur == null || !ctx.startModule.equals(cur.getName())) {
            try {
                moduleManager.setCurrentModule(ctx.startModule);
            } catch (IllegalArgumentException e) {
                moduleManager.setCurrentModule("user");
            }
            engineState.ops().setModuleContext(moduleManager.getCurrentModule().getName());
        }
        for (String m : ctx.modules) {
            if (m.equals(ctx.startModule) || "user".equals(m)) continue;
            try {
                moduleManager.importModule(m);
            } catch (RuntimeException e) {
                it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);
                if (LOGGER.isLoggable(Level.FINE)) LOGGER.log(Level.FINE, "import of " + m + " failed", e);
            }
        }
    }

    private void throwIfErrors(LoadResult r) {
        if (r.errors.isEmpty()) return;
        StringBuilder sb = new StringBuilder("Errors during consultV2 (").append(r.errors.size()).append("):\n");
        for (LoadError er : r.errors) sb.append("  ").append(er.message).append("\n");
        throw new PrologException(sb.toString());
    }

    // ---------------------------------------------------------------- include/1 (ISS-2025-0575)

    // START_CHANGE: ISS-2025-0575 - :- include(File): the clauses of File, read in place, in the
    // same load (module, initialization goals, clause count) as the including file.
    private void includeFile(Term spec, LoadContext ctx) {
        if (safeMode && (safeModeOptions == null || safeModeOptions.readDirs().isEmpty())) {
            throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.permissionError(
                "open", "source_sink", spec, "include/1"));
        }
        if (ctx.includeDepth > 64) {
            throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.resourceError(
                "include_depth", "include/1"));
        }
        java.io.File f = resolveSourceFile(spec, ctx.directory, "include/1");
        checkSafeRead(f, spec, "include/1");                                   // ISS-2025-0625
        String text = readSourceText(f, spec, "include/1");
        LoadContext child = new LoadContext(ctx, canonical(f));
        if (!ctx.includes.contains(child.file)) ctx.includes.add(child.file);   // ISS-2025-0736
        loadStack().push(child);                  // ISS-2025-0739: same load, same thread, same lock
        try { loadClauses(text, child); } finally { loadStack().pop(); }
    }
    // END_CHANGE: ISS-2025-0575

    // ---------------------------------------------------------------- file resolution

    private static String canonical(java.io.File f) {
        try { return f.getCanonicalPath(); } catch (java.io.IOException e) { return f.getAbsolutePath(); }
    }

    /** The path text of a source specification: an atom, a string, or Dir/File segments. */
    private static String specText(Term spec) {
        if (spec instanceof Atom) return ((Atom) spec).getName();
        if (spec instanceof it.denzosoft.jprolog.core.terms.PrologString) {
            return ((it.denzosoft.jprolog.core.terms.PrologString) spec).getStringValue();
        }
        if (spec instanceof CompoundTerm && "/".equals(((CompoundTerm) spec).getName())
                && ((CompoundTerm) spec).getArguments().size() == 2) {
            String a = specText(((CompoundTerm) spec).getArguments().get(0));
            String b = specText(((CompoundTerm) spec).getArguments().get(1));
            return (a == null || b == null) ? null : a + "/" + b;
        }
        return null;
    }

    /**
     * The file a source specification names: relative names resolve against {@code dir} (the
     * directory of the file being loaded), and {@code .pl} is tried when the name has no
     * extension (then {@code .prolog}, then the bare name). Raises the ISO errors.
     */
    java.io.File resolveSourceFile(Term spec, String dir, String ctx) {
        Term s = spec;
        if (s instanceof Variable) {
            throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError(ctx));
        }
        String text = specText(s);
        if (text == null) {
            throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.domainError("source_sink", s, ctx));
        }
        if (text.startsWith("~/")) text = System.getProperty("user.home") + text.substring(1);
        java.io.File base = new java.io.File(text);
        if (!base.isAbsolute() && dir != null) base = new java.io.File(dir, text);
        String name = base.getName();
        boolean hasExt = name.lastIndexOf('.') > 0;
        String[] candidates = hasExt ? new String[] { "", ".pl" } : new String[] { ".pl", ".prolog", "" };
        for (String ext : candidates) {
            java.io.File f = new java.io.File(base.getPath() + ext);
            if (f.isFile()) return f;
        }
        throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.existenceError("source_sink", s, ctx));
    }

    // START_CHANGE: ISS-2025-0737 - a source specification: a file name, or Alias(Path) searched
    // through file_search_path/2 (library(X) included) for a readable Prolog source
    java.io.File resolveLoadFile(Term spec0, String dir, String ctx) {
        Term spec = resolveTermSimple(spec0);
        if (spec instanceof CompoundTerm && ((CompoundTerm) spec).getArguments().size() == 1) {
            FileSearch.Options o = new FileSearch.Options();
            o.fileType = "prolog";
            o.access = "read";
            o.extensions = new ArrayList<>(Arrays.asList(".pl", ".prolog", ""));
            List<String> found = FileSearch.resolve(spec, o, this::solve, dir, ctx);
            if (found.isEmpty()) {
                throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.existenceError("source_sink", spec, ctx));
            }
            return new java.io.File(found.get(0));
        }
        return resolveSourceFile(spec, dir, ctx);
    }
    // END_CHANGE: ISS-2025-0737

    private static String readSourceText(java.io.File f, Term spec, String ctx) {
        try {
            return new String(java.nio.file.Files.readAllBytes(f.toPath()), java.nio.charset.StandardCharsets.UTF_8);
        } catch (java.io.IOException e) {
            throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.permissionError(
                "open", "source_sink", spec, ctx));
        }
    }

    // ---------------------------------------------------------------- the Java API

    /**
     * Load a source file (resolved against the directory of the file being loaded, or the working
     * directory), as {@code consult/1} does: a file loaded before is RELOADED (the user
     * predicates it defined are wiped first). Clause errors are collected in the result, not
     * thrown; a missing file raises {@code existence_error(source_sink, F)}.
     */
    public LoadResult loadFile(String path) {
        State prev = enterState();
        try {
            return loadSpec(new Atom(path), "true", false, null, "consult/1");
        } finally { exitState(prev); }
    }

    /** {@link #loadFile}, raising a PrologException listing the errors if there were any. */
    public LoadResult consultFile(String path) {
        LoadResult r = loadFile(path);
        throwIfErrors(r);
        return r;
    }

    /**
     * The engine side of consult/1, ensure_loaded/1, load_files/1,2 and {@code [F|Fs]}.
     *
     * @param ifMode   {@code true} (always load), {@code changed} (load unless loaded and
     *                 unmodified), {@code not_loaded} (load unless loaded)
     * @param runner   runs a directive goal (the calling query's machine), or null
     */
    public void loadFromGoal(Term spec, String ifMode, boolean mustBeModule,
                             java.util.function.Function<Term, Boolean> runner, String ctx) {
        spec = resolveTermSimple(spec);
        if (spec instanceof Variable) {
            throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError(ctx));
        }
        if (spec instanceof CompoundTerm && ".".equals(((CompoundTerm) spec).getName())
                && ((CompoundTerm) spec).getArguments().size() == 2) {
            Term cur = spec;
            while (cur instanceof CompoundTerm && ".".equals(((CompoundTerm) cur).getName())
                    && ((CompoundTerm) cur).getArguments().size() == 2) {
                loadFromGoal(((CompoundTerm) cur).getArguments().get(0), ifMode, mustBeModule, runner, ctx);
                cur = ((CompoundTerm) cur).getArguments().get(1);
            }
            if (cur instanceof Variable) {
                throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError(ctx));
            }
            if (!(cur instanceof Atom && "[]".equals(((Atom) cur).getName()))) {
                throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("list", spec, ctx));
            }
            return;
        }
        if (spec instanceof Atom && "[]".equals(((Atom) spec).getName())) return;
        LoadResult r = loadSpec(spec, ifMode, mustBeModule, runner, ctx);
        if (r != null) {
            for (LoadError e : r.errors) warn(e.toString());
        }
    }

    private static Term resolveTermSimple(Term t) {
        return it.denzosoft.jprolog.core.engine.v4.Unify.deref(t);
    }

    /** Print {@code Warning: text} on this engine's user_error (ISS-2025-0753: also for table/1). */
    public void warnUser(String text) { warn(text); }

    private void warn(String text) {
        it.denzosoft.jprolog.core.engine.v4.Streams st = it.denzosoft.jprolog.builtin.io.StreamManager.streams();
        java.io.PrintStream err = st.writerFor(st.userError());      // ISS-2025-0730: the live user_error
        if (err == null) err = System.err;
        err.println("Warning: " + text);
        err.flush();
    }

    /** Known SWI libraries whose predicates JProlog provides built in (a load is a no-op). */
    private static final Set<String> BUILTIN_LIBRARIES = new HashSet<>(Arrays.asList(
        "lists", "apply", "pairs", "coroutining", "clpfd", "between", "format", "readutil",
        "strings", "aggregate", "error", "debug", "yall", "tabling", "dif", "when", "system",
        "statistics", "occurs", "ordsets", "apply_macros", "dcg/basics", "dialect", "solution_sequences"));

    private LoadResult loadSpec(Term spec, String ifMode, boolean mustBeModule,
                                java.util.function.Function<Term, Boolean> runner, String ctx) {
        spec = resolveTermSimple(spec);
        if (spec instanceof CompoundTerm && "library".equals(((CompoundTerm) spec).getName())
                && ((CompoundTerm) spec).getArguments().size() == 1) {
            String lib = specText(resolveTermSimple(((CompoundTerm) spec).getArguments().get(0)));
            it.denzosoft.jprolog.core.engine.v4.Modules ms = getV4Engine().modules4();
            if (lib != null && ms.isModule(lib) && moduleManager.getModule(lib) == null) {
                ms.ensureLoaded(lib);
                return null;
            }
            if (lib != null && BUILTIN_LIBRARIES.contains(lib)) return null;
            // ISS-2025-0737: any other library(X) is searched on file_search_path/2 (below)
        }
        LoadContext outer = currentLoad();
        String dir = outer != null ? outer.directory : engineState.workingDirectory();   // ISS-2025-0745
        java.io.File f = resolveLoadFile(spec, dir, ctx);                        // ISS-2025-0737
        checkSafeRead(f, spec, ctx);                                           // ISS-2025-0625
        String abs = canonical(f);
        FileRecord rec;
        Map<String, List<Rule>> tails = null;                                   // ISS-2025-0794
        synchronized (loadedFiles) { rec = loadedFiles.get(abs); }               // ISS-2025-0739
        if (loadingOnThisThread(abs)) return null;           // ISS-2025-0739: a file that loads itself
        if (rec != null) {
            if ("not_loaded".equals(ifMode) || ("changed".equals(ifMode) && rec.modified == f.lastModified())) {
                importLoadedModule(rec);
                return null;
            }
            tails = unloadFile(rec);                                              // ISS-2025-0794
        }
        String text = readSourceText(f, spec, ctx);
        LoadContext lc = newLoadContext(abs, runner);
        LoadResult r;
        try {
            r = loadText(text, lc, true);
        } finally {
            // START_CHANGE: ISS-2025-0794 - other files' clauses go back behind the reloaded ones
            if (tails != null) {
                for (Map.Entry<String, List<Rule>> t : tails.entrySet()) {
                    String pi = t.getKey();
                    int slash = pi.lastIndexOf('/');
                    knowledgeBase.moveToEnd(pi.substring(0, slash), Integer.parseInt(pi.substring(slash + 1)), t.getValue());
                }
            }
            // END_CHANGE: ISS-2025-0794
        }
        FileRecord nr = new FileRecord(abs);
        nr.modified = f.lastModified();
        nr.module = r.module;
        nr.userPredicates.addAll(lc.userPredicates);
        nr.qualifiedClauses = lc.qualifiedClauses[0];                                  // ISS-2025-0730
        for (String inc : lc.includes) nr.includes.put(inc, new java.io.File(inc).lastModified());   // ISS-2025-0736
        synchronized (loadedFiles) { loadedFiles.put(abs, nr); }                // ISS-2025-0739
        if (mustBeModule && r.module == null) {
            throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.domainError("module_file", spec, ctx));
        }
        return r;
    }

    private void importLoadedModule(FileRecord rec) {
        if (rec.module == null) return;
        it.denzosoft.jprolog.core.module.Module cur = moduleManager.getCurrentModule();
        if (cur != null && rec.module.equals(cur.getName())) return;
        try { moduleManager.importModule(rec.module); } catch (RuntimeException e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);
        }
    }

    /** Reconsult: the user predicates a file defined are wiped before it is loaded again. */
    private Map<String, List<Rule>> unloadFile(FileRecord rec) {
        // START_CHANGE: ISS-2025-0730 - 4.6 wave Q3.1: only the clauses THIS file owns go (it
        // abolished every predicate the file touched, so clauses other files had added — to a
        // multifile predicate or not — and the dynamic declaration were lost)
        Map<String, List<Rule>> tails = new java.util.LinkedHashMap<>();         // ISS-2025-0794
        for (String pi : rec.userPredicates) {
            int slash = pi.lastIndexOf('/');
            String f = pi.substring(0, slash);
            int n = Integer.parseInt(pi.substring(slash + 1));
            List<Rule> tail = knowledgeBase.rulesAfterFirstOwnedBy(f, n, rec.path);   // ISS-2025-0794
            if (!tail.isEmpty()) tails.put(pi, tail);
            knowledgeBase.removeClausesOwnedBy(f, n, rec.path);
        }
        if (rec.qualifiedClauses) knowledgeBase.removeClausesOwnedBy(":", 2, rec.path);
        // END_CHANGE: ISS-2025-0730
        return tails;
    }

    /** make/0: reload every loaded file modified since it was loaded. Returns how many. */
    public int make(java.util.function.Function<Term, Boolean> runner) {
        List<FileRecord> recs;
        synchronized (loadedFiles) { recs = new ArrayList<>(loadedFiles.values()); }   // ISS-2025-0739
        int n = 0;
        for (FileRecord rec : recs) {
            java.io.File f = new java.io.File(rec.path);
            // ISS-2025-0736: a changed INCLUDED file reloads the file that includes it
            boolean incChanged = false;
            for (Map.Entry<String, Long> ie : rec.includes.entrySet()) {
                java.io.File inf = new java.io.File(ie.getKey());
                if (!inf.isFile() || inf.lastModified() != ie.getValue()) { incChanged = true; break; }
            }
            if (f.isFile() && (f.lastModified() != rec.modified || incChanged)) {
                loadFromGoal(new Atom(rec.path), "true", false, runner, "make/0");
                n++;
            }
        }
        return n;
    }

    /** source_file/1: the loaded files, in load order (absolute paths). */
    public List<String> loadedSourceFiles() {
        synchronized (loadedFiles) { return new ArrayList<>(loadedFiles.keySet()); }   // ISS-2025-0739
    }

    /** source_file/2: the user predicates ("name/arity") each loaded file defined. */
    public Map<String, Set<String>> loadedSourcePredicates() {
        Map<String, Set<String>> out = new LinkedHashMap<>();
        synchronized (loadedFiles) {                                             // ISS-2025-0739
            for (FileRecord r : loadedFiles.values()) out.put(r.path, new LinkedHashSet<>(r.userPredicates));
        }
        return out;
    }
    // END_CHANGE: ISS-2025-0574

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
                        // START_CHANGE: ISS-2025-0573 - `:- module(user, _)` switches back to user
                        // (it used to REPLACE the user module object), and every module a load
                        // declares is recorded so the load can import it when it ends.
                        if (TermUtils.getArity(directive) == 2
                                && TermUtils.getArgument((CompoundTerm) directive, 0) instanceof Atom
                                && "user".equals(((Atom) TermUtils.getArgument((CompoundTerm) directive, 0)).getName())) {
                            moduleManager.setCurrentModule("user");
                            engineState.ops().setModuleContext("user");
                            break;
                        }
                        if (TermUtils.getArity(directive) == 2 && currentLoad() != null
                                && TermUtils.getArgument((CompoundTerm) directive, 0) instanceof Atom) {
                            currentLoad().modules.add(((Atom) TermUtils.getArgument((CompoundTerm) directive, 0)).getName());
                        }
                        // END_CHANGE: ISS-2025-0573
                        if (moduleManager.parseModuleDirective(directive)) {
                            // START_CHANGE: R2 - publish current module name for op/3 visibility
                            // ISS-2025-0474: the operator store is per engine now (design B.12)
                            engineState.ops().setModuleContext(
                                moduleManager.getCurrentModule() != null
                                    ? moduleManager.getCurrentModule().getName()
                                    : "user");
                            // END_CHANGE: R2
                            if (LOGGER.isLoggable(Level.FINE)) LOGGER.log(Level.FINE, "Module directive processed: " + directive);   // ISS-2025-0573: was INFO noise on stderr
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
                    // START_CHANGE: ISS-2025-0730 - 4.6 wave Q3.1: multifile/1 defines the
                    // predicate(s) and discontiguous/1 silences the warning (they were no-ops)
                    case "discontiguous":
                    case "multifile":
                        for (Term a : ((CompoundTerm) directive).getArguments()) {
                            declarePredicates(a, functor, moduleManager.getCurrentModule().getName());
                        }
                        break;
                    // END_CHANGE: ISS-2025-0730
                    // START_CHANGE: ISS-2025-0279 - initialization(Goal): run Goal AFTER the whole
                    // file has been loaded (so it may reference predicates defined later in the file).
                    case "initialization":
                        if (TermUtils.getArity((CompoundTerm) directive) >= 1) {
                            // START_CHANGE: ISS-2025-0574 - the goals belong to their own load;
                            // initialization(G, now) runs G at once
                            Term g = TermUtils.getArgument((CompoundTerm) directive, 0);
                            Term when = TermUtils.getArity((CompoundTerm) directive) == 2
                                ? TermUtils.getArgument((CompoundTerm) directive, 1) : null;
                            LoadContext lc = currentLoad();
                            if (when instanceof Atom && "now".equals(((Atom) when).getName())) {
                                executeGoalDirective(g);
                            } else if (when instanceof Atom && "main".equals(((Atom) when).getName())
                                    && deferInitializationMain) {
                                initializationMain.add(g);                     // ISS-2025-0636
                            } else if (lc != null) {
                                lc.initGoals.add(g);
                            } else {
                                pendingInitializationGoals.add(g);
                            }
                            // END_CHANGE: ISS-2025-0574
                        }
                        break;
                    // END_CHANGE: ISS-2025-0279
                    default:
                        // ISO Prolog: unknown directives are executed as goals
                        executeGoalDirective(expandDirectiveGoal(directive));     // ISS-2025-0731
                    // END_CHANGE: ISS-2025-0122
                }
            } else if (directive instanceof Atom) {
                // START_CHANGE: ISS-2025-0122 - Execute atom directives as goals (e.g., :- run_all_tests.)
                executeGoalDirective(directive);
                // END_CHANGE: ISS-2025-0122
            }
        }
    }

    // START_CHANGE: ISS-2025-0730 - multifile/discontiguous declarations: PI, M:PI, sequences, lists
    /** multifile/1 and discontiguous/1 called as goals (in module {@code module}). */
    public void declarePredicates(Term spec, String kind, String module, boolean asGoal) {
        if (spec instanceof Variable) {
            throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError(kind + "/1"));
        }
        declarePredicates(spec, kind, module);
    }

    private void declarePredicates(Term spec, String kind, String module) {
        if (spec instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) spec;
            String f = c.getName();
            int ar = c.getArguments().size();
            if ((",".equals(f) || ".".equals(f)) && ar == 2) {
                declarePredicates(c.getArguments().get(0), kind, module);
                declarePredicates(c.getArguments().get(1), kind, module);
                return;
            }
            if (":".equals(f) && ar == 2 && c.getArguments().get(0) instanceof Atom) {
                declarePredicates(c.getArguments().get(1), kind, ((Atom) c.getArguments().get(0)).getName());
                return;
            }
            if (("/".equals(f) || "//".equals(f)) && ar == 2 && c.getArguments().get(0) instanceof Atom
                    && c.getArguments().get(1) instanceof it.denzosoft.jprolog.core.terms.Number) {
                String name = ((Atom) c.getArguments().get(0)).getName();
                int n = (int) ((it.denzosoft.jprolog.core.terms.Number) c.getArguments().get(1)).longValue()
                    + ("//".equals(f) ? 2 : 0);
                if ("discontiguous".equals(kind)) {
                    LoadContext lc = currentLoad();
                    if (lc != null) lc.discontiguous.add("user".equals(module) ? name + "/" + n : module + ":" + name + "/" + n);
                } else if ("user".equals(module)) {
                    knowledgeBase.markMultifile(name, n);
                } else {
                    ensureModule(module);
                    knowledgeBase.declareQualified(module, name, n);
                }
                return;
            }
        }
        if (spec instanceof Atom && "[]".equals(((Atom) spec).getName())) return;
        throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError(
            "predicate_indicator", spec, kind + "/1"));
    }
    // END_CHANGE: ISS-2025-0730

    // START_CHANGE: ISS-2025-0347 - parse ':- dynamic(PI)' (PI = Name/Arity, a ','-sequence of
    // indicators, or a list) and mark each procedure dynamic in the KnowledgeBase.
    private void processDynamicDirective(Term directive) {
        if (directive instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) directive;
            for (Term arg : c.getArguments()) {
                markDynamicIndicators(arg);
            }
        }
        if (LOGGER.isLoggable(java.util.logging.Level.FINE)) LOGGER.log(Level.FINE, "Dynamic directive processed: " + directive);   // ISS-2025-0550: lazy
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
            // START_CHANGE: ISS-2025-0574 - a directive runs ONCE (ISO 7.4.2), in the module being
            // loaded, and on the machine of the query that called consult/1 when there is one
            // (same inference budget, same cancellation)
            it.denzosoft.jprolog.core.module.Module cm = moduleManager.getCurrentModule();
            Term g = goal;
            if (cm != null && !"user".equals(cm.getName())) {
                g = new CompoundTerm(new Atom(":"), Arrays.asList((Term) new Atom(cm.getName()), g));
            }
            g = new CompoundTerm(new Atom("once"), Collections.singletonList(g));
            LoadContext lc = currentLoad();
            boolean ok;
            if (lc != null && lc.runner != null) {
                ok = Boolean.TRUE.equals(lc.runner.apply(g));
            } else {
                ok = !solve(g).isEmpty();                    // ISS-2025-0484: the selected engine
            }
            List<Map<String, Term>> solutions = ok ? Collections.singletonList(Collections.<String, Term>emptyMap())
                                                   : Collections.<Map<String, Term>>emptyList();
            // END_CHANGE: ISS-2025-0574
            if (solutions.isEmpty()) {
                // START_CHANGE: ISS-2025-0288 - surface a failed directive (was logged only at FINE)
                System.err.println("Warning: goal directive failed: " + goal);
                // END_CHANGE: ISS-2025-0288
                if (LOGGER.isLoggable(java.util.logging.Level.FINE)) LOGGER.log(Level.FINE, "Goal directive failed (no solutions): " + goal);   // ISS-2025-0550: lazy
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
    // START_CHANGE: ISS-2025-0735 - 4.6 wave Q3.4: use_module/2 honours its import list
    private void processUseModuleDirective(Term directive) {
        String mod = useModuleDirective(directive);
        if (mod != null && directive instanceof CompoundTerm && TermUtils.getArity(directive) == 2) {
            applyImportSpec(mod, TermUtils.getArgument((CompoundTerm) directive, 1), "use_module/2");
        }
    }

    /** use_module/1,2 as a goal (the file is loaded once, then imported per the list). */
    public void useModule(Term spec, Term imports, java.util.function.Function<Term, Boolean> runner) {
        State prev = enterState();
        try {
            Term s0 = resolveTermSimple(spec);
            if (s0 instanceof Variable) {
                throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError(
                    imports == null ? "use_module/1" : "use_module/2"));
            }
            List<Term> args = new ArrayList<>();
            args.add(s0);
            if (imports != null) args.add(resolveTermSimple(imports));
            Term d = new CompoundTerm(new Atom("use_module"), args);
            // the goal form shares the directive's logic; its directives run on the caller's machine
            LoadContext lc = newLoadContext(null, runner);
            loadStack().push(lc);
            try { processUseModuleDirective(d); } finally { loadStack().pop(); }
        } finally { exitState(prev); }
    }

    /** Restrict what the current module imports from {@code mod} to {@code list}. */
    private void applyImportSpec(String mod, Term list0, String ctx) {
        Term list = resolveTermSimple(list0);
        if (list instanceof Variable) {
            throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError(ctx));
        }
        it.denzosoft.jprolog.core.module.Module.ImportSpec spec = new it.denzosoft.jprolog.core.module.Module.ImportSpec();
        if (list instanceof CompoundTerm && "except".equals(((CompoundTerm) list).getName())
                && ((CompoundTerm) list).getArguments().size() == 1) {
            for (Term e : listElements(((CompoundTerm) list).getArguments().get(0), ctx)) {
                String[] a = importItem(e, ctx);
                if (a[1] != null) spec.aliases.put(a[1] + "/" + a[2], a[0]);
                spec.except.add(a[0] + "/" + a[2]);
            }
        } else if (list instanceof Atom && "all".equals(((Atom) list).getName())) {
            spec = null;
        } else {
            spec.only = new HashSet<>();
            for (Term e : listElements(list, ctx)) {
                String[] a = importItem(e, ctx);
                if (a[1] != null) spec.aliases.put(a[1] + "/" + a[2], a[0]);
                else spec.only.add(a[0] + "/" + a[2]);
            }
        }
        moduleManager.getCurrentModule().setImportSpec(mod, spec);
        moduleManager.touch();
    }

    private static List<Term> listElements(Term l0, String ctx) {
        List<Term> out = new ArrayList<>();
        Term l = resolveTermSimple(l0);
        while (l instanceof CompoundTerm && ".".equals(((CompoundTerm) l).getName())
                && ((CompoundTerm) l).getArguments().size() == 2) {
            out.add(resolveTermSimple(((CompoundTerm) l).getArguments().get(0)));
            l = resolveTermSimple(((CompoundTerm) l).getArguments().get(1));
        }
        if (l instanceof Variable) {
            throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError(ctx));
        }
        if (!(l instanceof Atom && "[]".equals(((Atom) l).getName()))) {
            throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("list", l0, ctx));
        }
        return out;
    }

    /** {name, alias-or-null, arity} of `N/A`, `N//A` or `PI as Alias`. */
    private static String[] importItem(Term e, String ctx) {
        String alias = null;
        if (e instanceof CompoundTerm && "as".equals(((CompoundTerm) e).getName())
                && ((CompoundTerm) e).getArguments().size() == 2) {
            Term al = resolveTermSimple(((CompoundTerm) e).getArguments().get(1));
            if (!(al instanceof Atom)) {
                throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("atom", al, ctx));
            }
            alias = ((Atom) al).getName();
            e = resolveTermSimple(((CompoundTerm) e).getArguments().get(0));
        }
        if (e instanceof CompoundTerm && ("/".equals(((CompoundTerm) e).getName()) || "//".equals(((CompoundTerm) e).getName()))
                && ((CompoundTerm) e).getArguments().size() == 2) {
            Term n = resolveTermSimple(((CompoundTerm) e).getArguments().get(0));
            Term a = resolveTermSimple(((CompoundTerm) e).getArguments().get(1));
            if (n instanceof Variable || a instanceof Variable) {
                throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError(ctx));
            }
            if (n instanceof Atom && a instanceof it.denzosoft.jprolog.core.terms.Number
                    && ((it.denzosoft.jprolog.core.terms.Number) a).isInteger()) {
                long ar = ((it.denzosoft.jprolog.core.terms.Number) a).longValue()
                    + ("//".equals(((CompoundTerm) e).getName()) ? 2 : 0);
                return new String[] { ((Atom) n).getName(), alias, Long.toString(ar) };
            }
        }
        if (e instanceof Variable) {
            throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError(ctx));
        }
        throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("predicate_indicator", e, ctx));
    }

    /** The existing directive logic; returns the module that was imported (or null). */
    private String useModuleDirective(Term directive) {
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
                        if (LOGGER.isLoggable(Level.FINE)) LOGGER.log(Level.FINE, "Library auto-load (built-in): " + moduleName);   // ISS-2025-0735: was INFO on stderr
                        return null;
                    }
                    // START_CHANGE: ISS-2025-0737 - the prelude modules and the libraries JProlog
                    // implements natively; anything else is searched on file_search_path/2
                    it.denzosoft.jprolog.core.engine.v4.Modules ms4 = getV4Engine().modules4();
                    if (ms4.isModule(moduleName) && moduleManager.getModule(moduleName) == null) {
                        ms4.ensureLoaded(moduleName);
                        return null;
                    }
                    if (BUILTIN_LIBRARIES.contains(moduleName)) return null;
                    // END_CHANGE: ISS-2025-0737
                }
            }
            // START_CHANGE: ISS-2025-0574 - use_module(File): a module FILE is loaded (once) and
            // imported into the loading module
            if ((moduleName == null || moduleManager.getModule(moduleName) == null)
                    && (!safeMode || (safeModeOptions != null && !safeModeOptions.readDirs().isEmpty()))   // ISS-2025-0625
                    ) {                                                            // ISS-2025-0737: library(X) searched too
                LoadContext lc = currentLoad();
                try {
                    java.io.File mf = resolveLoadFile(moduleTerm, lc != null ? lc.directory : engineState.workingDirectory(), "use_module/1");   // ISS-2025-0745
                    checkSafeRead(mf, moduleTerm, "use_module/1");                 // ISS-2025-0625
                    loadFromGoal(moduleTerm, "not_loaded", false, lc != null ? lc.runner : null, "use_module/1");
                    FileRecord fr;
                    synchronized (loadedFiles) { fr = loadedFiles.get(canonical(mf)); }   // ISS-2025-0739
                    return fr != null ? fr.module : null;
                } catch (PrologException notAFile) {
                    if (notAFile.isHalt()) throw notAFile;
                    if (safeMode && notAFile.getErrorTerm() instanceof CompoundTerm
                            && ((CompoundTerm) notAFile.getErrorTerm()).getArguments().size() == 2
                            && ((CompoundTerm) notAFile.getErrorTerm()).getArguments().get(0) instanceof CompoundTerm
                            && "permission_error".equals(((CompoundTerm) ((CompoundTerm) notAFile.getErrorTerm())
                                .getArguments().get(0)).getName())) {
                        throw notAFile;                                        // ISS-2025-0625
                    }
                    // not a file either: fall through to the module-name handling
                }
            }
            // END_CHANGE: ISS-2025-0574
            if (moduleName == null) {
                LOGGER.log(Level.WARNING, "use_module: unrecognized module spec: " + moduleTerm);
                return null;
            }
            try {
                moduleManager.importModule(moduleName);
                if (LOGGER.isLoggable(Level.FINE)) LOGGER.log(Level.FINE, "Module imported: " + moduleName);   // ISS-2025-0735: was INFO on stderr
                return moduleName;
            } catch (IllegalArgumentException e) {
                LOGGER.log(Level.WARNING, "Failed to import module: " + moduleName + " - " + e.getMessage());
            }
            // END_CHANGE: Round5 minor
        }
        return null;
    }
    // END_CHANGE: ISS-2025-0735
    
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
        // START_CHANGE: ISS-2025-0572 - comma lists, lists, `as` options and mode-directed heads;
        // an unsupported form raises (it was a logged no-op and the program then looped)
        if (directive instanceof CompoundTerm && TermUtils.getArity(directive) == 1) {
            // ISS-2025-0753: an accepted-but-unimplemented `as` option is reported, never silent
            for (String w : tableStore.declareSpec(TermUtils.getArgument((CompoundTerm) directive, 0), "table/1")) warn(w);
            return;
        }
        throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.domainError(
            "table_directive", directive, "table/1"));
        // END_CHANGE: ISS-2025-0572
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
        globals().put(name, value);                                // ISS-2025-0633
    }

    // START_CHANGE: ISS-2025-0633 - wave P6.5: global variables are per thread (SWI). A worker
    // machine (core.engine.v4.Workers) installs a store of its own for its thread; every other thread
    // — the one that runs top-level queries, an IDE background solve, an embedder thread — uses the
    // engine's store, so values survive from one top-level query to the next as before.
    private final ThreadLocal<java.util.Map<String, Term>> workerGlobals =
        new ThreadLocal<java.util.Map<String, Term>>();

    private java.util.Map<String, Term> globals() {
        java.util.Map<String, Term> w = workerGlobals.get();
        return (w != null) ? w : globalVariables;
    }

    /** Give the calling (worker) thread an empty global-variable store; returns what to restore. */
    public Object enterWorkerGlobals() {
        java.util.Map<String, Term> prev = workerGlobals.get();
        workerGlobals.set(new java.util.concurrent.ConcurrentHashMap<String, Term>());
        return prev;
    }

    /** Undo {@link #enterWorkerGlobals()}. */
    @SuppressWarnings("unchecked")
    public void exitWorkerGlobals(Object prev) {
        if (prev == null) workerGlobals.remove();
        else workerGlobals.set((java.util.Map<String, Term>) prev);
    }
    // END_CHANGE: ISS-2025-0633

    /**
     * Get a non-backtrackable global variable.
     * @param name The variable name
     * @return The stored value, or null if not set
     */
    public Term nbGetval(String name) {
        return globals().get(name);                                // ISS-2025-0633
    }

    /**
     * Delete a non-backtrackable global variable.
     * @param name The variable name
     */
    public void nbDelete(String name) {
        globals().remove(name);                                    // ISS-2025-0633
    }

    /**
     * Get all current global variables as a snapshot.
     * @return A copy of the global variables map
     */
    public Map<String, Term> nbCurrentAll() {
        return new HashMap<>(globals());                           // ISS-2025-0633
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
            // START_CHANGE: ISS-2025-0566 - the Java clause APIs read with the v2 reader too (the
            // legacy parser only behind -Djprolog.parser=legacy)
            for (Rule rule : parseClauseRules(clauseString)) {
            // END_CHANGE: ISS-2025-0566

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
    
    // START_CHANGE: ISS-2025-0566
    private List<Rule> parseClauseRules(String text) throws PrologParserException {
        if (!USE_V2_PARSER) {
            List<Rule> out = new ArrayList<>();
            for (java.lang.String clause : parser.extractClauses(text)) {
                if (!clause.trim().isEmpty()) out.add(parser.parseRule(clause.trim()));
            }
            return out;
        }
        State prev = enterState();
        try {
            List<Rule> out = new ArrayList<>();
            java.lang.String t = text.trim();
            if (!t.endsWith(".")) t = t + " .";
            for (Term c : it.denzosoft.jprolog.core.parser.v2.TermReader.parseProgram(t, engineState.ops().table())) {
                out.add(clauseTermToRule(c));
            }
            return out;
        } catch (RuntimeException e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);
            if (e instanceof PrologException) throw e;
            throw new PrologParserException(java.lang.String.valueOf(e.getMessage()));
        } finally {
            exitState(prev);
        }
    }
    // END_CHANGE: ISS-2025-0566

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
            List<Rule> rules = parseClauseRules(clauseString);   // ISS-2025-0566
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

    // START_CHANGE: ISS-2025-0671 - a query that does not parse raises the ISO
    // error(syntax_error(Message), query), like every other syntax error in the system; it used to
    // be a bare message ATOM ('Error parsing query: ...'), which catch(G, error(_, _), R) in an
    // embedder's wrapper could not match and which the CLI printed as a quoted atom.
    private static PrologException querySyntaxError(Exception e) {
        String m = e.getMessage() == null ? "syntax error" : e.getMessage().replaceAll(" at line \\d+$", "");
        return new PrologException(
            it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.syntaxError(m, "query"));
    }
    // END_CHANGE: ISS-2025-0671

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
                    throw querySyntaxError(e);   // ISS-2025-0671
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
            throw querySyntaxError(e);   // ISS-2025-0671
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
            throw querySyntaxError(e);   // ISS-2025-0671
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
            appendListing(sb, rules, null, -1);
        }
        
        return sb.toString();
    }

    // START_CHANGE: ISS-2025-0570 - 4.5 wave P3.7: listing is portray_clause/1 per clause (quoted,
    // operators, A/B variable names, `_` singletons, SWI body layout), grouped by predicate with
    // a `:- dynamic Name/Arity.` header for a dynamic one and a blank line after each predicate —
    // output that consults back to the same clauses (it printed Rule.toString(): `;(,(>(...`,
    // `lst(A b, it's).`, `_G27`).
    private void appendListing(StringBuilder sb, List<Rule> rules, String functor, int arity) {
        Map<String, List<Rule>> byPred = new LinkedHashMap<>();
        for (Rule r : rules) {
            Term h = r.getHead();
            String f = TermUtils.getFunctorName(h);
            int n = TermUtils.getArity(h);
            if (functor != null && (!functor.equals(f) || n != arity)) continue;
            byPred.computeIfAbsent(f + "/" + n, k -> new ArrayList<>()).add(r);
        }
        if (functor != null && byPred.isEmpty() && knowledgeBase.isDynamic(functor, arity)) {
            byPred.put(functor + "/" + arity, new ArrayList<>());
        }
        it.denzosoft.jprolog.core.engine.v4.Writer.Options o = new it.denzosoft.jprolog.core.engine.v4.Writer.Options();
        o.ops = engineState.ops().table();
        for (Map.Entry<String, List<Rule>> e : byPred.entrySet()) {
            String key = e.getKey();
            int slash = key.lastIndexOf('/');
            String f = key.substring(0, slash);
            int n = Integer.parseInt(key.substring(slash + 1));
            if (knowledgeBase.isDynamic(f, n)) {
                sb.append(":- dynamic ").append(
                    it.denzosoft.jprolog.core.engine.v4.Writer.format(
                        it.denzosoft.jprolog.core.engine.v4.Errors.pi(f, n),
                        it.denzosoft.jprolog.core.engine.v4.Writer.Options.writeq()))
                  .append(".\n\n");
            }
            for (Rule r : e.getValue()) {
                sb.append(it.denzosoft.jprolog.core.engine.v4.Writer.portrayClause(ruleTerm(r), o));
            }
            sb.append("\n");
        }
    }

    /** The clause term of a stored rule: Head, or Head :- (G1, ..., Gn). */
    private static Term ruleTerm(Rule r) {
        List<Term> body = r.getBody();
        if (body == null || body.isEmpty()) return r.getHead();
        Term conj = body.get(body.size() - 1);
        for (int i = body.size() - 2; i >= 0; i--) {
            conj = new CompoundTerm(new Atom(","), Arrays.asList(body.get(i), conj));
        }
        return new CompoundTerm(new Atom(":-"), Arrays.asList(r.getHead(), conj));
    }
    // END_CHANGE: ISS-2025-0570
    
    /**
     * Get listing output for specific predicate as string without printing.
     */
    public String getListingOutput(String predicateIndicator) {
        StringBuilder sb = new StringBuilder();
        List<Rule> rules = knowledgeBase.getRules();
        
        // Parse predicate indicator (e.g. "parent/2")
        int slashAt = predicateIndicator.lastIndexOf('/');
        if (slashAt <= 0) {
            sb.append("% Invalid predicate indicator: " + predicateIndicator + "\n");
            return sb.toString();
        }
        
        String functor = predicateIndicator.substring(0, slashAt);
        int arity;
        try {
            arity = Integer.parseInt(predicateIndicator.substring(slashAt + 1));
        } catch (NumberFormatException e) {
            sb.append("% Invalid arity in predicate indicator: " + predicateIndicator + "\n");
            return sb.toString();
        }
        
        int before = sb.length();
        appendListing(sb, rules, functor, arity);   // ISS-2025-0570
        if (sb.length() == before) {
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

    // START_CHANGE: ISS-2025-0625 - wave P6.1: the deny list by NAME, applied to the legacy registry
    // AND to the native v4 table (enableSafeMode used to walk the registry only). Every entry is a
    // predicate that reaches the host outside the denied packages: open/3,4 (builtin.io), the CSV
    // file predicates (builtin.csv) and log_to_file/1 (builtin.logging). The rest are defensive:
    // were any of them ever made native or moved, safe mode would still strip them.
    private static final String[] UNSAFE_PREDICATE_NAMES = {
        "open", "see", "seen", "tell", "told", "csv_read_file", "csv_write_file", "log_to_file",
        "consult", "ensure_loaded", "load_files", "make", "use_module", "absolute_file_name", "exists_file",
        "exists_directory", "delete_file", "shell", "getenv", "setenv"
    };
    /** Safe-mode options in force (null when safe mode is off). */
    private SafeModeOptions safeModeOptions;
    // END_CHANGE: ISS-2025-0625

    /**
     * Remove all host-touching built-ins (OS shell, Java FFI, filesystem, network, HTTP, JDBC,
     * persistence, threads, file streams, CSV files, the log file) from THIS engine — both the
     * legacy registry and the native table — so a subsequently consulted/queried (untrusted) program
     * cannot execute processes, reflect into the JVM, or read/write files, sockets or databases.
     * Irreversible for this instance. Returns the number of predicates removed. Use a fresh
     * {@link Prolog} per security domain. NOTE: this is a deny-list sandbox, not a full resource
     * sandbox — combine with an inference budget ({@link #setInferenceBudget(long)}) and a query
     * timeout. (ISS-2025-0338, ISS-2025-0625)
     */
    public int enableSafeMode() {
        return enableSafeMode(new SafeModeOptions());
    }

    // START_CHANGE: ISS-2025-0625 - wave P6.1
    /**
     * Safe mode with options: {@link SafeModeOptions#allowFileRead(String)} keeps {@code open/3,4}
     * (read mode only) and the loaders, restricted to the whitelisted directories.
     */
    public int enableSafeMode(SafeModeOptions options) {
        final SafeModeOptions opts = (options == null) ? new SafeModeOptions() : options;
        final boolean reads = !opts.readDirs().isEmpty();
        java.util.Set<String> loaders = new java.util.HashSet<String>(java.util.Arrays.asList(
            "consult", "ensure_loaded", "load_files", ".", "use_module"));   // ISS-2025-0735
        java.util.Set<String> deniedNames = new java.util.HashSet<String>(java.util.Arrays.asList(UNSAFE_PREDICATE_NAMES));
        int removed = 0;
        for (String name : new java.util.ArrayList<String>(builtInRegistry.getBuiltInNames())) {
            BuiltIn b = builtInRegistry.getBuiltIn(name);
            if (b == null) continue;
            String cls = b.getClass().getName();
            boolean deny = deniedNames.contains(name);
            for (String pkg : UNSAFE_BUILTIN_PACKAGES) {
                if (cls.contains(pkg)) { deny = true; break; }
            }
            if (!deny) continue;
            if (reads && loaders.contains(name)) continue;           // checked per file in loadSpec
            if (reads && "open".equals(name)) {
                builtInRegistry.registerBuiltIn(name, new SafeOpen(b, opts));
                continue;
            }
            builtInRegistry.unregisterBuiltIn(name);
            removed++;
        }
        it.denzosoft.jprolog.core.engine.v4.BuiltinTable natives = getV4Engine().natives();
        for (String key : natives.keys()) {
            int slash = key.lastIndexOf('/');
            if (deniedNames.contains(key.substring(0, slash))) {
                natives.unregister(key.substring(0, slash), Integer.parseInt(key.substring(slash + 1)));
                removed++;
            }
        }
        // START_CHANGE: ISS-2025-0672 - halt/0,1 are sandboxed unless the options allow them
        if (!opts.haltAllowed()) {
            for (int arity = 0; arity <= 1; arity++) {
                final int n = arity;
                natives.register("halt", n, (m, args) -> {
                    Term goal = (n == 0) ? new Atom("halt")
                        : new it.denzosoft.jprolog.core.terms.CompoundTerm(new Atom("halt"),
                              new Term[] {m.deref(args[0])});
                    throw it.denzosoft.jprolog.core.engine.v4.Errors.permission(
                        "call", "sandboxed", goal, "halt/" + n);
                });
            }
        }
        // END_CHANGE: ISS-2025-0672
        safeModeOptions = opts;
        safeMode = true;
        return removed;
    }

    /**
     * In safe mode, may the loaders and open/3,4 read {@code f}? Raises
     * {@code permission_error(open, source_sink, Spec)} when they may not; a no-op outside safe mode.
     */
    void checkSafeRead(java.io.File f, Term spec, String ctx) {
        if (!safeMode) return;
        if (safeModeOptions == null || !safeModeOptions.allowsRead(f)) {
            throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.permissionError(
                "open", "source_sink", spec, ctx));
        }
    }

    /** open/3,4 in safe mode with whitelisted directories: read mode only, inside the whitelist. */
    private static final class SafeOpen implements BuiltInWithContext {
        private final BuiltIn inner;
        private final SafeModeOptions opts;

        SafeOpen(BuiltIn inner, SafeModeOptions opts) { this.inner = inner; this.opts = opts; }

        private void check(Term query, Map<String, Term> bindings) {
            List<Term> a = query.getArguments();
            Term file = a.get(0).resolveBindings(bindings);
            Term mode = a.size() > 1 ? a.get(1).resolveBindings(bindings) : null;
            String path = (file instanceof Atom) ? ((Atom) file).getName()
                : (file instanceof it.denzosoft.jprolog.core.terms.PrologString)
                    ? ((it.denzosoft.jprolog.core.terms.PrologString) file).getStringValue() : null;
            boolean readMode = mode instanceof Atom && "read".equals(((Atom) mode).getName());
            if (path == null || !readMode || !opts.allowsRead(it.denzosoft.jprolog.core.engine.v4.EngineState.file(path))) {   // ISS-2025-0745
                throw new PrologException(it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.permissionError(
                    "open", "source_sink", file, "open/" + a.size()));
            }
        }

        @Override
        public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
            check(query, bindings);
            return inner.execute(query, bindings, solutions);
        }

        @Override
        public boolean executeWithContext(SolverContext solver, Term query, Map<String, Term> bindings,
                                          List<Map<String, Term>> solutions) {
            check(query, bindings);
            if (inner instanceof BuiltInWithContext) {
                return ((BuiltInWithContext) inner).executeWithContext(solver, query, bindings, solutions);
            }
            return inner.execute(query, bindings, solutions);
        }
    }
    // END_CHANGE: ISS-2025-0625

    public boolean isSafeMode() { return safeMode; }

    // ISS-2025-0339: per-query inference (step) budget for the default v2 engine; 0 = unlimited.
    private long inferenceBudget = 0;
    /** Abort any subsequent query by throwing {@link InferenceLimitException} after this many
     *  resolution steps. Deliberately NOT a PrologException, so an untrusted {@code catch/3}
     *  cannot trap it — the Java embedder must catch it. Bounds CPU on untrusted/runaway
     *  queries. 0 disables it. ISS-2025-0624 (4.5 wave P6.3): ONE budget per query — the worker
     *  machines of thread_create/2,3 and the concurrent_* family draw from it too — and natives
     *  that walk or build long lists charge it per element.
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
        // START_CHANGE: ISS-2025-0574 - the same loader core as consult/1 (module scope, include)
        LoadContext ctx = newLoadContext(null, null);
        ctx.diagnosticsName = filename;
        LoadResult r = loadText(program, ctx, true);
        List<CompilationError> errors = new ArrayList<>();
        for (LoadError e : r.errors) errors.add(new CompilationError(filename, e.line, e.message, "error"));
        return new CompilationResult(errors.isEmpty(), errors, r.clauses);
        // END_CHANGE: ISS-2025-0574
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
        // START_CHANGE: ISS-2025-0577 - 4.5 wave P3.6: the .jpc compiler reads with the v2 parser
        // (the consult reader: `a ===> b`, 'a''b', backquotes, {} all compile now) and records
        // EVERY clause as read — directives included, DCG rules untranslated — so loading the
        // .jpc runs them through the same clause handler as consult (dynamic, table, module,
        // initialization, op). Only op/3 is executed at compile time, because later clauses
        // need the operator to parse. (It used to run every directive at compile time and to
        // load DCG rules untranslated.)
        State prev = enterState();
        try {
            List<Rule> rules = new ArrayList<>();
            it.denzosoft.jprolog.core.parser.v2.TermReader reader =
                new it.denzosoft.jprolog.core.parser.v2.TermReader(
                    it.denzosoft.jprolog.core.parser.v2.Lexer.tokenize(source), engineState.ops().table());
            for (;;) {
                int line = reader.peekLine();
                Term t;
                try {
                    t = reader.nextClause();
                } catch (RuntimeException e) {
                    it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);
                    throw new java.io.IOException("Parse error during compilation at line " + line + ": " + messageOf(e), e);
                }
                if (t == null) break;
                Rule rule = clauseTermToRule(t);
                rule.setSourceLine(line);
                if (isDirective(rule)) {
                    Term d = TermUtils.getArgument((CompoundTerm) rule.getHead(), 0);
                    if (d instanceof CompoundTerm && "op".equals(((CompoundTerm) d).getName())
                            && ((CompoundTerm) d).getArguments().size() == 3) {
                        processOpDirective(d);
                    }
                }
                rules.add(rule);
            }
            long hash = it.denzosoft.jprolog.core.compiled.JpcWriter.computeSourceHash(source);
            new it.denzosoft.jprolog.core.compiled.JpcWriter()
                .write(rules, operatorTable, hash, out);
        } finally {
            exitState(prev);
        }
        // END_CHANGE: ISS-2025-0577
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
        throwIfErrors(loadCompiled(program, null));
    }

    // START_CHANGE: ISS-2025-0577 - a compiled program is one LOAD, through the consult handler
    private LoadResult loadCompiled(it.denzosoft.jprolog.core.compiled.JpcReader.CompiledProgram program, String file) {
        State prev = enterState();
        try {
            for (it.denzosoft.jprolog.core.operator.Operator op : program.operators) {
                operatorTable.defineOperator(op.getPrecedence(), op.getType(), op.getName());
            }
            LoadContext ctx = newLoadContext(file, null);
            beginLoad(ctx);                                                  // ISS-2025-0739
            {
                try {
                    for (Rule rule : program.rules) {
                        if (!!knowledgeBase.hasRules("term_expansion/2")) {
                            // the clause as read, so term_expansion/2 sees what consult shows it
                            boolean raw = rule.getBody() == null || rule.getBody().isEmpty();
                            handleClause(raw ? rule.getHead() : ruleTerm(rule), rule.getSourceLine(), ctx);
                        } else {
                            handleRule(rule, null, rule.getSourceLine(), ctx);
                        }
                    }
                    runInitGoals(ctx);
                } finally {
                    finishLoad(ctx);
                }
            }
            return new LoadResult(file, ctx.clauses[0], ctx.errors, ctx.modules.isEmpty() ? null : ctx.modules.get(0));
        } finally {
            exitState(prev);
        }
    }
    // END_CHANGE: ISS-2025-0577

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

        LoadResult compiled = null;   // ISS-2025-0577: clause errors are reported, never a fallback
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
                    // Hash matches — load compiled (ISS-2025-0577: through the consult handler)
                    compiled = loadCompiled(program, canonical(src));
                    if (LOGGER.isLoggable(Level.FINE)) LOGGER.log(Level.FINE, "Loaded compiled: " + jpcFile);
                }
            } catch (Exception e) {
                it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
                LOGGER.log(Level.WARNING, "Failed to load compiled file, falling back to source: " + e.getMessage());
            }
        }

        if (compiled != null) { throwIfErrors(compiled); return; }
        // Fall back to source consult + compile for next time
        consultFile(src.getPath());   // ISS-2025-0577: a file load (load context, reconsult)
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
