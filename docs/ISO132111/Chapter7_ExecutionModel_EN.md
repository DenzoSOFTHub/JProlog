# Chapter 7: Execution Model - JProlog Runtime System

## Overview

This chapter provides a comprehensive analysis of JProlog's execution model, covering the complete lifecycle from program loading to goal execution, including database operations, control constructs, I/O handling, and error management. It demonstrates how JProlog implements the ISO Prolog execution semantics with practical examples and detailed implementation insights.

---

## 7.4.2 Directives

### Definition and Purpose

Directives are instructions processed at load time that configure the Prolog system, declare predicate properties, or perform initialization tasks. JProlog implements a comprehensive directive system that supports both ISO standard and extension directives.

### Implementation Architecture

```java
public class DirectiveProcessor {
    private final Map<String, DirectiveHandler> handlers;
    private final DirectiveContext context;
    private final List<InitializationDirective> initializations;
    
    public DirectiveProcessor(PrologEngine engine) {
        this.handlers = new HashMap<>();
        this.context = new DirectiveContext(engine);
        this.initializations = new ArrayList<>();
        registerStandardDirectives();
    }
    
    private void registerStandardDirectives() {
        // Core ISO directives
        registerHandler("dynamic", new DynamicDirectiveHandler());
        registerHandler("multifile", new MultifileDirectiveHandler());
        registerHandler("discontiguous", new DiscontiguousDirectiveHandler());
        registerHandler("op", new OperatorDirectiveHandler());
        registerHandler("char_conversion", new CharConversionHandler());
        registerHandler("set_prolog_flag", new FlagDirectiveHandler());
        registerHandler("initialization", new InitializationHandler());
        
        // JProlog extensions
        registerHandler("module", new ModuleDirectiveHandler());
        registerHandler("use_module", new UseModuleHandler());
        registerHandler("encoding", new EncodingDirectiveHandler());
        registerHandler("meta_predicate", new MetaPredicateHandler());
    }
    
    public void processDirective(Directive directive) {
        String name = directive.getName();
        DirectiveHandler handler = handlers.get(name);
        
        if (handler == null) {
            // Check if it's a goal directive (:- Goal.)
            if (directive.isGoalDirective()) {
                processGoalDirective(directive);
                return;
            }
            throw new UnknownDirectiveException(
                "Unknown directive: " + name);
        }
        
        // Validate directive context
        validateDirectiveContext(directive, handler);
        
        // Process the directive
        try {
            handler.process(directive, context);
            logDirectiveProcessing(directive, true);
        } catch (DirectiveException e) {
            logDirectiveProcessing(directive, false);
            throw e;
        }
    }
}
```

### Dynamic Directive Handler

```java
public class DynamicDirectiveHandler implements DirectiveHandler {
    @Override
    public void process(Directive directive, DirectiveContext context) {
        for (Term indicator : directive.getArguments()) {
            PredicateIndicator pi = parsePredicateIndicator(indicator);
            
            // Mark predicate as dynamic
            context.getKnowledgeBase().markDynamic(pi);
            
            // Create dynamic predicate infrastructure
            setupDynamicPredicate(pi, context);
            
            // Register with database manager
            context.getDatabaseManager().registerDynamic(pi);
        }
    }
    
    private void setupDynamicPredicate(PredicateIndicator pi, 
            DirectiveContext context) {
        // Create indexing structures
        IndexManager indexManager = context.getIndexManager();
        indexManager.createIndex(pi, IndexType.FIRST_ARGUMENT);
        indexManager.createIndex(pi, IndexType.HASH_TABLE);
        
        // Setup assertion tracking
        AssertionTracker tracker = new AssertionTracker(pi);
        context.getAssertionManager().registerTracker(tracker);
        
        // Create specialized assert/retract methods
        generateDynamicMethods(pi, context);
    }
    
    private void generateDynamicMethods(PredicateIndicator pi,
            DirectiveContext context) {
        // Generate optimized assert method
        String assertMethod = String.format(
            "public void assert_%s_%d(Term[] args) {\n" +
            "    Clause clause = new Clause(new CompoundTerm(\"%s\", args));\n" +
            "    indexedInsert(clause);\n" +
            "    notifyAssertion(clause);\n" +
            "}",
            pi.getName(), pi.getArity(), pi.getName()
        );
        
        // Compile and register method
        DynamicMethodCompiler compiler = context.getMethodCompiler();
        compiler.compileAndRegister(pi, assertMethod);
    }
}
```

### Operator Definition Directive

```java
public class OperatorDirectiveHandler implements DirectiveHandler {
    @Override
    public void process(Directive directive, DirectiveContext context) {
        List<Term> args = directive.getArguments();
        if (args.size() != 3) {
            throw new DirectiveArgumentException(
                "op/3 directive requires exactly 3 arguments");
        }
        
        // Parse operator definition
        int precedence = parseInteger(args.get(0));
        String associativity = parseAtom(args.get(1));
        String symbol = parseAtom(args.get(2));
        
        // Validate operator definition
        validateOperator(precedence, associativity, symbol);
        
        // Register operator
        OperatorRegistry registry = context.getOperatorRegistry();
        registry.defineOperator(precedence, associativity, symbol);
        
        // Update parser tables
        context.getParser().updateOperatorTables();
        
        // Log operator definition
        context.log(String.format("Defined operator %s(%d, %s, %s)",
            symbol, precedence, associativity, symbol));
    }
    
    private void validateOperator(int precedence, String associativity,
            String symbol) {
        // Check precedence range
        if (precedence < 1 || precedence > 1200) {
            throw new OperatorDefinitionException(
                "Operator precedence must be between 1 and 1200");
        }
        
        // Validate associativity
        Set<String> validAssoc = Set.of(
            "fx", "fy", "xf", "yf", "xfx", "xfy", "yfx", "yfy"
        );
        if (!validAssoc.contains(associativity)) {
            throw new OperatorDefinitionException(
                "Invalid associativity: " + associativity);
        }
        
        // Check symbol validity
        if (symbol.isEmpty() || symbol.contains(" ")) {
            throw new OperatorDefinitionException(
                "Invalid operator symbol: " + symbol);
        }
    }
}
```

### Initialization Directive

```java
public class InitializationHandler implements DirectiveHandler {
    @Override
    public void process(Directive directive, DirectiveContext context) {
        List<Term> args = directive.getArguments();
        if (args.size() != 1) {
            throw new DirectiveArgumentException(
                "initialization/1 directive requires exactly 1 argument");
        }
        
        Term goal = args.get(0);
        
        // Create initialization task
        InitializationTask task = new InitializationTask(goal, context);
        
        // Add to initialization queue
        context.getInitializationQueue().add(task);
        
        // Mark for execution after loading
        context.markForInitialization(task);
    }
}

public class InitializationTask {
    private final Term goal;
    private final DirectiveContext context;
    private final long priority;
    
    public InitializationTask(Term goal, DirectiveContext context) {
        this.goal = goal;
        this.context = context;
        this.priority = System.currentTimeMillis();
    }
    
    public void execute() {
        try {
            QuerySolver solver = context.getQuerySolver();
            List<Substitution> solutions = solver.solve(goal);
            
            if (solutions.isEmpty()) {
                context.log("WARNING: Initialization goal failed: " + goal);
            } else {
                context.log("Initialization goal succeeded: " + goal);
            }
        } catch (Exception e) {
            context.log("ERROR: Initialization goal threw exception: " + 
                e.getMessage());
            throw new InitializationException(
                "Initialization failed for goal: " + goal, e);
        }
    }
}
```

---

## 7.4.3 Clauses

### Clause Structure and Types

JProlog implements a sophisticated clause management system that handles facts, rules, and their internal representations efficiently.

### Clause Implementation

```java
public abstract class Clause {
    protected final Term head;
    protected final UUID id;
    protected final long timestamp;
    protected volatile boolean active;
    
    public Clause(Term head) {
        this.head = head;
        this.id = UUID.randomUUID();
        this.timestamp = System.currentTimeMillis();
        this.active = true;
    }
    
    public abstract boolean isFact();
    public abstract boolean isRule();
    public abstract Term getBody();
    public abstract List<Variable> getVariables();
    public abstract Clause rename(VariableRenamer renamer);
    
    // Clause indexing support
    public abstract IndexKey getIndexKey();
    public abstract Set<IndexKey> getAllIndexKeys();
}

public class Fact extends Clause {
    public Fact(Term head) {
        super(head);
        if (head.containsVariables()) {
            throw new IllegalArgumentException(
                "Facts cannot contain variables: " + head);
        }
    }
    
    @Override
    public boolean isFact() { return true; }
    
    @Override
    public boolean isRule() { return false; }
    
    @Override
    public Term getBody() { return Atom.TRUE; }
    
    @Override
    public List<Variable> getVariables() { 
        return Collections.emptyList(); 
    }
    
    @Override
    public Clause rename(VariableRenamer renamer) {
        // Facts don't need renaming
        return this;
    }
    
    @Override
    public IndexKey getIndexKey() {
        if (head instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) head;
            return new IndexKey(compound.getName(), 
                compound.getFirstArgument());
        }
        return new IndexKey(head.toString(), null);
    }
}

public class Rule extends Clause {
    private final Term body;
    private final List<Variable> variables;
    
    public Rule(Term head, Term body) {
        super(head);
        this.body = body;
        this.variables = extractVariables(head, body);
    }
    
    @Override
    public boolean isFact() { return false; }
    
    @Override
    public boolean isRule() { return true; }
    
    @Override
    public Term getBody() { return body; }
    
    @Override
    public List<Variable> getVariables() { 
        return Collections.unmodifiableList(variables); 
    }
    
    @Override
    public Clause rename(VariableRenamer renamer) {
        Term renamedHead = renamer.rename(head);
        Term renamedBody = renamer.rename(body);
        return new Rule(renamedHead, renamedBody);
    }
    
    private List<Variable> extractVariables(Term head, Term body) {
        Set<Variable> varSet = new LinkedHashSet<>();
        collectVariables(head, varSet);
        collectVariables(body, varSet);
        return new ArrayList<>(varSet);
    }
}
```

### Clause Storage and Retrieval

```java
public class ClauseDatabase {
    private final Map<PredicateIndicator, ClauseCollection> clauses;
    private final IndexManager indexManager;
    private final ClauseValidator validator;
    
    public ClauseDatabase() {
        this.clauses = new ConcurrentHashMap<>();
        this.indexManager = new IndexManager();
        this.validator = new ClauseValidator();
    }
    
    public void addClause(Clause clause) {
        // Validate clause
        validator.validate(clause);
        
        // Get or create predicate collection
        PredicateIndicator pi = getPredicateIndicator(clause.getHead());
        ClauseCollection collection = clauses.computeIfAbsent(pi,
            k -> new ClauseCollection(k, indexManager));
        
        // Add clause to collection
        collection.addClause(clause);
        
        // Update indexes
        indexManager.indexClause(clause, pi);
        
        // Notify listeners
        notifyClauseAdded(clause, pi);
    }
    
    public List<Clause> getClauses(Term goal) {
        PredicateIndicator pi = getPredicateIndicator(goal);
        ClauseCollection collection = clauses.get(pi);
        
        if (collection == null) {
            return Collections.emptyList();
        }
        
        // Use indexing for efficient retrieval
        if (indexManager.hasIndex(pi, IndexType.FIRST_ARGUMENT)) {
            return collection.getIndexedClauses(goal);
        } else {
            return collection.getAllClauses();
        }
    }
    
    public boolean removeClause(Clause clause) {
        PredicateIndicator pi = getPredicateIndicator(clause.getHead());
        ClauseCollection collection = clauses.get(pi);
        
        if (collection != null) {
            boolean removed = collection.removeClause(clause);
            if (removed) {
                indexManager.removeFromIndex(clause, pi);
                notifyClauseRemoved(clause, pi);
                return true;
            }
        }
        return false;
    }
}

public class ClauseCollection {
    private final PredicateIndicator predicate;
    private final List<Clause> allClauses;
    private final Map<IndexKey, List<Clause>> indexedClauses;
    private final IndexManager indexManager;
    private final ReadWriteLock lock;
    
    public ClauseCollection(PredicateIndicator predicate, 
            IndexManager indexManager) {
        this.predicate = predicate;
        this.allClauses = new ArrayList<>();
        this.indexedClauses = new HashMap<>();
        this.indexManager = indexManager;
        this.lock = new ReentrantReadWriteLock();
    }
    
    public void addClause(Clause clause) {
        lock.writeLock().lock();
        try {
            allClauses.add(clause);
            
            // Add to indexes
            for (IndexKey key : clause.getAllIndexKeys()) {
                indexedClauses.computeIfAbsent(key, 
                    k -> new ArrayList<>()).add(clause);
            }
        } finally {
            lock.writeLock().unlock();
        }
    }
    
    public List<Clause> getIndexedClauses(Term goal) {
        lock.readLock().lock();
        try {
            IndexKey key = createIndexKey(goal);
            List<Clause> result = indexedClauses.get(key);
            return result != null ? new ArrayList<>(result) : 
                Collections.emptyList();
        } finally {
            lock.readLock().unlock();
        }
    }
}
```

---

## 7.5 Database

### 7.5.1 Preparing a Prolog Text for Execution

JProlog implements a comprehensive compilation pipeline that transforms Prolog text into executable form with optimizations and validation.

### Compilation Pipeline

```java
public class PrologCompiler {
    private final LexicalAnalyzer lexer;
    private final SyntaxAnalyzer parser;
    private final SemanticAnalyzer semanticAnalyzer;
    private final Optimizer optimizer;
    private final CodeGenerator codeGenerator;
    
    public CompilationResult compile(String prologText, 
            CompilationOptions options) {
        CompilationContext context = new CompilationContext(options);
        
        try {
            // Stage 1: Lexical Analysis
            List<Token> tokens = lexer.tokenize(prologText);
            context.setTokens(tokens);
            
            // Stage 2: Syntax Analysis
            AST ast = parser.parse(tokens);
            context.setAST(ast);
            
            // Stage 3: Semantic Analysis
            SemanticInfo semanticInfo = semanticAnalyzer.analyze(ast);
            context.setSemanticInfo(semanticInfo);
            
            // Stage 4: Optimization
            OptimizedAST optimizedAST = optimizer.optimize(ast, semanticInfo);
            context.setOptimizedAST(optimizedAST);
            
            // Stage 5: Code Generation
            ExecutableCode code = codeGenerator.generate(optimizedAST);
            context.setExecutableCode(code);
            
            // Stage 6: Database Loading
            loadIntoDatabase(code, context);
            
            return new CompilationResult(true, context);
            
        } catch (CompilationException e) {
            return new CompilationResult(false, e, context);
        }
    }
    
    private void loadIntoDatabase(ExecutableCode code, 
            CompilationContext context) {
        DatabaseLoader loader = new DatabaseLoader(context.getDatabase());
        
        // Load clauses
        for (CompiledClause clause : code.getClauses()) {
            loader.loadClause(clause);
        }
        
        // Process directives
        for (CompiledDirective directive : code.getDirectives()) {
            loader.processDirective(directive);
        }
        
        // Execute initialization goals
        for (InitializationGoal goal : code.getInitializations()) {
            loader.executeInitialization(goal);
        }
    }
}
```

### Pre-compilation Analysis

```java
public class PrecompilationAnalyzer {
    public PrecompilationInfo analyze(AST ast) {
        PrecompilationInfo info = new PrecompilationInfo();
        
        // Analyze predicate dependencies
        analyzeDependencies(ast, info);
        
        // Detect recursive predicates
        analyzeRecursion(ast, info);
        
        // Analyze clause patterns for optimization
        analyzeClausePatterns(ast, info);
        
        // Check for dynamic predicates
        analyzeDynamicPredicates(ast, info);
        
        // Analyze cut usage
        analyzeCutUsage(ast, info);
        
        return info;
    }
    
    private void analyzeDependencies(AST ast, PrecompilationInfo info) {
        DependencyGraph graph = new DependencyGraph();
        
        for (Clause clause : ast.getClauses()) {
            PredicateIndicator head = getPredicateIndicator(clause.getHead());
            
            if (clause.isRule()) {
                Set<PredicateIndicator> bodyPredicates = 
                    extractPredicates(clause.getBody());
                
                for (PredicateIndicator bodyPred : bodyPredicates) {
                    graph.addDependency(head, bodyPred);
                }
            }
        }
        
        info.setDependencyGraph(graph);
        
        // Detect strongly connected components (mutual recursion)
        List<Set<PredicateIndicator>> sccs = graph.getStronglyConnectedComponents();
        info.setMutuallyRecursiveGroups(sccs);
    }
    
    private void analyzeClausePatterns(AST ast, PrecompilationInfo info) {
        Map<PredicateIndicator, ClausePattern> patterns = new HashMap<>();
        
        for (Clause clause : ast.getClauses()) {
            PredicateIndicator pi = getPredicateIndicator(clause.getHead());
            ClausePattern pattern = patterns.computeIfAbsent(pi, 
                k -> new ClausePattern(k));
            
            pattern.addClause(clause);
        }
        
        // Analyze patterns for optimization opportunities
        for (ClausePattern pattern : patterns.values()) {
            analyzePattern(pattern, info);
        }
        
        info.setClausePatterns(patterns);
    }
    
    private void analyzePattern(ClausePattern pattern, PrecompilationInfo info) {
        // Check for deterministic predicates
        if (pattern.isDeterministic()) {
            info.addDeterministicPredicate(pattern.getPredicate());
        }
        
        // Check for indexable predicates
        if (pattern.isIndexable()) {
            info.addIndexablePredicate(pattern.getPredicate(), 
                pattern.getIndexableArguments());
        }
        
        // Check for tail-recursive predicates
        if (pattern.isTailRecursive()) {
            info.addTailRecursivePredicate(pattern.getPredicate());
        }
    }
}
```

### 7.5.2 Static and Dynamic Procedures

JProlog distinguishes between static and dynamic procedures with different storage and access mechanisms.

### Dynamic Procedure Management

```java
public class DynamicProcedureManager {
    private final Map<PredicateIndicator, DynamicPredicate> dynamicPredicates;
    private final Map<PredicateIndicator, StaticPredicate> staticPredicates;
    private final ChangeTracker changeTracker;
    
    public DynamicProcedureManager() {
        this.dynamicPredicates = new ConcurrentHashMap<>();
        this.staticPredicates = new HashMap<>();
        this.changeTracker = new ChangeTracker();
    }
    
    public void declareDynamic(PredicateIndicator pi) {
        if (staticPredicates.containsKey(pi)) {
            throw new ProcedureException(
                "Cannot redeclare static predicate as dynamic: " + pi);
        }
        
        DynamicPredicate dynPred = new DynamicPredicate(pi);
        dynamicPredicates.put(pi, dynPred);
        
        // Setup indexing for dynamic predicate
        setupDynamicIndexing(dynPred);
        
        // Enable change tracking
        changeTracker.trackPredicate(pi);
    }
    
    public void assertClause(Clause clause, AssertPosition position) {
        PredicateIndicator pi = getPredicateIndicator(clause.getHead());
        DynamicPredicate dynPred = dynamicPredicates.get(pi);
        
        if (dynPred == null) {
            throw new ProcedureException(
                "Cannot assert to non-dynamic predicate: " + pi);
        }
        
        // Record change for backtracking
        Change change = new AssertChange(clause, position);
        changeTracker.recordChange(pi, change);
        
        // Add clause to predicate
        dynPred.addClause(clause, position);
        
        // Update indexes
        dynPred.updateIndexes(clause, IndexOperation.ADD);
        
        // Notify listeners
        notifyAssertion(pi, clause);
    }
    
    public List<Clause> retractClauses(Term pattern) {
        PredicateIndicator pi = getPredicateIndicator(pattern);
        DynamicPredicate dynPred = dynamicPredicates.get(pi);
        
        if (dynPred == null) {
            throw new ProcedureException(
                "Cannot retract from non-dynamic predicate: " + pi);
        }
        
        List<Clause> retracted = dynPred.retractClauses(pattern);
        
        // Record changes
        for (Clause clause : retracted) {
            Change change = new RetractChange(clause);
            changeTracker.recordChange(pi, change);
        }
        
        // Update indexes
        for (Clause clause : retracted) {
            dynPred.updateIndexes(clause, IndexOperation.REMOVE);
        }
        
        // Notify listeners
        for (Clause clause : retracted) {
            notifyRetraction(pi, clause);
        }
        
        return retracted;
    }
}

public class DynamicPredicate {
    private final PredicateIndicator indicator;
    private final List<Clause> clauses;
    private final Map<IndexType, PredicateIndex> indexes;
    private final ReadWriteLock lock;
    
    public DynamicPredicate(PredicateIndicator indicator) {
        this.indicator = indicator;
        this.clauses = new ArrayList<>();
        this.indexes = new HashMap<>();
        this.lock = new ReentrantReadWriteLock();
    }
    
    public void addClause(Clause clause, AssertPosition position) {
        lock.writeLock().lock();
        try {
            switch (position) {
                case FIRST:
                    clauses.add(0, clause);
                    break;
                case LAST:
                    clauses.add(clause);
                    break;
                default:
                    throw new IllegalArgumentException("Invalid position: " + position);
            }
        } finally {
            lock.writeLock().unlock();
        }
    }
    
    public List<Clause> retractClauses(Term pattern) {
        lock.writeLock().lock();
        try {
            List<Clause> retracted = new ArrayList<>();
            Iterator<Clause> iterator = clauses.iterator();
            
            while (iterator.hasNext()) {
                Clause clause = iterator.next();
                if (unifies(clause.getHead(), pattern)) {
                    iterator.remove();
                    retracted.add(clause);
                }
            }
            
            return retracted;
        } finally {
            lock.writeLock().unlock();
        }
    }
    
    public List<Clause> getClauses(Term goal) {
        lock.readLock().lock();
        try {
            // Use indexing if available
            if (hasIndex(IndexType.FIRST_ARGUMENT)) {
                return getIndexedClauses(goal);
            } else {
                return new ArrayList<>(clauses);
            }
        } finally {
            lock.readLock().unlock();
        }
    }
}
```

### 7.5.3 Private and Public Procedures

JProlog implements module-based visibility control for procedures.

### Module System

```java
public class ModuleManager {
    private final Map<String, Module> modules;
    private final Module userModule;
    private String currentModule;
    
    public ModuleManager() {
        this.modules = new HashMap<>();
        this.userModule = new Module("user");
        this.currentModule = "user";
        
        modules.put("user", userModule);
        modules.put("system", createSystemModule());
    }
    
    public void declareModule(String name, List<PredicateIndicator> exports) {
        if (modules.containsKey(name)) {
            throw new ModuleException("Module already exists: " + name);
        }
        
        Module module = new Module(name);
        module.setExports(exports);
        modules.put(name, module);
    }
    
    public void setCurrentModule(String name) {
        if (!modules.containsKey(name)) {
            throw new ModuleException("Unknown module: " + name);
        }
        this.currentModule = name;
    }
    
    public boolean isVisible(PredicateIndicator pi, String fromModule) {
        Module targetModule = findModuleForPredicate(pi);
        if (targetModule == null) return false;
        
        // Always visible within same module
        if (targetModule.getName().equals(fromModule)) {
            return true;
        }
        
        // Check if predicate is exported
        return targetModule.isExported(pi);
    }
    
    public void exportPredicate(PredicateIndicator pi, String module) {
        Module mod = modules.get(module);
        if (mod == null) {
            throw new ModuleException("Unknown module: " + module);
        }
        
        mod.addExport(pi);
    }
    
    public void importPredicate(PredicateIndicator pi, 
            String fromModule, String toModule) {
        Module source = modules.get(fromModule);
        Module target = modules.get(toModule);
        
        if (source == null || target == null) {
            throw new ModuleException("Unknown module in import");
        }
        
        if (!source.isExported(pi)) {
            throw new ModuleException(
                "Predicate not exported: " + pi + " from " + fromModule);
        }
        
        target.addImport(pi, fromModule);
    }
}

public class Module {
    private final String name;
    private final Set<PredicateIndicator> exports;
    private final Map<PredicateIndicator, String> imports;
    private final Set<PredicateIndicator> localPredicates;
    
    public Module(String name) {
        this.name = name;
        this.exports = new HashSet<>();
        this.imports = new HashMap<>();
        this.localPredicates = new HashSet<>();
    }
    
    public boolean isExported(PredicateIndicator pi) {
        return exports.contains(pi);
    }
    
    public boolean isLocal(PredicateIndicator pi) {
        return localPredicates.contains(pi);
    }
    
    public String getImportModule(PredicateIndicator pi) {
        return imports.get(pi);
    }
    
    public void addLocalPredicate(PredicateIndicator pi) {
        localPredicates.add(pi);
    }
    
    public void addExport(PredicateIndicator pi) {
        exports.add(pi);
        localPredicates.add(pi); // Exported predicates are also local
    }
    
    public void addImport(PredicateIndicator pi, String fromModule) {
        imports.put(pi, fromModule);
    }
}
```

### 7.5.4 A Logical Database Update

JProlog implements logical database updates with transaction semantics and consistency guarantees.

### Transaction Management

```java
public class DatabaseTransaction {
    private final UUID transactionId;
    private final long startTime;
    private final List<DatabaseChange> changes;
    private final IsolationLevel isolationLevel;
    private volatile TransactionStatus status;
    
    public DatabaseTransaction(IsolationLevel isolationLevel) {
        this.transactionId = UUID.randomUUID();
        this.startTime = System.currentTimeMillis();
        this.changes = new ArrayList<>();
        this.isolationLevel = isolationLevel;
        this.status = TransactionStatus.ACTIVE;
    }
    
    public void addChange(DatabaseChange change) {
        if (status != TransactionStatus.ACTIVE) {
            throw new TransactionException(
                "Cannot modify inactive transaction");
        }
        changes.add(change);
    }
    
    public void commit() {
        if (status != TransactionStatus.ACTIVE) {
            throw new TransactionException(
                "Cannot commit inactive transaction");
        }
        
        try {
            // Validate all changes
            validateChanges();
            
            // Apply changes atomically
            applyChanges();
            
            status = TransactionStatus.COMMITTED;
            
        } catch (Exception e) {
            status = TransactionStatus.FAILED;
            throw new TransactionException("Commit failed", e);
        }
    }
    
    public void rollback() {
        if (status == TransactionStatus.COMMITTED) {
            throw new TransactionException(
                "Cannot rollback committed transaction");
        }
        
        // Undo changes in reverse order
        Collections.reverse(changes);
        for (DatabaseChange change : changes) {
            change.undo();
        }
        
        status = TransactionStatus.ROLLED_BACK;
    }
    
    private void validateChanges() {
        ChangeValidator validator = new ChangeValidator();
        for (DatabaseChange change : changes) {
            validator.validate(change);
        }
    }
    
    private void applyChanges() {
        for (DatabaseChange change : changes) {
            change.apply();
        }
    }
}

public abstract class DatabaseChange {
    protected final long timestamp;
    protected final PredicateIndicator predicate;
    protected boolean applied = false;
    
    public DatabaseChange(PredicateIndicator predicate) {
        this.timestamp = System.currentTimeMillis();
        this.predicate = predicate;
    }
    
    public abstract void apply();
    public abstract void undo();
    public abstract ChangeType getType();
    
    protected void markApplied() {
        this.applied = true;
    }
    
    protected void checkNotApplied() {
        if (applied) {
            throw new IllegalStateException("Change already applied");
        }
    }
}

public class AssertChange extends DatabaseChange {
    private final Clause clause;
    private final AssertPosition position;
    
    public AssertChange(PredicateIndicator predicate, Clause clause,
            AssertPosition position) {
        super(predicate);
        this.clause = clause;
        this.position = position;
    }
    
    @Override
    public void apply() {
        checkNotApplied();
        DatabaseManager.getInstance().assertClause(clause, position);
        markApplied();
    }
    
    @Override
    public void undo() {
        if (!applied) {
            throw new IllegalStateException("Change not applied");
        }
        DatabaseManager.getInstance().retractSpecificClause(clause);
    }
    
    @Override
    public ChangeType getType() {
        return ChangeType.ASSERT;
    }
}
```

---

## 7.6 Converting a Term to a Clause, and a Clause to a Term

### Conversion Infrastructure

JProlog provides comprehensive facilities for converting between terms and clauses, supporting meta-programming and dynamic code generation.

### Term to Clause Conversion

```java
public class TermClauseConverter {
    private final TermValidator validator;
    private final ClauseBuilder builder;
    
    public TermClauseConverter() {
        this.validator = new TermValidator();
        this.builder = new ClauseBuilder();
    }
    
    public Clause termToClause(Term term) {
        validator.validateClauseTerm(term);
        
        if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            
            if (compound.getName().equals(":-") && 
                compound.getArity() == 2) {
                // Rule: Head :- Body
                Term head = compound.getArgument(0);
                Term body = compound.getArgument(1);
                
                validator.validateHead(head);
                validator.validateBody(body);
                
                return builder.createRule(head, body);
                
            } else if (compound.getName().equals(":-") && 
                       compound.getArity() == 1) {
                // Directive: :- Goal
                Term goal = compound.getArgument(0);
                validator.validateGoal(goal);
                
                return builder.createDirective(goal);
            }
        }
        
        // Fact
        validator.validateHead(term);
        return builder.createFact(term);
    }
    
    public Term clauseToTerm(Clause clause) {
        if (clause instanceof Fact) {
            return clause.getHead();
            
        } else if (clause instanceof Rule) {
            Rule rule = (Rule) clause;
            return new CompoundTerm(":-", 
                rule.getHead(), rule.getBody());
                
        } else if (clause instanceof Directive) {
            Directive directive = (Directive) clause;
            return new CompoundTerm(":-", directive.getGoal());
            
        } else {
            throw new ConversionException(
                "Unknown clause type: " + clause.getClass());
        }
    }
}
```

### 7.6.1 Converting a Term to the Head of a Clause

```java
public class HeadConverter {
    public Term convertTermToHead(Term term) {
        validateHeadTerm(term);
        
        if (term instanceof Variable) {
            throw new ConversionException(
                "Head cannot be a variable: " + term);
        }
        
        if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            
            // Check for reserved functors
            if (isReservedFunctor(compound.getName())) {
                throw new ConversionException(
                    "Reserved functor in head: " + compound.getName());
            }
            
            // Validate arguments
            for (Term arg : compound.getArguments()) {
                validateHeadArgument(arg);
            }
        }
        
        return term;
    }
    
    private void validateHeadArgument(Term arg) {
        // Head arguments can be any valid term
        if (arg instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) arg;
            
            // Check for cut in head (invalid)
            if (compound.getName().equals("!")) {
                throw new ConversionException(
                    "Cut cannot appear in clause head");
            }
        }
    }
    
    private boolean isReservedFunctor(String functor) {
        Set<String> reserved = Set.of(
            ":-", "-->", "?-", ",", ";", "->", "!", 
            "\\+", "true", "fail"
        );
        return reserved.contains(functor);
    }
}
```

### 7.6.2 Converting a Term to the Body of a Clause

```java
public class BodyConverter {
    private final GoalValidator goalValidator;
    
    public BodyConverter() {
        this.goalValidator = new GoalValidator();
    }
    
    public Term convertTermToBody(Term term) {
        validateBodyTerm(term);
        
        if (term instanceof Variable) {
            // Variable bodies are allowed (call/1 semantics)
            return term;
        }
        
        if (term instanceof Atom) {
            if (term.equals(Atom.TRUE)) {
                return term; // true body
            }
            if (term.equals(Atom.FAIL)) {
                return term; // fail body
            }
            // Regular goal
            return term;
        }
        
        if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            return convertCompoundBody(compound);
        }
        
        throw new ConversionException(
            "Invalid body term: " + term);
    }
    
    private Term convertCompoundBody(CompoundTerm compound) {
        String functor = compound.getName();
        
        // Handle control constructs
        switch (functor) {
            case ",":
                // Conjunction
                return convertConjunction(compound);
                
            case ";":
                // Disjunction or if-then-else
                return convertDisjunction(compound);
                
            case "->":
                // If-then
                return convertIfThen(compound);
                
            case "\\+":
                // Negation as failure
                return convertNegation(compound);
                
            case "!":
                // Cut
                if (compound.getArity() != 0) {
                    throw new ConversionException(
                        "Cut must have arity 0");
                }
                return compound;
                
            default:
                // Regular goal
                validateGoal(compound);
                return compound;
        }
    }
    
    private Term convertConjunction(CompoundTerm conjunction) {
        if (conjunction.getArity() != 2) {
            throw new ConversionException(
                "Conjunction must have arity 2");
        }
        
        Term left = convertTermToBody(conjunction.getArgument(0));
        Term right = convertTermToBody(conjunction.getArgument(1));
        
        return new CompoundTerm(",", left, right);
    }
    
    private void validateGoal(Term goal) {
        goalValidator.validate(goal);
    }
}
```

### 7.6.3 Converting the Head of a Clause to a Term

```java
public class ClauseHeadExtractor {
    public Term extractHead(Clause clause) {
        Term head = clause.getHead();
        
        if (clause instanceof Fact) {
            // For facts, head is the entire clause
            return head;
            
        } else if (clause instanceof Rule) {
            // For rules, extract just the head part
            return head;
            
        } else if (clause instanceof Directive) {
            // Directives don't have meaningful heads
            throw new ConversionException(
                "Cannot extract head from directive");
        }
        
        return head;
    }
    
    public Term extractHeadAsCallableTerm(Clause clause) {
        Term head = extractHead(clause);
        
        // Ensure the head is callable
        if (head instanceof Variable) {
            throw new ConversionException(
                "Head cannot be variable for callable term");
        }
        
        return head;
    }
}
```

### 7.6.4 Converting the Body of a Clause to a Term

```java
public class ClauseBodyExtractor {
    public Term extractBody(Clause clause) {
        if (clause instanceof Fact) {
            // Facts have implicit 'true' body
            return Atom.TRUE;
            
        } else if (clause instanceof Rule) {
            Rule rule = (Rule) clause;
            return rule.getBody();
            
        } else if (clause instanceof Directive) {
            Directive directive = (Directive) clause;
            return directive.getGoal();
        }
        
        throw new ConversionException(
            "Unknown clause type: " + clause.getClass());
    }
    
    public List<Term> extractBodyGoals(Clause clause) {
        Term body = extractBody(clause);
        return flattenConjunction(body);
    }
    
    private List<Term> flattenConjunction(Term term) {
        List<Term> goals = new ArrayList<>();
        flattenConjunctionRecursive(term, goals);
        return goals;
    }
    
    private void flattenConjunctionRecursive(Term term, List<Term> goals) {
        if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            
            if (compound.getName().equals(",") && 
                compound.getArity() == 2) {
                // Conjunction - flatten recursively
                flattenConjunctionRecursive(compound.getArgument(0), goals);
                flattenConjunctionRecursive(compound.getArgument(1), goals);
                return;
            }
        }
        
        // Single goal
        goals.add(term);
    }
}
```

---

## 7.7 Executing a Prolog Goal

### 7.7.1 Execution

JProlog implements a sophisticated goal execution engine that handles the complete SLD resolution process with optimizations and debugging support.

### Execution Engine

```java
public class GoalExecutionEngine {
    private final ClauseDatabase database;
    private final UnificationEngine unifier;
    private final ChoicePointStack choicePoints;
    private final CutHandler cutHandler;
    private final BuiltInRegistry builtIns;
    private final ExecutionTracer tracer;
    
    public GoalExecutionEngine(ClauseDatabase database) {
        this.database = database;
        this.unifier = new UnificationEngine();
        this.choicePoints = new ChoicePointStack();
        this.cutHandler = new CutHandler();
        this.builtIns = new BuiltInRegistry();
        this.tracer = new ExecutionTracer();
    }
    
    public ExecutionResult execute(Term goal) {
        ExecutionContext context = createExecutionContext(goal);
        
        try {
            tracer.traceGoalEntry(goal, context);
            
            List<Substitution> solutions = solveGoal(goal, context);
            
            if (solutions.isEmpty()) {
                tracer.traceGoalFailure(goal, context);
                return ExecutionResult.failure(goal);
            } else {
                tracer.traceGoalSuccess(goal, solutions, context);
                return ExecutionResult.success(goal, solutions);
            }
            
        } catch (ExecutionException e) {
            tracer.traceGoalError(goal, e, context);
            return ExecutionResult.error(goal, e);
        }
    }
    
    private List<Substitution> solveGoal(Term goal, ExecutionContext context) {
        // Check for built-in predicate
        if (builtIns.isBuiltIn(goal)) {
            return executeBuiltIn(goal, context);
        }
        
        // User-defined predicate
        return executeUserDefined(goal, context);
    }
    
    private List<Substitution> executeUserDefined(Term goal, 
            ExecutionContext context) {
        List<Substitution> solutions = new ArrayList<>();
        
        // Get matching clauses
        List<Clause> clauses = database.getClauses(goal);
        if (clauses.isEmpty()) {
            return solutions; // No clauses = failure
        }
        
        // Try each clause
        for (int i = 0; i < clauses.size(); i++) {
            Clause clause = clauses.get(i);
            
            // Create choice point if more clauses remain
            if (i < clauses.size() - 1) {
                ChoicePoint cp = new ChoicePoint(goal, clauses.subList(i + 1, clauses.size()), context);
                choicePoints.push(cp);
            }
            
            // Try this clause
            List<Substitution> clauseSolutions = tryClause(goal, clause, context);
            solutions.addAll(clauseSolutions);
            
            // If this clause succeeded and there's a cut, remove choice points
            if (!clauseSolutions.isEmpty() && containsCut(clause)) {
                cutHandler.executeCut(context);
                break; // Cut prevents trying more clauses
            }
        }
        
        return solutions;
    }
    
    private List<Substitution> tryClause(Term goal, Clause clause, 
            ExecutionContext context) {
        // Rename variables in clause to avoid conflicts
        VariableRenamer renamer = new VariableRenamer();
        Clause renamedClause = clause.rename(renamer);
        
        // Try to unify goal with clause head
        Substitution unification = unifier.unify(goal, renamedClause.getHead());
        if (unification == null) {
            return Collections.emptyList(); // Unification failed
        }
        
        // Create new context with unification
        ExecutionContext newContext = context.extend(unification);
        
        // Execute clause body
        if (renamedClause instanceof Fact) {
            // Fact - immediate success
            return Collections.singletonList(unification);
        } else {
            // Rule - execute body
            Rule rule = (Rule) renamedClause;
            Term body = rule.getBody().applySubstitution(unification);
            
            return solveGoal(body, newContext);
        }
    }
}
```

### 7.7.2 Data Types for the Execution Model

```java
public class ExecutionContext {
    private final Substitution currentSubstitution;
    private final ExecutionEnvironment environment;
    private final int depth;
    private final long startTime;
    private final Map<String, Object> properties;
    
    public ExecutionContext() {
        this(new Substitution(), new ExecutionEnvironment(), 0);
    }
    
    private ExecutionContext(Substitution substitution,
            ExecutionEnvironment environment, int depth) {
        this.currentSubstitution = substitution;
        this.environment = environment;
        this.depth = depth;
        this.startTime = System.nanoTime();
        this.properties = new HashMap<>();
    }
    
    public ExecutionContext extend(Substitution newSubstitution) {
        Substitution combined = currentSubstitution.compose(newSubstitution);
        return new ExecutionContext(combined, environment, depth + 1);
    }
    
    public ExecutionContext enterScope(String scopeName) {
        ExecutionEnvironment newEnv = environment.createChild(scopeName);
        return new ExecutionContext(currentSubstitution, newEnv, depth);
    }
    
    public Term instantiate(Term term) {
        return term.applySubstitution(currentSubstitution);
    }
    
    public boolean isVariable(Term term) {
        Term instantiated = instantiate(term);
        return instantiated instanceof Variable;
    }
    
    public long getElapsedTime() {
        return System.nanoTime() - startTime;
    }
}

public class ExecutionEnvironment {
    private final String name;
    private final ExecutionEnvironment parent;
    private final Map<String, Object> bindings;
    private final Set<PredicateIndicator> localPredicates;
    
    public ExecutionEnvironment(String name) {
        this(name, null);
    }
    
    private ExecutionEnvironment(String name, ExecutionEnvironment parent) {
        this.name = name;
        this.parent = parent;
        this.bindings = new HashMap<>();
        this.localPredicates = new HashSet<>();
    }
    
    public ExecutionEnvironment createChild(String childName) {
        return new ExecutionEnvironment(childName, this);
    }
    
    public void bindVariable(String name, Object value) {
        bindings.put(name, value);
    }
    
    public Object lookupBinding(String name) {
        Object value = bindings.get(name);
        if (value != null || parent == null) {
            return value;
        }
        return parent.lookupBinding(name);
    }
    
    public void declareLocal(PredicateIndicator pi) {
        localPredicates.add(pi);
    }
    
    public boolean isLocal(PredicateIndicator pi) {
        return localPredicates.contains(pi) ||
               (parent != null && parent.isLocal(pi));
    }
}

public class ChoicePoint {
    private final Term goal;
    private final List<Clause> remainingClauses;
    private final ExecutionContext context;
    private final int level;
    private final long timestamp;
    
    public ChoicePoint(Term goal, List<Clause> remainingClauses,
            ExecutionContext context) {
        this.goal = goal;
        this.remainingClauses = new ArrayList<>(remainingClauses);
        this.context = context;
        this.level = context.getDepth();
        this.timestamp = System.currentTimeMillis();
    }
    
    public boolean hasMoreChoices() {
        return !remainingClauses.isEmpty();
    }
    
    public Clause nextChoice() {
        if (remainingClauses.isEmpty()) {
            throw new IllegalStateException("No more choices");
        }
        return remainingClauses.remove(0);
    }
    
    public ExecutionContext restore() {
        return context;
    }
}
```

### 7.7.3 Initialization

```java
public class ExecutionInitializer {
    private final List<InitializationGoal> initGoals;
    private final FlagManager flagManager;
    private final OperatorRegistry operatorRegistry;
    
    public void initialize(PrologEngine engine) {
        // Initialize system flags
        initializeSystemFlags();
        
        // Initialize standard operators
        initializeOperators();
        
        // Load system predicates
        loadSystemPredicates(engine);
        
        // Execute initialization goals
        executeInitializationGoals(engine);
        
        // Finalize initialization
        finalizeInitialization();
    }
    
    private void initializeSystemFlags() {
        flagManager.setFlag("bounded", true);
        flagManager.setFlag("max_integer", Long.MAX_VALUE);
        flagManager.setFlag("min_integer", Long.MIN_VALUE);
        flagManager.setFlag("integer_rounding_function", "toward_zero");
        flagManager.setFlag("char_conversion", "off");
        flagManager.setFlag("debug", "off");
        flagManager.setFlag("max_arity", 255);
        flagManager.setFlag("unknown", "error");
        flagManager.setFlag("double_quotes", "codes");
    }
    
    private void executeInitializationGoals(PrologEngine engine) {
        for (InitializationGoal goal : initGoals) {
            try {
                ExecutionResult result = engine.execute(goal.getTerm());
                if (!result.isSuccess()) {
                    handleInitializationFailure(goal, result);
                }
            } catch (Exception e) {
                handleInitializationError(goal, e);
            }
        }
    }
}
```

### 7.7.4 A Goal Succeeds

```java
public class SuccessHandler {
    public void handleSuccess(Term goal, List<Substitution> solutions,
            ExecutionContext context) {
        // Record success metrics
        recordSuccessMetrics(goal, solutions.size(), context);
        
        // Apply solutions to goal variables
        List<BindingSet> bindings = new ArrayList<>();
        for (Substitution solution : solutions) {
            BindingSet bindingSet = extractBindings(goal, solution);
            bindings.add(bindingSet);
        }
        
        // Notify success listeners
        notifySuccessListeners(goal, bindings, context);
        
        // Update execution trace
        if (context.isTraceEnabled()) {
            context.getTracer().recordSuccess(goal, bindings);
        }
        
        // Handle side effects
        if (hasSideEffects(goal)) {
            processSideEffects(goal, solutions, context);
        }
    }
    
    private BindingSet extractBindings(Term goal, Substitution solution) {
        BindingSet bindings = new BindingSet();
        
        Set<Variable> goalVariables = goal.getVariables();
        for (Variable var : goalVariables) {
            Term value = solution.lookup(var);
            if (value != null && !value.equals(var)) {
                bindings.bind(var.getName(), value);
            }
        }
        
        return bindings;
    }
    
    private void recordSuccessMetrics(Term goal, int solutionCount,
            ExecutionContext context) {
        ExecutionMetrics metrics = context.getMetrics();
        metrics.recordSuccess(goal, solutionCount, context.getElapsedTime());
    }
}
```

### 7.7.5 A Goal Fails

```java
public class FailureHandler {
    public void handleFailure(Term goal, ExecutionContext context) {
        // Record failure metrics
        recordFailureMetrics(goal, context);
        
        // Check for backtracking opportunities
        if (context.hasChoicePoints()) {
            initiateBacktracking(goal, context);
        } else {
            // Final failure
            handleFinalFailure(goal, context);
        }
        
        // Notify failure listeners
        notifyFailureListeners(goal, context);
        
        // Update execution trace
        if (context.isTraceEnabled()) {
            context.getTracer().recordFailure(goal);
        }
    }
    
    private void initiateBacktracking(Term goal, ExecutionContext context) {
        ChoicePointStack choicePoints = context.getChoicePoints();
        
        while (!choicePoints.isEmpty()) {
            ChoicePoint cp = choicePoints.pop();
            
            if (cp.hasMoreChoices()) {
                // Try next choice
                Clause nextClause = cp.nextChoice();
                
                // Restore context and continue
                ExecutionContext restoredContext = cp.restore();
                tryClauseExecution(cp.getGoal(), nextClause, restoredContext);
                return;
            }
        }
        
        // No more choice points - final failure
        handleFinalFailure(goal, context);
    }
    
    private void handleFinalFailure(Term goal, ExecutionContext context) {
        // Log final failure
        context.getLogger().logFailure(goal, context);
        
        // Clean up resources
        context.cleanup();
        
        // Check for unknown predicate
        PredicateIndicator pi = getPredicateIndicator(goal);
        if (!context.getDatabase().hasPredicate(pi) && 
            !context.getBuiltIns().isBuiltIn(pi)) {
            handleUnknownPredicate(pi, context);
        }
    }
    
    private void handleUnknownPredicate(PredicateIndicator pi,
            ExecutionContext context) {
        String unknownAction = context.getFlag("unknown").toString();
        
        switch (unknownAction) {
            case "error":
                throw new ExistenceError("procedure", pi.toString());
            case "warning":
                context.getLogger().warn("Unknown predicate: " + pi);
                break;
            case "fail":
                // Just fail silently
                break;
            default:
                throw new DomainError("unknown_action", unknownAction);
        }
    }
}
```

### 7.7.6 Re-executing a Goal

```java
public class GoalReexecutor {
    private final GoalExecutionEngine engine;
    private final ChoicePointManager choicePointManager;
    
    public ExecutionResult reexecute(Term goal, ExecutionContext context) {
        // Check if goal has been executed before
        ExecutionHistory history = context.getExecutionHistory();
        if (!history.hasBeenExecuted(goal)) {
            throw new ReexecutionException(
                "Goal has not been executed before: " + goal);
        }
        
        // Get previous execution state
        ExecutionState previousState = history.getExecutionState(goal);
        
        // Restore choice points
        restoreChoicePoints(previousState, context);
        
        // Continue execution from where it left off
        return continueExecution(goal, previousState, context);
    }
    
    private ExecutionResult continueExecution(Term goal,
            ExecutionState previousState, ExecutionContext context) {
        
        // Check for more solutions via backtracking
        if (choicePointManager.hasChoicePoints(context)) {
            return backtrackForMoreSolutions(goal, context);
        }
        
        // No more solutions
        return ExecutionResult.noMoreSolutions(goal);
    }
    
    private ExecutionResult backtrackForMoreSolutions(Term goal,
            ExecutionContext context) {
        ChoicePoint cp = choicePointManager.getCurrentChoicePoint(context);
        
        if (cp != null && cp.hasMoreChoices()) {
            // Try next choice
            Clause nextClause = cp.nextChoice();
            return engine.tryClause(goal, nextClause, context);
        }
        
        return ExecutionResult.noMoreSolutions(goal);
    }
}
```

---

## 7.8 Control Constructs

### 7.8.1 true/0

```java
public class TrueBuiltIn implements BuiltIn {
    @Override
    public String getName() { return "true"; }
    
    @Override
    public int getArity() { return 0; }
    
    @Override
    public ExecutionResult execute(List<Term> args, ExecutionContext context) {
        if (!args.isEmpty()) {
            throw new TypeError("true/0 called with arguments");
        }
        
        // true always succeeds with empty substitution
        return ExecutionResult.success(Collections.singletonList(
            new Substitution()));
    }
    
    @Override
    public boolean isDeterministic() { return true; }
    
    @Override
    public Set<CallMode> getSupportedModes() {
        return Set.of(CallMode.SEMIDET);
    }
}
```

### 7.8.2 fail/0

```java
public class FailBuiltIn implements BuiltIn {
    @Override
    public String getName() { return "fail"; }
    
    @Override
    public int getArity() { return 0; }
    
    @Override
    public ExecutionResult execute(List<Term> args, ExecutionContext context) {
        if (!args.isEmpty()) {
            throw new TypeError("fail/0 called with arguments");
        }
        
        // fail always fails
        return ExecutionResult.failure();
    }
    
    @Override
    public boolean isDeterministic() { return true; }
    
    @Override
    public Set<CallMode> getSupportedModes() {
        return Set.of(CallMode.FAILURE);
    }
}
```

### 7.8.3 call/1

```java
public class CallBuiltIn implements BuiltIn {
    private final GoalExecutionEngine engine;
    
    public CallBuiltIn(GoalExecutionEngine engine) {
        this.engine = engine;
    }
    
    @Override
    public String getName() { return "call"; }
    
    @Override
    public int getArity() { return 1; }
    
    @Override
    public ExecutionResult execute(List<Term> args, ExecutionContext context) {
        if (args.size() != 1) {
            throw new TypeError("call/1 expects exactly 1 argument");
        }
        
        Term goal = args.get(0).applySubstitution(context.getSubstitution());
        
        // Validate that goal is callable
        if (!isCallable(goal)) {
            throw new TypeError("call/1 argument must be callable: " + goal);
        }
        
        // Execute the goal
        try {
            return engine.execute(goal, context);
        } catch (ExecutionException e) {
            // Propagate execution errors
            throw e;
        }
    }
    
    private boolean isCallable(Term term) {
        if (term instanceof Atom) {
            return true;
        }
        if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            // Check that functor is not a variable
            return !compound.getName().isEmpty();
        }
        return false;
    }
    
    @Override
    public boolean isDeterministic() { return false; }
    
    @Override
    public Set<CallMode> getSupportedModes() {
        return Set.of(CallMode.NONDET);
    }
}
```

### 7.8.4 !/0 - Cut

```java
public class CutBuiltIn implements BuiltIn {
    private final CutHandler cutHandler;
    
    public CutBuiltIn(CutHandler cutHandler) {
        this.cutHandler = cutHandler;
    }
    
    @Override
    public String getName() { return "!"; }
    
    @Override
    public int getArity() { return 0; }
    
    @Override
    public ExecutionResult execute(List<Term> args, ExecutionContext context) {
        if (!args.isEmpty()) {
            throw new TypeError("!/0 called with arguments");
        }
        
        // Execute cut: remove choice points
        cutHandler.executeCut(context);
        
        // Cut always succeeds
        return ExecutionResult.success(Collections.singletonList(
            new Substitution()));
    }
    
    @Override
    public boolean isDeterministic() { return true; }
    
    @Override
    public boolean hasSideEffects() { return true; }
    
    @Override
    public Set<CallMode> getSupportedModes() {
        return Set.of(CallMode.SEMIDET);
    }
}

public class CutHandler {
    public void executeCut(ExecutionContext context) {
        ChoicePointStack choicePoints = context.getChoicePoints();
        int currentLevel = context.getLevel();
        
        // Remove all choice points created at the current level or deeper
        choicePoints.removeIf(cp -> cp.getLevel() >= currentLevel);
        
        // Mark cut as executed in current scope
        context.markCutExecuted();
        
        // Log cut execution for debugging
        if (context.isTraceEnabled()) {
            context.getTracer().recordCut(currentLevel);
        }
    }
    
    public boolean wasCutExecuted(ExecutionContext context) {
        return context.wasCutExecuted();
    }
}
```

### 7.8.5 (,)/2 - Conjunction

```java
public class ConjunctionBuiltIn implements BuiltIn {
    private final GoalExecutionEngine engine;
    
    @Override
    public String getName() { return ","; }
    
    @Override
    public int getArity() { return 2; }
    
    @Override
    public ExecutionResult execute(List<Term> args, ExecutionContext context) {
        if (args.size() != 2) {
            throw new TypeError("(,)/2 expects exactly 2 arguments");
        }
        
        Term firstGoal = args.get(0);
        Term secondGoal = args.get(1);
        
        List<Substitution> solutions = new ArrayList<>();
        
        // Execute first goal
        ExecutionResult firstResult = engine.execute(firstGoal, context);
        
        if (!firstResult.isSuccess()) {
            return ExecutionResult.failure(); // First goal failed
        }
        
        // For each solution of the first goal, execute the second goal
        for (Substitution firstSolution : firstResult.getSolutions()) {
            ExecutionContext newContext = context.extend(firstSolution);
            
            // Apply first solution to second goal
            Term instantiatedSecondGoal = 
                secondGoal.applySubstitution(firstSolution);
            
            // Execute second goal
            ExecutionResult secondResult = 
                engine.execute(instantiatedSecondGoal, newContext);
            
            if (secondResult.isSuccess()) {
                // Combine solutions
                for (Substitution secondSolution : secondResult.getSolutions()) {
                    Substitution combined = firstSolution.compose(secondSolution);
                    solutions.add(combined);
                }
            }
        }
        
        if (solutions.isEmpty()) {
            return ExecutionResult.failure();
        } else {
            return ExecutionResult.success(solutions);
        }
    }
    
    @Override
    public boolean isDeterministic() { return false; }
}
```

### 7.8.6 (;)/2 - Disjunction

```java
public class DisjunctionBuiltIn implements BuiltIn {
    private final GoalExecutionEngine engine;
    
    @Override
    public String getName() { return ";"; }
    
    @Override
    public int getArity() { return 2; }
    
    @Override
    public ExecutionResult execute(List<Term> args, ExecutionContext context) {
        if (args.size() != 2) {
            throw new TypeError("(;)/2 expects exactly 2 arguments");
        }
        
        Term leftGoal = args.get(0);
        Term rightGoal = args.get(1);
        
        // Check if this is if-then-else: (Cond -> Then ; Else)
        if (leftGoal instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) leftGoal;
            if (compound.getName().equals("->") && compound.getArity() == 2) {
                return executeIfThenElse(compound, rightGoal, context);
            }
        }
        
        // Regular disjunction: try left goal first
        List<Substitution> solutions = new ArrayList<>();
        
        ExecutionResult leftResult = engine.execute(leftGoal, context);
        if (leftResult.isSuccess()) {
            solutions.addAll(leftResult.getSolutions());
        }
        
        // Then try right goal
        ExecutionResult rightResult = engine.execute(rightGoal, context);
        if (rightResult.isSuccess()) {
            solutions.addAll(rightResult.getSolutions());
        }
        
        if (solutions.isEmpty()) {
            return ExecutionResult.failure();
        } else {
            return ExecutionResult.success(solutions);
        }
    }
    
    private ExecutionResult executeIfThenElse(CompoundTerm ifThen,
            Term elseGoal, ExecutionContext context) {
        Term condition = ifThen.getArgument(0);
        Term thenGoal = ifThen.getArgument(1);
        
        // Try condition
        ExecutionResult conditionResult = engine.execute(condition, context);
        
        if (conditionResult.isSuccess()) {
            // Condition succeeded - execute then part for each solution
            List<Substitution> solutions = new ArrayList<>();
            
            for (Substitution conditionSolution : conditionResult.getSolutions()) {
                ExecutionContext newContext = context.extend(conditionSolution);
                Term instantiatedThen = thenGoal.applySubstitution(conditionSolution);
                
                ExecutionResult thenResult = engine.execute(instantiatedThen, newContext);
                if (thenResult.isSuccess()) {
                    for (Substitution thenSolution : thenResult.getSolutions()) {
                        solutions.add(conditionSolution.compose(thenSolution));
                    }
                }
            }
            
            return solutions.isEmpty() ? ExecutionResult.failure() : 
                ExecutionResult.success(solutions);
        } else {
            // Condition failed - execute else part
            return engine.execute(elseGoal, context);
        }
    }
}
```

---

## 7.9 Evaluating an Expression

### Arithmetic Expression Evaluator

```java
public class ArithmeticEvaluator {
    private final Map<String, ArithmeticFunction> functions;
    
    public ArithmeticEvaluator() {
        this.functions = new HashMap<>();
        registerStandardFunctions();
    }
    
    private void registerStandardFunctions() {
        // Basic arithmetic
        functions.put("+", new AdditionFunction());
        functions.put("-", new SubtractionFunction());
        functions.put("*", new MultiplicationFunction());
        functions.put("/", new DivisionFunction());
        functions.put("//", new IntegerDivisionFunction());
        functions.put("mod", new ModuloFunction());
        functions.put("rem", new RemainderFunction());
        functions.put("**", new PowerFunction());
        
        // Mathematical functions
        functions.put("abs", new AbsoluteValueFunction());
        functions.put("sign", new SignFunction());
        functions.put("max", new MaxFunction());
        functions.put("min", new MinFunction());
        functions.put("sqrt", new SquareRootFunction());
        functions.put("sin", new SineFunction());
        functions.put("cos", new CosineFunction());
        functions.put("tan", new TangentFunction());
        
        // Bitwise operations
        functions.put("/\\", new BitwiseAndFunction());
        functions.put("\\/", new BitwiseOrFunction());
        functions.put("xor", new BitwiseXorFunction());
        functions.put("<<", new LeftShiftFunction());
        functions.put(">>", new RightShiftFunction());
        functions.put("\\", new BitwiseNotFunction());
    }
    
    public Number evaluateExpression(Term expression, ExecutionContext context) {
        if (expression instanceof Number) {
            return (Number) expression;
        }
        
        if (expression instanceof Variable) {
            Term value = context.instantiate(expression);
            if (value instanceof Variable) {
                throw new InstantiationError("Arithmetic expression contains uninstantiated variable");
            }
            return evaluateExpression(value, context);
        }
        
        if (expression instanceof Atom) {
            // Check for special constants
            Atom atom = (Atom) expression;
            switch (atom.getName()) {
                case "pi":
                    return new Number(Math.PI);
                case "e":
                    return new Number(Math.E);
                default:
                    throw new TypeError("Invalid arithmetic expression: " + atom);
            }
        }
        
        if (expression instanceof CompoundTerm) {
            return evaluateCompoundExpression((CompoundTerm) expression, context);
        }
        
        throw new TypeError("Invalid arithmetic expression: " + expression);
    }
    
    private Number evaluateCompoundExpression(CompoundTerm expression,
            ExecutionContext context) {
        String functor = expression.getName();
        List<Term> args = expression.getArguments();
        
        ArithmeticFunction function = functions.get(functor);
        if (function == null) {
            throw new EvaluationError("Unknown arithmetic function: " + functor);
        }
        
        // Evaluate arguments
        List<Number> evaluatedArgs = new ArrayList<>();
        for (Term arg : args) {
            evaluatedArgs.add(evaluateExpression(arg, context));
        }
        
        // Validate arity
        if (evaluatedArgs.size() != function.getArity()) {
            throw new TypeError(String.format(
                "Function %s expects %d arguments, got %d",
                functor, function.getArity(), evaluatedArgs.size()));
        }
        
        // Execute function
        try {
            return function.evaluate(evaluatedArgs);
        } catch (ArithmeticException e) {
            throw new EvaluationError("Arithmetic error: " + e.getMessage());
        }
    }
}

public abstract class ArithmeticFunction {
    public abstract int getArity();
    public abstract Number evaluate(List<Number> args);
    
    protected void checkArity(List<Number> args, int expected) {
        if (args.size() != expected) {
            throw new IllegalArgumentException(
                "Expected " + expected + " arguments, got " + args.size());
        }
    }
    
    protected double asDouble(Number n) {
        return n.getValue().doubleValue();
    }
    
    protected long asLong(Number n) {
        return n.getValue().longValue();
    }
}

public class AdditionFunction extends ArithmeticFunction {
    @Override
    public int getArity() { return 2; }
    
    @Override
    public Number evaluate(List<Number> args) {
        checkArity(args, 2);
        
        Number left = args.get(0);
        Number right = args.get(1);
        
        if (left.isInteger() && right.isInteger()) {
            return new Number(asLong(left) + asLong(right));
        } else {
            return new Number(asDouble(left) + asDouble(right));
        }
    }
}
```

---

## 7.10 Input/Output

### 7.10.1 Sources and Sinks

```java
public class IOManager {
    private final Map<Term, Stream> openStreams;
    private Stream currentInput;
    private Stream currentOutput;
    private final StreamFactory streamFactory;
    
    public IOManager() {
        this.openStreams = new ConcurrentHashMap<>();
        this.streamFactory = new StreamFactory();
        
        // Initialize standard streams
        this.currentInput = streamFactory.createStandardInput();
        this.currentOutput = streamFactory.createStandardOutput();
        
        // Register standard streams
        openStreams.put(new Atom("user_input"), currentInput);
        openStreams.put(new Atom("user_output"), currentOutput);
        openStreams.put(new Atom("user_error"), streamFactory.createStandardError());
    }
    
    public Stream openStream(Term source, String mode, List<Term> options) {
        validateSource(source);
        validateMode(mode);
        
        StreamOptions streamOptions = parseOptions(options);
        Stream stream = streamFactory.createStream(source, mode, streamOptions);
        
        // Generate stream alias or use provided one
        Term alias = streamOptions.getAlias();
        if (alias == null) {
            alias = generateStreamAlias();
        }
        
        openStreams.put(alias, stream);
        return stream;
    }
    
    public void closeStream(Term streamOrAlias) {
        Stream stream = resolveStream(streamOrAlias);
        if (stream == null) {
            throw new ExistenceError("stream", streamOrAlias.toString());
        }
        
        try {
            stream.close();
            
            // Remove from open streams
            openStreams.entrySet().removeIf(entry -> entry.getValue() == stream);
            
            // Reset current streams if necessary
            if (stream == currentInput) {
                currentInput = openStreams.get(new Atom("user_input"));
            }
            if (stream == currentOutput) {
                currentOutput = openStreams.get(new Atom("user_output"));
            }
            
        } catch (IOException e) {
            throw new SystemError("Failed to close stream: " + e.getMessage());
        }
    }
    
    private void validateSource(Term source) {
        if (source instanceof Atom) {
            // File name
            String filename = ((Atom) source).getName();
            if (filename.isEmpty()) {
                throw new DomainError("source_sink", source);
            }
        } else {
            throw new TypeError("Source must be an atom");
        }
    }
    
    private void validateMode(String mode) {
        Set<String> validModes = Set.of("read", "write", "append");
        if (!validModes.contains(mode)) {
            throw new DomainError("io_mode", new Atom(mode));
        }
    }
}
```

### 7.10.2 Streams

```java
public abstract class Stream {
    protected final String alias;
    protected final String mode;
    protected final StreamOptions options;
    protected volatile boolean closed;
    
    public Stream(String alias, String mode, StreamOptions options) {
        this.alias = alias;
        this.mode = mode;
        this.options = options;
        this.closed = false;
    }
    
    public abstract Term read() throws IOException;
    public abstract void write(Term term) throws IOException;
    public abstract void flush() throws IOException;
    public abstract void close() throws IOException;
    
    public boolean isInput() {
        return mode.equals("read");
    }
    
    public boolean isOutput() {
        return mode.equals("write") || mode.equals("append");
    }
    
    public boolean isClosed() {
        return closed;
    }
    
    protected void checkNotClosed() {
        if (closed) {
            throw new ExistenceError("stream", alias);
        }
    }
    
    protected void checkInputMode() {
        if (!isInput()) {
            throw new PermissionError("input", "stream", new Atom(alias));
        }
    }
    
    protected void checkOutputMode() {
        if (!isOutput()) {
            throw new PermissionError("output", "stream", new Atom(alias));
        }
    }
}

public class FileStream extends Stream {
    private final File file;
    private BufferedReader reader;
    private BufferedWriter writer;
    private final TermReader termReader;
    private final TermWriter termWriter;
    
    public FileStream(File file, String mode, StreamOptions options) {
        super(file.getName(), mode, options);
        this.file = file;
        this.termReader = new TermReader();
        this.termWriter = new TermWriter(options);
        
        initializeStream();
    }
    
    private void initializeStream() {
        try {
            if (isInput()) {
                reader = new BufferedReader(new FileReader(file, 
                    options.getEncoding()));
            } else if (mode.equals("write")) {
                writer = new BufferedWriter(new FileWriter(file, 
                    options.getEncoding()));
            } else if (mode.equals("append")) {
                writer = new BufferedWriter(new FileWriter(file, 
                    true, options.getEncoding()));
            }
        } catch (IOException e) {
            throw new SystemError("Failed to open file: " + e.getMessage());
        }
    }
    
    @Override
    public Term read() throws IOException {
        checkNotClosed();
        checkInputMode();
        
        if (options.getType() == StreamType.BINARY) {
            return readByte();
        } else {
            return termReader.readTerm(reader);
        }
    }
    
    @Override
    public void write(Term term) throws IOException {
        checkNotClosed();
        checkOutputMode();
        
        if (options.getType() == StreamType.BINARY) {
            writeByte(term);
        } else {
            termWriter.writeTerm(term, writer);
        }
    }
    
    private Term readByte() throws IOException {
        int b = reader.read();
        if (b == -1) {
            return new Atom("end_of_file");
        }
        return new Number(b);
    }
    
    private void writeByte(Term term) throws IOException {
        if (!(term instanceof Number)) {
            throw new TypeError("Byte must be a number");
        }
        
        Number num = (Number) term;
        int value = num.getValue().intValue();
        
        if (value < 0 || value > 255) {
            throw new TypeError("Byte must be between 0 and 255");
        }
        
        writer.write(value);
    }
}

public class StreamOptions {
    private final StreamType type;
    private final Charset encoding;
    private final Term alias;
    private final boolean autoFlush;
    private final int bufferSize;
    
    public StreamOptions() {
        this(StreamType.TEXT, StandardCharsets.UTF_8, null, false, 8192);
    }
    
    public StreamOptions(StreamType type, Charset encoding, Term alias,
            boolean autoFlush, int bufferSize) {
        this.type = type;
        this.encoding = encoding;
        this.alias = alias;
        this.autoFlush = autoFlush;
        this.bufferSize = bufferSize;
    }
    
    // Getters
    public StreamType getType() { return type; }
    public Charset getEncoding() { return encoding; }
    public Term getAlias() { return alias; }
    public boolean isAutoFlush() { return autoFlush; }
    public int getBufferSize() { return bufferSize; }
}

public enum StreamType {
    TEXT, BINARY
}
```

---

## 7.11 Flags

### Flag Management System

```java
public class PrologFlagManager {
    private final Map<String, PrologFlag> flags;
    private final List<FlagChangeListener> listeners;
    
    public PrologFlagManager() {
        this.flags = new ConcurrentHashMap<>();
        this.listeners = new ArrayList<>();
        initializeStandardFlags();
    }
    
    private void initializeStandardFlags() {
        // Integer type flags
        setFlag("bounded", true, true, "Integer type is bounded");
        setFlag("max_integer", Long.MAX_VALUE, true, "Maximum integer value");
        setFlag("min_integer", Long.MIN_VALUE, true, "Minimum integer value");
        setFlag("integer_rounding_function", "toward_zero", true, 
            "Integer rounding function");
        
        // Character processing flags
        setFlag("char_conversion", "off", false, "Character conversion mode");
        
        // Debugging flags
        setFlag("debug", "off", false, "Debug mode");
        setFlag("unknown", "error", false, "Unknown predicate handling");
        
        // System limits
        setFlag("max_arity", 255, true, "Maximum arity for compound terms");
        
        // I/O flags
        setFlag("double_quotes", "codes", false, "Double quote representation");
        
        // Implementation-specific flags
        setFlag("dialect", "jprolog", true, "Prolog dialect");
        setFlag("version", "2.0.15", true, "JProlog version");
        setFlag("encoding", "UTF-8", false, "Default character encoding");
        setFlag("trace", "off", false, "Execution tracing");
        setFlag("optimize", "on", false, "Query optimization");
    }
    
    public Object getCurrentPrologFlag(String name) {
        PrologFlag flag = flags.get(name);
        if (flag == null) {
            throw new DomainError("prolog_flag", new Atom(name));
        }
        return flag.getValue();
    }
    
    public void setPrologFlag(String name, Object value) {
        PrologFlag flag = flags.get(name);
        if (flag == null) {
            throw new DomainError("prolog_flag", new Atom(name));
        }
        
        if (flag.isReadOnly()) {
            throw new PermissionError("modify", "flag", new Atom(name));
        }
        
        validateFlagValue(name, value);
        
        Object oldValue = flag.getValue();
        flag.setValue(value);
        
        // Notify listeners
        notifyFlagChanged(name, oldValue, value);
        
        // Apply flag-specific behavior
        applyFlagBehavior(name, value);
    }
    
    private void validateFlagValue(String name, Object value) {
        switch (name) {
            case "debug":
            case "trace":
                if (!value.equals("on") && !value.equals("off")) {
                    throw new DomainError("on_off", new Atom(value.toString()));
                }
                break;
                
            case "unknown":
                if (!Set.of("error", "fail", "warning").contains(value)) {
                    throw new DomainError("unknown_action", new Atom(value.toString()));
                }
                break;
                
            case "double_quotes":
                if (!Set.of("codes", "chars", "atom").contains(value)) {
                    throw new DomainError("double_quotes_option", 
                        new Atom(value.toString()));
                }
                break;
                
            case "char_conversion":
                if (!value.equals("on") && !value.equals("off")) {
                    throw new DomainError("on_off", new Atom(value.toString()));
                }
                break;
        }
    }
    
    private void applyFlagBehavior(String name, Object value) {
        switch (name) {
            case "debug":
                setDebugMode(value.equals("on"));
                break;
                
            case "trace":
                setTraceMode(value.equals("on"));
                break;
                
            case "char_conversion":
                setCharacterConversion(value.equals("on"));
                break;
                
            case "unknown":
                setUnknownPredicateHandling(value.toString());
                break;
        }
    }
}

public class PrologFlag {
    private final String name;
    private volatile Object value;
    private final boolean readOnly;
    private final String description;
    
    public PrologFlag(String name, Object value, boolean readOnly, 
            String description) {
        this.name = name;
        this.value = value;
        this.readOnly = readOnly;
        this.description = description;
    }
    
    public Object getValue() { return value; }
    
    public void setValue(Object value) {
        if (readOnly) {
            throw new IllegalStateException("Flag is read-only: " + name);
        }
        this.value = value;
    }
    
    public boolean isReadOnly() { return readOnly; }
    public String getName() { return name; }
    public String getDescription() { return description; }
}
```

---

## 7.12 Errors

### Error Classification and Handling

```java
public class ErrorManager {
    private final ErrorClassifier classifier;
    private final ErrorHandler handler;
    private final ErrorLogger logger;
    
    public ErrorManager() {
        this.classifier = new ErrorClassifier();
        this.handler = new ErrorHandler();
        this.logger = new ErrorLogger();
    }
    
    public void handleError(Throwable error, ExecutionContext context) {
        // Classify the error
        ErrorInfo errorInfo = classifier.classify(error, context);
        
        // Log the error
        logger.logError(errorInfo);
        
        // Handle based on error type
        ErrorResponse response = handler.handleError(errorInfo, context);
        
        // Execute response
        executeErrorResponse(response, context);
    }
    
    private void executeErrorResponse(ErrorResponse response, 
            ExecutionContext context) {
        switch (response.getType()) {
            case PROPAGATE:
                throw response.getException();
                
            case CATCH:
                executeCatchHandler(response, context);
                break;
                
            case FAIL:
                // Goal fails silently
                break;
                
            case WARNING:
                context.getLogger().warn(response.getMessage());
                break;
        }
    }
}

// ISO Standard Error Classification
public abstract class PrologError extends RuntimeException {
    protected final Term errorTerm;
    
    public PrologError(String message, Term errorTerm) {
        super(message);
        this.errorTerm = errorTerm;
    }
    
    public Term getErrorTerm() { return errorTerm; }
    public abstract String getErrorClass();
}

public class InstantiationError extends PrologError {
    public InstantiationError(String message) {
        super(message, new CompoundTerm("error", 
            new Atom("instantiation_error"), 
            new Variable("Context")));
    }
    
    @Override
    public String getErrorClass() { return "instantiation_error"; }
}

public class TypeError extends PrologError {
    private final String expectedType;
    private final Term culprit;
    
    public TypeError(String expectedType, Term culprit) {
        super(String.format("Type error: expected %s, found %s", 
            expectedType, culprit),
            new CompoundTerm("error",
                new CompoundTerm("type_error", 
                    new Atom(expectedType), culprit),
                new Variable("Context")));
        this.expectedType = expectedType;
        this.culprit = culprit;
    }
    
    @Override
    public String getErrorClass() { return "type_error"; }
}

public class DomainError extends PrologError {
    private final String validDomain;
    private final Term culprit;
    
    public DomainError(String validDomain, Term culprit) {
        super(String.format("Domain error: %s not in domain %s", 
            culprit, validDomain),
            new CompoundTerm("error",
                new CompoundTerm("domain_error",
                    new Atom(validDomain), culprit),
                new Variable("Context")));
        this.validDomain = validDomain;
        this.culprit = culprit;
    }
    
    @Override
    public String getErrorClass() { return "domain_error"; }
}

public class ExistenceError extends PrologError {
    private final String objectType;
    private final Term culprit;
    
    public ExistenceError(String objectType, Term culprit) {
        super(String.format("Existence error: %s %s does not exist", 
            objectType, culprit),
            new CompoundTerm("error",
                new CompoundTerm("existence_error",
                    new Atom(objectType), culprit),
                new Variable("Context")));
        this.objectType = objectType;
        this.culprit = culprit;
    }
    
    @Override
    public String getErrorClass() { return "existence_error"; }
}

public class PermissionError extends PrologError {
    private final String operation;
    private final String permissionType;
    private final Term culprit;
    
    public PermissionError(String operation, String permissionType, 
            Term culprit) {
        super(String.format("Permission error: cannot %s %s %s",
            operation, permissionType, culprit),
            new CompoundTerm("error",
                new CompoundTerm("permission_error",
                    new Atom(operation),
                    new Atom(permissionType),
                    culprit),
                new Variable("Context")));
        this.operation = operation;
        this.permissionType = permissionType;
        this.culprit = culprit;
    }
    
    @Override
    public String getErrorClass() { return "permission_error"; }
}

public class RepresentationError extends PrologError {
    private final String limitType;
    
    public RepresentationError(String limitType) {
        super("Representation error: " + limitType,
            new CompoundTerm("error",
                new CompoundTerm("representation_error",
                    new Atom(limitType)),
                new Variable("Context")));
        this.limitType = limitType;
    }
    
    @Override
    public String getErrorClass() { return "representation_error"; }
}

public class EvaluationError extends PrologError {
    private final String errorType;
    
    public EvaluationError(String errorType) {
        super("Evaluation error: " + errorType,
            new CompoundTerm("error",
                new CompoundTerm("evaluation_error",
                    new Atom(errorType)),
                new Variable("Context")));
        this.errorType = errorType;
    }
    
    @Override
    public String getErrorClass() { return "evaluation_error"; }
}

public class ResourceError extends PrologError {
    private final String resourceType;
    
    public ResourceError(String resourceType) {
        super("Resource error: " + resourceType,
            new CompoundTerm("error",
                new CompoundTerm("resource_error",
                    new Atom(resourceType)),
                new Variable("Context")));
        this.resourceType = resourceType;
    }
    
    @Override
    public String getErrorClass() { return "resource_error"; }
}

public class SyntaxError extends PrologError {
    private final String errorType;
    
    public SyntaxError(String errorType) {
        super("Syntax error: " + errorType,
            new CompoundTerm("error",
                new CompoundTerm("syntax_error",
                    new Atom(errorType)),
                new Variable("Context")));
        this.errorType = errorType;
    }
    
    @Override
    public String getErrorClass() { return "syntax_error"; }
}

public class SystemError extends PrologError {
    public SystemError(String message) {
        super("System error: " + message,
            new CompoundTerm("error",
                new CompoundTerm("system_error",
                    new Atom(message)),
                new Variable("Context")));
    }
    
    @Override
    public String getErrorClass() { return "system_error"; }
}
```

---

## Conclusion

This comprehensive analysis of JProlog's execution model demonstrates a sophisticated implementation that closely follows ISO Prolog semantics while providing practical extensions and optimizations. Key architectural achievements include:

1. **Complete Execution Pipeline**: From directive processing through goal execution with proper error handling
2. **Dynamic Database Management**: Full support for assert/retract operations with transaction semantics
3. **Advanced Control Constructs**: Proper implementation of cut, conjunction, disjunction, and if-then-else
4. **Comprehensive I/O System**: Full stream-based I/O with support for text and binary modes
5. **ISO-Compliant Error Handling**: Complete error taxonomy with proper exception propagation
6. **Performance Optimizations**: Choice point management, indexing, and execution monitoring

The implementation provides a solid foundation for both educational use and production applications, with careful attention to correctness, performance, and maintainability.