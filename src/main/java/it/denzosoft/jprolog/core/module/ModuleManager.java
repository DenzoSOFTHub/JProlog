package it.denzosoft.jprolog.core.module;

import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.util.TermUtils;

import java.util.*;
import java.util.logging.Logger;

/**
 * Manages the module system for ISO Prolog compliance.
 * Handles module creation, imports, exports, and predicate resolution.
 */
public class ModuleManager {
    // START_CHANGE: ISS-2025-0167 - Logger for name collision warnings
    private static final Logger LOGGER = Logger.getLogger(ModuleManager.class.getName());
    // END_CHANGE: ISS-2025-0167

    private final Map<String, Module> modules;
    private volatile Module currentModule;
    private Module userModule;

    // START_CHANGE: ISS-2025-0466 - engine v4 wave W6: a monotone modification stamp. The v4
    // `core.engine.v4.Modules` owner resolves predicates itself (this manager stays the
    // consult-time RECORDER shared with the legacy and v2 engines) and mirrors the user-defined
    // modules; the stamp is how it knows when the mirror is stale. Bumped by every write.
    private volatile long stamp = 1;

    /** Monotone counter bumped by every structural change (v4 mirror invalidation). */
    public long getStamp() { return stamp; }

    /** Record a change made through a {@link Module} handle this manager cannot observe
     *  (an export, a {@code meta_predicate} or a {@code module_transparent} declaration). */
    public void touch() { STAMP.incrementAndGet(this); }            // ISS-2025-0739: atomic, lock-free
    private static final java.util.concurrent.atomic.AtomicLongFieldUpdater<ModuleManager> STAMP =
        java.util.concurrent.atomic.AtomicLongFieldUpdater.newUpdater(ModuleManager.class, "stamp");
    // END_CHANGE: ISS-2025-0466

    // START_CHANGE: ISS-2025-0739 - 4.6 wave Q3.8: while a thread is LOADING, the module its
    // clauses go to and its `:- module` switches are its own (loads of different files may run
    // in parallel); outside a load the shared current (type-in) module applies, as before.
    private static final class LoadScope {
        Module current;
        int depth;
    }

    private final ThreadLocal<LoadScope> loadScope = new ThreadLocal<>();

    /** The calling thread enters a load (nested loads nest). */
    public void enterLoad() {
        LoadScope sc = loadScope.get();
        if (sc == null) {
            sc = new LoadScope();
            sc.current = currentModule;
            loadScope.set(sc);
        }
        sc.depth++;
    }

    /** The calling thread leaves a load; the outermost exit drops its module scope. */
    public void exitLoad() {
        LoadScope sc = loadScope.get();
        if (sc != null && --sc.depth <= 0) loadScope.remove();
    }

    private Module cur() {
        LoadScope sc = loadScope.get();
        return sc != null ? sc.current : currentModule;
    }
    // END_CHANGE: ISS-2025-0739
    
    /**
     * Create a new module manager with default 'user' module.
     */
    public ModuleManager() {
        this.modules = new java.util.concurrent.ConcurrentHashMap<>();   // ISS-2025-0739: loads run in parallel
        this.userModule = new Module("user");
        this.modules.put("user", userModule);
        this.currentModule = userModule;
    }
    
    /**
     * Create or get a module.
     * 
     * @param name The module name
     * @return The module
     */
    public Module getOrCreateModule(String name) {
        Module existing = modules.get(name);
        if (existing != null) return existing;
        touch();                                                  // ISS-2025-0466, 0739: atomic
        return modules.computeIfAbsent(name, Module::new);
    }
    
    /**
     * Create a module with explicit export list.
     * 
     * @param name The module name
     * @param exportList List of predicates to export
     * @return The created module
     */
    public Module createModule(String name, List<PredicateSignature> exportList) {
        Module module = new Module(name, exportList);
        modules.put(name, module);
        touch();                                                  // ISS-2025-0466, 0739: atomic
        return module;
    }
    
    /**
     * Set the current module context.
     * 
     * @param moduleName The module name
     */
    public void setCurrentModule(String moduleName) {
        Module module = modules.get(moduleName);
        if (module == null) {
            throw new IllegalArgumentException("Module not found: " + moduleName);
        }
        LoadScope sc = loadScope.get();                           // ISS-2025-0739
        if (sc != null) sc.current = module; else this.currentModule = module;
        touch();                                                  // ISS-2025-0466
    }
    
    /**
     * Get the current module.
     * 
     * @return The current module
     */
    public Module getCurrentModule() {
        return cur();                                             // ISS-2025-0739
    }
    
    /**
     * Get the user module (default module).
     * 
     * @return The user module
     */
    public Module getUserModule() {
        return userModule;
    }
    
    /**
     * Get a module by name.
     * 
     * @param name The module name
     * @return The module, or null if not found
     */
    public Module getModule(String name) {
        return modules.get(name);
    }
    
    /**
     * Add a rule to the current module.
     * 
     * @param rule The rule to add
     */
    public void addRule(Rule rule) {
        cur().addRule(rule);
        touch();                                                  // ISS-2025-0466, 0739: atomic
    }

    /** {@link #addRule(Rule)} into a module the caller already holds (ISS-2025-0739: the loader). */
    public void addRule(Module module, Rule rule) {
        module.addRule(rule);
        touch();
    }
    
    /**
     * Import a module into the current module.
     *
     * @param moduleName The module to import
     */
    public void importModule(String moduleName) {
        Module module = modules.get(moduleName);
        if (module == null) {
            throw new IllegalArgumentException("Module not found: " + moduleName);
        }
        // START_CHANGE: ISS-2025-0167 - Name collision detection on import
        for (PredicateSignature sig : module.getExportedPredicates()) {
            checkImportCollision(sig, moduleName);
        }
        // END_CHANGE: ISS-2025-0167
        cur().importModule(module);
        touch();                                                  // ISS-2025-0466, 0739: atomic
    }

    /**
     * Import specific predicates from a module.
     *
     * @param moduleName The module to import from
     * @param predicates List of predicates to import
     */
    public void importModule(String moduleName, List<PredicateSignature> predicates) {
        Module module = modules.get(moduleName);
        if (module == null) {
            throw new IllegalArgumentException("Module not found: " + moduleName);
        }
        // START_CHANGE: ISS-2025-0167 - Name collision detection on import
        for (PredicateSignature sig : predicates) {
            checkImportCollision(sig, moduleName);
        }
        // END_CHANGE: ISS-2025-0167
        cur().importModule(module, predicates);
        touch();                                                  // ISS-2025-0466, 0739: atomic
    }

    // START_CHANGE: ISS-2025-0167 - Name collision detection
    /**
     * Check if importing a predicate would collide with an existing predicate
     * in the current module (locally defined or imported from another module).
     * Logs a warning if a collision is detected.
     *
     * @param sig The predicate signature being imported
     * @param sourceModuleName The name of the module being imported from
     */
    private void checkImportCollision(PredicateSignature sig, String sourceModuleName) {
        if (cur().isLocallyDefined(sig)) {
            LOGGER.warning("Import collision: predicate " + sig +
                " from module '" + sourceModuleName +
                "' conflicts with locally defined predicate in module '" +
                cur().getName() + "'");
        } else {
            Module existingSource = cur().resolvePredicate(sig);
            if (existingSource != null && !existingSource.getName().equals(sourceModuleName)) {
                LOGGER.warning("Import collision: predicate " + sig +
                    " from module '" + sourceModuleName +
                    "' conflicts with predicate already imported from module '" +
                    existingSource.getName() + "' in module '" +
                    cur().getName() + "'");
            }
        }
    }
    // END_CHANGE: ISS-2025-0167
    
    /**
     * Resolve a predicate call with module qualification.
     * 
     * @param term The term to resolve (may be module:predicate)
     * @return ModuleQualifiedCall with resolved module and predicate
     */
    public ModuleQualifiedCall resolvePredicate(Term term) {
        if (term instanceof CompoundTerm && ":".equals(TermUtils.getFunctorName(term)) && TermUtils.getArity(term) == 2) {
            // Module qualified call: Module:Predicate
            Term moduleTerm = TermUtils.getArgument((CompoundTerm) term, 0);
            Term predicateTerm = TermUtils.getArgument((CompoundTerm) term, 1);
            
            if (moduleTerm instanceof Atom) {
                String moduleName = ((Atom) moduleTerm).getName();
                Module module = modules.get(moduleName);
                if (module == null) {
                    throw new IllegalArgumentException("Module not found: " + moduleName);
                }
                return new ModuleQualifiedCall(module, predicateTerm);
            }
        }
        
        // Unqualified call - resolve in current module context
        String functor = term instanceof Atom ? ((Atom) term).getName() : TermUtils.getFunctorName(term);
        int arity = term instanceof Atom ? 0 : TermUtils.getArity(term);
        PredicateSignature signature = new PredicateSignature(functor, arity);

        // START_CHANGE: ISS-2025-0165 - Use internal resolution for current module, external for others
        // Current module can see all its own predicates (internal access)
        Module resolvedModule = cur().resolvePredicate(signature);
        if (resolvedModule != null && resolvedModule != cur()) {
            // The predicate was found in an imported module - verify export visibility
            resolvedModule = resolvedModule.resolvePredicateForExternalAccess(signature);
        }
        if (resolvedModule == null && cur() != userModule) {
            // Fall back to user module - use external access since it's a different module
            resolvedModule = userModule.resolvePredicateForExternalAccess(signature);
        } else if (resolvedModule == null) {
            // Current module IS the user module - use internal access
            resolvedModule = userModule.resolvePredicate(signature);
        }
        // END_CHANGE: ISS-2025-0165

        return new ModuleQualifiedCall(resolvedModule, term);
    }
    
    /**
     * Get all rules for a predicate across all modules.
     * 
     * @param term The predicate term
     * @return List of rules from appropriate module
     */
    public List<Rule> getRulesForPredicate(Term term) {
        ModuleQualifiedCall call = resolvePredicate(term);
        if (call.getModule() == null) {
            return new ArrayList<>();
        }
        
        String functor = call.getPredicate() instanceof Atom ? 
            ((Atom) call.getPredicate()).getName() : TermUtils.getFunctorName(call.getPredicate());
        int arity = call.getPredicate() instanceof Atom ? 0 : TermUtils.getArity(call.getPredicate());
        
        PredicateSignature signature = new PredicateSignature(functor, arity);
        return call.getModule().getRulesForPredicate(signature);
    }
    
    /**
     * List all modules.
     * 
     * @return Set of all module names
     */
    public Set<String> getAllModuleNames() {
        return new HashSet<>(modules.keySet());
    }
    
    /**
     * Get all modules.
     * 
     * @return Map of all modules
     */
    public Map<String, Module> getAllModules() {
        return new HashMap<>(modules);
    }
    
    /**
     * Parse a module directive: :- module(Name, ExportList).
     * 
     * @param directive The module directive term
     * @return true if successfully parsed
     */
    public boolean parseModuleDirective(Term directive) {
        if (directive instanceof CompoundTerm && "module".equals(TermUtils.getFunctorName(directive)) && TermUtils.getArity(directive) == 2) {
            CompoundTerm moduleTerm = (CompoundTerm) directive;
            Term nameTerm = TermUtils.getArgument(moduleTerm, 0);
            Term exportTerm = TermUtils.getArgument(moduleTerm, 1);
            
            if (nameTerm instanceof Atom) {
                String name = ((Atom) nameTerm).getName();
                List<PredicateSignature> exportList = parseExportList(exportTerm);
                
                Module module = createModule(name, exportList);
                setCurrentModule(name);
                return true;
            }
        }
        return false;
    }
    
    /**
     * Parse export list from term.
     * 
     * @param exportTerm The export list term
     * @return List of predicate signatures
     */
    private List<PredicateSignature> parseExportList(Term exportTerm) {
        List<PredicateSignature> exportList = new ArrayList<>();
        
        if (exportTerm instanceof CompoundTerm && "[]".equals(TermUtils.getFunctorName(exportTerm))) {
            // Empty list
            return exportList;
        }
        
        // Parse list of functor/arity terms
        List<Term> terms = extractListTerms(exportTerm);
        for (Term term : terms) {
            if (term instanceof CompoundTerm && "/".equals(TermUtils.getFunctorName(term)) && TermUtils.getArity(term) == 2) {
                CompoundTerm sigTerm = (CompoundTerm) term;
                Term functorTerm = TermUtils.getArgument(sigTerm, 0);
                Term arityTerm = TermUtils.getArgument(sigTerm, 1);
                
                if (functorTerm instanceof Atom && arityTerm instanceof it.denzosoft.jprolog.core.terms.Number) {
                    String functor = ((Atom) functorTerm).getName();
                    int arity = ((it.denzosoft.jprolog.core.terms.Number) arityTerm).getValue().intValue();
                    exportList.add(new PredicateSignature(functor, arity));
                }
            }
        }
        
        return exportList;
    }
    
    /**
     * Extract terms from a Prolog list.
     * 
     * @param listTerm The list term
     * @return List of terms
     */
    private List<Term> extractListTerms(Term listTerm) {
        List<Term> terms = new ArrayList<>();
        Term current = listTerm;
        
        while (current instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(current)) && TermUtils.getArity(current) == 2) {
            CompoundTerm cons = (CompoundTerm) current;
            terms.add(TermUtils.getArgument(cons, 0));
            current = TermUtils.getArgument(cons, 1);
        }
        
        return terms;
    }
    
    /**
     * Reset to default state with only user module.
     */
    public void reset() {
        modules.clear();
        userModule = new Module("user");
        modules.put("user", userModule);
        currentModule = userModule;
        touch();                                                  // ISS-2025-0466, 0739: atomic
    }
    
    @Override
    public String toString() {
        return "ModuleManager{modules=" + modules.keySet() + ", current=" + currentModule.getName() + "}";
    }
}