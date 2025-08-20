package it.denzosoft.jprolog.core.module;

import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.util.TermUtils;

import java.util.*;

/**
 * Manages the module system for ISO Prolog compliance.
 * Handles module creation, imports, exports, and predicate resolution.
 */
public class ModuleManager {
    
    private final Map<String, Module> modules;
    private Module currentModule;
    private Module userModule;
    
    /**
     * Create a new module manager with default 'user' module.
     */
    public ModuleManager() {
        this.modules = new HashMap<>();
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
        this.currentModule = module;
    }
    
    /**
     * Get the current module.
     * 
     * @return The current module
     */
    public Module getCurrentModule() {
        return currentModule;
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
        currentModule.addRule(rule);
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
        currentModule.importModule(module);
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
        currentModule.importModule(module, predicates);
    }
    
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
        
        Module resolvedModule = currentModule.resolvePredicate(signature);
        if (resolvedModule == null) {
            // Fall back to user module if not found
            resolvedModule = userModule.resolvePredicate(signature);
        }
        
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
    }
    
    @Override
    public String toString() {
        return "ModuleManager{modules=" + modules.keySet() + ", current=" + currentModule.getName() + "}";
    }
}