package it.denzosoft.jprolog.core.module;

import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.util.TermUtils;

import java.util.*;

/**
 * Represents a Prolog module with its own namespace for predicates.
 * Implements ISO Prolog module system.
 */
public class Module {
    
    private final String name;
    private final Set<PredicateSignature> exportedPredicates;
    private final Map<PredicateSignature, Module> importedPredicates;
    private final List<Rule> localRules;
    private final Map<String, Module> importedModules;
    private final Set<String> metaPredicates;
    
    /**
     * Create a new module.
     * 
     * @param name The module name
     */
    public Module(String name) {
        this.name = name;
        this.exportedPredicates = new HashSet<>();
        this.importedPredicates = new HashMap<>();
        this.localRules = new ArrayList<>();
        this.importedModules = new HashMap<>();
        this.metaPredicates = new HashSet<>();
    }
    
    /**
     * Create a module with explicit export list.
     * 
     * @param name The module name
     * @param exportList List of predicates to export
     */
    public Module(String name, List<PredicateSignature> exportList) {
        this(name);
        this.exportedPredicates.addAll(exportList);
    }
    
    /**
     * Add a rule to this module.
     * 
     * @param rule The rule to add
     */
    public void addRule(Rule rule) {
        localRules.add(rule);
        
        // Auto-export predicates if no explicit export list
        if (exportedPredicates.isEmpty()) {
            Term head = rule.getHead();
            if (head instanceof Atom) {
                exportedPredicates.add(new PredicateSignature(((Atom) head).getName(), 0));
            } else {
                String functor = TermUtils.getFunctorName(head);
                int arity = TermUtils.getArity(head);
                exportedPredicates.add(new PredicateSignature(functor, arity));
            }
        }
    }
    
    /**
     * Import a predicate from another module.
     * 
     * @param signature The predicate signature
     * @param sourceModule The module to import from
     */
    public void importPredicate(PredicateSignature signature, Module sourceModule) {
        if (sourceModule.isExported(signature)) {
            importedPredicates.put(signature, sourceModule);
        } else {
            throw new IllegalArgumentException("Predicate " + signature + 
                " is not exported from module " + sourceModule.getName());
        }
    }
    
    /**
     * Import all exported predicates from another module.
     * 
     * @param module The module to import from
     */
    public void importModule(Module module) {
        importedModules.put(module.getName(), module);
        for (PredicateSignature signature : module.getExportedPredicates()) {
            importedPredicates.put(signature, module);
        }
    }
    
    /**
     * Import specific predicates from another module.
     * 
     * @param module The module to import from
     * @param predicates List of predicates to import
     */
    public void importModule(Module module, List<PredicateSignature> predicates) {
        importedModules.put(module.getName(), module);
        for (PredicateSignature signature : predicates) {
            importPredicate(signature, module);
        }
    }
    
    /**
     * Check if a predicate is exported by this module.
     * 
     * @param signature The predicate signature
     * @return true if exported
     */
    public boolean isExported(PredicateSignature signature) {
        return exportedPredicates.contains(signature);
    }
    
    /**
     * Check if a predicate is locally defined in this module.
     * 
     * @param signature The predicate signature
     * @return true if locally defined
     */
    public boolean isLocallyDefined(PredicateSignature signature) {
        return localRules.stream()
            .anyMatch(rule -> {
                Term head = rule.getHead();
                String functor = head instanceof Atom ? ((Atom) head).getName() : TermUtils.getFunctorName(head);
                int arity = head instanceof Atom ? 0 : TermUtils.getArity(head);
                return signature.equals(new PredicateSignature(functor, arity));
            });
    }
    
    /**
     * Resolve a predicate call to the appropriate module.
     * 
     * @param signature The predicate signature
     * @return The module that defines the predicate, or null if not found
     */
    public Module resolvePredicate(PredicateSignature signature) {
        // Check local predicates first
        if (isLocallyDefined(signature)) {
            return this;
        }
        
        // Check imported predicates
        return importedPredicates.get(signature);
    }
    
    /**
     * Get all rules for a specific predicate.
     * 
     * @param signature The predicate signature
     * @return List of rules, or empty list if not found
     */
    public List<Rule> getRulesForPredicate(PredicateSignature signature) {
        return localRules.stream()
            .filter(rule -> {
                Term head = rule.getHead();
                String functor = head instanceof Atom ? ((Atom) head).getName() : TermUtils.getFunctorName(head);
                int arity = head instanceof Atom ? 0 : TermUtils.getArity(head);
                return signature.equals(new PredicateSignature(functor, arity));
            })
            .collect(ArrayList::new, ArrayList::add, ArrayList::addAll);
    }
    
    /**
     * Export a predicate from this module.
     * 
     * @param signature The predicate signature to export
     */
    public void exportPredicate(PredicateSignature signature) {
        exportedPredicates.add(signature);
    }
    
    /**
     * Mark a predicate as meta-predicate.
     * 
     * @param signature The predicate signature
     */
    public void declareMeta(String signature) {
        metaPredicates.add(signature);
    }
    
    /**
     * Check if a predicate is declared as meta-predicate.
     * 
     * @param signature The predicate signature
     * @return true if meta-predicate
     */
    public boolean isMetaPredicate(String signature) {
        return metaPredicates.contains(signature);
    }
    
    // Getters
    public String getName() { return name; }
    public Set<PredicateSignature> getExportedPredicates() { return new HashSet<>(exportedPredicates); }
    public List<Rule> getLocalRules() { return new ArrayList<>(localRules); }
    public Map<String, Module> getImportedModules() { return new HashMap<>(importedModules); }
    
    @Override
    public String toString() {
        return "Module{" + name + ", exports=" + exportedPredicates.size() + 
               ", rules=" + localRules.size() + "}";
    }
    
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Module)) return false;
        Module module = (Module) o;
        return Objects.equals(name, module.name);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(name);
    }
}