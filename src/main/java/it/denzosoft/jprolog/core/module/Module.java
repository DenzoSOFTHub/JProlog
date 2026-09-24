package it.denzosoft.jprolog.core.module;

import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.operator.Operator;
import it.denzosoft.jprolog.core.operator.OperatorTable;
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
    // START_CHANGE: ISS-2025-0248 - distinguish empty-explicit list from "no list provided"
    private boolean hasExplicitExportList = false;
    // END_CHANGE: ISS-2025-0248
    private final Map<PredicateSignature, Module> importedPredicates;
    private final List<Rule> localRules;
    private final Map<String, Module> importedModules;
    private final Set<String> metaPredicates;
    // START_CHANGE: ISS-2025-0167 - meta_predicate/1 declarations with argument specs
    private final Map<PredicateSignature, List<String>> metaPredicateDeclarations;
    // END_CHANGE: ISS-2025-0167
    // START_CHANGE: ISS-2025-0167 - module_transparent/1 support
    private final Set<PredicateSignature> transparentPredicates;
    // END_CHANGE: ISS-2025-0167
    // START_CHANGE: ISS-2025-0167 - Re-export mechanism
    private final Map<PredicateSignature, Module> reexports;
    // END_CHANGE: ISS-2025-0167
    // START_CHANGE: ISS-2025-0167 - Per-module operator scope
    private final OperatorTable localOperators;
    // END_CHANGE: ISS-2025-0167

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
        // START_CHANGE: ISS-2025-0167 - Initialize new fields
        this.metaPredicateDeclarations = new HashMap<>();
        this.transparentPredicates = new HashSet<>();
        this.reexports = new HashMap<>();
        this.localOperators = OperatorTable.createEmpty();
        // END_CHANGE: ISS-2025-0167
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
        // START_CHANGE: ISS-2025-0248 - mark explicit list (even if empty)
        this.hasExplicitExportList = true;
        // END_CHANGE: ISS-2025-0248
    }

    /**
     * Add a rule to this module.
     *
     * @param rule The rule to add
     */
    // START_CHANGE: ISS-2025-0784 - 4.6 wave Q6 (extra 4): the v4 engine mirrors this module
    // (core.engine.v4.Modules). A load that queries the module after every clause (a module-local
    // goal_expansion/2) made it rebuild the WHOLE mirror per clause — cubic in the clause count.
    // It now asks what changed: the rule list only grows (getLocalRuleCount/getLocalRulesFrom),
    // and everything else it mirrors (exports, imports, import specs, meta declarations) bumps
    // this version.
    private volatile long structVersion;

    private void structChanged() { structVersion++; }

    /** Changes whenever something other than the rule list changes. */
    public long getStructVersion() { return structVersion; }

    /** Number of local rules (they are only ever appended). */
    public synchronized int getLocalRuleCount() { return localRules.size(); }

    /** The local rules from position {@code from} on. */
    public synchronized List<Rule> getLocalRulesFrom(int from) {
        return new ArrayList<>(localRules.subList(Math.min(from, localRules.size()), localRules.size()));
    }

    /** Was the module declared with an export list (possibly empty)? */
    public boolean hasExplicitExportList() { return hasExplicitExportList; }
    // END_CHANGE: ISS-2025-0784

    public synchronized void addRule(Rule rule) {
        localRules.add(rule);
        // ISS-2025-0731: does this module define goal_expansion/2 (checked per clause at load time)
        Term kh = rule.getHead();
        if (kh instanceof it.denzosoft.jprolog.core.terms.CompoundTerm
                && ((it.denzosoft.jprolog.core.terms.CompoundTerm) kh).getArguments().size() == 2
                && "goal_expansion".equals(((it.denzosoft.jprolog.core.terms.CompoundTerm) kh).getName())) {
            hasGoalExpansion = true;
        }

        // START_CHANGE: ISS-2025-0248 - only auto-export when no explicit list given
        if (!hasExplicitExportList) {
            Term head = rule.getHead();
            if (head instanceof Atom) {
                exportedPredicates.add(new PredicateSignature(((Atom) head).getName(), 0));
            } else {
                String functor = TermUtils.getFunctorName(head);
                int arity = TermUtils.getArity(head);
                exportedPredicates.add(new PredicateSignature(functor, arity));
            }
        }
        // END_CHANGE: ISS-2025-0248
    }
    
    /**
     * Import a predicate from another module.
     * 
     * @param signature The predicate signature
     * @param sourceModule The module to import from
     */
    public synchronized void importPredicate(PredicateSignature signature, Module sourceModule) {
        structChanged();                                           // ISS-2025-0784
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
    public synchronized void importModule(Module module) {
        structChanged();                                           // ISS-2025-0784
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
    public synchronized void importModule(Module module, List<PredicateSignature> predicates) {
        structChanged();                                           // ISS-2025-0784
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
     * Returns any locally defined predicate regardless of export status.
     * Use this for internal module resolution (within the same module).
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

    // START_CHANGE: ISS-2025-0165 - Enforce module visibility for external access
    /**
     * Resolve a predicate call for external access (from another module).
     * Only returns exported predicates, enforcing module encapsulation.
     * Private (non-exported) predicates are not visible externally.
     *
     * @param signature The predicate signature
     * @return The module that defines the predicate, or null if not found or not exported
     */
    public Module resolvePredicateForExternalAccess(PredicateSignature signature) {
        // Check local predicates - only if exported
        if (isLocallyDefined(signature) && isExported(signature)) {
            return this;
        }

        // Check imported predicates (these were already export-checked at import time)
        return importedPredicates.get(signature);
    }
    // END_CHANGE: ISS-2025-0165
    
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
        structChanged();                                           // ISS-2025-0784
        exportedPredicates.add(signature);
    }
    
    /**
     * Mark a predicate as meta-predicate.
     * 
     * @param signature The predicate signature
     */
    public void declareMeta(String signature) {
        structChanged();                                           // ISS-2025-0784
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
    
    // START_CHANGE: ISS-2025-0167 - meta_predicate/1 declarations
    /**
     * Declare a meta-predicate with argument specifications.
     * Each arg spec is one of: "+" (input), "-" (output), "?" (any),
     * or an integer N meaning "call with N extra args".
     *
     * @param sig The predicate signature
     * @param argSpecs List of argument specifications
     */
    public void declareMetaPredicate(PredicateSignature sig, List<String> argSpecs) {
        structChanged();                                           // ISS-2025-0784
        metaPredicateDeclarations.put(sig, new ArrayList<>(argSpecs));
    }

    /**
     * Get the meta-predicate declaration for a predicate.
     *
     * @param sig The predicate signature
     * @return List of argument specs, or null if not declared as meta-predicate
     */
    public List<String> getMetaPredicateDeclaration(PredicateSignature sig) {
        List<String> specs = metaPredicateDeclarations.get(sig);
        return specs != null ? new ArrayList<>(specs) : null;
    }

    // START_CHANGE: ISS-2025-0469 - engine v4 wave W6 needs the whole table to mirror it into
    // core.engine.v4.Modules, where it drives meta-argument qualification.
    /**
     * All meta-predicate declarations of this module.
     *
     * @return Map of predicate signature to its argument specifications
     */
    public Map<PredicateSignature, List<String>> getMetaPredicateDeclarations() {
        return new HashMap<>(metaPredicateDeclarations);
    }
    // END_CHANGE: ISS-2025-0469
    // END_CHANGE: ISS-2025-0167

    // START_CHANGE: ISS-2025-0167 - module_transparent/1 support
    /**
     * Declare a predicate as transparent (uses caller's module context).
     *
     * @param sig The predicate signature
     */
    public void declareTransparent(PredicateSignature sig) {
        structChanged();                                           // ISS-2025-0784
        transparentPredicates.add(sig);
    }

    /**
     * Check if a predicate is declared as transparent.
     *
     * @param sig The predicate signature
     * @return true if the predicate is transparent
     */
    public boolean isTransparent(PredicateSignature sig) {
        return transparentPredicates.contains(sig);
    }
    // END_CHANGE: ISS-2025-0167

    // START_CHANGE: ISS-2025-0167 - Re-export mechanism
    /**
     * Re-export a predicate from a source module. The predicate is added to both
     * this module's exports and imports, so modules importing this module also
     * gain access to the re-exported predicate.
     *
     * @param sig The predicate signature to re-export
     * @param sourceModule The module that originally defines the predicate
     */
    public void reexport(PredicateSignature sig, Module sourceModule) {
        structChanged();                                           // ISS-2025-0784
        reexports.put(sig, sourceModule);
        exportedPredicates.add(sig);
        importedPredicates.put(sig, sourceModule);
    }

    /**
     * Get the map of re-exported predicates to their source modules.
     *
     * @return Map of re-exported predicate signatures to source modules
     */
    public Map<PredicateSignature, Module> getReexports() {
        return new HashMap<>(reexports);
    }
    // END_CHANGE: ISS-2025-0167

    // START_CHANGE: ISS-2025-0167 - Per-module operator scope
    /**
     * Define an operator in this module's local operator table.
     *
     * @param precedence The operator precedence
     * @param type The operator type string (e.g., "xfx", "yfx")
     * @param name The operator name
     */
    public void defineOperator(int precedence, String type, String name) {
        Operator.Type opType = Operator.parseType(type);
        localOperators.defineOperator(precedence, opType, name);
    }

    /**
     * Get this module's local operator table.
     *
     * @return The module's local operator table
     */
    public OperatorTable getOperatorTable() {
        return localOperators;
    }
    // END_CHANGE: ISS-2025-0167

    // START_CHANGE: ISS-2025-0735 - 4.6 wave Q3.4: use_module(File, Imports). What this module
    // imports from another one: everything (no spec), only some indicators, everything except
    // some, and aliases (`p/1 as q` makes q/1 here call the other module's p/1).
    /** The import list of one {@code use_module/2}. Keys are {@code "name/arity"}. */
    public static final class ImportSpec {
        /** null = every export. */
        public java.util.Set<String> only;
        public final java.util.Set<String> except = new java.util.HashSet<>();
        /** alias key ("new/arity") -> the imported predicate's name. */
        public final java.util.Map<String, String> aliases = new java.util.LinkedHashMap<>();
    }

    private final Map<String, ImportSpec> importSpecs = new HashMap<>();

    /** Has this module a goal_expansion/2 clause (ISS-2025-0731)? */
    private volatile boolean hasGoalExpansion;

    public boolean definesGoalExpansion() { return hasGoalExpansion; }

    /**
     * Record the import list for {@code module} (null = import everything, which drops a
     * previous restriction). Two restricted imports of the same module are merged.
     */
    public synchronized void setImportSpec(String module, ImportSpec spec) {
        structChanged();                                           // ISS-2025-0784
        if (spec == null) { importSpecs.remove(module); return; }
        ImportSpec old = importSpecs.get(module);
        if (old != null) {
            if (old.only == null || spec.only == null) spec.only = null;
            else spec.only.addAll(old.only);
            spec.except.retainAll(old.except);
            for (Map.Entry<String, String> e : old.aliases.entrySet()) spec.aliases.putIfAbsent(e.getKey(), e.getValue());
        }
        importSpecs.put(module, spec);
    }

    public synchronized Map<String, ImportSpec> getImportSpecs() { return new HashMap<>(importSpecs); }
    // END_CHANGE: ISS-2025-0735

    // Getters
    public String getName() { return name; }
    // ISS-2025-0739: synchronized — a module may be written by a load on another thread
    public synchronized Set<PredicateSignature> getExportedPredicates() { return new HashSet<>(exportedPredicates); }
    public synchronized List<Rule> getLocalRules() { return new ArrayList<>(localRules); }
    public synchronized Map<String, Module> getImportedModules() { return new HashMap<>(importedModules); }
    
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