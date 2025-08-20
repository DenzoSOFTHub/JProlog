package it.denzosoft.jprolog.core.module;

import it.denzosoft.jprolog.core.terms.Term;

/**
 * Represents a module-qualified predicate call.
 */
public class ModuleQualifiedCall {
    
    private final Module module;
    private final Term predicate;
    
    /**
     * Create a module-qualified call.
     * 
     * @param module The module (may be null if not resolved)
     * @param predicate The predicate term
     */
    public ModuleQualifiedCall(Module module, Term predicate) {
        this.module = module;
        this.predicate = predicate;
    }
    
    /**
     * Get the module.
     * 
     * @return The module, or null if not resolved
     */
    public Module getModule() {
        return module;
    }
    
    /**
     * Get the predicate term.
     * 
     * @return The predicate term
     */
    public Term getPredicate() {
        return predicate;
    }
    
    /**
     * Check if the call is resolved to a module.
     * 
     * @return true if resolved
     */
    public boolean isResolved() {
        return module != null;
    }
    
    @Override
    public String toString() {
        if (module != null) {
            return module.getName() + ":" + predicate;
        } else {
            return predicate.toString();
        }
    }
}