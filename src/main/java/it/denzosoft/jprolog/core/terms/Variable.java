package it.denzosoft.jprolog.core.terms;

import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;


public class Variable extends Term {

    private String name;
    // START_CHANGE: ISS-2025-0164 - Thread-safe anonymous variable counter
    private static final AtomicInteger anonymousCounter = new AtomicInteger(0);
    // END_CHANGE: ISS-2025-0164
    private final boolean isAnonymous;

    // START_CHANGE: ISS-2025-0168 - Occurs check flag (default: false for performance)
    /**
     * Global flag controlling whether the occurs check is performed during
     * standard unification. When false (default), the occurs check is skipped
     * for performance, matching most Prolog implementations. When true, the
     * occurs check is always performed. The unify_with_occurs_check/2 built-in
     * always performs the check regardless of this flag.
     */
    private static final AtomicBoolean occursCheckEnabled = new AtomicBoolean(false);

    /**
     * Set the global occurs_check flag.
     *
     * @param enabled true to enable occurs check in standard unification
     */
    public static void setOccursCheckEnabled(boolean enabled) {
        occursCheckEnabled.set(enabled);
    }

    /**
     * Get the current state of the global occurs_check flag.
     *
     * @return true if occurs check is enabled
     */
    public static boolean isOccursCheckEnabled() {
        return occursCheckEnabled.get();
    }
    // END_CHANGE: ISS-2025-0168

    public Variable(String name) {
        if ("_".equals(name)) {
            // Anonymous variable - each instance gets a unique name
            this.name = "_G" + anonymousCounter.incrementAndGet();
            this.isAnonymous = true;
        } else {
            this.name = name;
            this.isAnonymous = false;
        }
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
    
    public boolean isAnonymous() {
        return isAnonymous;
    }
    
    public String getDisplayName() {
        return isAnonymous ? "_" : name;
    }

   // START_CHANGE: ISS-2025-0012 - Complete redesign with iterative dereferencing
   @Override
   public boolean unify(Term term, Map<String, Term> substitution) {
        // Dereference this variable iteratively to avoid recursion
        Term derefThis = dereferenceIterative(this, substitution);
        Term derefTerm = dereferenceIterative(term, substitution);

        // If both sides are the same after dereferencing, they unify
        if (derefThis.equals(derefTerm)) {
            return true;
        }

        // If the dereferenced term is still a variable, handle variable-to-term binding
        if (derefThis instanceof Variable) {
            Variable var = (Variable) derefThis;

            // START_CHANGE: ISS-2025-0168 - Conditional occurs check based on global flag
            // Occurs check: prevent circular references (only when flag is enabled)
            if (occursCheckEnabled.get() && occursCheckIterative(var, derefTerm, substitution)) {
                return false; // Unification fails if variable occurs in the term
            }
            // END_CHANGE: ISS-2025-0168

            // Bind the variable to the term
            substitution.put(var.name, derefTerm);
            return true;
        }

        // If dereferenced term is also a variable, bind to the non-variable side
        if (derefTerm instanceof Variable) {
            Variable var = (Variable) derefTerm;

            // START_CHANGE: ISS-2025-0168 - Conditional occurs check based on global flag
            // Occurs check: prevent circular references (only when flag is enabled)
            if (occursCheckEnabled.get() && occursCheckIterative(var, derefThis, substitution)) {
                return false; // Unification fails if variable occurs in the term
            }
            // END_CHANGE: ISS-2025-0168

            // Bind the variable to the term
            substitution.put(var.name, derefThis);
            return true;
        }

        // Both sides are non-variables, delegate to standard unification
        return derefThis.unify(derefTerm, substitution);
   }

   // START_CHANGE: ISS-2025-0091 - Fast-path dereferencing without HashSet allocation
   /**
    * Iteratively dereference a term following substitution chains.
    * Uses a fast path for short chains (typical case: 1-3 levels) that avoids
    * HashSet allocation. Falls back to HashSet-based cycle detection only for
    * deep chains (>16 levels), which are extremely rare.
    */
   private Term dereferenceIterative(Term term, Map<String, Term> substitution) {
        Term current = term;
        int depth = 0;

        // Fast path: no allocation for typical short variable chains
        while (current instanceof Variable) {
            String varName = ((Variable) current).name;
            Term bound = substitution.get(varName);
            if (bound == null) {
                break;
            }
            current = bound;
            if (++depth > 16) {
                // Deep chain: fall back to HashSet-based cycle detection
                return dereferenceWithCycleDetection(current, substitution);
            }
        }

        return current;
   }

   /**
    * Fallback dereference with HashSet cycle detection for deep chains.
    */
   private Term dereferenceWithCycleDetection(Term term, Map<String, Term> substitution) {
        java.util.Set<String> visited = new java.util.HashSet<>();
        Term current = term;

        while (current instanceof Variable) {
            String varName = ((Variable) current).name;
            if (!visited.add(varName)) {
                break; // Cycle detected
            }
            Term bound = substitution.get(varName);
            if (bound == null) {
                break;
            }
            current = bound;
        }

        return current;
   }
   // END_CHANGE: ISS-2025-0091
   // END_CHANGE: ISS-2025-0012

    // START_CHANGE: ISS-2025-0012 - Implement iterative occurs check
    /**
     * Iterative occurs check to prevent infinite recursion.
     * Checks if a variable occurs within a term after dereferencing.
     */
    private boolean occursCheckIterative(Variable variable, Term term, Map<String, Term> substitution) {
        // Dereference the term first
        Term derefTerm = dereferenceIterative(term, substitution);
        
        // Now check if the variable occurs in the dereferenced term
        return occursInTerm(variable, derefTerm, substitution, new java.util.HashSet<>());
    }
    
    /**
     * Helper method to check if a variable occurs within a term structure.
     * Uses visited set to prevent infinite recursion on circular structures.
     */
    private boolean occursInTerm(Variable variable, Term term, Map<String, Term> substitution, java.util.Set<String> visited) {
        if (term instanceof Variable) {
            Variable termVar = (Variable) term;
            
            // Direct match
            if (variable.getName().equals(termVar.getName())) {
                return true;
            }
            
            // Avoid cycles in variable chains
            if (visited.contains(termVar.getName())) {
                return false;
            }
            
            // Check if this variable has a substitution
            if (substitution.containsKey(termVar.getName())) {
                visited.add(termVar.getName());
                boolean result = occursInTerm(variable, substitution.get(termVar.getName()), substitution, visited);
                visited.remove(termVar.getName());
                return result;
            } else {
                return false;
            }
        } else if (term instanceof CompoundTerm) {
            CompoundTerm compoundTerm = (CompoundTerm) term;
            for (Term arg : compoundTerm.getArguments()) {
                if (occursInTerm(variable, arg, substitution, visited)) {
                    return true;
                }
            }
            return false;
        } else {
            return false; // Atoms, Numbers, and Lists cannot contain variables
        }
    }
    
    // START_CHANGE: ISS-2025-0178 - Remove dead legacy occurs() stub
    // Removed unused backwards-compatibility occurs() method that just delegated to occursCheckIterative()
    // END_CHANGE: ISS-2025-0178
    // END_CHANGE: ISS-2025-0012


    @Override
    public boolean isGround() {
        return false; // Variables are never ground until they are substituted
    }

     @Override
    public String toString() {
        return getDisplayName();
    }

    @Override
    public Term copy() {
        if (isAnonymous) {
            // Create a fresh anonymous variable
            return new Variable("_");
        } else {
            // Regular variable keeps the same name
            return new Variable(this.name);
        }
    }
    
    // START_CHANGE: ISS-2025-0091 - Fully iterative resolveBindings without HashSet
    @Override
    public Term resolveBindings(Map<String, Term> bindings) {
        // Iterative variable chain resolution - no allocation for typical chains
        Term current = bindings.get(this.name);
        if (current == null) {
            return this;
        }

        int depth = 0;
        while (current instanceof Variable) {
            Term next = bindings.get(((Variable) current).getName());
            if (next == null) {
                return current; // Unbound variable at end of chain
            }
            current = next;
            if (++depth > 16) {
                // Extremely deep chain - fall back to cycle detection
                return resolveBindingsWithCycleDetection(bindings, new java.util.HashSet<>());
            }
        }

        // current is now a non-variable term - resolve its bindings too
        return current.resolveBindings(bindings);
    }
    // END_CHANGE: ISS-2025-0091

    /**
     * Resolve bindings with cycle detection to prevent infinite recursion.
     */
    private Term resolveBindingsWithCycleDetection(Map<String, Term> bindings, java.util.Set<String> visited) {
        if (!visited.add(this.name)) {
            return this; // Circular reference
        }

        Term bound = bindings.get(this.name);
        if (bound == null) {
            return this;
        }
        if (bound instanceof Variable) {
            return ((Variable) bound).resolveBindingsWithCycleDetection(bindings, visited);
        }
        return bound.resolveBindings(bindings);
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        Variable variable = (Variable) obj;
        return name != null ? name.equals(variable.name) : variable.name == null;
    }
    
    @Override
    public int hashCode() {
        return name != null ? name.hashCode() : 0;
    }
}
