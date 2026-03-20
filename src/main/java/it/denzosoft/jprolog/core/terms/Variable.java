package it.denzosoft.jprolog.core.terms;

import java.util.Map;


public class Variable extends Term {

    private String name;
    private static int anonymousCounter = 0;
    private final boolean isAnonymous;

    public Variable(String name) {
        if ("_".equals(name)) {
            // Anonymous variable - each instance gets a unique name
            this.name = "_G" + (++anonymousCounter);
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

            // Occurs check: prevent circular references
            if (occursCheckIterative(var, derefTerm, substitution)) {
                return false; // Unification fails if variable occurs in the term
            }

            // Bind the variable to the term
            substitution.put(var.name, derefTerm);
            return true;
        }

        // If dereferenced term is also a variable, bind to the non-variable side
        if (derefTerm instanceof Variable) {
            Variable var = (Variable) derefTerm;

            // Occurs check: prevent circular references
            if (occursCheckIterative(var, derefThis, substitution)) {
                return false; // Unification fails if variable occurs in the term
            }

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
    
    // Legacy occurs method for backwards compatibility
    private boolean occurs(Variable variable, Term term, Map<String, Term> substitution) {
        return occursCheckIterative(variable, term, substitution);
    }
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
