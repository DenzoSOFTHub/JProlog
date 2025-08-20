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
   
   /**
    * Iteratively dereference a term following substitution chains.
    * Avoids recursion to prevent StackOverflowError.
    */
   private Term dereferenceIterative(Term term, Map<String, Term> substitution) {
        java.util.Set<String> visited = new java.util.HashSet<>();
        Term current = term;
        
        while (current instanceof Variable) {
            String varName = ((Variable) current).name;
            
            // Cycle detection
            if (visited.contains(varName)) {
                break; // Return current to break the cycle
            }
            
            // If no substitution exists, we've reached the end
            if (!substitution.containsKey(varName)) {
                break;
            }
            
            visited.add(varName);
            current = substitution.get(varName);
        }
        
        return current;
   }
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
    
    @Override
    public Term resolveBindings(Map<String, Term> bindings) {
        if (bindings.containsKey(this.name)) {
            return bindings.get(this.name).resolveBindings(bindings);
        }
        return this;
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
