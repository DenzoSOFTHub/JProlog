package it.denzosoft.jprolog.core.terms;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;



public class CompoundTerm extends Term {

    private Atom functor;
    private List<Term> arguments;
    // START_CHANGE: ISS-2025-0091 - Cache unmodifiable view to avoid wrapping on every call
    private List<Term> unmodifiableArguments;
    // END_CHANGE: ISS-2025-0091

    public CompoundTerm(Atom functor, List<Term> arguments) {
        this.functor = functor;
        this.arguments = new ArrayList<>(arguments); // Make a copy
    }

    public Atom getFunctor() {
        return functor;
    }

    // START_CHANGE: ISS-2025-0091 - Cache unmodifiable view, create once on first access
    @Override
    public List<Term> getArguments() {
        if (unmodifiableArguments == null) {
            unmodifiableArguments = Collections.unmodifiableList(arguments);
        }
        return unmodifiableArguments;
    }
    // END_CHANGE: ISS-2025-0091

    @Override
    public String getName() {
        return functor.getName();
    }

    // START_CHANGE: ISS-2025-0091 - Optimize unification with key-snapshot rollback
    // Instead of copying the entire HashMap (keys + values + rehash), we snapshot
    // only the key set. On success (common case), we save the putAll cost entirely.
    // On failure, we rollback by removing keys not in the snapshot.
    @Override
    public boolean unify(Term term, Map<String, Term> substitution) {
        if (term instanceof Variable) {
            return term.unify(this, substitution);
        } else if (term instanceof CompoundTerm) {
            CompoundTerm otherCompound = (CompoundTerm) term;
            if (!this.functor.getName().equals(otherCompound.functor.getName()) ||
                this.arguments.size() != otherCompound.arguments.size()) {
                return false;
            }

            // Snapshot existing keys for rollback (cheaper than full HashMap copy)
            java.util.Set<String> savedKeys = new java.util.HashSet<>(substitution.keySet());

            for (int i = 0; i < this.arguments.size(); i++) {
                if (!this.arguments.get(i).unify(otherCompound.arguments.get(i), substitution)) {
                    // Rollback: remove all bindings added during this compound unification
                    if (substitution.size() > savedKeys.size()) {
                        substitution.keySet().retainAll(savedKeys);
                    }
                    return false;
                }
            }
            // Success: all bindings already in the map, no putAll needed
            return true;
        } else {
            return false;
        }
    }
    // END_CHANGE: ISS-2025-0091

    @Override
    public boolean isGround() {
        for (Term arg : arguments) {
            if (!arg.isGround()) {
                return false;
            }
        }
        return true;
    }

    @Override
    public String toString() {
        if (arguments.isEmpty()) {
            return functor.toString();
        }
        
        // START_CHANGE: ISS-2025-0019 - Improved list representation with ISO-compliant formatting
        // Special formatting for Prolog lists (functor "." with 2 arguments)
        if (".".equals(functor.getName()) && arguments.size() == 2) {
            return formatAsList();
        }
        // END_CHANGE: ISS-2025-0019
        
        // START_CHANGE: ISS-2025-0091 - Use StringBuilder instead of stream for toString
        StringBuilder sb = new StringBuilder(functor.getName()).append('(');
        for (int i = 0; i < arguments.size(); i++) {
            if (i > 0) sb.append(", ");
            sb.append(arguments.get(i).toString());
        }
        sb.append(')');
        return sb.toString();
        // END_CHANGE: ISS-2025-0091
    }
    
    // START_CHANGE: ISS-2025-0019 - Helper method for formatting lists in ISO-compliant way
    /**
     * Format this compound term as an ISO-compliant Prolog list [a,b,c]
     * Only called when this is a list structure (functor "." with 2 args)
     */
    private String formatAsList() {
        List<String> elements = new ArrayList<>();
        Term current = this;
        
        // Traverse the list structure collecting elements
        while (current instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) current;
            if (!".".equals(compound.functor.getName()) || compound.arguments.size() != 2) {
                break; // Not a proper list structure
            }
            
            elements.add(compound.arguments.get(0).toString());
            current = compound.arguments.get(1);
        }
        
        // Handle the tail
        if (current instanceof Atom && "[]".equals(((Atom) current).getName())) {
            // Proper list ending with []
            return "[" + String.join(", ", elements) + "]";
        } else {
            // Improper list with non-empty tail  
            return "[" + String.join(", ", elements) + "|" + current.toString() + "]";
        }
    }
    // END_CHANGE: ISS-2025-0019

    @Override
    public Term copy() {
        List<Term> copiedArguments = new ArrayList<>();
        for (Term arg : arguments) {
            copiedArguments.add(arg.copy());
        }
        return new CompoundTerm(functor, copiedArguments);
    }
    
    @Override
    public Term resolveBindings(Map<String, Term> bindings) {
        List<Term> resolvedArguments = new ArrayList<>();
        for (Term arg : arguments) {
            resolvedArguments.add(arg.resolveBindings(bindings));
        }
        return new CompoundTerm(functor, resolvedArguments);
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        CompoundTerm that = (CompoundTerm) obj;
        return functor.equals(that.functor) && arguments.equals(that.arguments);
    }
    
    @Override
    public int hashCode() {
        return java.util.Objects.hash(functor, arguments);
    }
}
