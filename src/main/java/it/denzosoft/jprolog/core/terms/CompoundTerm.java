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

    // START_CHANGE: ISS-2025-0096 - Optimized compound unification rollback
    // Fast path for LayeredMap: uses mark/rollback (O(K) where K = additions).
    // Standard HashMap path: uses key snapshot + retainAll (unchanged semantics).
    // On success (common case in matching rules): no rollback needed.
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

            int argCount = this.arguments.size();

            // Fast path for LayeredMap: use O(K) mark/rollback
            if (substitution instanceof it.denzosoft.jprolog.core.engine.LayeredMap) {
                it.denzosoft.jprolog.core.engine.LayeredMap layered =
                    (it.denzosoft.jprolog.core.engine.LayeredMap) substitution;
                int mark = layered.mark();
                for (int i = 0; i < argCount; i++) {
                    if (!this.arguments.get(i).unify(otherCompound.arguments.get(i), substitution)) {
                        layered.rollbackToMark(mark);
                        return false;
                    }
                }
                return true;
            }

            // START_CHANGE: ISS-2025-0163 - Correct rollback: snapshot full map, not just keys
            // retainAll(savedKeys) only removes added keys but doesn't restore overwritten values.
            // Full snapshot ensures correct rollback even if values are overwritten.
            java.util.Map<String, Term> snapshot = new java.util.HashMap<>(substitution);
            for (int i = 0; i < argCount; i++) {
                if (!this.arguments.get(i).unify(otherCompound.arguments.get(i), substitution)) {
                    // Restore substitution to pre-unification state
                    substitution.clear();
                    substitution.putAll(snapshot);
                    return false;
                }
            }
            // END_CHANGE: ISS-2025-0163
            return true;
        } else {
            return false;
        }
    }
    // END_CHANGE: ISS-2025-0096

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
    
    // START_CHANGE: ISS-2025-0100 - Skip allocation when no arguments change
    @Override
    public Term resolveBindings(Map<String, Term> bindings) {
        // First pass: check if anything actually changes
        int argCount = arguments.size();
        List<Term> resolvedArguments = null; // Lazy allocation
        for (int i = 0; i < argCount; i++) {
            Term arg = arguments.get(i);
            Term resolved = arg.resolveBindings(bindings);
            if (resolved != arg && resolvedArguments == null) {
                // Something changed — allocate and backfill
                resolvedArguments = new ArrayList<>(argCount);
                for (int j = 0; j < i; j++) {
                    resolvedArguments.add(arguments.get(j));
                }
            }
            if (resolvedArguments != null) {
                resolvedArguments.add(resolved);
            }
        }
        if (resolvedArguments == null) {
            return this; // Nothing changed — no allocation
        }
        return new CompoundTerm(functor, resolvedArguments);
    }
    // END_CHANGE: ISS-2025-0100
    
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
