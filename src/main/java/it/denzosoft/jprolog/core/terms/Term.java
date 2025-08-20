package it.denzosoft.jprolog.core.terms;

import java.util.List;
import java.util.Map;


public abstract class Term {

    public abstract boolean unify(Term term, Map<String, Term> substitution);
    public abstract boolean isGround();
    public abstract Term copy();

    @Override
    public abstract java.lang.String toString();

    // Optional: If all Term subtypes should support numeric evaluation
    public Double getValue() {
        return null; // Or throw UnsupportedOperationException if not applicable
    }

    // Optional: Helper methods for subclasses to avoid casting
    public java.lang.String getName() { return null; } //For Atom and CompoundTerm
    public List<Term> getArguments() { return null; } //For CompoundTerm.
    
    public Term resolveBindings(Map<java.lang.String, Term> bindings) {
        return this; // Default implementation - should be overridden by subclasses that need it
    }
    
    @Override
    public abstract boolean equals(Object obj);
    
    @Override
    public abstract int hashCode();
}
