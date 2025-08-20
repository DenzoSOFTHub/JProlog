package it.denzosoft.jprolog.core.module;

import java.util.Objects;

/**
 * Represents a predicate signature (functor/arity) for module system.
 */
public class PredicateSignature {
    
    private final String functor;
    private final int arity;
    
    /**
     * Create a predicate signature.
     * 
     * @param functor The predicate functor
     * @param arity The predicate arity
     */
    public PredicateSignature(String functor, int arity) {
        this.functor = Objects.requireNonNull(functor, "Functor cannot be null");
        if (arity < 0) {
            throw new IllegalArgumentException("Arity cannot be negative");
        }
        this.arity = arity;
    }
    
    /**
     * Parse a predicate signature from string format "functor/arity".
     * 
     * @param signature The signature string
     * @return The parsed signature
     */
    public static PredicateSignature parse(String signature) {
        if (signature == null || signature.trim().isEmpty()) {
            throw new IllegalArgumentException("Signature cannot be null or empty");
        }
        
        int slashIndex = signature.lastIndexOf('/');
        if (slashIndex == -1) {
            throw new IllegalArgumentException("Invalid signature format: " + signature);
        }
        
        String functor = signature.substring(0, slashIndex).trim();
        String arityStr = signature.substring(slashIndex + 1).trim();
        
        try {
            int arity = Integer.parseInt(arityStr);
            return new PredicateSignature(functor, arity);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid arity in signature: " + signature);
        }
    }
    
    /**
     * Get the functor.
     * 
     * @return The functor
     */
    public String getFunctor() {
        return functor;
    }
    
    /**
     * Get the arity.
     * 
     * @return The arity
     */
    public int getArity() {
        return arity;
    }
    
    /**
     * Check if this signature matches a functor and arity.
     * 
     * @param functor The functor to check
     * @param arity The arity to check
     * @return true if matches
     */
    public boolean matches(String functor, int arity) {
        return this.functor.equals(functor) && this.arity == arity;
    }
    
    @Override
    public String toString() {
        return functor + "/" + arity;
    }
    
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof PredicateSignature)) return false;
        PredicateSignature that = (PredicateSignature) o;
        return arity == that.arity && Objects.equals(functor, that.functor);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(functor, arity);
    }
}