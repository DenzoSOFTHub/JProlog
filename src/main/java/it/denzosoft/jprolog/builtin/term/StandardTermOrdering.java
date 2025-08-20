package it.denzosoft.jprolog.builtin.term;

import it.denzosoft.jprolog.core.terms.*;

import java.util.List;

/**
 * Implementation of ISO 13211-1 standard term ordering.
 * 
 * Standard term order: Variables < Numbers < Atoms < Compound Terms
 * 
 * - Variables: alphabetically by name
 * - Numbers: by numeric value (integers < floats for same value)  
 * - Atoms: alphabetically by name
 * - Compound Terms: by arity, then functor name, then arguments left-to-right
 */
public class StandardTermOrdering {
    
    /**
     * Compare two terms according to ISO standard term ordering.
     * 
     * @param term1 First term
     * @param term2 Second term
     * @return -1 if term1 < term2, 0 if equal, +1 if term1 > term2
     */
    public static int compare(Term term1, Term term2) {
        // Handle null cases
        if (term1 == null && term2 == null) return 0;
        if (term1 == null) return -1;
        if (term2 == null) return 1;
        
        // Get term type ordering values
        int type1 = getTermTypeOrder(term1);
        int type2 = getTermTypeOrder(term2);
        
        // Different types - compare by type order
        if (type1 != type2) {
            return Integer.compare(type1, type2);
        }
        
        // Same type - compare within type
        switch (type1) {
            case 1: return compareVariables((Variable) term1, (Variable) term2);
            case 2: return compareNumbers((it.denzosoft.jprolog.core.terms.Number) term1, (it.denzosoft.jprolog.core.terms.Number) term2);
            case 3: return compareAtoms((Atom) term1, (Atom) term2);
            case 4: return compareCompoundTerms((CompoundTerm) term1, (CompoundTerm) term2);
            default: return 0;
        }
    }
    
    /**
     * Get the ordering value for a term type.
     * Variables=1, Numbers=2, Atoms=3, CompoundTerms=4
     */
    private static int getTermTypeOrder(Term term) {
        if (term instanceof Variable) return 1;
        if (term instanceof it.denzosoft.jprolog.core.terms.Number) return 2;
        if (term instanceof Atom) return 3;
        if (term instanceof CompoundTerm) return 4;
        return 5; // Unknown types go last
    }
    
    /**
     * Compare two variables alphabetically by name.
     */
    private static int compareVariables(Variable var1, Variable var2) {
        String name1 = var1.getName();
        String name2 = var2.getName();
        return name1.compareTo(name2);
    }
    
    /**
     * Compare two numbers by numeric value.
     * Integers < floats for same value (per ISO standard).
     */
    private static int compareNumbers(it.denzosoft.jprolog.core.terms.Number num1, it.denzosoft.jprolog.core.terms.Number num2) {
        double val1 = num1.getValue();
        double val2 = num2.getValue();
        
        // Compare numeric values first
        int valueCompare = Double.compare(val1, val2);
        if (valueCompare != 0) {
            return valueCompare;
        }
        
        // Same value - integers come before floats
        boolean isInt1 = isInteger(num1);
        boolean isInt2 = isInteger(num2);
        
        if (isInt1 && !isInt2) return -1; // integer < float
        if (!isInt1 && isInt2) return 1;  // float > integer
        return 0; // same type and value
    }
    
    /**
     * Check if a Number represents an integer value.
     */
    private static boolean isInteger(it.denzosoft.jprolog.core.terms.Number num) {
        double val = num.getValue();
        return val == Math.floor(val) && !Double.isInfinite(val);
    }
    
    /**
     * Compare two atoms alphabetically by name.
     */
    private static int compareAtoms(Atom atom1, Atom atom2) {
        String name1 = atom1.getName();
        String name2 = atom2.getName();
        return name1.compareTo(name2);
    }
    
    /**
     * Compare two compound terms:
     * 1. By arity (number of arguments)
     * 2. By functor name 
     * 3. By arguments left-to-right
     */
    private static int compareCompoundTerms(CompoundTerm comp1, CompoundTerm comp2) {
        // Compare by arity first
        List<Term> args1 = comp1.getArguments();
        List<Term> args2 = comp2.getArguments();
        
        int arity1 = (args1 != null) ? args1.size() : 0;
        int arity2 = (args2 != null) ? args2.size() : 0;
        
        int arityCompare = Integer.compare(arity1, arity2);
        if (arityCompare != 0) {
            return arityCompare;
        }
        
        // Same arity - compare by functor name
        String functor1 = comp1.getName();
        String functor2 = comp2.getName();
        
        int functorCompare = functor1.compareTo(functor2);
        if (functorCompare != 0) {
            return functorCompare;
        }
        
        // Same functor and arity - compare arguments left-to-right
        if (args1 != null && args2 != null) {
            for (int i = 0; i < args1.size(); i++) {
                int argCompare = compare(args1.get(i), args2.get(i));
                if (argCompare != 0) {
                    return argCompare;
                }
            }
        }
        
        return 0; // Completely identical terms
    }
    
    /**
     * Check if two terms are identical according to standard term ordering.
     */
    public static boolean identical(Term term1, Term term2) {
        return compare(term1, term2) == 0;
    }
    
    /**
     * Compare for less-than relationship.
     */
    public static boolean lessThan(Term term1, Term term2) {
        return compare(term1, term2) < 0;
    }
    
    /**
     * Compare for less-than-or-equal relationship.
     */
    public static boolean lessThanOrEqual(Term term1, Term term2) {
        return compare(term1, term2) <= 0;
    }
    
    /**
     * Compare for greater-than relationship.
     */
    public static boolean greaterThan(Term term1, Term term2) {
        return compare(term1, term2) > 0;
    }
    
    /**
     * Compare for greater-than-or-equal relationship.
     */
    public static boolean greaterThanOrEqual(Term term1, Term term2) {
        return compare(term1, term2) >= 0;
    }
}