package it.denzosoft.jprolog.util;

import it.denzosoft.jprolog.core.terms.*;

import java.util.Arrays;
import java.util.List;

/**
 * Utility methods for creating and manipulating terms.
 */
public class TermUtils {
    
    /**
     * Create a compound term with string functor and term arguments.
     * 
     * @param functor The functor name
     * @param args The arguments
     * @return The compound term
     */
    public static CompoundTerm createCompound(String functor, Term... args) {
        return new CompoundTerm(new Atom(functor), Arrays.asList(args));
    }
    
    /**
     * Create a compound term with string functor and term list.
     * 
     * @param functor The functor name
     * @param args The arguments list
     * @return The compound term
     */
    public static CompoundTerm createCompound(String functor, List<Term> args) {
        return new CompoundTerm(new Atom(functor), args);
    }
    
    /**
     * Get functor name from term (handles both Atom and CompoundTerm).
     * 
     * @param term The term
     * @return The functor name, or null if not applicable
     */
    public static String getFunctorName(Term term) {
        if (term instanceof Atom) {
            return ((Atom) term).getName();
        } else if (term instanceof CompoundTerm) {
            return ((CompoundTerm) term).getFunctor().getName();
        }
        return null;
    }
    
    /**
     * Get arity from term (handles both Atom and CompoundTerm).
     * 
     * @param term The term
     * @return The arity
     */
    public static int getArity(Term term) {
        if (term instanceof Atom) {
            return 0;
        } else if (term instanceof CompoundTerm) {
            return ((CompoundTerm) term).getArguments().size();
        }
        return 0;
    }
    
    /**
     * Get argument from compound term safely.
     * 
     * @param term The term
     * @param index The argument index
     * @return The argument, or null if not available
     */
    public static Term getArgument(Term term, int index) {
        if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            List<Term> args = compound.getArguments();
            if (index >= 0 && index < args.size()) {
                return args.get(index);
            }
        }
        return null;
    }
    
    /**
     * Check if term has specific functor and arity.
     * 
     * @param term The term
     * @param functor The functor name
     * @param arity The arity
     * @return true if matches
     */
    public static boolean hasFunctor(Term term, String functor, int arity) {
        return functor.equals(getFunctorName(term)) && arity == getArity(term);
    }
    
    /**
     * Extract elements from a Prolog list term.
     * 
     * @param listTerm The list term
     * @return List of elements
     */
    public static List<Term> extractListElements(Term listTerm) {
        List<Term> elements = new java.util.ArrayList<>();
        Term current = listTerm;
        
        while (hasFunctor(current, ".", 2)) {
            elements.add(getArgument(current, 0));
            current = getArgument(current, 1);
        }
        
        return elements;
    }
    
    /**
     * Create a Prolog list from Java list.
     * 
     * @param elements The elements
     * @return The Prolog list term
     */
    public static Term createList(List<Term> elements) {
        Term result = new Atom("[]");
        
        for (int i = elements.size() - 1; i >= 0; i--) {
            result = createCompound(".", elements.get(i), result);
        }
        
        return result;
    }
}