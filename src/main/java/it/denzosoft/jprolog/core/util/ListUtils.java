package it.denzosoft.jprolog.core.util;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.ArrayList;
import java.util.List;

public final class ListUtils {
    
    // Prevent instantiation
    private ListUtils() {}
    
    /**
     * Extract elements from a Prolog list term.
     * 
     * @param list The list term to extract elements from
     * @return List of elements, or empty list for malformed lists
     */
    public static List<Term> extractElements(Term list) {
        List<Term> elements = new ArrayList<>();
        Term current = list;
        
        while (current instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) current;
            if (compound.getName().equals(".") && compound.getArguments().size() == 2) {
                elements.add(compound.getArguments().get(0));
                current = compound.getArguments().get(1);
            } else {
                break;
            }
        }
        
        return elements;
    }
    
    /**
     * Check if a term represents the empty list.
     * 
     * @param term The term to check
     * @return true if term is the empty list atom []
     */
    public static boolean isEmptyList(Term term) {
        return term instanceof Atom && ((Atom) term).getName().equals("[]");
    }
    
    /**
     * Create a Prolog list term from a list of elements.
     * 
     * @param elements The elements to include in the list
     * @return The list term representation
     */
    public static Term createList(List<Term> elements) {
        Term result = new Atom("[]");
        for (int i = elements.size() - 1; i >= 0; i--) {
            List<Term> args = new ArrayList<>();
            args.add(elements.get(i));
            args.add(result);
            result = new CompoundTerm(new Atom("."), args);
        }
        return result;
    }
    
    /**
     * Create a Prolog list term from a list of elements with a custom tail.
     * 
     * @param elements The elements to include in the list
     * @param tail The tail of the list
     * @return The list term representation
     */
    public static Term createListWithTail(List<Term> elements, Term tail) {
        Term result = tail;
        for (int i = elements.size() - 1; i >= 0; i--) {
            List<Term> args = new ArrayList<>();
            args.add(elements.get(i));
            args.add(result);
            result = new CompoundTerm(new Atom("."), args);
        }
        return result;
    }
}
