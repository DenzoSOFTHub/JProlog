package it.denzosoft.jprolog.core.util;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class ListUtils {

    // START_CHANGE: ISS-2025-0076 - Optimize list construction
    /** Cached empty list atom to avoid repeated allocations */
    private static final Atom EMPTY_LIST = new Atom("[]");

    /** Cached dot functor atom to avoid repeated allocations */
    private static final Atom DOT_FUNCTOR = new Atom(".");
    // END_CHANGE: ISS-2025-0076

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

    // START_CHANGE: ISS-2025-0245 - Proper-list (closed-spine) test for append/3 mode selection
    /**
     * Check if a term is a proper list: a (possibly empty) chain of '.'/2 cons
     * cells whose final tail is the empty list []. This is a STRUCTURAL test on
     * the spine only; list elements may be unbound variables. Unlike isGround(),
     * a partial list such as [a|T] (T unbound) is NOT a proper list.
     *
     * @param term The (already binding-resolved) term to check
     * @return true if term is a closed-spine proper list
     */
    public static boolean isProperList(Term term) {
        Term current = term;
        while (current instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) current;
            if (c.getName().equals(".") && c.getArguments().size() == 2) {
                current = c.getArguments().get(1);
            } else {
                return false;
            }
        }
        return isEmptyList(current);
    }
    // END_CHANGE: ISS-2025-0245

    // START_CHANGE: ISS-2025-0076 - Optimize list construction
    /**
     * Create a Prolog list term from a list of elements.
     * Uses cached atoms and Arrays.asList to avoid per-element ArrayList allocation.
     *
     * @param elements The elements to include in the list
     * @return The list term representation
     */
    public static Term createList(List<Term> elements) {
        Term result = EMPTY_LIST;
        for (int i = elements.size() - 1; i >= 0; i--) {
            result = new CompoundTerm(DOT_FUNCTOR, Arrays.asList(elements.get(i), result));
        }
        return result;
    }

    /**
     * Create a Prolog list term from a list of elements with a custom tail.
     * Uses cached atoms and Arrays.asList to avoid per-element ArrayList allocation.
     *
     * @param elements The elements to include in the list
     * @param tail The tail of the list
     * @return The list term representation
     */
    public static Term createListWithTail(List<Term> elements, Term tail) {
        Term result = tail;
        for (int i = elements.size() - 1; i >= 0; i--) {
            result = new CompoundTerm(DOT_FUNCTOR, Arrays.asList(elements.get(i), result));
        }
        return result;
    }
    // END_CHANGE: ISS-2025-0076
}
