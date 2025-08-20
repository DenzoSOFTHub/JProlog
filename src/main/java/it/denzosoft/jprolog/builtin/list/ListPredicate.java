package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.List;
import java.util.Map;

public abstract class ListPredicate implements BuiltIn {
    
    /**
     * Extract elements from a Prolog list term.
     * 
     * @param list The list term to extract elements from
     * @return List of elements, or empty list for malformed lists
     */
    protected List<Term> extractElements(Term list) {
        return ListUtils.extractElements(list);
    }
    
    /**
     * Check if a term represents the empty list.
     * 
     * @param term The term to check
     * @return true if term is the empty list atom []
     */
    protected boolean isEmptyList(Term term) {
        return ListUtils.isEmptyList(term);
    }
    
    /**
     * Create a Prolog list term from a list of elements.
     * 
     * @param elements The elements to include in the list
     * @return The list term representation
     */
    protected Term createList(List<Term> elements) {
        return ListUtils.createList(elements);
    }
    
    /**
     * Validate that the predicate has the expected number of arguments.
     * 
     * @param query The query term
     * @param expectedArgs The expected number of arguments
     * @throws PrologEvaluationException if argument count doesn't match
     */
    protected void validateArgumentCount(Term query, int expectedArgs) {
        if (query.getArguments().size() != expectedArgs) {
            throw new PrologEvaluationException(
                String.format("%s/%d requires exactly %d arguments.", 
                    query.getName(), expectedArgs, expectedArgs));
        }
    }
    
    /**
     * Validate that a term is ground.
     * 
     * @param term The term to validate
     * @param predicateName The name of the predicate for error messages
     * @param argumentPosition The position of the argument (1-based)
     * @throws PrologEvaluationException if the term is not ground
     */
    protected void validateGround(Term term, String predicateName, int argumentPosition) {
        if (!term.isGround()) {
            throw new PrologEvaluationException(
                String.format("%s/%d: Argument %d must be ground.", 
                    predicateName, getExpectedArity(), argumentPosition));
        }
    }
    
    /**
     * Get the expected arity of this predicate.
     * Subclasses should override this method.
     * 
     * @return The expected arity
     */
    protected int getExpectedArity() {
        return 0; // Should be overridden
    }
}
