package it.denzosoft.jprolog;

import it.denzosoft.jprolog.terms.Atom;
import it.denzosoft.jprolog.terms.CompoundTerm;
import it.denzosoft.jprolog.terms.Term;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public final class CollectionUtils {

    // Prevent instantiation
    private CollectionUtils() {}

    /**
     * Generic list collector implementation for collection predicates.
     * 
     * @param collectorType The type of collector (findall, bagof, setof)
     * @param query The query term
     * @param bindings Current variable bindings
     * @param solutions Solution list to add to
     * @param querySolver Query solver for solving subgoals
     * @return true if successful
     */
    public static boolean genericListCollector(String collectorType, Term query, Map<String, Term> bindings,
                                               List<Map<String, Term>> solutions, QuerySolver querySolver) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException(collectorType + "/3 requires exactly 3 arguments.");
        }

        Term template = query.getArguments().get(0);
        Term goal = query.getArguments().get(1);
        Term listVariable = query.getArguments().get(2);

        List<Term> collectedTerms = new ArrayList<>();
        List<Map<String, Term>> tempSolutions = new ArrayList<>();
        
        try {
            // Solve the goal to get all solutions
            boolean success = querySolver.solve(goal, bindings, tempSolutions, CutStatus.notOccurred());
            if (!success && tempSolutions.isEmpty()) {
                return false;
            }
        } catch (Exception e) {
            throw new PrologEvaluationException("Error solving goal in " + collectorType + ": " + e.getMessage(), e);
        }

        // Process each solution to create the collected terms
        for (Map<String, Term> solution : tempSolutions) {
            Term resolvedTemplate = template.copy().resolveBindings(solution);
            collectedTerms.add(resolvedTemplate);
        }

        // Create the list term and unify with the list variable
        Term collectedList = createListTerm(collectedTerms);
        return listVariable.unify(collectedList, bindings);
    }

    /**
     * Create a Prolog list term from a Java list of terms.
     * 
     * @param terms The terms to include in the list
     * @return The list term representation
     */
    public static Term createListTerm(List<Term> terms) {
        if (terms == null || terms.isEmpty()) {
            return new Atom("[]");  // Empty list
        }
        
        // Start with empty list and build up
        Term result = new Atom("[]");
        for (int i = terms.size() - 1; i >= 0; i--) {
            List<Term> args = new ArrayList<>();
            args.add(terms.get(i));
            args.add(result);
            result = new CompoundTerm(new Atom("."), args);
        }
        return result;
    }
}
