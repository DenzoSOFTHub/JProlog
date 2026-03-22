package it.denzosoft.jprolog.core.utils;

import it.denzosoft.jprolog.core.engine.CutStatus;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public final class CollectionUtils {

    // Prevent instantiation
    private CollectionUtils() {}

    // START_CHANGE: ISS-2025-0091 - Cache immutable atoms for list construction
    private static final Atom EMPTY_LIST = new Atom("[]");
    private static final Atom DOT = new Atom(".");
    // END_CHANGE: ISS-2025-0091

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
        Term rawGoal = query.getArguments().get(1);
        Term listVariable = query.getArguments().get(2);

        // START_CHANGE: ISS-2025-0107 - Resolve bindings on goal before solving (meta-variable support)
        // When goal is a variable bound to a term (e.g., findall(X, Goal, L) where Goal=member(X,[a,b,c])),
        // we must resolve it to the actual goal term before attempting to solve it.
        Term resolvedGoal = rawGoal.resolveBindings(bindings);
        // END_CHANGE: ISS-2025-0107

        // START_CHANGE: ISS-2025-0062 - Handle ^ existential quantification in bagof/setof
        // Strip existential quantification: Var^Goal -> Goal (ignore Var for grouping)
        Term goal = stripExistentialQuantification(resolvedGoal);
        // END_CHANGE: ISS-2025-0062

        List<Term> collectedTerms = new ArrayList<>();
        List<Map<String, Term>> tempSolutions = new ArrayList<>();
        
        try {
            // Solve the goal to get all solutions
            querySolver.solve(goal, bindings, tempSolutions, CutStatus.notOccurred());
            // START_CHANGE: ISS-2025-0072 - findall returns empty list on no solutions (ISO compliant)
            // ISO Prolog: findall/3 succeeds with empty list when goal has no solutions.
            // bagof/3 and setof/3 should fail when goal has no solutions.
            if (tempSolutions.isEmpty() && !"findall".equals(collectorType)) {
                return false;
            }
            // END_CHANGE: ISS-2025-0072
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
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (listVariable.unify(collectedList, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    // START_CHANGE: ISS-2025-0062 - Strip existential quantification from goal
    /**
     * Strip existential quantification operators (^) from a goal.
     * E.g., X^Y^goal(X,Y,Z) -> goal(X,Y,Z)
     */
    private static Term stripExistentialQuantification(Term goal) {
        if (goal instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) goal;
            if ("^".equals(ct.getName()) && ct.getArguments().size() == 2) {
                return stripExistentialQuantification(ct.getArguments().get(1));
            }
        }
        return goal;
    }
    // END_CHANGE: ISS-2025-0062

    /**
     * Create a Prolog list term from a Java list of terms.
     * 
     * @param terms The terms to include in the list
     * @return The list term representation
     */
    // START_CHANGE: ISS-2025-0118 - Convert Prolog list term to Java List
    /**
     * Convert a Prolog list term (.(H,T) chains ending in []) to a Java List.
     * Returns null if the term is not a proper list.
     */
    public static List<Term> termToList(Term term) {
        List<Term> result = new ArrayList<>();
        Term current = term;
        while (current instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) current;
            if (".".equals(ct.getName()) && ct.getArguments().size() == 2) {
                result.add(ct.getArguments().get(0));
                current = ct.getArguments().get(1);
            } else {
                return null;
            }
        }
        if (current instanceof Atom && "[]".equals(((Atom) current).getName())) {
            return result;
        }
        return null;
    }
    // END_CHANGE: ISS-2025-0118

    // START_CHANGE: ISS-2025-0091 - Reuse cached Atom instances, use Arrays.asList
    public static Term createListTerm(List<Term> terms) {
        if (terms == null || terms.isEmpty()) {
            return EMPTY_LIST;
        }

        // Start with empty list and build up using cached atoms
        Term result = EMPTY_LIST;
        for (int i = terms.size() - 1; i >= 0; i--) {
            result = new CompoundTerm(DOT, java.util.Arrays.asList(terms.get(i), result));
        }
        return result;
    }
    // END_CHANGE: ISS-2025-0091
}
