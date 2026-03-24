package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.*;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class Sort implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("sort/2 requires exactly 2 arguments.");
        }

        Term inputList = query.getArguments().get(0).resolveBindings(bindings);
        Term sortedList = query.getArguments().get(1);

        if (inputList.isGround()) {
            List<Term> elements = ListUtils.extractElements(inputList);

            // START_CHANGE: ISS-2025-0184 - ISO standard term ordering and structural dedup
            // Remove duplicates using structural equality (toString comparison)
            List<Term> uniqueElements = new ArrayList<>();
            for (Term term : elements) {
                boolean found = false;
                for (Term existing : uniqueElements) {
                    if (existing.toString().equals(term.toString())) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    uniqueElements.add(term);
                }
            }

            Collections.sort(uniqueElements, Sort::compareTerms);
            // END_CHANGE: ISS-2025-0184

            Term sortedListTerm = ListUtils.createList(uniqueElements);

            if (sortedList.unify(sortedListTerm, bindings)) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            return false;
        } else {
            return false;
        }
    }

    // START_CHANGE: ISS-2025-0184 - ISO standard order of terms
    /**
     * Compare two terms using ISO standard order:
     * Variables < Numbers < Atoms < Strings < Compound terms.
     * Numbers compared numerically, atoms lexicographically,
     * compounds by arity, then functor name, then arguments.
     */
    public static int compareTerms(Term t1, Term t2) {
        int rank1 = termRank(t1);
        int rank2 = termRank(t2);
        if (rank1 != rank2) return Integer.compare(rank1, rank2);

        // Same category
        if (t1 instanceof Variable) {
            return ((Variable) t1).getName().compareTo(((Variable) t2).getName());
        }
        if (t1 instanceof it.denzosoft.jprolog.core.terms.Number) {
            double d1 = ((it.denzosoft.jprolog.core.terms.Number) t1).doubleValue();
            double d2 = ((it.denzosoft.jprolog.core.terms.Number) t2).doubleValue();
            return Double.compare(d1, d2);
        }
        if (t1 instanceof Atom) {
            return ((Atom) t1).getName().compareTo(((Atom) t2).getName());
        }
        if (t1 instanceof PrologString) {
            return t1.toString().compareTo(t2.toString());
        }
        if (t1 instanceof CompoundTerm && t2 instanceof CompoundTerm) {
            CompoundTerm c1 = (CompoundTerm) t1;
            CompoundTerm c2 = (CompoundTerm) t2;
            int a1 = c1.getArguments() != null ? c1.getArguments().size() : 0;
            int a2 = c2.getArguments() != null ? c2.getArguments().size() : 0;
            if (a1 != a2) return Integer.compare(a1, a2);
            int fc = c1.getFunctor().getName().compareTo(c2.getFunctor().getName());
            if (fc != 0) return fc;
            for (int i = 0; i < a1; i++) {
                int ac = compareTerms(c1.getArguments().get(i), c2.getArguments().get(i));
                if (ac != 0) return ac;
            }
            return 0;
        }
        return t1.toString().compareTo(t2.toString());
    }

    private static int termRank(Term t) {
        if (t instanceof Variable) return 0;
        if (t instanceof it.denzosoft.jprolog.core.terms.Number) return 1;
        if (t instanceof Atom) return 2;
        if (t instanceof PrologString) return 3;
        if (t instanceof CompoundTerm) return 4;
        return 5;
    }
    // END_CHANGE: ISS-2025-0184
}
