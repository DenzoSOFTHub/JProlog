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
        int arity = query.getArguments().size();
        // START_CHANGE: ISS-2025-0220 - sort/4 with Key + Order
        if (arity == 4) {
            return executeSort4(query, bindings, solutions);
        }
        // END_CHANGE: ISS-2025-0220
        if (arity != 2) {
            throw new PrologEvaluationException("sort/2 or sort/4 expected.");
        }

        Term inputList = query.getArguments().get(0).resolveBindings(bindings);
        Term sortedList = query.getArguments().get(1);

        // ISS-2025-0335: sort by standard order of terms — a list need only be PROPER (complete),
        // not ground; unbound variables are valid elements (lowest in standard order).
        if (it.denzosoft.jprolog.core.util.ListUtils.isProperList(inputList)) {
            List<Term> elements = ListUtils.extractElements(inputList);

            Collections.sort(elements, Sort::compareTerms);
            List<Term> uniqueElements = new ArrayList<>();
            for (int i = 0; i < elements.size(); i++) {
                if (i == 0 || compareTerms(elements.get(i), elements.get(i - 1)) != 0) {
                    uniqueElements.add(elements.get(i));
                }
            }

            Term sortedListTerm = ListUtils.createList(uniqueElements);

            if (sortedList.unify(sortedListTerm, bindings)) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            return false;
        } else {
            // START_CHANGE: ISS-2025-0351 - ISO 8.4.3.3: instantiation_error on a partial list,
            // type_error(list, Culprit) on a non-list, instead of silent failure
            throw notAProperList(inputList, "sort/2");
            // END_CHANGE: ISS-2025-0351
        }
    }

    // START_CHANGE: ISS-2025-0351 - shared ISO error for sort/2, sort/4, msort/2, keysort/2:
    // a partial list (var tail, including a plain variable) raises instantiation_error, any
    // other non-proper-list raises type_error(list, Culprit). Cycle-safe spine walk.
    static it.denzosoft.jprolog.core.exceptions.PrologException notAProperList(Term list, String context) {
        java.util.IdentityHashMap<Term, Boolean> visited = new java.util.IdentityHashMap<>();
        Term tail = list;
        while (tail instanceof CompoundTerm
                && ((CompoundTerm) tail).getName().equals(".")
                && ((CompoundTerm) tail).getArguments().size() == 2) {
            if (visited.put(tail, Boolean.TRUE) != null) break; // cyclic spine: not a partial list
            tail = ((CompoundTerm) tail).getArguments().get(1);
        }
        if (tail instanceof Variable) {
            return new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError(context));
        }
        return new it.denzosoft.jprolog.core.exceptions.PrologException(
            it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("list", list, context));
    }
    // END_CHANGE: ISS-2025-0351

    // START_CHANGE: ISS-2025-0220 - sort(+Key, +Order, +List, -Sorted)
    private boolean executeSort4(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        Term keyT = query.getArguments().get(0).resolveBindings(bindings);
        Term orderT = query.getArguments().get(1).resolveBindings(bindings);
        Term inputList = query.getArguments().get(2).resolveBindings(bindings);
        Term resultVar = query.getArguments().get(3);

        // START_CHANGE: ISS-2025-0418 - ISO error terms for Key/Order validation (instantiation,
        // type and domain errors instead of generic PrologEvaluationException)
        if (keyT instanceof Variable || orderT instanceof Variable) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError("sort/4"));
        }
        if (!(keyT instanceof it.denzosoft.jprolog.core.terms.Number) || !((it.denzosoft.jprolog.core.terms.Number) keyT).isInteger()) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("integer", keyT, "sort/4"));
        }
        int key = (int) ((it.denzosoft.jprolog.core.terms.Number) keyT).longValue();
        if (key < 0) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.domainError("not_less_than_zero", keyT, "sort/4"));
        }
        if (!(orderT instanceof Atom)) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("atom", orderT, "sort/4"));
        }
        String order = ((Atom) orderT).getName();
        boolean ascending;
        boolean dedup;
        switch (order) {
            case "@<":  ascending = true;  dedup = true; break;
            case "@=<": ascending = true;  dedup = false; break;
            case "@>":  ascending = false; dedup = true; break;
            case "@>=": ascending = false; dedup = false; break;
            default:
                throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                    it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.domainError("order", orderT, "sort/4"));
        }
        // END_CHANGE: ISS-2025-0418
        // START_CHANGE: ISS-2025-0351 - ISO errors instead of silent failure (was ISS-2025-0335 return false)
        if (!it.denzosoft.jprolog.core.util.ListUtils.isProperList(inputList)) {
            throw notAProperList(inputList, "sort/4");
        }
        // END_CHANGE: ISS-2025-0351
        List<Term> elements = ListUtils.extractElements(inputList);
        // START_CHANGE: ISS-2025-0418 - validate every element's key up front: the comparator is
        // never invoked for lists of fewer than two elements, so sort(2, @<, [f(a)], L) must not
        // silently skip the check
        if (key > 0) {
            for (Term e : elements) {
                extractKey(e, key);
            }
        }
        // END_CHANGE: ISS-2025-0418
        java.util.Comparator<Term> cmp = (a, b) -> {
            Term ka = key == 0 ? a : extractKey(a, key);
            Term kb = key == 0 ? b : extractKey(b, key);
            int c = compareTerms(ka, kb);
            return ascending ? c : -c;
        };
        List<Term> sorted = new ArrayList<>(elements);
        sorted.sort(cmp);
        if (dedup) {
            List<Term> out = new ArrayList<>(sorted.size());
            for (int i = 0; i < sorted.size(); i++) {
                if (i == 0 || cmp.compare(sorted.get(i), sorted.get(i - 1)) != 0) out.add(sorted.get(i));
            }
            sorted = out;
        }
        Term sortedTerm = ListUtils.createList(sorted);
        if (resultVar.unify(sortedTerm, bindings)) {
            solutions.add(new HashMap<>(bindings));
            return true;
        }
        return false;
    }

    // START_CHANGE: ISS-2025-0418 - with Key > 0 every element must be a compound with at least
    // Key arguments (SWI semantics): non-compound -> type_error(compound, Elem), Key beyond the
    // arity -> domain_error(argument_index, Key) — never silently sort by the whole element.
    private static Term extractKey(Term t, int key) {
        if (!(t instanceof CompoundTerm)) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("compound", t, "sort/4"));
        }
        CompoundTerm ct = (CompoundTerm) t;
        if (key > ct.getArguments().size()) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.domainError("argument_index",
                    new it.denzosoft.jprolog.core.terms.Number((long) key), "sort/4"));
        }
        return ct.getArguments().get(key - 1);
    }
    // END_CHANGE: ISS-2025-0418
    // END_CHANGE: ISS-2025-0220

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
            // START_CHANGE: ISS-2025-0261 - ISO standard order: compare by value, and on a tie a
            // float sorts before an integer (they are distinct terms, so sort/2 must NOT dedup
            // 1 and 1.0). Integers compare exactly via BigInteger.
            it.denzosoft.jprolog.core.terms.Number n1 = (it.denzosoft.jprolog.core.terms.Number) t1;
            it.denzosoft.jprolog.core.terms.Number n2 = (it.denzosoft.jprolog.core.terms.Number) t2;
            int c;
            if (n1.isInteger() && n2.isInteger()) {
                c = n1.bigIntegerValue().compareTo(n2.bigIntegerValue());
            } else {
                c = Double.compare(n1.doubleValue(), n2.doubleValue());
            }
            if (c != 0) return c;
            if (n1.isInteger() == n2.isInteger()) return 0;
            return n1.isInteger() ? 1 : -1; // float (smaller) before int
            // END_CHANGE: ISS-2025-0261
        }
        if (t1 instanceof Atom) {
            return ((Atom) t1).getName().compareTo(((Atom) t2).getName());
        }
        if (t1 instanceof PrologString) {
            // START_CHANGE: ISS-2025-0348 - compare string CONTENT, not the escaped quoted form,
            // so sort/2 and compare/3 (StandardTermOrdering) agree on one total order.
            return ((PrologString) t1).getStringValue().compareTo(((PrologString) t2).getStringValue());
            // END_CHANGE: ISS-2025-0348
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
