package it.denzosoft.jprolog.core.utils;

import it.denzosoft.jprolog.builtin.list.Sort;
import it.denzosoft.jprolog.core.engine.CutStatus;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

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
        Term resolvedGoal = rawGoal.resolveBindings(bindings);
        // END_CHANGE: ISS-2025-0107

        // START_CHANGE: ISS-2025-0196 - bagof/setof: collect existential vars + witness grouping
        Set<String> existentialVars = new HashSet<>();
        Term goal = stripExistentialAndCollect(resolvedGoal, existentialVars);
        boolean isFindall = "findall".equals(collectorType);
        boolean isSetof = "setof".equals(collectorType);

        List<Map<String, Term>> tempSolutions = new ArrayList<>();
        try {
            querySolver.solve(goal, bindings, tempSolutions, CutStatus.notOccurred());
            if (tempSolutions.isEmpty() && !isFindall) {
                return false;
            }
        } catch (PrologException e) {
            throw e;
        } catch (Exception e) {
            throw new PrologEvaluationException("Error solving goal in " + collectorType + ": " + e.getMessage(), e);
        }

        if (isFindall) {
            List<Term> collected = new ArrayList<>();
            for (Map<String, Term> sol : tempSolutions) {
                collected.add(template.copy().resolveBindings(sol));
            }
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (listVariable.unify(createListTerm(collected), newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        }

        // bagof/setof — compute witness (free) variables: vars(Goal) - vars(Template) - existential - pre-bound
        Set<String> templateVars = new LinkedHashSet<>();
        collectVars(template, templateVars);
        Set<String> goalVars = new LinkedHashSet<>();
        collectVars(goal, goalVars);
        Set<String> witnessVars = new LinkedHashSet<>(goalVars);
        witnessVars.removeAll(templateVars);
        witnessVars.removeAll(existentialVars);
        // Exclude variables already bound to non-variable terms on entry
        witnessVars.removeIf(v -> {
            Term b = bindings.get(v);
            return b != null && !(b instanceof Variable);
        });

        if (witnessVars.isEmpty()) {
            // No grouping needed — one solution
            List<Term> collected = new ArrayList<>();
            for (Map<String, Term> sol : tempSolutions) {
                collected.add(template.copy().resolveBindings(sol));
            }
            if (isSetof) collected = sortAndDedup(collected);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (listVariable.unify(createListTerm(collected), newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        }

        // Group by witness binding signature; preserve discovery order via LinkedHashMap
        Map<String, List<Term>> groups = new LinkedHashMap<>();
        Map<String, Map<String, Term>> groupWitness = new LinkedHashMap<>();
        for (Map<String, Term> sol : tempSolutions) {
            Map<String, Term> witness = new TreeMap<>();
            for (String wv : witnessVars) {
                Term v = sol.containsKey(wv) ? sol.get(wv).resolveBindings(sol) : new Variable(wv);
                witness.put(wv, v);
            }
            StringBuilder sig = new StringBuilder();
            for (Map.Entry<String, Term> e : witness.entrySet()) {
                sig.append(e.getKey()).append('=').append(termCanonical(e.getValue())).append(';');
            }
            String key = sig.toString();
            groups.computeIfAbsent(key, k -> new ArrayList<>())
                  .add(template.copy().resolveBindings(sol));
            groupWitness.putIfAbsent(key, witness);
        }

        // For setof, also sort the GROUPS by witness-binding order (ISO §8.10.3 says results enumerated in standard order)
        List<String> groupOrder = new ArrayList<>(groups.keySet());
        if (isSetof) {
            groupOrder.sort(java.util.Comparator.naturalOrder());
        }

        boolean any = false;
        for (String key : groupOrder) {
            List<Term> bag = groups.get(key);
            if (isSetof) bag = sortAndDedup(bag);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            boolean ok = true;
            for (Map.Entry<String, Term> we : groupWitness.get(key).entrySet()) {
                Variable v = new Variable(we.getKey());
                if (!v.unify(we.getValue(), newBindings)) { ok = false; break; }
            }
            if (!ok) continue;
            if (listVariable.unify(createListTerm(bag), newBindings)) {
                solutions.add(newBindings);
                any = true;
            }
        }
        return any;
        // END_CHANGE: ISS-2025-0196
    }

    // START_CHANGE: ISS-2025-0195 - sort+dedup helper for setof/3
    private static List<Term> sortAndDedup(List<Term> in) {
        List<Term> sorted = new ArrayList<>(in);
        sorted.sort(Sort::compareTerms);
        List<Term> out = new ArrayList<>(sorted.size());
        for (int i = 0; i < sorted.size(); i++) {
            if (i == 0 || Sort.compareTerms(sorted.get(i), sorted.get(i - 1)) != 0) {
                out.add(sorted.get(i));
            }
        }
        return out;
    }

    private static String termCanonical(Term t) {
        // Stable canonical key — uses toString of resolved term; sufficient for grouping
        return t == null ? "<null>" : t.toString();
    }
    // END_CHANGE: ISS-2025-0195

    // START_CHANGE: ISS-2025-0196 - collect variables in a term
    private static void collectVars(Term t, Set<String> out) {
        if (t instanceof Variable) {
            String n = ((Variable) t).getName();
            if (n != null && !"_".equals(n)) out.add(n);
        } else if (t instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) t;
            if (ct.getArguments() != null) {
                for (Term a : ct.getArguments()) collectVars(a, out);
            }
        }
    }
    // END_CHANGE: ISS-2025-0196

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

    // START_CHANGE: ISS-2025-0196 - strip ^ and record existential variable names
    private static Term stripExistentialAndCollect(Term goal, Set<String> existentialVars) {
        while (goal instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) goal;
            if ("^".equals(ct.getName()) && ct.getArguments().size() == 2) {
                collectVars(ct.getArguments().get(0), existentialVars);
                goal = ct.getArguments().get(1);
            } else {
                break;
            }
        }
        return goal;
    }
    // END_CHANGE: ISS-2025-0196

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
