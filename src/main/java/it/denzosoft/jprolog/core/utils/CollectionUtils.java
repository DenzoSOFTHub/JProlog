package it.denzosoft.jprolog.core.utils;

// START_CHANGE: ISS-2025-0384 - ISO error terms for non-callable goals
import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
// END_CHANGE: ISS-2025-0384
import it.denzosoft.jprolog.builtin.list.Sort;
import it.denzosoft.jprolog.core.engine.SolverContext;
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
                                               List<Map<String, Term>> solutions, SolverContext querySolver) {
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

        // START_CHANGE: ISS-2025-0384 - ISO 8.10.1.3/8.10.2.3/8.10.3.3: Goal (after stripping
        // ^/2 quantifiers) must be callable: instantiation_error when unbound, type_error(callable)
        // when it is neither an atom nor a compound term.
        if (goal instanceof Variable) {
            throw new PrologException(ISOErrorTerms.instantiationError(collectorType + "/3"));
        }
        if (!(goal instanceof Atom) && !(goal instanceof CompoundTerm)) {
            throw new PrologException(ISOErrorTerms.typeError("callable", goal, collectorType + "/3"));
        }
        // END_CHANGE: ISS-2025-0384

        // START_CHANGE: ISS-2025-0416 - ISO 8.10.1.3(c): Instances must be a list or a partial
        // list, otherwise type_error(list, Instances) — never silent failure.
        if (isFindall) {
            checkInstancesArgument(listVariable.resolveBindings(bindings), "findall/3");
        }
        // END_CHANGE: ISS-2025-0416

        List<Map<String, Term>> tempSolutions = new ArrayList<>();
        try {
            querySolver.solveMeta(goal, bindings, tempSolutions)   /* ISS-2025-0431 - ENG-04 */;
            if (tempSolutions.isEmpty() && !isFindall) {
                return false;
            }
        } catch (PrologException e) {
            throw e;
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw new PrologEvaluationException("Error solving goal in " + collectorType + ": " + e.getMessage(), e);
        }

        if (isFindall) {
            List<Term> collected = new ArrayList<>();
            for (Map<String, Term> sol : tempSolutions) {
                collected.add(collectInstance(template, sol));
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
                collected.add(collectInstance(template, sol));
            }
            if (isSetof) collected = sortAndDedup(collected);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (listVariable.unify(createListTerm(collected), newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        }

        // START_CHANGE: ISS-2025-0411/ISS-2025-0412 - ISO 8.10.2.1 witness grouping: a solution
        // joins an existing group when its witness tuple is a VARIANT of the group's witness
        // (witnesses are then unified, merging unbound witness variables), and setof/3 enumerates
        // the groups in the STANDARD ORDER of the witness terms — not by string signatures.
        List<String> wvOrder = new ArrayList<>(witnessVars);
        Collections.sort(wvOrder);

        List<List<List<Term>>> groupTuples = new ArrayList<>();   // per group: all member witness tuples
        List<List<Term>> groupBags = new ArrayList<>();           // per group: collected template instances
        for (Map<String, Term> sol : tempSolutions) {
            List<Term> tuple = new ArrayList<>(wvOrder.size());
            for (String wv : wvOrder) {
                Term v = sol.containsKey(wv) ? sol.get(wv).resolveBindings(sol) : new Variable(wv);
                tuple.add(v);
            }
            int gi = -1;
            for (int i = 0; i < groupTuples.size(); i++) {
                if (isVariantTuple(groupTuples.get(i).get(0), tuple)) { gi = i; break; }
            }
            if (gi < 0) {
                List<List<Term>> tuples = new ArrayList<>();
                tuples.add(tuple);
                groupTuples.add(tuples);
                groupBags.add(new ArrayList<>());
                gi = groupTuples.size() - 1;
            } else {
                groupTuples.get(gi).add(tuple);
            }
            groupBags.get(gi).add(collectInstance(template, sol));
        }

        // bagof/3 keeps discovery order; setof/3 enumerates groups in the standard order of the
        // (representative) witness tuples (ISO 8.10.3.1 sorts the Witness+Template pairs).
        List<Integer> groupOrder = new ArrayList<>();
        for (int i = 0; i < groupTuples.size(); i++) groupOrder.add(i);
        if (isSetof) {
            groupOrder.sort((i, j) -> compareTuples(groupTuples.get(i).get(0), groupTuples.get(j).get(0)));
        }

        boolean any = false;
        for (int gi : groupOrder) {
            List<Term> bag = groupBags.get(gi);
            if (isSetof) bag = sortAndDedup(bag);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            boolean ok = true;
            // Unify every member tuple with the witness variables: the first tuple binds them,
            // the later (variant) tuples unify against it — merging witness variables (A = B).
            for (List<Term> tuple : groupTuples.get(gi)) {
                for (int k = 0; k < wvOrder.size() && ok; k++) {
                    Variable v = new Variable(wvOrder.get(k));
                    if (!v.unify(tuple.get(k), newBindings)) ok = false;
                }
                if (!ok) break;
            }
            if (!ok) continue;
            if (listVariable.unify(createListTerm(bag), newBindings)) {
                solutions.add(newBindings);
                any = true;
            }
        }
        return any;
        // END_CHANGE: ISS-2025-0411/ISS-2025-0412
        // END_CHANGE: ISS-2025-0196
    }

    // START_CHANGE: ISS-2025-0382 - collected instances must be renamed-apart copies (ISO 8.10.x:
    // findall/bagof/setof collect *instances* of the template; variables left unbound by a solution
    // must come out as fresh variables, never as aliases of the caller's template variables).
    // Variable.copy() preserves the name and binding maps are name-keyed, so a plain copy aliases
    // the caller's variables; rename them apart with a per-instance unique prefix (mirrors the v2
    // engine's own native findall renaming).
    private static final java.util.concurrent.atomic.AtomicInteger RENAME_COUNTER =
            new java.util.concurrent.atomic.AtomicInteger();

    private static Term collectInstance(Term template, Map<String, Term> sol) {
        Term instance = template.copy().resolveBindings(sol);
        return renameApart(instance, RENAME_COUNTER.getAndIncrement(), new HashMap<>());
    }

    private static Term renameApart(Term t, int id, Map<String, Variable> map) {
        if (t instanceof Variable) {
            String name = ((Variable) t).getName();
            return map.computeIfAbsent(name, nm -> new Variable("_C" + id + "_" + nm));
        }
        if (t instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) t;
            List<Term> args = new ArrayList<>(ct.getArguments().size());
            for (Term a : ct.getArguments()) args.add(renameApart(a, id, map));
            return new CompoundTerm(new Atom(ct.getName()), args);
        }
        return t;
    }
    // END_CHANGE: ISS-2025-0382

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

    // END_CHANGE: ISS-2025-0195

    // START_CHANGE: ISS-2025-0411/ISS-2025-0412 - variant test + standard-order comparison on
    // witness tuples (replaces the toString signature grouping of ISS-2025-0195/0196).
    private static int compareTuples(List<Term> a, List<Term> b) {
        for (int i = 0; i < a.size(); i++) {
            int c = it.denzosoft.jprolog.builtin.term.StandardTermOrdering.compare(a.get(i), b.get(i));
            if (c != 0) return c;
        }
        return 0;
    }

    private static boolean isVariantTuple(List<Term> a, List<Term> b) {
        Map<String, String> ab = new HashMap<>();
        Map<String, String> ba = new HashMap<>();
        for (int i = 0; i < a.size(); i++) {
            if (!isVariant(a.get(i), b.get(i), ab, ba)) return false;
        }
        return true;
    }

    /** Structural equality up to a consistent (bidirectional) renaming of variables. */
    private static boolean isVariant(Term a, Term b, Map<String, String> ab, Map<String, String> ba) {
        if (a instanceof Variable && b instanceof Variable) {
            String an = ((Variable) a).getName();
            String bn = ((Variable) b).getName();
            String mappedA = ab.get(an);
            String mappedB = ba.get(bn);
            if (mappedA == null && mappedB == null) {
                ab.put(an, bn);
                ba.put(bn, an);
                return true;
            }
            return bn.equals(mappedA) && an.equals(mappedB);
        }
        if (a instanceof Variable || b instanceof Variable) return false;
        if (a instanceof CompoundTerm && b instanceof CompoundTerm) {
            CompoundTerm ca = (CompoundTerm) a;
            CompoundTerm cb = (CompoundTerm) b;
            int na = ca.getArguments() != null ? ca.getArguments().size() : 0;
            int nb = cb.getArguments() != null ? cb.getArguments().size() : 0;
            if (na != nb || !ca.getName().equals(cb.getName())) return false;
            for (int i = 0; i < na; i++) {
                if (!isVariant(ca.getArguments().get(i), cb.getArguments().get(i), ab, ba)) return false;
            }
            return true;
        }
        if (a instanceof CompoundTerm || b instanceof CompoundTerm) return false;
        // atomic terms (atoms, numbers, strings): identical iff equal in the standard order
        return a.getClass() == b.getClass() && Sort.compareTerms(a, b) == 0;
    }
    // END_CHANGE: ISS-2025-0411/ISS-2025-0412

    // START_CHANGE: ISS-2025-0416 - ISO 8.10.1.3(c): findall/3's Instances argument must be a
    // list or a partial list; anything else raises type_error(list, Instances). Shared by the
    // legacy collector above and the engine's native findall.
    public static void checkInstancesArgument(Term instances, String context) {
        Term tail = instances;
        java.util.IdentityHashMap<Term, Boolean> seen = new java.util.IdentityHashMap<>();
        while (tail instanceof CompoundTerm
                && ".".equals(((CompoundTerm) tail).getName())
                && ((CompoundTerm) tail).getArguments().size() == 2) {
            if (seen.put(tail, Boolean.TRUE) != null) break;   // cyclic spine: not a (partial) list
            tail = ((CompoundTerm) tail).getArguments().get(1);
        }
        if (tail instanceof Variable) return;                                      // partial list / var
        if (tail instanceof Atom && "[]".equals(((Atom) tail).getName())) return;  // proper list
        throw new PrologException(ISOErrorTerms.typeError("list", instances, context));
    }
    // END_CHANGE: ISS-2025-0416

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
