package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.ResourceGuard;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.List;

// START_CHANGE: ISS-2025-0451/ISS-2025-0452 - engine v4 wave W3, design B.5/B.6: the control and
// collection built-ins that used to run their sub-goals on the recursive solver now run on the
// machine itself (Machine.findAll / runOnce / pushGoal), with no Java recursion, no name-keyed
// binding maps and no eager materialisation of the caller's continuation.
/**
 * Native control and collection built-ins for the v4 machine.
 *
 * <ul>
 *   <li>{@code phrase/2,3} (ISS-2025-0451) — translates the grammar body with the default v2
 *       {@link it.denzosoft.jprolog.core.dcg.v2.DCGTranslator} and <b>pushes</b> the goal. The
 *       legacy built-in solved it through a nested recursive solver, which capped a parse at the
 *       2 000-deep recursion limit (LIM-030/L-02); the native version runs the parse in the main
 *       drive loop, so a 1 000 000-token list parses at the default JVM stack and the inference
 *       budget and the Stop interrupt apply inside it.</li>
 *   <li>{@code bagof/3}, {@code setof/3} (ISS-2025-0452) — ISO 8.10.2/8.10.3 with {@code ^}
 *       handling, free-variable grouping by <b>variant</b> witness (ISS-2025-0411) and, for
 *       {@code setof/3}, group enumeration in the standard order of the witnesses
 *       (ISS-2025-0412). Groups are handed out lazily through a {@link Generator}.</li>
 *   <li>{@code aggregate_all/3} — the SWI forms {@code count}, {@code count(T)}, {@code sum(E)},
 *       {@code max(E)}, {@code min(E)}, {@code max(E,W)}, {@code min(E,W)}, {@code bag(T)},
 *       {@code set(T)}, accumulated over {@link Machine#forEachSolution} (ISS-2025-0522), keeping
 *       the ISS-2025-0413 (max/min fail on no solution) and ISS-2025-0414 (exact BigInteger sums)
 *       rules.</li>
 *   <li>{@code with_output_to/2} — captures the goal's output through the thread-local
 *       {@code StreamManager} override (and {@code System.out} for the built-ins that still write
 *       there), running the goal once on this machine.</li>
 * </ul>
 */
final class NativeControl {

    private NativeControl() {}

    private static final Atom NIL = new Atom("[]");
    private static final Atom TRUE = new Atom("true");
    private static final Atom MINUS = new Atom("-");
    private static final Atom WITNESS = new Atom("$w");

    static void register(BuiltinTable t) {
        t.register("phrase", 2, new PhraseB(2));
        t.register("phrase", 3, new PhraseB(3));
        // START_CHANGE: ISS-2025-0797 - 4.6 wave Q7: enhanced_phrase/2,3 are phrase/2,3. The registry
        // class (builtin.dcg.EnhancedPhrase) never ran the grammar: it expanded the body and answered
        // TRUE for any non-trivial goal, so enhanced_phrase(Var, L) and enhanced_phrase(f(x), L)
        // "succeeded". The native table wins over the registry.
        t.register("enhanced_phrase", 2, new PhraseB(2));
        t.register("enhanced_phrase", 3, new PhraseB(3));
        // END_CHANGE: ISS-2025-0797
        t.register("bagof", 3, new BagofB(false));
        t.register("setof", 3, new BagofB(true));
        t.register("aggregate_all", 3, new AggregateAllB());
        // START_CHANGE: ISS-2025-0716 - wave Q2.7: library(aggregate)'s bagof/setof-based forms
        t.register("aggregate", 3, new AggregateB(false));
        t.register("aggregate", 4, new AggregateB(true));
        t.register("aggregate_all", 4, new AggregateAll4B());
        t.register("$aggregate_list", 3, new AggregateListB());
        // END_CHANGE: ISS-2025-0716
        t.register("with_output_to", 2, new WithOutputToB());
    }

    // ------------------------------------------------------------------ phrase/2,3

    private static final java.util.concurrent.atomic.AtomicLong PHRASE_SEQ =
        new java.util.concurrent.atomic.AtomicLong();

    private static final class PhraseB implements Builtin {
        private final int arity;
        PhraseB(int arity) { this.arity = arity; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            String ind = "phrase/" + arity;
            Term list = args[1];
            Term rest = (arity == 2) ? (Term) NIL : args[2];
            checkListSpine(m, list, ind);
            checkListSpine(m, rest, ind);
            Term body = m.deref(args[0]);
            if (body instanceof Number || body instanceof PrologString) {
                throw Errors.type("callable", body, ind);
            }
            Term goal;
            if (body instanceof Variable) {
                // An unbound body must raise instantiation_error from call/3, never loop back here.
                goal = new CompoundTerm(new Atom("call"), Arrays.asList(body, list, rest));
            } else {
                it.denzosoft.jprolog.core.dcg.v2.DCGTranslator tr =
                    new it.denzosoft.jprolog.core.dcg.v2.DCGTranslator(
                        "_PhraseS" + PHRASE_SEQ.getAndIncrement() + "_");
                // The BODY is resolved (it is small and the translator pattern-matches on it);
                // the token list and the rest are passed as cells, so a million-element list is
                // never walked, let alone copied.
                goal = tr.body(m.resolve(body), list, rest);
            }
            m.pushGoal(goal);
            return Outcome.SUSPENDED;
        }
    }

    /**
     * ISO 13211-3: phrase/2,3's list arguments must be a variable, a partial list or a proper list.
     * Tortoise-and-hare so a cyclic spine terminates without an O(n) identity set.
     */
    private static void checkListSpine(Machine m, Term t, String ind) {
        ResourceGuard g = m.guard();
        Term slow = Unify.deref(t);
        Term fast = slow;
        int n = 0;
        while (true) {
            if (!isCons(fast)) break;
            fast = Unify.deref(((CompoundTerm) fast).getArguments().get(1));
            if (!isCons(fast)) break;
            fast = Unify.deref(((CompoundTerm) fast).getArguments().get(1));
            slow = Unify.deref(((CompoundTerm) slow).getArguments().get(1));
            if (fast == slow) return;                        // cyclic: accept, as the legacy code did
            if ((++n & 0xFFF) == 0 && g != null) g.step();
        }
        Term tail = fast;
        if (tail instanceof Variable) return;
        if (tail instanceof Atom && "[]".equals(((Atom) tail).getName())) return;
        if (tail instanceof PrologString) return;            // legacy code-list-as-string input
        throw Errors.type("list", m.resolve(t), ind);
    }

    private static boolean isCons(Term t) {
        return t instanceof CompoundTerm && ".".equals(((CompoundTerm) t).getName())
            && ((CompoundTerm) t).getArguments().size() == 2;
    }

    // ------------------------------------------------------------------ bagof/3, setof/3

    private static final class BagofB implements Builtin {
        private final boolean sorted;
        BagofB(boolean sorted) { this.sorted = sorted; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            final String ind = sorted ? "setof/3" : "bagof/3";
            final Term template = args[0];
            final Term bagArg = args[2];

            List<Variable> existential = new ArrayList<Variable>();
            Term goal = m.deref(args[1]);
            while (goal instanceof CompoundTerm && "^".equals(((CompoundTerm) goal).getName())
                    && ((CompoundTerm) goal).getArguments().size() == 2) {
                Unify.termVariables(((CompoundTerm) goal).getArguments().get(0), existential, m.guard());
                goal = m.deref(((CompoundTerm) goal).getArguments().get(1));
            }
            if (goal instanceof Variable) throw Errors.instantiation(ind);
            if (!(goal instanceof Atom) && !(goal instanceof CompoundTerm)) {
                throw Errors.type("callable", goal, ind);
            }
            m.checkBody(goal, sorted ? "setof" : "bagof", 3);             // ISS-2025-0518

            // Free (witness) variables: vars(Goal) minus vars(Template) minus the ^-quantified ones.
            IdentityHashMap<Variable, Boolean> excluded = new IdentityHashMap<Variable, Boolean>();
            List<Variable> tv = new ArrayList<Variable>();
            Unify.termVariables(template, tv, m.guard());
            for (int i = 0; i < tv.size(); i++) excluded.put(tv.get(i), Boolean.TRUE);
            for (int i = 0; i < existential.size(); i++) excluded.put(existential.get(i), Boolean.TRUE);
            List<Variable> gv = new ArrayList<Variable>();
            Unify.termVariables(goal, gv, m.guard());
            final List<Term> free = new ArrayList<Term>();
            for (int i = 0; i < gv.size(); i++) {
                if (!excluded.containsKey(gv.get(i))) free.add(gv.get(i));
            }

            if (free.isEmpty()) {
                List<Term> items = m.findAll(template, goal);
                if (items.isEmpty()) return Outcome.FAILURE;
                if (sorted) items = sortDedup(items, m.guard());
                return m.unify(bagArg, Machine.makeList(items)) ? Outcome.SUCCESS : Outcome.FAILURE;
            }

            final Term witness = new CompoundTerm(WITNESS, free);
            Term pairTemplate = new CompoundTerm(MINUS, Arrays.asList(witness, template));
            List<Term> pairs = m.findAll(pairTemplate, goal);
            if (pairs.isEmpty()) return Outcome.FAILURE;

            // START_CHANGE: ISS-2025-0520 - wave P1.7: grouping in O(n log n). Each solution's
            // witness was compared against every group found so far with a two-way subsumes test —
            // O(W^2) in the number of distinct witnesses (10 000 took 3.7 s, 40 000 over 7 min).
            // Now every witness gets a VARIANT KEY (itself when ground, otherwise a copy whose
            // variables are numbered in order of first occurrence, so two witnesses have identical
            // keys exactly when they are variants), the solutions are stable-sorted by key, and each
            // run of equal keys is one group. The group ORDER is unchanged: bagof/3 hands groups
            // out in order of their first solution, setof/3 in the standard order of the witness.
            final int np = pairs.size();
            final Term[] ws = new Term[np];
            final Term[] items = new Term[np];
            final Term[] keys = new Term[np];
            for (int i = 0; i < np; i++) {
                CompoundTerm pair = (CompoundTerm) Unify.deref(pairs.get(i));
                ws[i] = pair.getArguments().get(0);
                items[i] = pair.getArguments().get(1);
                keys[i] = variantKey(ws[i], m);
            }
            pairs = null;
            final ResourceGuard guard = m.guard();
            Integer[] byKey = new Integer[np];
            for (int i = 0; i < np; i++) byKey[i] = Integer.valueOf(i);
            Arrays.sort(byKey, new java.util.Comparator<Integer>() {        // stable (TimSort)
                @Override public int compare(Integer x, Integer y) {
                    return Unify.compareTerms(keys[x.intValue()], keys[y.intValue()], guard);
                }
            });
            final List<int[]> groups = new ArrayList<int[]>();
            int runStart = 0;
            for (int i = 1; i <= np; i++) {
                if (i == np || Unify.compareTerms(keys[byKey[i].intValue()], keys[byKey[runStart].intValue()], guard) != 0) {
                    int[] members = new int[i - runStart];
                    for (int j = runStart; j < i; j++) members[j - runStart] = byKey[j].intValue();
                    groups.add(members);                    // members are in solution order
                    runStart = i;
                }
            }
            final int ngroups = groups.size();
            final int[][] order = groups.toArray(new int[ngroups][]);
            Arrays.sort(order, sorted
                ? new java.util.Comparator<int[]>() {
                      @Override public int compare(int[] x, int[] y) {
                          return Unify.compareTerms(ws[x[0]], ws[y[0]], guard);
                      }
                  }
                : new java.util.Comparator<int[]>() {
                      @Override public int compare(int[] x, int[] y) { return Integer.compare(x[0], y[0]); }
                  });
            // END_CHANGE: ISS-2025-0520

            final int[] cursor = {0};
            Generator gen = new Generator() {
                @Override
                public boolean next(Machine mm) {
                    while (cursor[0] < ngroups) {
                        int[] members = order[cursor[0]++];
                        Bindings b = mm.bindings();
                        int mark = b.mark();
                        b.forceTrail++;
                        boolean ok;
                        try {
                            ok = true;
                            for (int k = 0; k < members.length && ok; k++) ok = mm.unify(witness, ws[members[k]]);
                            if (ok) {
                                List<Term> bag = new ArrayList<Term>(members.length);
                                for (int k = 0; k < members.length; k++) bag.add(items[members[k]]);
                                // START_CHANGE: ISS-2025-0519 - wave P1.6: setof/3 sorts the
                                // group AFTER its witnesses were unified (ISO 8.10.3.4), when the
                                // items are instantiated: `setof(X, member(X, [Y, Y]), L)` is
                                // [Y], not the [Y, Y] that sorting the two distinct copies gave.
                                // Lazily, too: a group that is never enumerated is never sorted.
                                if (sorted) bag = sortDedup(bag, mm.guard());
                                // END_CHANGE: ISS-2025-0519
                                ok = mm.unify(bagArg, Machine.makeList(bag));
                            }
                            // ISS-2025-0448 ordering: undo INSIDE the extent, close it afterwards.
                            if (!ok) b.undo(mark);
                        } finally {
                            b.forceTrail--;
                        }
                        if (ok) {
                            if (cursor[0] >= ngroups) mm.lastSolution();
                            return true;
                        }
                    }
                    return false;
                }
            };
            return m.pushGenerator(gen) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }
    }

    // START_CHANGE: ISS-2025-0520
    private static final Atom VKEY = new Atom("$bagof_var");

    /**
     * The variant key of a witness: the witness itself when it is ground; otherwise a copy in which
     * the i-th distinct variable (depth-first, left to right) is {@code '$bagof_var'(i)}. Two
     * witnesses are variants exactly when their keys are identical ({@code ==}).
     */
    static Term variantKey(Term w, Machine m) {                     // ISS-2025-0710: shared
        List<Variable> vs = new ArrayList<Variable>();
        Unify.termVariables(w, vs, m.guard());
        if (vs.isEmpty()) return w;
        Bindings b = m.bindings();
        int mark = b.mark();
        b.forceTrail++;
        try {
            for (int i = 0; i < vs.size(); i++) {
                b.bind(vs.get(i), new CompoundTerm(VKEY, Collections.singletonList((Term) Number.valueOf(i))));
            }
            return Unify.resolve(w, m.guard());
        } finally {
            b.undo(mark);                  // ISS-2025-0448: undo inside the extent, then close it
            b.forceTrail--;
        }
    }
    // END_CHANGE: ISS-2025-0520

    static List<Term> sortDedup(List<Term> in, final ResourceGuard g) {
        List<Term> out = new ArrayList<Term>(in);
        Collections.sort(out, new java.util.Comparator<Term>() {
            @Override public int compare(Term a, Term b) { return Unify.compareTerms(a, b, g); }
        });
        List<Term> uniq = new ArrayList<Term>(out.size());
        for (int i = 0; i < out.size(); i++) {
            if (i == 0 || Unify.compareTerms(out.get(i), out.get(i - 1), g) != 0) uniq.add(out.get(i));
        }
        return uniq;
    }

    // ------------------------------------------------------------------ aggregate_all/3

    // START_CHANGE: ISS-2025-0522 - wave P1.9: aggregate_all/3 as SWI-Prolog's library(aggregate)
    // defines it. count, count(T), sum(E), max(E), min(E), max(E, W), min(E, W), bag(T), set(T):
    // E is EVALUATED (sum(X*2), max(X+1)); max/min(E, W) answer max(Value, Witness); count, sum,
    // max and min accumulate while the goal runs (O(1) memory — no solution list is built); an
    // empty max/min fails and an empty sum is 0; an unbound spec is an instantiation_error and
    // anything else a domain_error(aggregate_spec, Spec). Before: max(X, W) collected a LIST, an
    // expression raised type_error(number, X*2), an unknown spec silently became bag/1, and the
    // non-standard max(X-W) pair form compared on X (it now evaluates X-W, as SWI does).
    private static final class AggregateAllB implements Builtin {
        @Override
        public Outcome call(final Machine m, Term[] args) {
            Term spec = m.deref(args[0]);
            Term goal = m.deref(args[1]);
            if (spec instanceof Variable) throw Errors.instantiation("aggregate_all/3");
            if (goal instanceof Variable) throw Errors.instantiation("aggregate_all/3");
            if (!(goal instanceof Atom) && !(goal instanceof CompoundTerm)) {
                throw Errors.type("callable", goal, "aggregate_all/3");
            }
            String f;
            int n;
            List<Term> sa;
            if (spec instanceof Atom) { f = ((Atom) spec).getName(); n = 0; sa = Collections.<Term>emptyList(); }
            else if (spec instanceof CompoundTerm) {
                f = ((CompoundTerm) spec).getName();
                sa = ((CompoundTerm) spec).getArguments();
                n = sa.size();
            } else {
                throw Errors.domain("aggregate_spec", m.resolve(spec), "aggregate_all/3");
            }
            boolean known = (n == 0 && "count".equals(f))
                || (n == 1 && ("count".equals(f) || "sum".equals(f) || "max".equals(f) || "min".equals(f)
                               || "bag".equals(f) || "set".equals(f)))
                || (n == 2 && ("max".equals(f) || "min".equals(f)));
            if (!known) throw Errors.domain("aggregate_spec", m.resolve(spec), "aggregate_all/3");
            m.checkBody(goal, "aggregate_all", 3);                         // ISS-2025-0518

            Term result;
            if ("count".equals(f)) {
                final long[] c = {0};
                m.forEachSolution(goal, new Machine.SolutionVisitor() {
                    @Override public boolean visit() { c[0]++; return true; }
                });
                result = Number.valueOf(c[0]);
            } else if ("sum".equals(f)) {
                final Term expr = sa.get(0);
                final SumAcc acc = new SumAcc();
                m.forEachSolution(goal, new Machine.SolutionVisitor() {
                    @Override public boolean visit() { acc.add(m.evalNum(expr)); return true; }
                });
                result = acc.result();
            } else if ("max".equals(f) || "min".equals(f)) {
                final boolean wantMax = "max".equals(f);
                final Term expr = sa.get(0);
                final Term wit = (n == 2) ? sa.get(1) : null;
                final Number[] best = {null};
                final Term[] bestW = {null};
                m.forEachSolution(goal, new Machine.SolutionVisitor() {
                    @Override public boolean visit() {
                        Number v = m.evalNum(expr);
                        if (best[0] == null || (wantMax ? numCompare(v, best[0]) > 0 : numCompare(v, best[0]) < 0)) {
                            best[0] = v;
                            if (wit != null) bestW[0] = m.copy(wit);   // the bindings are undone later
                        }
                        return true;
                    }
                });
                if (best[0] == null) return Outcome.FAILURE;                // ISS-2025-0413
                result = (wit == null) ? best[0]
                    : new CompoundTerm(new Atom(f), Arrays.asList((Term) best[0], bestW[0]));
            } else if ("bag".equals(f)) {
                result = Machine.makeList(m.findAll(sa.get(0), goal));
            } else {                                                        // set
                result = Machine.makeList(sortDedup(m.findAll(sa.get(0), goal), m.guard()));
            }
            return m.unify(args[2], result) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    // START_CHANGE: ISS-2025-0716 - wave Q2.7: aggregate/3, aggregate/4 and aggregate_all/4 as
    // SWI-Prolog's library(aggregate) defines them:
    //   aggregate(T, G, R)       :- bagof(Pattern, G, L), aggregate_list(T, L, R).
    //   aggregate(T, D, G, R)    :- setof(D-Pattern, G, Ps), pairs_values(Ps, L), aggregate_list(..).
    //   aggregate_all(T, D, G, R):- findall(D-Pattern, G, Ps0), sort(Ps0, Ps), pairs_values(..), ..
    // so aggregate/3,4 GROUP by the free variables of G (bagof semantics, `^` included) and fail
    // when G has no solution, while aggregate_all/4 counts only distinct D-Pattern pairs. The
    // template is one of count, count(T), sum(E), max(E), min(E), max(E, W), min(E, W), bag(T),
    // set(T), or a compound of such specs (r(count, sum(X)) answers r(C, S)). The goal is pushed
    // onto the machine (bagof/setof/findall are natives), so grouping and backtracking over the
    // groups are exactly bagof/3's; '$aggregate_list'/3 folds each group.
    private static final Atom PAT = new Atom("$aggr");
    private static final Atom ATOM_COUNT = new Atom("count");

    /** True when {@code spec} (dereferenced) is one aggregation spec. */
    private static boolean simpleSpec(Term spec) {
        if (spec instanceof Atom) return "count".equals(((Atom) spec).getName());
        if (!(spec instanceof CompoundTerm)) return false;
        String f = ((CompoundTerm) spec).getName();
        int n = ((CompoundTerm) spec).arity();
        return (n == 1 && ("count".equals(f) || "sum".equals(f) || "max".equals(f) || "min".equals(f)
                           || "bag".equals(f) || "set".equals(f)))
            || (n == 2 && ("max".equals(f) || "min".equals(f)));
    }

    /** Validate the template; answer the bagof pattern it collects. */
    private static Term aggrPattern(Machine m, Term spec0, String ind) {
        Term spec = m.deref(spec0);
        if (spec instanceof Variable) throw Errors.instantiation(ind);
        if (simpleSpec(spec)) return simplePattern(spec);
        if (spec instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) spec;
            Term[] ps = new Term[c.arity()];
            for (int i = 0; i < ps.length; i++) {
                Term a = m.deref(c.arg(i));
                if (a instanceof Variable) throw Errors.instantiation(ind);
                if (!simpleSpec(a)) throw Errors.domain("aggregate_spec", m.resolve(spec), ind);
                ps[i] = simplePattern(a);
            }
            return new CompoundTerm(PAT, ps);
        }
        throw Errors.domain("aggregate_spec", m.resolve(spec), ind);
    }

    private static Term simplePattern(Term spec) {
        if (spec instanceof Atom) return ATOM_COUNT;                       // count: nothing to collect
        CompoundTerm c = (CompoundTerm) spec;
        if (c.arity() == 2) return new CompoundTerm(MINUS, Arrays.asList(c.arg(0), c.arg(1)));
        return c.arg(0);
    }

    /** The goal of aggregate/3,4: bound and callable under its {@code ^} prefix. */
    private static void aggrGoal(Machine m, Term goal0, String ind) {
        Term g = m.deref(goal0);
        while (g instanceof CompoundTerm && "^".equals(((CompoundTerm) g).getName()) && ((CompoundTerm) g).arity() == 2) {
            g = m.deref(((CompoundTerm) g).arg(1));
        }
        if (g instanceof Variable) throw Errors.instantiation(ind);
        if (!(g instanceof Atom) && !(g instanceof CompoundTerm)) throw Errors.type("callable", m.resolve(g), ind);
    }

    private static Term conj(Term a, Term b) { return new CompoundTerm(new Atom(","), Arrays.asList(a, b)); }
    private static Term call3(String f, Term a, Term b, Term c) {
        return new CompoundTerm(new Atom(f), Arrays.asList(a, b, c));
    }

    private static final class AggregateB implements Builtin {
        private final boolean withDiscriminator;
        AggregateB(boolean withDiscriminator) { this.withDiscriminator = withDiscriminator; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            String ind = withDiscriminator ? "aggregate/4" : "aggregate/3";
            Term pattern = aggrPattern(m, args[0], ind);
            Term goal = args[withDiscriminator ? 2 : 1];
            Term result = args[withDiscriminator ? 3 : 2];
            aggrGoal(m, goal, ind);
            Variable list = new Variable();
            if (withDiscriminator) {
                Term pair = new CompoundTerm(MINUS, Arrays.asList(args[1], pattern));
                m.pushGoal(conj(call3("setof", pair, goal, list),
                                call3("$aggregate_list", m.deref(args[0]), new CompoundTerm(new Atom("$values"),
                                      Collections.singletonList((Term) list)), result)));
            } else {
                m.pushGoal(conj(call3("bagof", pattern, goal, list),
                                call3("$aggregate_list", m.deref(args[0]), list, result)));
            }
            return Outcome.SUSPENDED;
        }
    }

    private static final class AggregateAll4B implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term pattern = aggrPattern(m, args[0], "aggregate_all/4");
            Term goal = m.deref(args[2]);
            if (goal instanceof Variable) throw Errors.instantiation("aggregate_all/4");
            if (!(goal instanceof Atom) && !(goal instanceof CompoundTerm)) {
                throw Errors.type("callable", m.resolve(goal), "aggregate_all/4");
            }
            m.checkBody(goal, "aggregate_all", 4);
            Term pair = new CompoundTerm(MINUS, Arrays.asList(args[1], pattern));
            List<Term> pairs = sortDedup(m.findAll(pair, goal), m.guard());
            Term folded = aggregateList(m, m.deref(args[0]), values(pairs), "aggregate_all/4");
            if (folded == null) return Outcome.FAILURE;
            return m.unify(args[3], folded) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static List<Term> values(List<Term> pairs) {
        List<Term> out = new ArrayList<Term>(pairs.size());
        for (int i = 0; i < pairs.size(); i++) out.add(((CompoundTerm) Unify.deref(pairs.get(i))).arg(1));
        return out;
    }

    /** '$aggregate_list'(+Spec, +Items, -Result): fold one group. Items may be '$values'(Pairs). */
    private static final class AggregateListB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term src = m.deref(args[1]);
            boolean pairs = src instanceof CompoundTerm && "$values".equals(((CompoundTerm) src).getName());
            List<Term> items = NativeLibrary.elements(pairs ? ((CompoundTerm) src).arg(0) : src, m.guard());
            if (items == null) return Outcome.FAILURE;
            if (pairs) items = values(items);
            Term folded = aggregateList(m, m.deref(args[0]), items, "aggregate/3");
            if (folded == null) return Outcome.FAILURE;
            return m.unify(args[2], folded) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    /** Fold the pattern instances of one group; null when the fold fails (max/min of nothing). */
    private static Term aggregateList(Machine m, Term spec, List<Term> items, String ind) {
        if (!simpleSpec(spec)) {                                       // a compound of specs
            CompoundTerm c = (CompoundTerm) spec;
            Term[] out = new Term[c.arity()];
            for (int i = 0; i < out.length; i++) {
                List<Term> col = new ArrayList<Term>(items.size());
                for (int j = 0; j < items.size(); j++) col.add(((CompoundTerm) Unify.deref(items.get(j))).arg(i));
                out[i] = aggregateList(m, m.deref(c.arg(i)), col, ind);
                if (out[i] == null) return null;
            }
            return new CompoundTerm(c.getFunctor(), out);
        }
        String f = (spec instanceof Atom) ? ((Atom) spec).getName() : ((CompoundTerm) spec).getName();
        int n = (spec instanceof Atom) ? 0 : ((CompoundTerm) spec).arity();
        if ("count".equals(f)) return Number.valueOf(items.size());
        if ("bag".equals(f)) return Machine.makeList(items);
        if ("set".equals(f)) return Machine.makeList(sortDedup(items, m.guard()));
        if ("sum".equals(f)) {
            SumAcc acc = new SumAcc();
            for (int i = 0; i < items.size(); i++) acc.add(m.evalNum(items.get(i), ind));
            return acc.result();
        }
        boolean wantMax = "max".equals(f);
        Number best = null;
        Term bestW = null;
        for (int i = 0; i < items.size(); i++) {
            Term it = Unify.deref(items.get(i));
            Term e = (n == 2) ? ((CompoundTerm) it).arg(0) : it;
            Number v = m.evalNum(e, ind);
            if (best == null || (wantMax ? numCompare(v, best) > 0 : numCompare(v, best) < 0)) {
                best = v;
                if (n == 2) bestW = ((CompoundTerm) it).arg(1);
            }
        }
        if (best == null) return null;
        return (n == 2) ? new CompoundTerm(new Atom(f), Arrays.asList((Term) best, bestW)) : best;
    }
    // END_CHANGE: ISS-2025-0716

    /** ISS-2025-0414: exact integer sums (long, then BigInteger), float contagion. */
    private static final class SumAcc {
        private long l;
        private java.math.BigInteger big;
        private double d;
        private boolean isFloat;
        // START_CHANGE: ISS-2025-0712 - an exact rational sum once a rational is added
        private java.math.BigInteger rn, rd;
        // END_CHANGE: ISS-2025-0712

        void add(Number n) {
            // START_CHANGE: ISS-2025-0712
            if (!isFloat && !n.isFloat() && (rn != null || n instanceof it.denzosoft.jprolog.core.terms.Rational)) {
                if (rn == null) { rn = (big != null) ? big : java.math.BigInteger.valueOf(l); rd = java.math.BigInteger.ONE; }
                java.math.BigInteger a = it.denzosoft.jprolog.core.terms.Rational.numeratorOf(n);
                java.math.BigInteger b = it.denzosoft.jprolog.core.terms.Rational.denominatorOf(n);
                rn = rn.multiply(b).add(a.multiply(rd));
                rd = rd.multiply(b);
                java.math.BigInteger g = rn.gcd(rd);
                if (g.signum() != 0 && !g.equals(java.math.BigInteger.ONE)) { rn = rn.divide(g); rd = rd.divide(g); }
                return;
            }
            // END_CHANGE: ISS-2025-0712
            if (isFloat || !n.isInteger()) {
                if (!isFloat) {
                    isFloat = true;
                    d = (rn != null) ? it.denzosoft.jprolog.core.terms.Rational.of(rn, rd).doubleValue()   // ISS-2025-0712
                      : (big != null) ? big.doubleValue() : (double) l;
                }
                d += n.doubleValue();
            } else if (big == null && n.fitsInLong()) {
                long x = n.longValue();
                long r = l + x;
                if (((l ^ r) & (x ^ r)) < 0) {                              // overflow
                    big = java.math.BigInteger.valueOf(l).add(java.math.BigInteger.valueOf(x));
                } else {
                    l = r;
                }
            } else {
                if (big == null) big = java.math.BigInteger.valueOf(l);
                big = big.add(n.bigIntegerValue());
            }
        }

        Term result() {
            if (isFloat) return new Number(d, false);
            if (rn != null) return it.denzosoft.jprolog.core.terms.Rational.of(rn, rd);   // ISS-2025-0712
            if (big == null) return Number.valueOf(l);
            return (big.bitLength() <= 63) ? Number.valueOf(big.longValue()) : new Number(big);
        }
    }

    private static int numCompare(Number x, Number y) {
        // ISS-2025-0712: integers and rationals compare exactly
        if (!x.isInteger() || !y.isInteger()) {
            if (!x.isFloat() && !y.isFloat()) return it.denzosoft.jprolog.core.terms.Rational.compareExact(x, y);
        }
        if (x.isInteger() && y.isInteger()) {
            if (x.fitsInLong() && y.fitsInLong()) return Long.compare(x.longValue(), y.longValue());
            return x.bigIntegerValue().compareTo(y.bigIntegerValue());
        }
        return Double.compare(x.doubleValue(), y.doubleValue());
    }
    // END_CHANGE: ISS-2025-0522

    // ------------------------------------------------------------------ with_output_to/2

    private static final class WithOutputToB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term target = m.deref(args[0]);
            if (target instanceof Variable) throw Errors.instantiation("with_output_to/2");
            if (!(target instanceof CompoundTerm) || ((CompoundTerm) target).getArguments().size() != 1) {
                throw Errors.domain("output_sink", target, "with_output_to/2");
            }
            String kind = ((CompoundTerm) target).getName();
            Term sink = ((CompoundTerm) target).getArguments().get(0);

            java.io.ByteArrayOutputStream buf = new java.io.ByteArrayOutputStream();
            java.io.PrintStream capture;
            try {
                capture = new ColumnPrintStream(new java.io.PrintStream(buf, true, "UTF-8"), true);   // ISS-2025-0714
            } catch (java.io.UnsupportedEncodingException e) {
                capture = new java.io.PrintStream(buf, true);
            }
            // START_CHANGE: ISS-2025-0472 - wave W7 closes LIM-025: the capture is the thread-local
            // output override alone. Swapping System.out as well used to be necessary because a
            // handful of built-ins printed to System.out directly; they all go through
            // StreamManager.out() now, so a concurrent capture on another thread is no longer
            // corrupted by this one.
            java.io.PrintStream prevThread =
                it.denzosoft.jprolog.builtin.io.StreamManager.threadLocalOutput();
            boolean ok;
            try {
                it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(capture);
                ok = m.runOnce(args[1]);
            } finally {
                capture.flush();
                it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(prevThread);
            }
            // END_CHANGE: ISS-2025-0472
            if (!ok) return Outcome.FAILURE;
            String text;
            try {
                text = buf.toString("UTF-8");
            } catch (java.io.UnsupportedEncodingException e) {
                text = buf.toString();
            }
            Term value;
            if ("atom".equals(kind)) {
                value = new Atom(text);
            } else if ("string".equals(kind)) {
                value = new PrologString(text);
            } else if ("codes".equals(kind)) {
                value = NativeIo.textToList(text, true);            // ISS-2025-0606: code points
            } else if ("chars".equals(kind)) {
                value = NativeIo.textToList(text, false);           // ISS-2025-0606: code points
            } else {
                throw Errors.domain("output_sink", target, "with_output_to/2");
            }
            return m.unify(sink, value) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }
}
// END_CHANGE: ISS-2025-0451/ISS-2025-0452
