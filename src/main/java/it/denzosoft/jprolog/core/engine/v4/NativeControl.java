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
        t.register("bagof", 3, new BagofB(false));
        t.register("setof", 3, new BagofB(true));
        t.register("aggregate_all", 3, new AggregateAllB());
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
    private static Term variantKey(Term w, Machine m) {
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

    /** ISS-2025-0414: exact integer sums (long, then BigInteger), float contagion. */
    private static final class SumAcc {
        private long l;
        private java.math.BigInteger big;
        private double d;
        private boolean isFloat;

        void add(Number n) {
            if (isFloat || !n.isInteger()) {
                if (!isFloat) { isFloat = true; d = (big != null) ? big.doubleValue() : (double) l; }
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
            if (big == null) return Number.valueOf(l);
            return (big.bitLength() <= 63) ? Number.valueOf(big.longValue()) : new Number(big);
        }
    }

    private static int numCompare(Number x, Number y) {
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
                capture = new java.io.PrintStream(buf, true, "UTF-8");
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
