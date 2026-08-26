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
 *   <li>{@code aggregate_all/3} — {@code count} / {@code sum} / {@code max} / {@code min} /
 *       {@code bag} / {@code set} over {@link Machine#findAll}, keeping the ISS-2025-0413
 *       (max/min fail on no solution) and ISS-2025-0414 (exact BigInteger sums,
 *       {@code type_error(number, T)}) rules.</li>
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

            final List<List<Term>> groupWitnesses = new ArrayList<List<Term>>();
            final List<List<Term>> groupItems = new ArrayList<List<Term>>();
            for (int i = 0; i < pairs.size(); i++) {
                CompoundTerm pair = (CompoundTerm) Unify.deref(pairs.get(i));
                Term w = pair.getArguments().get(0);
                Term item = pair.getArguments().get(1);
                int gi = -1;
                for (int j = 0; j < groupWitnesses.size(); j++) {
                    if (isVariant(groupWitnesses.get(j).get(0), w, m)) { gi = j; break; }
                }
                if (gi < 0) {
                    List<Term> ws = new ArrayList<Term>();
                    ws.add(w);
                    groupWitnesses.add(ws);
                    groupItems.add(new ArrayList<Term>());
                    gi = groupWitnesses.size() - 1;
                } else {
                    groupWitnesses.get(gi).add(w);
                }
                groupItems.get(gi).add(item);
            }

            final int ngroups = groupWitnesses.size();
            final Integer[] order = new Integer[ngroups];
            for (int i = 0; i < ngroups; i++) order[i] = Integer.valueOf(i);
            if (sorted) {
                final ResourceGuard g = m.guard();
                Arrays.sort(order, new java.util.Comparator<Integer>() {
                    @Override public int compare(Integer x, Integer y) {
                        return Unify.compareTerms(groupWitnesses.get(x.intValue()).get(0),
                                                  groupWitnesses.get(y.intValue()).get(0), g);
                    }
                });
                for (int i = 0; i < ngroups; i++) {
                    groupItems.set(i, sortDedup(groupItems.get(i), m.guard()));
                }
            }

            final int[] cursor = {0};
            Generator gen = new Generator() {
                @Override
                public boolean next(Machine mm) {
                    while (cursor[0] < ngroups) {
                        int gi = order[cursor[0]++].intValue();
                        Bindings b = mm.bindings();
                        int mark = b.mark();
                        b.forceTrail++;
                        boolean ok;
                        try {
                            ok = true;
                            List<Term> ws = groupWitnesses.get(gi);
                            for (int k = 0; k < ws.size() && ok; k++) ok = mm.unify(witness, ws.get(k));
                            if (ok) ok = mm.unify(bagArg, Machine.makeList(groupItems.get(gi)));
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

    /** {@code A} and {@code B} are variants: each subsumes the other. */
    private static boolean isVariant(Term a, Term b, Machine m) {
        return Unify.subsumes(a, b, m.bindings()) && Unify.subsumes(b, a, m.bindings());
    }

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

    private static final class AggregateAllB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term spec = m.deref(args[0]);
            Term goal = m.deref(args[1]);
            if (goal instanceof Variable) throw Errors.instantiation("aggregate_all/3");
            if (!(goal instanceof Atom) && !(goal instanceof CompoundTerm)) {
                throw Errors.type("callable", goal, "aggregate_all/3");
            }
            Term result;
            if (spec instanceof Atom && "count".equals(((Atom) spec).getName())) {
                result = Number.valueOf(m.findAll(TRUE, goal).size());
            } else if (spec instanceof CompoundTerm && ((CompoundTerm) spec).getArguments().size() == 1) {
                CompoundTerm cs = (CompoundTerm) spec;
                String f = cs.getName();
                Term inner = cs.getArguments().get(0);
                if ("count".equals(f)) {
                    result = Number.valueOf(m.findAll(inner, goal).size());
                } else if ("sum".equals(f)) {
                    result = sum(m.findAll(inner, goal));
                } else if ("max".equals(f) || "min".equals(f)) {
                    result = extremum(m.findAll(inner, goal), "max".equals(f), m.guard());
                    if (result == null) return Outcome.FAILURE;          // ISS-2025-0413
                } else if ("bag".equals(f)) {
                    result = Machine.makeList(m.findAll(inner, goal));
                } else if ("set".equals(f)) {
                    result = Machine.makeList(sortDedup(m.findAll(inner, goal), m.guard()));
                } else {
                    result = Machine.makeList(m.findAll(spec, goal));
                }
            } else {
                result = Machine.makeList(m.findAll(spec, goal));
            }
            return m.unify(args[2], result) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    /** ISS-2025-0414: exact integer sums, float contagion, type_error(number, T) on anything else. */
    private static Term sum(List<Term> items) {
        java.math.BigInteger intSum = java.math.BigInteger.ZERO;
        double floatSum = 0.0;
        boolean sawFloat = false;
        for (int i = 0; i < items.size(); i++) {
            Term t = Unify.deref(items.get(i));
            if (!(t instanceof Number)) throw Errors.type("number", t, "aggregate_all/3");
            Number n = (Number) t;
            if (n.isInteger() && !sawFloat) {
                intSum = intSum.add(n.bigIntegerValue());
            } else {
                if (!sawFloat) { sawFloat = true; floatSum = intSum.doubleValue(); }
                floatSum += n.doubleValue();
            }
        }
        return sawFloat ? new Number(floatSum, false) : new Number(intSum);
    }

    /**
     * ISS-2025-0413: null (i.e. aggregate_all/3 fails) when there is no solution;
     * {@code type_error(number, T)} on a non-numeric element. As an extension the
     * {@code Value-Witness} pair form is compared on its numeric left-hand side, so
     * {@code aggregate_all(max(X-W), ..., Max)} answers with the winning pair.
     */
    private static Term extremum(List<Term> items, boolean wantMax, ResourceGuard g) {
        Term best = null;
        Number bestKey = null;
        for (int i = 0; i < items.size(); i++) {
            Term t = Unify.deref(items.get(i));
            Number key = keyOf(t);
            if (best == null) { best = t; bestKey = key; continue; }
            int c = numCompare(key, bestKey);
            if (wantMax ? c > 0 : c < 0) { best = t; bestKey = key; }
        }
        return best;
    }

    private static Number keyOf(Term t) {
        if (t instanceof Number) return (Number) t;
        if (t instanceof CompoundTerm && "-".equals(((CompoundTerm) t).getName())
                && ((CompoundTerm) t).getArguments().size() == 2) {
            Term left = Unify.deref(((CompoundTerm) t).getArguments().get(0));
            if (left instanceof Number) return (Number) left;
        }
        throw Errors.type("number", t, "aggregate_all/3");
    }

    private static int numCompare(Number x, Number y) {
        if (x.isInteger() && y.isInteger()) return x.bigIntegerValue().compareTo(y.bigIntegerValue());
        return Double.compare(x.doubleValue(), y.doubleValue());
    }

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
                List<Term> cs = new ArrayList<Term>(text.length());
                for (int i = 0; i < text.length(); i++) cs.add(Number.valueOf(text.charAt(i)));
                value = Machine.makeList(cs);
            } else if ("chars".equals(kind)) {
                List<Term> cs = new ArrayList<Term>(text.length());
                for (int i = 0; i < text.length(); i++) cs.add(new Atom(String.valueOf(text.charAt(i))));
                value = Machine.makeList(cs);
            } else {
                throw Errors.domain("output_sink", target, "with_output_to/2");
            }
            return m.unify(sink, value) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }
}
// END_CHANGE: ISS-2025-0451/ISS-2025-0452
