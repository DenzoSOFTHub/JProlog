package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.ResourceGuard;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

// START_CHANGE: ISS-2025-0486 - wave W9: the last built-ins LIM-037 named as "still eager".
/**
 * The v4 natives added by wave W9: {@code sort/4}, {@code predsort/3}, {@code max_list/2},
 * {@code min_list/2}, {@code current_op/3}, {@code nb_getval/2} and {@code b_getval/2}.
 *
 * <p>All of them used to run through {@link LegacyBuiltinAdapter}: the goal was dereferenced into a
 * whole new term, the built-in produced a {@code List<Map<String,Term>>} and the adapter unified
 * every named entry back into a cell. For the list predicates that is two extra O(list) walks per
 * call; for {@code current_op/3} it also meant materialising every visible operator before the
 * first solution. Here they read cells directly and, where they are nondeterministic
 * ({@code current_op/3}), hand out one answer per redo.
 *
 * <p>Semantics are unchanged — deliberately so. The ISO error terms of {@code sort/4}
 * (ISS-2025-0418), the SWI failure modes of {@code predsort/3} (ISS-2025-0419) and the
 * {@code existence_error(variable, Name)} of {@code nb_getval/2} are reproduced exactly, and the
 * registry implementations stay registered for the v2 fallback engine.
 */
final class NativeMisc {

    private NativeMisc() {}

    private static final Atom NIL = new Atom("[]");

    static void register(BuiltinTable t) {
        t.register("sort", 4, new Sort4());
        t.register("predsort", 3, new PredSort3());
        // START_CHANGE: ISS-2025-0608 - P4.15: statistics/0,2 native, SWI keys and shapes
        t.register("statistics", 2, new StatisticsB());
        t.register("statistics", 0, new Statistics0B());
        // END_CHANGE: ISS-2025-0608
        t.register("max_list", 2, new MinMaxList(true));
        t.register("min_list", 2, new MinMaxList(false));
        t.register("current_op", 3, new CurrentOp());
        t.register("nb_getval", 2, new GetVal("nb_getval/2"));
        t.register("b_getval", 2, new GetVal("b_getval/2"));
        // START_CHANGE: ISS-2025-0500 - 4.2 wave C: op/3 and the character-conversion pair join
        // current_op/3 on the engine's own Ops store. They were the last built-ins outside
        // builtin.clpfd.v2 that pushed an undo action through the public core.engine.v4.Undo
        // doorway; a native has the Machine in its hand and pushes onto its trail directly, which
        // is what lets Undo become package-private.
        t.register("op", 3, new OpB());
        t.register("char_conversion", 2, new CharConvB());
        t.register("current_char_conversion", 2, new CurrentCharConv());
        // END_CHANGE: ISS-2025-0500
    }

    // ------------------------------------------------------------------ op/3

    // START_CHANGE: ISS-2025-0500 - 4.2 wave C.
    /**
     * {@code op(+Precedence, +Type, +Name)} over the CALLING ENGINE's operator store.
     *
     * <p>Three properties the registry version could not offer together:
     * <ul>
     *   <li>it writes into {@code m.engine().prolog().getOps()} — the store of the engine whose
     *       machine is running — never a process-global table. (The registry {@code op/3} that is
     *       still registered under the name, {@code builtin.system.OperatorDefinition}, has read
     *       {@code Ops.current()} since W7; the class that really did capture
     *       {@code OperatorTable.getDefault()} in its constructor was {@code builtin.system.Op},
     *       which was never registered at all and is deleted in this wave.)</li>
     *   <li>the definition is permanent (ISS-2025-0612; until 4.4.0 it was undone on
     *       backtracking through the running machine's trail);</li>
     *   <li>the module scoping W7 introduced is unchanged: {@code Ops.define/3} attributes the
     *       operator to the module currently in context, and {@code current_op/3} filters by it.</li>
     * </ul>
     *
     * <p>Modes are the registry version's (the name may be an atom or a proper list of atoms,
     * ISS-2025-0283); the <b>error terms are ISO 8.14.3.3</b> since 4.4.0 (ISS-2025-0504) — an
     * unbound argument is {@code instantiation_error}, a non-integer priority
     * {@code type_error(integer, P)} (ISS-2025-0278), a non-atom specifier
     * {@code type_error(atom, T)}, a name that is neither an atom nor a list
     * {@code type_error(list, N)}, a priority outside 0..1200
     * {@code domain_error(operator_priority, P)}, an unknown specifier
     * {@code domain_error(operator_specifier, T)}, {@code ','} (whatever the priority)
     * {@code permission_error(modify, operator, ',')} and {@code '|'} outside its ISO window
     * (priority 0, or an infix specifier with priority >= 1001)
     * {@code permission_error(create, operator, '|')}. The checks run in ISO order:
     * instantiation, then type, then domain, then permission, so a goal with two faults reports
     * the one the standard names.
     */
    private static final class OpB implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Term precT = m.deref(args[0]);
            Term typeT = m.deref(args[1]);
            Term nameT = m.deref(args[2]);
            // START_CHANGE: ISS-2025-0504 - ISO 8.14.3.3 error terms, in the standard's own order.
            // (a)(b)(c) instantiation
            if (precT instanceof Variable || typeT instanceof Variable) throw Errors.instantiation("op/3");
            // (d)(e) type
            if (!(precT instanceof Number)) throw Errors.type("integer", m.resolve(precT), "op/3");
            if (!((Number) precT).isInteger()) {                       // ISS-2025-0278
                throw Errors.type("integer", m.resolve(precT), "op/3");
            }
            // (f)(h) the name: an atom, or a proper list of atoms (ISS-2025-0283). A variable
            // anywhere in it is (c)/(d) instantiation_error; a non-list is (f) type_error(list, N);
            // a non-atom element is (h) type_error(atom, E). ISO tests the name's shape BEFORE the
            // specifier's type, so op(700, 1, 2) is type_error(list, 2).
            List<String> names = opNames(m, nameT);
            // (g)
            if (!(typeT instanceof Atom)) throw Errors.type("atom", m.resolve(typeT), "op/3");
            // (h)(i) domain
            int precedence = (int) ((Number) precT).longValue();
            String type = ((Atom) typeT).getName();
            if (precedence < 0 || precedence > 1200) {
                throw Errors.domain("operator_priority", m.resolve(precT), "op/3");
            }
            if (!isOperatorType(type)) {
                throw Errors.domain("operator_specifier", m.resolve(typeT), "op/3");
            }
            // (j)(k)(l) permission
            for (int i = 0; i < names.size(); i++) {
                String n = names.get(i);
                if (",".equals(n)) {
                    throw Errors.permission("modify", "operator", new Atom(","), "op/3");
                }
                if ("|".equals(n) && precedence != 0
                        && !(precedence >= 1001 && isInfix(type))) {
                    throw Errors.permission("create", "operator", new Atom("|"), "op/3");
                }
            }
            // END_CHANGE: ISS-2025-0504
            Ops ops = m.engine().prolog().getOps();
            // START_CHANGE: ISS-2025-0612 - P4.18 (decision §8): op/3 is PERMANENT (ISO 8.14.3,
            // SWI): the undo action the store hands back is no longer pushed on the trail, so
            // forall(member(O, [zfoo, zbar]), op(700, xfx, O)) defines both operators.
            for (int i = 0; i < names.size(); i++) {
                ops.define(precedence, type, names.get(i));
            }
            // END_CHANGE: ISS-2025-0612
            return Outcome.SUCCESS;
        }
    }

    // START_CHANGE: ISS-2025-0504
    private static boolean isInfix(String t) {
        return "xfx".equals(t) || "xfy".equals(t) || "yfx".equals(t);
    }

    /** An ISO "character": a one-character atom. */
    private static boolean isOneCharAtom(Term t) {
        return (t instanceof Atom) && ((Atom) t).getName().length() == 1;
    }
    // END_CHANGE: ISS-2025-0504

    private static boolean isOperatorType(String t) {
        return "fx".equals(t) || "fy".equals(t) || "xfx".equals(t) || "xfy".equals(t)
            || "yfx".equals(t) || "xf".equals(t) || "yf".equals(t);
    }

    // START_CHANGE: ISS-2025-0504 - the same acceptance as before (an atom, or a proper list of
    // atoms), but each rejection now names the ISO error clause instead of collapsing into one
    // "must be an atom or a list of atoms" message atom.
    /** An atom, or a proper list of atoms; raises the ISO 8.14.3.3 (c)/(f)/(g) error otherwise. */
    private static List<String> opNames(Machine m, Term nameT) {
        List<String> out = new ArrayList<String>();
        if (nameT instanceof Variable) throw Errors.instantiation("op/3");
        if (nameT instanceof Atom && !"[]".equals(((Atom) nameT).getName())) {
            out.add(((Atom) nameT).getName());
            return out;
        }
        Term cur = nameT;
        while (cur instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) cur;
            if (!".".equals(c.getName()) || c.getArguments().size() != 2) {
                throw Errors.type("list", m.resolve(nameT), "op/3");
            }
            Term h = Unify.deref(c.getArguments().get(0));
            if (h instanceof Variable) throw Errors.instantiation("op/3");
            if (!(h instanceof Atom)) throw Errors.type("atom", m.resolve(h), "op/3");
            out.add(((Atom) h).getName());
            cur = Unify.deref(c.getArguments().get(1));
        }
        if (cur instanceof Variable) throw Errors.instantiation("op/3");
        if (cur instanceof Atom && "[]".equals(((Atom) cur).getName())) {
            // op(P, T, []) — the empty list is a name of no operators, which the registry version
            // rejected. ISO's (f) is the closest clause: [] is not an operator name.
            if (out.isEmpty()) throw Errors.domain("non_empty_list", NIL, "op/3");
            return out;
        }
        throw Errors.type("list", m.resolve(nameT), "op/3");
    }
    // END_CHANGE: ISS-2025-0504

    // ------------------------------------------------------------------ char_conversion/2

    /**
     * {@code char_conversion(+From, +To)} on the engine's own conversion table (ISO 8.14.5).
     * {@code From == To} removes the entry. Undone on backtracking, and — unlike the
     * {@code static} table it replaces — invisible to every other {@code Prolog} instance.
     */
    private static final class CharConvB implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Term f = m.deref(args[0]);
            Term t = m.deref(args[1]);
            // START_CHANGE: ISS-2025-0504 - ISO 8.14.5.3: a variable is instantiation_error, and
            // anything that is not a one-character atom is representation_error(character) — not
            // one message atom covering both.
            if (f instanceof Variable || t instanceof Variable) {
                throw Errors.instantiation("char_conversion/2");
            }
            if (!isOneCharAtom(f) || !isOneCharAtom(t)) {
                throw Errors.representation("character", "char_conversion/2");
            }
            // END_CHANGE: ISS-2025-0504
            Ops ops = m.engine().prolog().getOps();
            ops.convert(((Atom) f).getName().charAt(0), ((Atom) t).getName().charAt(0));   // ISS-2025-0612: permanent
            return Outcome.SUCCESS;
        }
    }

    /**
     * {@code current_char_conversion(?From, ?To)} — a generator, one pair per redo. With
     * {@code From} bound the answer is deterministic ({@code To} defaults to {@code From}); with
     * {@code From} unbound the declared conversions come first, then the identity conversions of
     * the printable ASCII range, exactly as the registry version enumerated them.
     */
    private static final class CurrentCharConv implements Builtin {
        @Override public Outcome call(Machine m, final Term[] args) {
            Ops ops = m.engine().prolog().getOps();
            Term f = m.deref(args[0]);
            Term t = m.deref(args[1]);
            // START_CHANGE: ISS-2025-0504 - ISO 8.14.6.3: a bound argument that is not a
            // one-character atom is representation_error(character), where the registry version
            // simply failed.
            if (!(f instanceof Variable) && !isOneCharAtom(f)) {
                throw Errors.representation("character", "current_char_conversion/2");
            }
            if (!(t instanceof Variable) && !isOneCharAtom(t)) {
                throw Errors.representation("character", "current_char_conversion/2");
            }
            // END_CHANGE: ISS-2025-0504
            if (!(f instanceof Variable)) {
                char c = ((Atom) f).getName().charAt(0);
                return m.unify(args[1], new Atom(String.valueOf(ops.converted(c))))
                    ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            final java.util.Map<Character, Character> table = ops.conversions();
            final List<char[]> pairs = new ArrayList<char[]>();
            if (!(t instanceof Variable)) {                      // From unbound, To bound
                char to = ((Atom) t).getName().charAt(0);
                for (java.util.Map.Entry<Character, Character> e : table.entrySet()) {
                    if (e.getValue().charValue() == to) {
                        pairs.add(new char[] { e.getKey().charValue(), to });
                    }
                }
            } else {                                             // both unbound
                for (java.util.Map.Entry<Character, Character> e : table.entrySet()) {
                    pairs.add(new char[] { e.getKey().charValue(), e.getValue().charValue() });
                }
                for (char c = 32; c < 127; c++) {
                    if (!table.containsKey(Character.valueOf(c))) pairs.add(new char[] { c, c });
                }
            }
            if (pairs.isEmpty()) return Outcome.FAILURE;
            final int[] i = {0};
            Generator gen = new Generator() {
                @Override public boolean next(Machine mm) {
                    while (i[0] < pairs.size()) {
                        char[] pr = pairs.get(i[0]++);
                        if (i[0] >= pairs.size()) mm.lastSolution();
                        // ONE mark/undo extent around the pair of unifications (invariant 12).
                        Bindings b = mm.bindings();
                        int mark = b.mark();
                        b.forceTrail++;
                        boolean ok;
                        try {
                            ok = Unify.unify(args[0], new Atom(String.valueOf(pr[0])), b)
                              && Unify.unify(args[1], new Atom(String.valueOf(pr[1])), b);
                            if (!ok) b.undo(mark);
                        } finally {
                            b.forceTrail--;
                        }
                        if (ok) return true;
                    }
                    return false;
                }
            };
            return m.pushGenerator(gen) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }
    }
    // END_CHANGE: ISS-2025-0500

    // ------------------------------------------------------------------ sort/4

    /** {@code sort(+Key, +Order, +List, -Sorted)} (ISS-2025-0220, errors from ISS-2025-0418). */
    private static final class Sort4 implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            final ResourceGuard g = m.guard();
            Term keyT = m.deref(args[0]);
            Term orderT = m.deref(args[1]);
            if (keyT instanceof Variable || orderT instanceof Variable) {
                throw Errors.instantiation("sort/4");
            }
            if (!(keyT instanceof Number) || !((Number) keyT).isInteger()) {
                throw Errors.type("integer", m.resolve(keyT), "sort/4");
            }
            final int key = (int) ((Number) keyT).longValue();
            if (key < 0) throw Errors.domain("not_less_than_zero", m.resolve(keyT), "sort/4");
            if (!(orderT instanceof Atom)) throw Errors.type("atom", m.resolve(orderT), "sort/4");
            final boolean ascending;
            final boolean dedup;
            String order = ((Atom) orderT).getName();
            if ("@<".equals(order))        { ascending = true;  dedup = true;  }
            else if ("@=<".equals(order))  { ascending = true;  dedup = false; }
            else if ("@>".equals(order))   { ascending = false; dedup = true;  }
            else if ("@>=".equals(order))  { ascending = false; dedup = false; }
            else throw Errors.domain("order", m.resolve(orderT), "sort/4");

            List<Term> es = NativeLibrary.elements(args[2], g);
            if (es == null) throw NativeLibrary.notAProperList(m, args[2], "sort/4");
            if (key > 0) for (int i = 0; i < es.size(); i++) extractKey(m, es.get(i), key);

            final Comparator<Term> cmp = new Comparator<Term>() {
                @Override public int compare(Term a, Term b) {
                    Term ka = (key == 0) ? a : extractKey(null, a, key);
                    Term kb = (key == 0) ? b : extractKey(null, b, key);
                    int c = Unify.compareTerms(ka, kb, g);
                    return ascending ? c : -c;
                }
            };
            List<Term> sorted = new ArrayList<Term>(es);
            Collections.sort(sorted, cmp);
            if (dedup) {
                List<Term> out = new ArrayList<Term>(sorted.size());
                for (int i = 0; i < sorted.size(); i++) {
                    if (i == 0 || cmp.compare(sorted.get(i), sorted.get(i - 1)) != 0) out.add(sorted.get(i));
                }
                sorted = out;
            }
            return m.unify(args[3], NativeLibrary.listOf(sorted, NIL)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }

        /** With Key > 0 an element must be a compound with at least Key arguments (ISS-2025-0418). */
        private static Term extractKey(Machine m, Term t, int key) {
            Term d = Unify.deref(t);
            if (!(d instanceof CompoundTerm)) {
                throw Errors.type("compound", (m == null) ? d : m.resolve(d), "sort/4");
            }
            CompoundTerm c = (CompoundTerm) d;
            if (key > c.getArguments().size()) {
                throw Errors.domain("argument_index", Number.valueOf(key), "sort/4");
            }
            return c.getArguments().get(key - 1);
        }
    }

    // ------------------------------------------------------------------ predsort/3

    /** {@code predsort(+Pred, +List, -Sorted)}: {@code call(Pred, Order, X, Y)}, {@code =} merges. */
    private static final class PredSort3 implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Term pred = m.deref(args[0]);
            if (pred instanceof Variable) throw Errors.instantiation("predsort/3");
            if (!(pred instanceof Atom) && !(pred instanceof CompoundTerm)) {
                throw Errors.type("callable", m.resolve(pred), "predsort/3");
            }
            List<Term> es = NativeLibrary.elements(args[1], m.guard());
            if (es == null) return Outcome.FAILURE;   // SWI: predsort fails on a non-list
            // START_CHANGE: ISS-2025-0549 - wave P2.11: an array merge sort (no sub-list copies
            // per level) calling the comparator through one reusable goal shape: the functor atom
            // and the fixed arguments are taken once, each call builds one argument array.
            Term[] arr = es.toArray(new Term[es.size()]);
            Cmp cmp = new Cmp(pred);
            int n = mergeSort(m, cmp, arr, new Term[arr.length], 0, arr.length);
            if (n < 0) return Outcome.FAILURE;   // SWI: Pred failed on some pair
            List<Term> sorted = Arrays.asList(arr).subList(0, n);
            return m.unify(args[2], NativeLibrary.listOf(sorted, NIL)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }

        /** The comparator goal {@code call(Pred, O, A, B)} with Pred's functor and args prefetched. */
        private static final class Cmp {
            final Atom functor;
            final Term[] fixed;
            Cmp(Term pred) {
                if (pred instanceof CompoundTerm) {
                    CompoundTerm c = (CompoundTerm) pred;
                    functor = c.getFunctor();
                    fixed = new Term[c.arity()];
                    for (int i = 0; i < fixed.length; i++) fixed[i] = c.arg(i);
                } else {
                    functor = (Atom) pred;
                    fixed = new Term[0];
                }
            }
            Term goal(Term o, Term a, Term b) {
                Term[] as = new Term[fixed.length + 3];
                System.arraycopy(fixed, 0, as, 0, fixed.length);
                as[fixed.length] = o;
                as[fixed.length + 1] = a;
                as[fixed.length + 2] = b;
                return new CompoundTerm(functor, as);
            }
        }

        /**
         * Sort {@code a[lo..hi)} in place (stable; '=' drops the right-hand element) and return the
         * new end of the range, or -1 when the comparator failed or answered outside {@code <,=,>}.
         */
        private static int mergeSort(Machine m, Cmp cmp, Term[] a, Term[] tmp, int lo, int hi) {
            if (hi - lo <= 1) return hi;
            int mid = (lo + hi) >>> 1;
            int le = mergeSort(m, cmp, a, tmp, lo, mid);
            if (le < 0) return -1;
            int re = mergeSort(m, cmp, a, tmp, mid, hi);
            if (re < 0) return -1;
            int i = lo, j = mid, k = lo;
            while (i < le && j < re) {
                m.guard().step();
                int o = compare(m, cmp, a[i], a[j]);
                if (o == Integer.MIN_VALUE) return -1;
                if (o < 0)      tmp[k++] = a[i++];
                else if (o > 0) tmp[k++] = a[j++];
                else            { tmp[k++] = a[i++]; j++; }   // '=' merges
            }
            while (i < le) tmp[k++] = a[i++];
            while (j < re) tmp[k++] = a[j++];
            System.arraycopy(tmp, lo, a, lo, k - lo);
            return k;
        }

        /**
         * One {@code call(Pred, Order, A, B)}: -1/0/1, or MIN_VALUE for "fail". The comparison runs
         * inside its own mark/undo extent (invariant 1: undo BEFORE closing the extent), so a
         * comparator that binds parts of the elements leaves nothing behind.
         */
        private static int compare(Machine m, Cmp cmp, Term a, Term b) {
            Variable order = new Variable();
            Term goal = cmp.goal(order, a, b);
            Bindings bs = m.bindings();
            int mark = bs.mark();
            bs.forceTrail++;
            String out = null;
            try {
                if (m.runOnce(goal)) {
                    Term o = Unify.deref(order);
                    if (o instanceof Atom) out = ((Atom) o).getName();
                }
                bs.undo(mark);
            } finally {
                bs.forceTrail--;
            }
            // An Order outside <, =, > makes predsort FAIL (SWI, ISS-2025-0419) - never an error,
            // and never a silent default ordering.
            if ("<".equals(out)) return -1;
            if (">".equals(out)) return 1;
            if ("=".equals(out)) return 0;
            return Integer.MIN_VALUE;
        }
        // END_CHANGE: ISS-2025-0549
    }

    // ------------------------------------------------------------------ max_list/2, min_list/2

    /** {@code max_list(+List, -N)} / {@code min_list(+List, -N)}: numbers only, fails otherwise. */
    private static final class MinMaxList implements Builtin {
        private final boolean max;
        MinMaxList(boolean max) { this.max = max; }

        // START_CHANGE: ISS-2025-0603 - P4.10: SWI's max_list/min_list evaluate their elements
        // (Max is max(Max0, X)): a non-number raises type_error(evaluable, ...) instead of
        // failing, big integers compare exactly, and a partial list is instantiation_error.
        @Override public Outcome call(Machine m, Term[] args) {
            List<Term> es = NativeLibrary.elements(args[0], m.guard());
            String ind = max ? "max_list/2" : "min_list/2";
            if (es == null) throw NativeLibrary.notAProperList(m, args[0], ind);
            if (es.isEmpty()) return Outcome.FAILURE;
            Number best = m.evalNum(es.get(0), ind);
            Atom fn = new Atom(max ? "max" : "min");
            int n = es.size();
            int i = 1;
            if (best.isInteger() && best.fitsInLong()) {     // primitive scan while all are longs
                long b = best.longValue();
                int bi = 0;
                for (; i < n; i++) {
                    Term e = Unify.deref(es.get(i));
                    if (!(e instanceof Number) || !((Number) e).isInteger() || !((Number) e).fitsInLong()) break;
                    long y = ((Number) e).longValue();
                    if (max ? y > b : y < b) { b = y; bi = i; }
                }
                best = (bi == 0) ? best : (Number) Unify.deref(es.get(bi));
            }
            for (; i < n; i++) {
                best = m.evalNum(new CompoundTerm(fn, new Term[] { best, es.get(i) }), ind);
                if ((i & 0x3FF) == 0) m.guard().step();
            }
            return m.unify(args[1], best) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
        // END_CHANGE: ISS-2025-0603
    }

    // START_CHANGE: ISS-2025-0608 - P4.15: statistics/2 with SWI-Prolog's keys and value shapes.
    // cputime/process_cputime are FLOAT seconds, inferences comes from the machine's step
    // counter, runtime/walltime/real_time/system_time are [Total, SinceLast] pairs. The output is
    // UNIFIED (statistics(runtime, [T|_]) works; the registry version compared with equals()).
    // An unknown key is domain_error(statistics_key, K).
    private static final ThreadLocal<long[]> LAST = new ThreadLocal<long[]>() {
        @Override protected long[] initialValue() { return new long[4]; }  // runtime, walltime, real_time, system_time
    };

    private static long processCpuNanos() {
        java.lang.management.OperatingSystemMXBean os = java.lang.management.ManagementFactory.getOperatingSystemMXBean();
        if (os instanceof com.sun.management.OperatingSystemMXBean) {
            long t = ((com.sun.management.OperatingSystemMXBean) os).getProcessCpuTime();
            if (t >= 0) return t;
        }
        java.lang.management.ThreadMXBean tb = java.lang.management.ManagementFactory.getThreadMXBean();
        return tb.isCurrentThreadCpuTimeSupported() ? tb.getCurrentThreadCpuTime() : System.nanoTime();
    }

    private static long threadCpuNanos() {
        java.lang.management.ThreadMXBean tb = java.lang.management.ManagementFactory.getThreadMXBean();
        return tb.isCurrentThreadCpuTimeSupported() ? tb.getCurrentThreadCpuTime() : processCpuNanos();
    }

    private static long threadUserNanos() {
        java.lang.management.ThreadMXBean tb = java.lang.management.ManagementFactory.getThreadMXBean();
        return tb.isCurrentThreadCpuTimeSupported() ? tb.getCurrentThreadUserTime() : processCpuNanos();
    }

    static long inferences(Machine m) {
        ResourceGuard g = m.guard();
        return m.engine().inferences() + (g == null ? 0 : g.getSteps());
    }

    private static Term pair(long total, int slot) {
        long[] last = LAST.get();
        long since = total - last[slot];
        last[slot] = total;
        return Machine.makeList(Arrays.<Term>asList(Number.valueOf(total), Number.valueOf(since)));
    }

    private static Term two(long a, long b) {
        return Machine.makeList(Arrays.<Term>asList(Number.valueOf(a), Number.valueOf(b)));
    }

    static final List<String> STAT_KEYS = Arrays.asList(
        "runtime", "cputime", "process_cputime", "inferences", "walltime", "real_time", "epoch",
        "process_epoch", "system_time", "stack", "stack_limit", "localused", "globalused",
        "trailused", "heapused", "local", "global", "trail", "heap", "threads",
        "garbage_collection", "atoms", "functors", "predicates", "modules", "clauses", "codes",
        "c_stack", "thread_cputime", "errors", "warnings");

    static Term statistic(Machine m, String key) {
        java.lang.management.MemoryMXBean mem = java.lang.management.ManagementFactory.getMemoryMXBean();
        java.lang.management.RuntimeMXBean rt = java.lang.management.ManagementFactory.getRuntimeMXBean();
        switch (key) {
            case "runtime":         return pair(threadUserNanos() / 1_000_000L, 0);
            case "cputime":         return new Number(threadCpuNanos() / 1e9);
            case "thread_cputime":  return new Number(threadCpuNanos() / 1e9);
            case "process_cputime": return new Number(processCpuNanos() / 1e9);
            case "inferences":      return Number.valueOf(inferences(m));
            case "walltime":        return pair(System.currentTimeMillis() - rt.getStartTime(), 1);
            case "real_time":       return pair(System.currentTimeMillis() / 1000L, 2);
            case "system_time": {
                long sys = Math.max(0, threadCpuNanos() - threadUserNanos()) / 1_000_000L;
                return pair(sys, 3);
            }
            case "epoch": case "process_epoch": return new Number(rt.getStartTime() / 1000.0);
            case "stack": case "globalused": case "heapused":
                return Number.valueOf(mem.getHeapMemoryUsage().getUsed());
            case "stack_limit": {
                long max = mem.getHeapMemoryUsage().getMax();
                return Number.valueOf(max < 0 ? Runtime.getRuntime().maxMemory() : max);
            }
            case "localused": case "c_stack": return Number.valueOf(mem.getNonHeapMemoryUsage().getUsed());
            case "trailused": return Number.valueOf(0L);
            case "heap": case "global": {
                long used = mem.getHeapMemoryUsage().getUsed();
                return two(used, Math.max(0, mem.getHeapMemoryUsage().getCommitted() - used));
            }
            case "local": {
                long used = mem.getNonHeapMemoryUsage().getUsed();
                return two(used, Math.max(0, mem.getNonHeapMemoryUsage().getCommitted() - used));
            }
            case "trail": return two(0, 0);
            case "threads": return Number.valueOf((long) Thread.activeCount());
            case "garbage_collection": {
                long count = 0, time = 0;
                for (java.lang.management.GarbageCollectorMXBean b
                        : java.lang.management.ManagementFactory.getGarbageCollectorMXBeans()) {
                    if (b.getCollectionCount() > 0) count += b.getCollectionCount();
                    if (b.getCollectionTime() > 0) time += b.getCollectionTime();
                }
                return Machine.makeList(Arrays.<Term>asList(Number.valueOf(count), Number.valueOf(0L), Number.valueOf(time)));
            }
            case "predicates": {
                it.denzosoft.jprolog.core.engine.KnowledgeBase kb = m.engine().kb();
                return Number.valueOf(kb == null ? 0L : (long) kb.getCurrentPredicates().size());
            }
            case "clauses": {
                it.denzosoft.jprolog.core.engine.KnowledgeBase kb = m.engine().kb();
                return Number.valueOf(kb == null ? 0L : (long) kb.getRules().size());
            }
            case "modules": return Number.valueOf((long) Math.max(1, m.engine().modules4().currentModuleTerms().size()));
            case "atoms": case "functors": case "codes": case "errors": case "warnings":
                return Number.valueOf(0L);
            default: return null;
        }
    }

    private static final class StatisticsB implements Builtin {
        @Override public Outcome call(Machine m, final Term[] args) {
            Term k = m.deref(args[0]);
            if (k instanceof Variable) throw Errors.instantiation("statistics/2");
            if (!(k instanceof Atom)) throw Errors.type("atom", m.resolve(k), "statistics/2");
            Term v = statistic(m, ((Atom) k).getName());
            if (v == null) throw Errors.domain("statistics_key", k, "statistics/2");
            return m.unify(args[1], v) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    /** statistics/0: SWI's summary, to user_error. */
    private static final class Statistics0B implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            java.lang.management.RuntimeMXBean rt = java.lang.management.ManagementFactory.getRuntimeMXBean();
            java.lang.management.MemoryMXBean mem = java.lang.management.ManagementFactory.getMemoryMXBean();
            double cpu = threadCpuNanos() / 1e9;
            StringBuilder sb = new StringBuilder();
            sb.append("% Started at ").append(new java.util.Date(rt.getStartTime())).append('\n');
            sb.append(String.format(java.util.Locale.ROOT, "%% %.3f seconds cpu time for %,d inferences%n",
                cpu, inferences(m)));
            sb.append(String.format(java.util.Locale.ROOT, "%% %,d bytes heap in use, %,d bytes non-heap, %d threads%n",
                mem.getHeapMemoryUsage().getUsed(), mem.getNonHeapMemoryUsage().getUsed(), Thread.activeCount()));
            Streams st = it.denzosoft.jprolog.builtin.io.StreamManager.streams();
            java.io.PrintStream ps = st.writerFor(st.userError());
            ps.print(sb);
            ps.flush();
            return Outcome.SUCCESS;
        }
    }
    // END_CHANGE: ISS-2025-0608

    // ------------------------------------------------------------------ current_op/3

    /** {@code current_op(?P, ?Type, ?Name)} — lazy over the engine's own operator store. */
    private static final class CurrentOp implements Builtin {
        @Override public Outcome call(Machine m, final Term[] args) {
            // START_CHANGE: ISS-2025-0504 - ISO 8.14.4.3. A bound argument of the wrong shape used
            // to make current_op/3 simply fail (no operator could ever match it); the standard asks
            // for a type or domain error, and a silent failure hides the caller's typo.
            Term pT = m.deref(args[0]);
            Term sT = m.deref(args[1]);
            Term nT = m.deref(args[2]);
            if (!(pT instanceof Variable)) {
                if (!(pT instanceof Number) || !((Number) pT).isInteger()) {
                    throw Errors.type("integer", m.resolve(pT), "current_op/3");
                }
                long pv = ((Number) pT).longValue();
                if (pv < 0 || pv > 1200) {
                    throw Errors.domain("operator_priority", m.resolve(pT), "current_op/3");
                }
            }
            if (!(sT instanceof Variable)) {
                if (!(sT instanceof Atom)) throw Errors.type("atom", m.resolve(sT), "current_op/3");
                if (!isOperatorType(((Atom) sT).getName())) {
                    throw Errors.domain("operator_specifier", m.resolve(sT), "current_op/3");
                }
            }
            if (!(nT instanceof Variable) && !(nT instanceof Atom)) {
                throw Errors.type("atom", m.resolve(nT), "current_op/3");
            }
            // END_CHANGE: ISS-2025-0504
            final List<Ops.Def> defs = Ops.current().visible();
            if (defs.isEmpty()) return Outcome.FAILURE;
            final int[] i = {0};
            Generator gen = new Generator() {
                @Override public boolean next(Machine mm) {
                    while (i[0] < defs.size()) {
                        Ops.Def d = defs.get(i[0]++);
                        if (i[0] >= defs.size()) mm.lastSolution();
                        // ONE extent for the three unifications (invariant 12): unify binds as it
                        // walks, so undoing only the failed one would leave the earlier arguments
                        // bound and every later operator would then fail to match.
                        Bindings b = mm.bindings();
                        int mark = b.mark();
                        b.forceTrail++;
                        boolean ok;
                        try {
                            ok = Unify.unify(args[0], Number.valueOf(d.precedence), b)
                              && Unify.unify(args[1], new Atom(d.type), b)
                              && Unify.unify(args[2], new Atom(d.name), b);
                            if (!ok) b.undo(mark);
                        } finally {
                            b.forceTrail--;
                        }
                        if (ok) return true;
                    }
                    return false;
                }
            };
            return m.pushGenerator(gen) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ nb_getval/2, b_getval/2

    /** {@code nb_getval(+Name, ?Value)} / {@code b_getval(+Name, ?Value)}. */
    private static final class GetVal implements Builtin {
        private final String ind;
        GetVal(String ind) { this.ind = ind; }

        @Override public Outcome call(Machine m, Term[] args) {
            Term nameT = m.deref(args[0]);
            if (nameT instanceof Variable) throw Errors.instantiation(ind);
            if (!(nameT instanceof Atom)) throw Errors.type("atom", m.resolve(nameT), ind);
            Prolog p = m.engine().prolog();
            Term v = (p == null) ? null : p.nbGetval(((Atom) nameT).getName());
            if (v == null) throw Errors.existence("variable", nameT, ind);
            return m.unify(args[1], v) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }
}
// END_CHANGE: ISS-2025-0486
