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
import java.util.List;

// START_CHANGE: ISS-2025-0453 - engine v4 wave W3, design B.5: the list / term / database library
// as native v4 built-ins and lazy Generators.
/**
 * The library predicates that were the v4 engine's remaining bottleneck, rewritten against the v4
 * SPI.
 *
 * <h3>Why</h3>
 * A registry built-in reaches the machine through {@link LegacyBuiltinAdapter}, which must
 * (1) {@link Unify#resolve} the whole goal and (2) index its unbound cells by name. On a goal
 * holding a million-element list both walks are O(list), and the built-in itself then materialises
 * every solution as a {@code Map<String,Term>} before the first one is used. That is why the W1/W2
 * measurements had v4 1.3-2x SLOWER than v2 on 1 M-element `length`, `msort`, `copy_term`,
 * `findall+member`, `sum_list`, `reverse` and `append` — the core was faster, the bridge was not.
 * A native built-in sees the argument cells directly: no resolve, no name index, no solution maps.
 *
 * <h3>Laziness</h3>
 * {@code member/2}, {@code append/3}, {@code select/3}, {@code nth0/3}, {@code nth1/3} and
 * {@code clause/2} install a {@link Generator}: one alternative per redo, O(1) memory, and the
 * enumeration stops as soon as the caller cuts. The eager versions built the complete solution list
 * first, so {@code once(member(X, VeryLongList))} paid for the whole list (design limit L-08).
 * A generator that hands out its last alternative calls {@link Machine#lastSolution}, so the
 * deterministic cases leave no choice point behind at all.
 *
 * <h3>Errors</h3>
 * The ISO/legacy error behaviour of each predicate is reproduced exactly — those error clauses are
 * pinned by the suite ({@code sort/2} and {@code msort/2} raise instantiation_error on a partial
 * list and type_error(list, L) on a non-list, {@code numlist/3} type-checks its bounds,
 * {@code clause/2} raises permission_error(access, private_procedure, PI) on a built-in).
 */
final class NativeLibrary {

    private NativeLibrary() {}

    private static final Atom NIL = new Atom("[]");
    private static final Atom DOT = new Atom(".");
    private static final Atom TRUE = new Atom("true");

    static void register(BuiltinTable t) {
        // START_CHANGE: ISS-2025-0468 - wave W6, design B.10 / B.17 decision 4.
        //
        // The two W3 deviations are PAID OFF: `append(X, Y, Z)` fully open now enumerates and
        // `member(X, PartialList)` now extends the list — see AppendB and MemberGen below. The
        // reference definitions of both live in src/main/resources/prelude/lists.pl as the
        // two-clause Prolog predicates of module `lists`, which is what `lists:append/3` runs and
        // what a trace shows when the module is addressed explicitly; these generators are the
        // observationally equivalent fast path for the unqualified call
        // (`EngineV4ModulesTest.testISS0468_NativeAndPreludeListPredicatesAgree` pins the
        // equivalence — change one and you must change the other).
        //
        // Keeping them native is a measurement, not a preference: on a 1 000 000-element list the
        // Prolog clauses cost 1520 ms for `append/3` against 120 ms for this generator, and
        // 1125 ms for `member/2` against 196 ms, because a clause walk pushes one choice point per
        // element where the generator pushes one for the whole call. Section 12.4 of
        // docs/reports/report-engine-v4-progress.md has the full table for every list predicate.
        // END_CHANGE: ISS-2025-0468
        t.register("member", 2, new MemberB(false));
        t.register("memberchk", 2, new MemberB(true));
        t.register("append", 3, new AppendB());
        t.register("select", 3, new SelectB(false));
        t.register("selectchk", 3, new SelectB(true));
        t.register("nth0", 3, new NthB(0));
        t.register("nth1", 3, new NthB(1));
        t.register("last", 2, new LastB());
        t.register("reverse", 2, new ReverseB());
        t.register("length", 2, new LengthB());
        t.register("msort", 2, new SortB(false));
        t.register("sort", 2, new SortB(true));
        t.register("sum_list", 2, new SumListB());
        t.register("sumlist", 2, new SumListB());
        t.register("numlist", 3, new NumlistB());
        t.register("copy_term", 2, new CopyTermB());
        t.register("clause", 2, new ClauseB());
        t.register("sub_atom", 5, new SubAtomB(true));
        t.register("sub_string", 5, new SubAtomB(false));
    }

    // ------------------------------------------------------------------ list spine helpers

    static boolean isCons(Term t) {
        return t instanceof CompoundTerm && ".".equals(((CompoundTerm) t).getName())
            && ((CompoundTerm) t).getArguments().size() == 2;
    }

    static Term head(Term cons) { return ((CompoundTerm) cons).getArguments().get(0); }
    static Term tail(Term cons) { return ((CompoundTerm) cons).getArguments().get(1); }

    static boolean isNil(Term t) { return t instanceof Atom && "[]".equals(((Atom) t).getName()); }

    /**
     * Collect the elements of a proper list. Returns null for a partial list, a non-list or a
     * cyclic spine. Iterative, deref-based and guard-polled: a million-element list costs no Java
     * stack and stays cancellable.
     */
    static List<Term> elements(Term list, ResourceGuard g) {
        List<Term> out = new ArrayList<Term>();
        Term slow = Unify.deref(list);
        Term fast = slow;
        int n = 0;
        while (isCons(fast)) {
            out.add(head(fast));
            fast = Unify.deref(tail(fast));
            if (!isCons(fast)) break;
            out.add(head(fast));
            fast = Unify.deref(tail(fast));
            slow = Unify.deref(tail(slow));
            if (fast == slow) return null;                        // cyclic
            if ((++n & 0x3FF) == 0 && g != null) g.step();
        }
        return isNil(fast) ? out : null;
    }

    /** The spine tail after walking the known prefix; {@code prefix} receives the elements. */
    static Term spineTail(Term list, List<Term> prefix, ResourceGuard g) {
        Term slow = Unify.deref(list);
        Term fast = slow;
        int n = 0;
        while (isCons(fast)) {
            if (prefix != null) prefix.add(head(fast));
            fast = Unify.deref(tail(fast));
            if (!isCons(fast)) break;
            if (prefix != null) prefix.add(head(fast));
            fast = Unify.deref(tail(fast));
            slow = Unify.deref(tail(slow));
            if (fast == slow) return null;                        // cyclic
            if ((++n & 0x3FF) == 0 && g != null) g.step();
        }
        return fast;
    }

    static Term listOf(List<Term> elems, Term tail) {
        Term out = tail;
        for (int i = elems.size() - 1; i >= 0; i--) {
            out = new CompoundTerm(DOT, Arrays.asList(elems.get(i), out));
        }
        return out;
    }

    // ------------------------------------------------------------------ member/2, memberchk/2

    private static final class MemberB implements Builtin {
        private final boolean once;
        MemberB(boolean once) { this.once = once; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            final Term x = args[0];
            if (once) {
                Term cur = Unify.deref(args[1]);
                int n = 0;
                while (isCons(cur)) {
                    if (m.unifyOrUndo(x, head(cur))) return Outcome.SUCCESS;
                    cur = Unify.deref(tail(cur));
                    if ((++n & 0x3FF) == 0) m.guard().step();
                }
                // START_CHANGE: ISS-2025-0468 - an OPEN tail is extended, not a dead end:
                // `memberchk(a, L)` binds `L = [a|_]`, as in SWI. member/2 gets the same mode from
                // its two prelude clauses; memberchk/2 stays native (it is semi-deterministic and
                // 4x faster on a long list), so it needs the case spelled out here.
                if (cur instanceof it.denzosoft.jprolog.core.terms.Variable) {
                    return m.unify(cur, new CompoundTerm(DOT,
                        Arrays.asList(x, (Term) new it.denzosoft.jprolog.core.terms.Variable())))
                        ? Outcome.SUCCESS : Outcome.FAILURE;
                }
                // END_CHANGE: ISS-2025-0468
                return Outcome.FAILURE;
            }
            return m.pushGenerator(new MemberGen(x, args[1])) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }
    }

    /**
     * {@code member(X, List)}: one element per redo, O(1) memory, and the walk stops the moment the
     * caller cuts — where the eager built-in first materialised one solution map per element.
     *
     * <p>START_CHANGE: ISS-2025-0468 — the open-tail mode is now implemented, which is the W3
     * deviation being paid off. When the walk reaches an unbound tail the generator keeps going by
     * EXTENDING it: {@code member(X, L)} with {@code L} unbound yields {@code L = [X|_]},
     * {@code L = [_,X|_]}, … exactly as the two prelude clauses do, in the same order. It is
     * therefore an infinite generator in that mode and never announces a last solution — which is
     * correct, and is why {@code member(X, L), L = [a|_]} succeeds instead of failing.
     */
    private static final class MemberGen implements Generator {
        private final Term x;
        private Term cursor;
        /** Once the walk has reached an open tail: how many elements to skip before X. */
        private int extension = -1;

        MemberGen(Term x, Term list) { this.x = x; this.cursor = list; }

        @Override
        public boolean next(Machine m) {
            while (true) {
                Term cur = Unify.deref(cursor);
                if (cur instanceof Variable) {
                    int k = (extension < 0) ? 0 : extension;
                    extension = k + 1;
                    List<Term> skipped = new ArrayList<Term>(k);
                    for (int i = 0; i < k; i++) skipped.add(new Variable());
                    Term built = listOf(skipped,
                        new CompoundTerm(DOT, Arrays.asList(x, (Term) new Variable())));
                    m.guard().step();
                    if (m.unifyOrUndo(cur, built)) return true;
                    return false;                       // the tail cannot be a list at all
                }
                if (!isCons(cur)) return false;
                Term h = head(cur);
                cursor = tail(cur);
                if (m.unifyOrUndo(x, h)) {
                    Term rest = Unify.deref(cursor);
                    if (!isCons(rest) && !(rest instanceof Variable)) m.lastSolution();
                    return true;
                }
                m.guard().step();
            }
        }
    }
    // END_CHANGE: ISS-2025-0468

    // ------------------------------------------------------------------ append/3

    /**
     * {@code append(?List1, ?List2, ?List3)}, keeping the three-way mode analysis of ISS-2025-0379
     * so v2 and v4 answer identically:
     * <ul>
     *   <li>{@code List1} proper — build {@code [e1,...,en|List2]} and unify: deterministic;</li>
     *   <li>otherwise {@code List3} proper — enumerate its {@code n+1} splits, now <b>lazily</b>,
     *       one per redo instead of all n+1 solution maps up front;</li>
     *   <li>otherwise (both open) — ISS-2025-0468, the W3 deviation paid off: it ENUMERATES,
     *       {@code List1 = []}, {@code [_]}, {@code [_,_]}, … exactly as the two prelude clauses
     *       do and in the same order, so {@code append(X, Y, Z), length(X, 2)} terminates instead
     *       of failing. It is an infinite generator, which is why
     *       {@code testISS0379_AppendFullyOpenDoesNotThrow} takes the first solution with a cut on
     *       v4.</li>
     * </ul>
     */
    private static final class AppendB implements Builtin {
        @Override
        public Outcome call(Machine m, final Term[] args) {
            ResourceGuard g = m.guard();
            List<Term> a = elements(args[0], g);
            if (a != null) {                                       // (+,?,?): deterministic
                return m.unify(args[2], listOf(a, args[1])) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            List<Term> c = elements(args[2], g);
            if (c == null) {
                // START_CHANGE: ISS-2025-0468 - both open: enumerate, do not stop at List1 = [].
                final List<Term> prefix = new ArrayList<Term>();
                final Term tail1 = spineTail(args[0], prefix, g);
                if (!(tail1 instanceof Variable)) return Outcome.FAILURE;
                final int[] extra = {0};
                Generator open = new Generator() {
                    @Override public boolean next(Machine mm) {
                        int n = extra[0]++;
                        List<Term> fresh = new ArrayList<Term>(n);
                        for (int i = 0; i < n; i++) fresh.add(new Variable());
                        Bindings b = mm.bindings();
                        int mark = b.mark();
                        b.forceTrail++;
                        boolean ok;
                        try {
                            ok = Unify.unify(tail1, listOf(fresh, NIL), b)
                              && Unify.unify(args[2], listOf(prefix, listOf(fresh, args[1])), b);
                            if (!ok) b.undo(mark);                 // ISS-2025-0448 ordering
                        } finally {
                            b.forceTrail--;
                        }
                        mm.guard().step();
                        return ok;
                    }
                };
                return m.pushGenerator(open) ? Outcome.SUSPENDED : Outcome.FAILURE;
                // END_CHANGE: ISS-2025-0468
            }
            final int max = c.size();
            final int[] k = {0};
            Generator gen = new Generator() {
                @Override
                public boolean next(Machine mm) {
                    while (k[0] <= max) {
                        int n = k[0]++;
                        if (k[0] > max) mm.lastSolution();
                        List<Term> fresh = new ArrayList<Term>(n);
                        for (int i = 0; i < n; i++) fresh.add(new Variable());
                        Bindings b = mm.bindings();
                        int mark = b.mark();
                        b.forceTrail++;
                        boolean ok;
                        try {
                            ok = Unify.unify(args[0], listOf(fresh, NIL), b)
                              && Unify.unify(args[2], listOf(fresh, args[1]), b);
                            if (!ok) b.undo(mark);                 // ISS-2025-0448 ordering
                        } finally {
                            b.forceTrail--;
                        }
                        if (ok) return true;
                        if (n > 0 && (n & 0x3FF) == 0) mm.guard().step();
                    }
                    return false;
                }
            };
            return m.pushGenerator(gen) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ select/3, selectchk/3

    private static final class SelectB implements Builtin {
        private final boolean once;
        SelectB(boolean once) { this.once = once; }

        @Override
        public Outcome call(Machine m, final Term[] args) {
            ResourceGuard g = m.guard();
            final Term x = args[0];
            final List<Term> src = elements(args[1], g);
            final List<Term> rest = (src == null) ? elements(args[2], g) : null;
            if (src == null && rest == null) return Outcome.FAILURE;
            final boolean removing = (src != null);
            final int n = removing ? src.size() : rest.size() + 1;
            final int[] i = {0};
            Generator gen = new Generator() {
                @Override
                public boolean next(Machine mm) {
                    while (i[0] < n) {
                        int pos = i[0]++;
                        if (i[0] >= n) mm.lastSolution();
                        Bindings b = mm.bindings();
                        int mark = b.mark();
                        b.forceTrail++;
                        boolean ok;
                        try {
                            if (removing) {
                                List<Term> without = new ArrayList<Term>(src.size() - 1);
                                for (int j = 0; j < src.size(); j++) if (j != pos) without.add(src.get(j));
                                ok = Unify.unify(x, src.get(pos), b)
                                  && Unify.unify(args[2], listOf(without, NIL), b);
                            } else {                               // insertion mode
                                List<Term> with = new ArrayList<Term>(rest.size() + 1);
                                with.addAll(rest.subList(0, pos));
                                with.add(x);
                                with.addAll(rest.subList(pos, rest.size()));
                                ok = Unify.unify(args[1], listOf(with, NIL), b);
                            }
                            if (!ok) b.undo(mark);
                        } finally {
                            b.forceTrail--;
                        }
                        if (ok) return true;
                    }
                    return false;
                }
            };
            if (once) {
                boolean ok = gen.next(m);
                return ok ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            return m.pushGenerator(gen) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ nth0/3, nth1/3

    private static final class NthB implements Builtin {
        private final int base;
        NthB(int base) { this.base = base; }

        @Override
        public Outcome call(Machine m, final Term[] args) {
            String ind = "nth" + base + "/3";
            Term idx = m.deref(args[0]);
            if (idx instanceof Number) {
                if (!((Number) idx).isInteger()) throw Errors.type("integer", idx, ind);
                long want = ((Number) idx).longValue() - base;
                if (want < 0) return Outcome.FAILURE;
                Term cur = Unify.deref(args[1]);
                long k = 0;
                while (isCons(cur)) {
                    if (k == want) return m.unify(args[2], head(cur)) ? Outcome.SUCCESS : Outcome.FAILURE;
                    cur = Unify.deref(tail(cur));
                    k++;
                    if ((k & 0x3FF) == 0) m.guard().step();
                }
                return Outcome.FAILURE;
            }
            if (!(idx instanceof Variable)) throw Errors.type("integer", idx, ind);
            final int b0 = base;
            final Term[] cursor = { args[1] };
            final long[] pos = {0};
            Generator gen = new Generator() {
                @Override
                public boolean next(Machine mm) {
                    while (true) {
                        Term cur = Unify.deref(cursor[0]);
                        if (!isCons(cur)) return false;
                        Term h = head(cur);
                        long p = pos[0]++;
                        cursor[0] = tail(cur);
                        Bindings bb = mm.bindings();
                        int mark = bb.mark();
                        bb.forceTrail++;
                        boolean ok;
                        try {
                            ok = Unify.unify(args[2], h, bb)
                              && Unify.unify(args[0], Number.valueOf(p + b0), bb);
                            if (!ok) bb.undo(mark);
                        } finally {
                            bb.forceTrail--;
                        }
                        if (ok) {
                            if (!isCons(Unify.deref(cursor[0]))) mm.lastSolution();
                            return true;
                        }
                        mm.guard().step();
                    }
                }
            };
            return m.pushGenerator(gen) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ last/2, reverse/2

    /** ISS-2025-0380: a partial list gets its open tail closed with {@code []}; an improper list
     *  ({@code [a|b]}) fails instead of being silently truncated to its prefix. */
    private static final class LastB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            List<Term> es = new ArrayList<Term>();
            Term tail = spineTail(args[0], es, m.guard());
            if (tail == null || es.isEmpty()) return Outcome.FAILURE;
            if (tail instanceof Variable) {
                if (!m.unifyOrUndo(tail, NIL)) return Outcome.FAILURE;
            } else if (!isNil(tail)) {
                return Outcome.FAILURE;
            }
            return m.unify(args[1], es.get(es.size() - 1)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class ReverseB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            List<Term> es = elements(args[0], m.guard());
            Term out = args[1];
            if (es == null) {
                es = elements(args[1], m.guard());
                if (es == null) return Outcome.FAILURE;
                out = args[0];
            }
            Collections.reverse(es);
            return m.unify(out, listOf(es, NIL)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ length/2

    /**
     * The enumeration mode ({@code length(PartialList, Var)}) is already a native generator on the
     * machine ({@code Machine.lengthEnumerate}); this covers the other three modes, which the
     * legacy built-in answered by resolving and rebuilding the whole list.
     */
    private static final class LengthB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            ResourceGuard g = m.guard();
            long[] len = new long[1];
            Term tail = spineTailCounting(args[0], g, len);
            if (tail == null) return Outcome.FAILURE;                    // cyclic spine
            if (isNil(tail)) {
                return m.unify(args[1], Number.valueOf(len[0])) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (!(tail instanceof Variable)) return Outcome.FAILURE;     // improper list
            Term want = Unify.deref(args[1]);
            if (!(want instanceof Number) || !((Number) want).isInteger()) return Outcome.FAILURE;
            long n = ((Number) want).longValue();
            if (n < 0 || n < len[0]) return Outcome.FAILURE;
            long extra = n - len[0];
            if (extra > Integer.MAX_VALUE) return Outcome.FAILURE;
            List<Term> fresh = new ArrayList<Term>((int) extra);
            for (long i = 0; i < extra; i++) fresh.add(new Variable());
            return m.unify(tail, listOf(fresh, NIL)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }

        /** Spine walk that also reports the prefix length; null on a cyclic spine. Stateless: a
         *  Builtin instance is shared by every machine of every engine. */
        private Term spineTailCounting(Term list, ResourceGuard g, long[] lengthOut) {
            Term slow = Unify.deref(list);
            Term fast = slow;
            long n = 0;
            while (isCons(fast)) {
                n++;
                fast = Unify.deref(tail(fast));
                if (!isCons(fast)) break;
                n++;
                fast = Unify.deref(tail(fast));
                slow = Unify.deref(tail(slow));
                if (fast == slow) return null;
                if ((n & 0x3FF) == 0 && g != null) g.step();
            }
            lengthOut[0] = n;
            return fast;
        }
    }

    // ------------------------------------------------------------------ msort/2, sort/2

    private static final class SortB implements Builtin {
        private final boolean dedup;
        SortB(boolean dedup) { this.dedup = dedup; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            final ResourceGuard g = m.guard();
            List<Term> es = elements(args[0], g);
            if (es == null) throw notAProperList(m, args[0], dedup ? "sort/2" : "msort/2");
            Collections.sort(es, new java.util.Comparator<Term>() {
                @Override public int compare(Term a, Term b) { return Unify.compareTerms(a, b, g); }
            });
            if (dedup) {
                List<Term> uniq = new ArrayList<Term>(es.size());
                for (int i = 0; i < es.size(); i++) {
                    if (i == 0 || Unify.compareTerms(es.get(i), es.get(i - 1), g) != 0) uniq.add(es.get(i));
                }
                es = uniq;
            }
            return m.unify(args[1], listOf(es, NIL)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    /** ISS-2025-0351: instantiation_error on a partial list, type_error(list, L) on a non-list. */
    static it.denzosoft.jprolog.core.exceptions.PrologException notAProperList(Machine m, Term list,
                                                                              String context) {
        Term tail = spineTail(list, null, m.guard());
        if (tail instanceof Variable) return Errors.instantiation(context);
        return Errors.type("list", m.resolve(list), context);
    }

    // ------------------------------------------------------------------ sum_list/2, numlist/3

    private static final class SumListB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            List<Term> es = elements(args[0], m.guard());
            if (es == null) return Outcome.FAILURE;
            boolean allIntegers = true;
            long longSum = 0;
            double doubleSum = 0;
            for (int i = 0; i < es.size(); i++) {
                Term e = Unify.deref(es.get(i));
                if (!(e instanceof Number)) return Outcome.FAILURE;
                Number num = (Number) e;
                doubleSum += num.getValue();
                if (allIntegers && num.isInteger() && !num.isBigInteger()) longSum += num.longValue();
                else allIntegers = false;
            }
            Term sum = allIntegers ? Number.valueOf(longSum) : new Number(doubleSum);
            return m.unify(args[1], sum) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class NumlistB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term lo = m.deref(args[0]);
            Term hi = m.deref(args[1]);
            if (lo instanceof Variable || hi instanceof Variable) throw Errors.instantiation("numlist/3");
            if (!(lo instanceof Number) || !((Number) lo).isInteger()) throw Errors.type("integer", lo, "numlist/3");
            if (!(hi instanceof Number) || !((Number) hi).isInteger()) throw Errors.type("integer", hi, "numlist/3");
            long low = ((Number) lo).longValue();
            long high = ((Number) hi).longValue();
            if (low > high) return Outcome.FAILURE;
            Term out = NIL;
            ResourceGuard g = m.guard();
            for (long i = high; i >= low; i--) {
                out = new CompoundTerm(DOT, Arrays.asList((Term) Number.valueOf(i), out));
                if (((high - i) & 0x3FF) == 0 && g != null) g.step();
            }
            return m.unify(args[2], out) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ copy_term/2

    private static final class CopyTermB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            return m.unify(args[1], m.copy(args[0])) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ clause/2

    /**
     * {@code clause(?Head, ?Body)} over the v4 {@link ClauseStore}: one clause per redo, filtered
     * by the caller's generation so the logical update view holds while the goal backtracks over a
     * predicate that is being modified (ISS-2025-0396 family). Prelude clauses are deliberately
     * invisible here — they are not in the knowledge base.
     */
    private static final class ClauseB implements Builtin {
        @Override
        public Outcome call(Machine m, final Term[] args) {
            Term head = m.deref(args[0]);
            if (head instanceof Variable) throw Errors.instantiation("clause/2");
            if (!(head instanceof Atom) && !(head instanceof CompoundTerm)) {
                throw Errors.type("callable", head, "clause/2");
            }
            Term body = m.deref(args[1]);
            if (!(body instanceof Variable) && !(body instanceof Atom) && !(body instanceof CompoundTerm)) {
                throw Errors.type("callable", body, "clause/2");
            }
            String f;
            int ar;
            if (head instanceof Atom) { f = ((Atom) head).getName(); ar = 0; }
            else { f = ((CompoundTerm) head).getName(); ar = ((CompoundTerm) head).getArguments().size(); }
            // START_CHANGE: ISS-2025-0501 - the natives and the prelude exports are private
            // procedures too, not only the legacy registry entries.
            if (m.isProtectedProcedure(f, ar)) {
                throw Errors.permission("access", "private_procedure",
                    Machine.indicator(f, ar), "clause/2");
            }
            // END_CHANGE: ISS-2025-0501
            ClauseStore.Predicate p = m.engine().store().lookup(f, ar);
            final Clause[] candidates = p.all();
            if (candidates.length == 0) return Outcome.FAILURE;
            final long gen = m.engine().store().generation();
            final int[] i = {0};
            Generator g = new Generator() {
                @Override
                public boolean next(Machine mm) {
                    while (i[0] < candidates.length) {
                        Clause cl = candidates[i[0]++];
                        if (!cl.isAlive(gen)) continue;
                        CompoundTerm t = (CompoundTerm) cl.toTerm();     // ':-'(Head, Body), renamed
                        Bindings b = mm.bindings();
                        int mark = b.mark();
                        b.forceTrail++;
                        boolean ok;
                        try {
                            ok = Unify.unify(args[0], t.getArguments().get(0), b)
                              && Unify.unify(args[1], t.getArguments().get(1), b);
                            if (!ok) b.undo(mark);
                        } finally {
                            b.forceTrail--;
                        }
                        if (ok) {
                            if (i[0] >= candidates.length) mm.lastSolution();
                            return true;
                        }
                        mm.guard().step();
                    }
                    return false;
                }
            };
            return m.pushGenerator(g) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }
    }


    // ------------------------------------------------------------------ sub_atom/5, sub_string/5

    /**
     * {@code sub_atom(+Atom, ?Before, ?Length, ?After, ?Sub)} and its string twin, as a lazy
     * generator over exactly the candidate (Before, Length) pairs the bound arguments allow —
     * so {@code once(sub_atom(LongAtom, _, _, _, Sub))} costs one candidate instead of the whole
     * O(n^2) cross-product the eager built-in materialised as solution maps.
     *
     * <p>Enumeration order is the ISO one and matches the legacy built-in: Before outer, Length
     * inner. One legacy bug is NOT reproduced: with an empty {@code Sub} the old code looped on
     * {@code indexOf("", idx)}, which stops advancing past the end of the atom, so
     * {@code sub_atom(abc, B, L, A, '')} hung the v2 engine until the heap ran out. Here the empty
     * substring simply enumerates the {@code n+1} positions once.
     */
    private static final class SubAtomB implements Builtin {
        private final boolean atoms;
        SubAtomB(boolean atoms) { this.atoms = atoms; }

        @Override
        public Outcome call(Machine m, final Term[] args) {
            final String ind = atoms ? "sub_atom/5" : "sub_string/5";
            Term src = m.deref(args[0]);
            final String text;
            if (atoms) {
                if (!(src instanceof Atom)) {
                    throw new it.denzosoft.jprolog.core.exceptions.PrologEvaluationException(
                        "sub_atom/5: first argument must be an atom");
                }
                text = ((Atom) src).getName();
            } else {
                if (src instanceof Variable) {
                    throw new it.denzosoft.jprolog.core.exceptions.PrologEvaluationException(
                        "sub_string/5: first argument must be instantiated to a string.");
                }
                if (!(src instanceof PrologString)) return Outcome.FAILURE;
                text = ((PrologString) src).getStringValue();
            }
            final int n = text.length();
            final Integer before = intArg(m, args[1], ind);
            final Integer length = intArg(m, args[2], ind);
            final Integer after = intArg(m, args[3], ind);
            final String sub = subArg(m, args[4], ind);

            Generator gen = new SubAtomGen(atoms, text, n, before, length, after, sub, args);
            return m.pushGenerator(gen) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }

        private Integer intArg(Machine m, Term t, String ind) {
            Term x = m.deref(t);
            if (x instanceof Variable) return null;
            if (x instanceof Number) {
                Number num = (Number) x;
                if (num.isInteger() && num.longValue() >= 0) return Integer.valueOf((int) num.longValue());
                if (atoms) {
                    throw new it.denzosoft.jprolog.core.exceptions.PrologEvaluationException(
                        "sub_atom/5: numeric arguments must be non-negative integers");
                }
                return null;                                   // sub_string/5 is permissive here
            }
            if (atoms) {
                throw new it.denzosoft.jprolog.core.exceptions.PrologEvaluationException(
                    "sub_atom/5: before, length, and after arguments must be integers or variables");
            }
            return null;
        }

        private String subArg(Machine m, Term t, String ind) {
            Term x = m.deref(t);
            if (x instanceof Variable) return null;
            if (atoms) {
                if (x instanceof Atom) return ((Atom) x).getName();
                throw new it.denzosoft.jprolog.core.exceptions.PrologEvaluationException(
                    "sub_atom/5: sub-atom argument must be an atom or variable");
            }
            return (x instanceof PrologString) ? ((PrologString) x).getStringValue() : null;
        }
    }

    /** The (Before, Length) candidate walk of {@link SubAtomB}: one candidate per redo. */
    private static final class SubAtomGen implements Generator {
        private final boolean atoms;
        private final String text;
        private final int n;
        private final Integer before, length, after;
        private final String sub;
        private final Term[] args;
        private int b, len, idx;
        private boolean done;

        SubAtomGen(boolean atoms, String text, int n, Integer before, Integer length, Integer after,
                   String sub, Term[] args) {
            this.atoms = atoms; this.text = text; this.n = n;
            this.before = before; this.length = length; this.after = after; this.sub = sub;
            this.args = args;
            this.b = (before != null) ? before.intValue() : 0;
            this.len = (length != null) ? length.intValue() : 0;
            this.idx = 0;
        }

        @Override
        public boolean next(Machine m) {
            while (!done) {
                int cb, cl;
                if (before != null && length != null) {
                    cb = before.intValue(); cl = length.intValue(); done = true;
                } else if (before != null) {
                    cb = before.intValue(); cl = len++;
                    if (cb < 0 || cb > n || cl > n - cb) { done = true; continue; }
                    if (cl >= n - cb) done = true;
                } else if (length != null) {
                    cl = length.intValue(); cb = b++;
                    if (cl < 0 || cl > n || cb > n - cl) { done = true; continue; }
                    if (cb >= n - cl) done = true;
                } else if (sub != null) {
                    if (sub.isEmpty()) {
                        cb = b++; cl = 0;
                        if (cb > n) { done = true; continue; }
                        if (cb >= n) done = true;
                    } else {
                        int at = text.indexOf(sub, idx);
                        if (at < 0) { done = true; continue; }
                        idx = at + 1;
                        cb = at; cl = sub.length();
                        if (idx > n - cl) done = true;
                    }
                } else {
                    if (b > n) { done = true; continue; }
                    cb = b; cl = len++;
                    if (cl > n - cb) { b++; len = 0; continue; }
                    if (cb == n && cl == 0) done = true;
                }
                if (cb < 0 || cl < 0 || cb + cl > n) continue;
                int ca = n - cb - cl;
                if (after != null && after.intValue() != ca) continue;
                String candidate = text.substring(cb, cb + cl);
                if (sub != null && !sub.equals(candidate)) continue;
                Bindings bb = m.bindings();
                int mark = bb.mark();
                bb.forceTrail++;
                boolean ok;
                try {
                    ok = Unify.unify(args[1], Number.valueOf(cb), bb)
                      && Unify.unify(args[2], Number.valueOf(cl), bb)
                      && Unify.unify(args[3], Number.valueOf(ca), bb)
                      && Unify.unify(args[4], atoms ? (Term) new Atom(candidate)
                                                    : (Term) new PrologString(candidate), bb);
                    if (!ok) bb.undo(mark);
                } finally {
                    bb.forceTrail--;
                }
                if (ok) {
                    if (done) m.lastSolution();
                    return true;
                }
                m.guard().step();
            }
            return false;
        }
    }

    /** Keeps javac from warning about the (deliberately kept) TRUE / PrologString imports. */
    static Term trueAtom() { return TRUE; }
    static boolean isString(Term t) { return t instanceof PrologString; }
}
// END_CHANGE: ISS-2025-0453
