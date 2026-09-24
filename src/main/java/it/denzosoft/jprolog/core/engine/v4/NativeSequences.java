package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.ResourceGuard;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.TreeSet;

// START_CHANGE: ISS-2025-0710 - wave Q2.1 (4.6 completeness program): library(solution_sequences).
/**
 * SWI-Prolog's {@code library(solution_sequences)}: {@code limit/2}, {@code offset/2},
 * {@code call_nth/2}, {@code distinct/1,2} and {@code order_by/2}.
 *
 * <p>The first four are LAZY: the goal runs inside the caller's continuation through
 * {@link Machine#pushFiltered}, and a Java-side counter (or the set of witnesses seen so far) —
 * which, like SWI's {@code nb_setarg/3} state, is not undone on backtracking — decides about each
 * solution as it arrives. When {@code limit/2} (or {@code call_nth/2} with a bound N) has its last
 * solution it cuts the goal's choice points exactly as {@code !} does, so a
 * {@code setup_call_cleanup/3} inside the goal runs its cleanup at that moment and an infinite
 * generator is abandoned. {@code order_by/2} is necessarily eager (it collects every solution with
 * {@code findall}, stable-sorts them — asc/desc per key, the first spec the primary key — and hands
 * them out one by one), as SWI's is.
 *
 * <p>Arguments are checked as SWI checks them: an unbound count is an instantiation error, a count
 * that is not an integer a {@code type_error(integer, C)}, a negative {@code offset/2} count a
 * {@code domain_error(not_less_than_zero, C)}; {@code limit(0, G)} and {@code limit(-1, G)} fail;
 * {@code limit(infinite, G)} is {@code call(G)}.
 */
final class NativeSequences {

    private NativeSequences() {}

    static void register(BuiltinTable t) {
        t.register("limit", 2, new LimitB());
        t.register("offset", 2, new OffsetB());
        t.register("call_nth", 2, new CallNthB());
        t.register("distinct", 1, new DistinctB(1));
        t.register("distinct", 2, new DistinctB(2));
        t.register("order_by", 2, new OrderByB());
    }

    /** The goal argument: bound and callable (a control construct's spine is checked too). */
    static Term goalArg(Machine m, Term g, String name, int arity) {
        Term d = m.deref(g);
        String ind = name + "/" + arity;
        if (d instanceof Variable) throw Errors.instantiation(ind);
        if (!(d instanceof Atom) && !(d instanceof CompoundTerm)) throw Errors.type("callable", m.resolve(d), ind);
        m.checkBody(d, name, arity);
        return d;
    }

    /** An integer count argument (big integers are fine: they are simply never reached). */
    private static Number countArg(Machine m, Term c, String ind) {
        Term d = m.deref(c);
        if (d instanceof Variable) throw Errors.instantiation(ind);
        if (!(d instanceof Number) || !((Number) d).isInteger()) throw Errors.type("integer", m.resolve(d), ind);
        return (Number) d;
    }

    private static long clampLong(Number n) {
        return n.fitsInLong() ? n.longValue() : (n.bigIntegerValue().signum() > 0 ? Long.MAX_VALUE : Long.MIN_VALUE);
    }

    // ------------------------------------------------------------------ limit/2

    private static final class LimitB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term c = m.deref(args[0]);
            Term goal = goalArg(m, args[1], "limit", 2);
            if (c instanceof Atom && "infinite".equals(((Atom) c).getName())) {
                m.pushGoal(goal);
                return Outcome.SUSPENDED;
            }
            final long max = clampLong(countArg(m, c, "limit/2"));
            if (max <= 0) return Outcome.FAILURE;
            final long[] seen = {0};
            m.pushFiltered(goal, new Machine.SolutionFilter() {
                @Override public int onSolution(Machine mm) {
                    return (++seen[0] >= max) ? ACCEPT_LAST : ACCEPT;
                }
            });
            return Outcome.SUSPENDED;
        }
    }

    // ------------------------------------------------------------------ offset/2

    private static final class OffsetB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Number n = countArg(m, args[0], "offset/2");
            Term goal = goalArg(m, args[1], "offset", 2);
            if (n.bigIntegerValue().signum() < 0) throw Errors.domain("not_less_than_zero", n, "offset/2");
            final long skip = clampLong(n);
            if (skip == 0) {
                m.pushGoal(goal);
                return Outcome.SUSPENDED;
            }
            final long[] seen = {0};
            m.pushFiltered(goal, new Machine.SolutionFilter() {
                @Override public int onSolution(Machine mm) {
                    return (seen[0]++ < skip) ? REJECT : ACCEPT;
                }
            });
            return Outcome.SUSPENDED;
        }
    }

    // ------------------------------------------------------------------ call_nth/2

    private static final class CallNthB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term goal = goalArg(m, args[0], "call_nth", 2);
            final Term nthArg = args[1];
            Term nd = m.deref(nthArg);
            final long[] seen = {0};
            if (nd instanceof Variable) {
                m.pushFiltered(goal, new Machine.SolutionFilter() {
                    @Override public int onSolution(Machine mm) {
                        return mm.unify(nthArg, Number.valueOf(++seen[0])) ? ACCEPT : REJECT;
                    }
                });
                return Outcome.SUSPENDED;
            }
            if (!(nd instanceof Number) || !((Number) nd).isInteger()) throw Errors.type("integer", m.resolve(nd), "call_nth/2");
            Number n = (Number) nd;
            if (n.bigIntegerValue().signum() < 0) throw Errors.type("nonneg", n, "call_nth/2");   // SWI: must_be(nonneg)
            if (n.bigIntegerValue().signum() == 0) return Outcome.FAILURE;
            final long want = clampLong(n);
            m.pushFiltered(goal, new Machine.SolutionFilter() {
                @Override public int onSolution(Machine mm) {
                    return (++seen[0] == want) ? ACCEPT_LAST : REJECT;
                }
            });
            return Outcome.SUSPENDED;
        }
    }

    // ------------------------------------------------------------------ distinct/1,2

    private static final class DistinctB implements Builtin {
        private final int arity;
        DistinctB(int arity) { this.arity = arity; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            Term goal = goalArg(m, args[arity - 1], "distinct", arity);
            final Term witness = (arity == 1) ? goal : args[0];
            final ResourceGuard g = m.guard();
            // The seen set holds VARIANT keys (NativeControl.variantKey: ground, variables numbered
            // by first occurrence), ordered by the standard order — two solutions are duplicates
            // exactly when their witnesses are variants, as SWI's trie-based distinct/2.
            final TreeSet<Term> seen = new TreeSet<Term>(new Comparator<Term>() {
                @Override public int compare(Term a, Term b) { return Unify.compareTerms(a, b, g); }
            });
            m.pushFiltered(goal, new Machine.SolutionFilter() {
                @Override public int onSolution(Machine mm) {
                    // resolved first: a ground witness IS its own key, and it must not be the
                    // live cell whose value the next solution changes
                    return seen.add(NativeControl.variantKey(mm.resolve(witness), mm)) ? ACCEPT : REJECT;
                }
            });
            return Outcome.SUSPENDED;
        }
    }

    // ------------------------------------------------------------------ order_by/2

    private static final Atom KEYS = new Atom("$order_keys");
    private static final Atom PAIR = new Atom("-");

    private static final class OrderByB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term spec = m.deref(args[0]);
            if (spec instanceof Variable) throw Errors.instantiation("order_by/2");
            List<Term> specs = NativeLibrary.elements(spec, m.guard());
            if (specs == null) throw Errors.type("list", m.resolve(spec), "order_by/2");
            if (specs.isEmpty()) throw Errors.domain("non_empty_list", spec, "order_by/2");
            final int k = specs.size();
            final boolean[] desc = new boolean[k];
            Term[] witnesses = new Term[k];
            for (int i = 0; i < k; i++) {
                Term s = m.deref(specs.get(i));
                if (s instanceof Variable) throw Errors.instantiation("order_by/2");
                if (!(s instanceof CompoundTerm) || ((CompoundTerm) s).arity() != 1
                        || !("asc".equals(((CompoundTerm) s).getName()) || "desc".equals(((CompoundTerm) s).getName()))) {
                    throw Errors.domain("order_specifier", m.resolve(s), "order_by/2");
                }
                desc[i] = "desc".equals(((CompoundTerm) s).getName());
                witnesses[i] = ((CompoundTerm) s).arg(0);
            }
            Term goal = goalArg(m, args[1], "order_by", 2);
            final Term template = new CompoundTerm(PAIR, Arrays.asList((Term) new CompoundTerm(KEYS, witnesses), goal));
            final List<Term> results = m.findAll(template, goal);
            if (results.isEmpty()) return Outcome.FAILURE;
            final ResourceGuard g = m.guard();
            final Term[] sorted = results.toArray(new Term[0]);
            // stable (TimSort): solutions with equal keys keep their order, as SWI's sort/4 does
            Arrays.sort(sorted, new Comparator<Term>() {
                @Override public int compare(Term x, Term y) {
                    CompoundTerm kx = (CompoundTerm) ((CompoundTerm) x).arg(0);
                    CompoundTerm ky = (CompoundTerm) ((CompoundTerm) y).arg(0);
                    for (int i = 0; i < k; i++) {
                        int c = Unify.compareTerms(kx.arg(i), ky.arg(i), g);
                        if (c != 0) return desc[i] ? -c : c;
                    }
                    return 0;
                }
            });
            final int[] cursor = {0};
            return m.pushGenerator(new Generator() {
                @Override public boolean next(Machine mm) {
                    while (cursor[0] < sorted.length) {
                        Term r = sorted[cursor[0]++];
                        boolean last = cursor[0] >= sorted.length;
                        if (mm.unifyOrUndo(template, r)) {
                            if (last) mm.lastSolution();
                            return true;
                        }
                    }
                    return false;
                }
            }) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }
    }
}
// END_CHANGE: ISS-2025-0710
