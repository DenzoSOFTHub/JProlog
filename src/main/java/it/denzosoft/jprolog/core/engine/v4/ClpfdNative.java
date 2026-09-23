package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.builtin.clpfd.v2.ClpfdV2Bridge;
import it.denzosoft.jprolog.builtin.clpfd.v2.Constraint;
import it.denzosoft.jprolog.builtin.clpfd.v2.IntervalDomain;
import it.denzosoft.jprolog.builtin.clpfd.v2.Labeler;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0471 - engine v4 wave W6: label/1 and labeling/2 as v4 natives.
/**
 * The CLP(FD) predicates on the cell model.
 *
 * <h3>Posting</h3>
 * A posting native ({@code in/2}, {@code #=/2}, {@code #<==>/2}, {@code element/3}, ...) hands
 * the dereferenced goal to {@link ClpfdV2Bridge}, which compiles it into the per-query
 * {@code ClpStore} and records the store's rollback on the machine's trail; the native then binds
 * every FD cell whose domain propagation has reduced to one value ({@link #bindDetermined}). The
 * attribute hook ({@code '$clpfd_unify_hook'/2}, woken when an FD cell is bound by plain
 * unification) does the same since ISS-2025-0640, so {@code X+Y #= 9, X = 4} binds {@code Y}.
 *
 * <h3>Labeling (ISS-2025-0642)</h3>
 * Labeling is LAZY and runs on the machine's own choice points, the way SWI's library(clpfd) does
 * it in Prolog: one call selects a variable (leftmost / ff / ffc / min / max), pushes the
 * continuation {@code '$clpfd_label'(Vars, Code)}, and installs a {@link Generator} over the
 * branching alternatives of that variable (step: {@code X = V ; X #\= V}; enum: every value;
 * bisect: {@code X #=< M ; X #> M}). Each alternative binds or narrows through the ordinary trail,
 * so the store's own rollback runs when the machine backtracks, and the first solution costs only
 * the first branch of the search tree. The previous implementation enumerated the whole tree
 * before handing out the first answer.
 *
 * <p>{@code min(Expr)}/{@code max(Expr)} (ISS-2025-0643) are branch and bound: the optimum is found
 * inside the store, then the native pushes SWI's own continuation
 * {@code (Expr #= Opt, labeling(Rest, Vars) ; Expr #\= Opt, labeling(Opts, Vars))}, so the
 * solutions come in objective order.
 *
 * <p>The legacy registry built-ins ({@code builtin.clpfd.v2.ClpfdV2Builtins}) stay registered;
 * they are what a direct registry call would run.
 */
final class ClpfdNative {

    private ClpfdNative() {}

    // START_CHANGE: ISS-2025-0500 - 4.2 wave C: the CLP(FD) bridge records its backtrackable store
    // mutations through a sink it declares itself; the engine points that sink at the running
    // machine's trail. That is the last external user of core.engine.v4.Undo, which is
    // package-private again.
    static {
        it.denzosoft.jprolog.builtin.clpfd.v2.ClpfdV2Bridge.setUndoSink(
            new it.denzosoft.jprolog.builtin.clpfd.v2.ClpfdV2Bridge.UndoSink() {
                @Override public void record(Runnable undo) { Undo.record(undo); }
            });
        // START_CHANGE: ISS-2025-0645 - a long propagation polls the running machine's guard, so
        // the inference budget and a Stop request (thread interrupt) reach it.
        it.denzosoft.jprolog.builtin.clpfd.v2.ClpStore.setPollHook(new Runnable() {
            @Override public void run() {
                Machine m = Undo.current();
                if (m != null) m.guard().stepChecked();
            }
        });
        // END_CHANGE: ISS-2025-0645
    }
    // END_CHANGE: ISS-2025-0500

    static void register(BuiltinTable t) {
        t.register("label", 1, new Label(false));
        t.register("labeling", 2, new Label(true));
        // START_CHANGE: ISS-2025-0642 - the lazy labeling continuation, and indomain/1 = label([X])
        t.register("$clpfd_label", 2, new LabelStep());
        t.register("indomain", 1, new Indomain());
        // END_CHANGE: ISS-2025-0642
        // START_CHANGE: ISS-2025-0486 - wave W9: the posting predicates are natives too, so that
        // propagation binds CELLS instead of naming variables in a Map<String,Term> that
        // LegacyBuiltinAdapter had to translate back through ClpfdV2Bridge.cellFor/1 (LIM-037).
        t.register("in", 2, new In());
        t.register("#=", 2, new Cmp(Constraint.Rel.EQ));
        t.register("#\\=", 2, new Cmp(Constraint.Rel.NE));
        t.register("#<", 2, new Cmp(Constraint.Rel.LT));
        t.register("#>", 2, new Cmp(Constraint.Rel.GT));
        t.register("#=<", 2, new Cmp(Constraint.Rel.LE));
        t.register("#>=", 2, new Cmp(Constraint.Rel.GE));
        t.register("all_different", 1, new AllDifferent(false));
        t.register("all_distinct", 1, new AllDifferent(true));        // ISS-2025-0651: Regin GAC
        // END_CHANGE: ISS-2025-0486
        // START_CHANGE: ISS-2025-0646 - ins/2; sum/3 and scalar_product/4 live in prelude/clpfd.pl
        // (so a user program's own sum/3 still wins) on top of these internal natives.
        t.register("ins", 2, new Ins());
        t.register("$clpfd_sum", 3, new SumB());
        t.register("$clpfd_scalar_product", 4, new ScalarProduct());
        // END_CHANGE: ISS-2025-0646
        // START_CHANGE: ISS-2025-0647 - reification and the boolean connectives
        t.register("#<==>", 2, new BoolFormula("#<==>"));
        t.register("#==>", 2, new BoolFormula("#==>"));
        t.register("#<==", 2, new BoolFormula("#<=="));
        t.register("#\\/", 2, new BoolFormula("#\\/"));
        t.register("#/\\", 2, new BoolFormula("#/\\"));
        t.register("#\\", 2, new BoolFormula("#\\"));
        t.register("#\\", 1, new BoolFormula("#\\"));
        // END_CHANGE: ISS-2025-0647
        // START_CHANGE: ISS-2025-0649 - domain reflection
        t.register("fd_dom", 2, new FdDom());
        t.register("fd_size", 2, new FdSize());
        t.register("fd_inf", 2, new FdBound(true));
        t.register("fd_sup", 2, new FdBound(false));
        t.register("fd_var", 1, new FdVarB());
        // END_CHANGE: ISS-2025-0649
        // START_CHANGE: ISS-2025-0650 - global constraints (public names in prelude/clpfd.pl)
        t.register("$clpfd_element", 3, new ElementB());
        t.register("$clpfd_tuples_in", 2, new TuplesIn());
        t.register("$clpfd_gcc", 2, new Gcc());
        // END_CHANGE: ISS-2025-0650
    }

    // START_CHANGE: ISS-2025-0486 - the cell-model posting predicates.
    /** Empty: on v4 a native receives dereferenced terms, so the bridge needs no substitution. */
    private static final Map<String, Term> NO_BINDINGS = java.util.Collections.emptyMap();

    /**
     * Bind every FD cell whose domain propagation has determined ({@code X #= 2} binds
     * {@code X = 2}, and {@code C in 1..3, D #= C*2+1, C #= 1} binds {@code D = 3} even though the
     * posting goal never mentions {@code D} — ISS-2025-0357).
     */
    static Builtin.Outcome bindDetermined(Machine m) {
        for (Map.Entry<Variable, Number> e : ClpfdV2Bridge.determinedCells().entrySet()) {
            Variable cell = e.getKey();
            if (cell.ref != null) continue;
            if (!m.unify(cell, e.getValue())) return Builtin.Outcome.FAILURE;   // ISS-2025-0644: may be big
        }
        return Builtin.Outcome.SUCCESS;
    }

    // START_CHANGE: ISS-2025-0640 - the attribute hook binds what the binding determined.
    /**
     * {@code '$clpfd_unify_hook'} body: the FD cell {@code cell} was bound to {@code other} by
     * plain unification. Narrow and propagate (the bridge), then bind every cell propagation has
     * fixed — {@code X in 0..9, Y in 0..9, X+Y #= 9, X = 4} must answer {@code Y = 5}, as SWI does;
     * the store used to know it ({@code Y in 5..5}) without binding {@code Y}.
     */
    static Builtin.Outcome onBound(Machine m, Variable cell, Term other) {
        if (!ClpfdV2Bridge.onBindCell(cell, other)) return Builtin.Outcome.FAILURE;
        return bindDetermined(m);
    }
    // END_CHANGE: ISS-2025-0640

    /** An exact integer term. */
    private static Number num(BigInteger v) {
        return v.bitLength() <= 63 ? Number.valueOf(v.longValue()) : new Number(v);
    }

    /** {@code Var in Dom}. */
    private static final class In implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            // START_CHANGE: ISS-2025-0641 - domains are N, Lo..Hi (inf/sup bounds) and D1 \/ D2;
            // ISS-2025-0510: a wrong shape is type_error(clpfd_domain, D), an unbound one
            // instantiation_error.
            IntervalDomain dom = ClpfdV2Bridge.parseDomain(m.deref(args[1]), "in/2");
            if (!ClpfdV2Bridge.postDomain(m.deref(args[0]), dom)) return Outcome.FAILURE;
            // END_CHANGE: ISS-2025-0641
            return bindDetermined(m);
        }
    }

    // START_CHANGE: ISS-2025-0646 - Vars ins Dom
    private static final class Ins implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            List<Term> xs = properList(m, args[0], "ins/2");
            IntervalDomain dom = ClpfdV2Bridge.parseDomain(m.deref(args[1]), "ins/2");
            for (Term x : xs) {
                if (!ClpfdV2Bridge.postDomain(m.deref(x), dom)) return Outcome.FAILURE;
            }
            return bindDetermined(m);
        }
    }
    // END_CHANGE: ISS-2025-0646

    /** {@code Left #rel Right}. */
    private static final class Cmp implements Builtin {
        private final Constraint.Rel rel;
        Cmp(Constraint.Rel rel) { this.rel = rel; }
        @Override public Outcome call(Machine m, Term[] args) {
            return post(m, m.resolve(args[0]), rel, m.resolve(args[1]));
        }
    }

    /** Post {@code l rel r}; {@code X #= Ground} evaluates exactly and unifies (SWI). */
    private static Builtin.Outcome post(Machine m, Term l, Constraint.Rel rel, Term r) {
        // START_CHANGE: ISS-2025-0644 - X #= 10^12*10^12 binds X to the exact big integer
        if (rel == Constraint.Rel.EQ) {
            Term var = null, expr = null;
            if (l instanceof Variable && ClpfdV2Bridge.isGround(r)) { var = l; expr = r; }
            else if (r instanceof Variable && ClpfdV2Bridge.isGround(l)) { var = r; expr = l; }
            if (var != null) {
                BigInteger v = ClpfdV2Bridge.evalGround(expr);
                if (v == null || !m.unify(var, num(v))) return Builtin.Outcome.FAILURE;
                return bindDetermined(m);
            }
        }
        // END_CHANGE: ISS-2025-0644
        if (!ClpfdV2Bridge.postCmp(l, rel, r, NO_BINDINGS)) return Builtin.Outcome.FAILURE;
        return bindDetermined(m);
    }

    /** {@code all_different(List)} / {@code all_distinct(List)}. */
    private static final class AllDifferent implements Builtin {
        private final boolean strong;
        AllDifferent(boolean strong) { this.strong = strong; }
        @Override public Outcome call(Machine m, Term[] args) {
            String ind = strong ? "all_distinct/1" : "all_different/1";
            List<Term> elems = properList(m, args[0], ind);
            for (int i = 0; i < elems.size(); i++) elems.set(i, m.resolve(elems.get(i)));
            boolean ok = strong ? ClpfdV2Bridge.postAllDistinct(elems)
                                : ClpfdV2Bridge.postAllDifferent(elems, NO_BINDINGS);
            if (!ok) return Outcome.FAILURE;
            return bindDetermined(m);
        }
    }
    // END_CHANGE: ISS-2025-0486

    // START_CHANGE: ISS-2025-0646 - sum/3 and scalar_product/4
    /** A balanced {@code +} tree (no deep left spine for long lists); 0 for no terms. */
    private static Term plusTree(List<Term> ts, int from, int to) {
        if (from >= to) return Number.valueOf(0);
        if (to - from == 1) return ts.get(from);
        int mid = (from + to) >>> 1;
        return new CompoundTerm(new Atom("+"), Arrays.asList(plusTree(ts, from, mid), plusTree(ts, mid, to)));
    }

    private static Constraint.Rel relation(Machine m, Term op, String ind) {
        Term o = m.deref(op);
        if (o instanceof Variable) throw Errors.instantiation(ind);
        Constraint.Rel rel = (o instanceof Atom) ? ClpfdV2Bridge.relOf(((Atom) o).getName()) : null;
        if (rel == null) throw Errors.domain("scalar_product_relation", m.resolve(o), ind);
        return rel;
    }

    /** {@code '$clpfd_sum'(Vars, Op, Expr)}. */
    private static final class SumB implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            List<Term> vs = properList(m, args[0], "sum/3");
            Constraint.Rel rel = relation(m, args[1], "sum/3");
            for (int i = 0; i < vs.size(); i++) vs.set(i, m.resolve(vs.get(i)));
            return post(m, plusTree(vs, 0, vs.size()), rel, m.resolve(args[2]));
        }
    }

    /** {@code '$clpfd_scalar_product'(Coeffs, Vars, Op, Expr)}. */
    private static final class ScalarProduct implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            List<Term> cs = properList(m, args[0], "scalar_product/4");
            List<Term> vs = properList(m, args[1], "scalar_product/4");
            Constraint.Rel rel = relation(m, args[2], "scalar_product/4");
            if (cs.size() != vs.size()) return Outcome.FAILURE;
            List<Term> products = new ArrayList<Term>(cs.size());
            for (int i = 0; i < cs.size(); i++) {
                Term c = m.deref(cs.get(i));
                if (c instanceof Variable) throw Errors.instantiation("scalar_product/4");
                if (!(c instanceof Number) || !((Number) c).isInteger()) {
                    throw Errors.type("integer", m.resolve(c), "scalar_product/4");
                }
                products.add(new CompoundTerm(new Atom("*"), Arrays.asList(c, m.resolve(vs.get(i)))));
            }
            return post(m, plusTree(products, 0, products.size()), rel, m.resolve(args[3]));
        }
    }
    // END_CHANGE: ISS-2025-0646

    // START_CHANGE: ISS-2025-0647 - #<==>, #==>, #<==, #\/, #/\, #\ (xor and negation)
    private static final class BoolFormula implements Builtin {
        private final Atom op;
        BoolFormula(String op) { this.op = new Atom(op); }
        @Override public Outcome call(Machine m, Term[] args) {
            List<Term> as = new ArrayList<Term>(args.length);
            for (Term a : args) as.add(m.resolve(a));
            if (!ClpfdV2Bridge.postBoolean(new CompoundTerm(op, as))) return Outcome.FAILURE;
            return bindDetermined(m);
        }
    }
    // END_CHANGE: ISS-2025-0647

    // START_CHANGE: ISS-2025-0649 - fd_dom/2, fd_size/2, fd_inf/2, fd_sup/2, fd_var/1
    /** The domain of a term for the reflection predicates (integers are singletons). */
    private static IntervalDomain domainOf(Machine m, Term x, String ind) {
        Term t = m.deref(x);
        if (t instanceof Variable) {
            IntervalDomain d = ClpfdV2Bridge.cellDomain((Variable) t);
            return d == null ? IntervalDomain.ALL : d;
        }
        if (t instanceof Number && ((Number) t).isInteger()) {
            BigInteger v = ((Number) t).bigIntegerValue();
            if (v.bitLength() > 62) return null;                        // a big integer: see callers
            return IntervalDomain.singleton(v.longValue());
        }
        throw Errors.type("integer", m.resolve(t), ind);
    }

    private static final class FdDom implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Term x = m.deref(args[0]);
            IntervalDomain d = domainOf(m, x, "fd_dom/2");
            Term dom = (d == null)
                ? new CompoundTerm(new Atom(".."), Arrays.asList(x, x))
                : (d.isSingleton() ? new CompoundTerm(new Atom(".."),
                        Arrays.asList((Term) Number.valueOf(d.value()), Number.valueOf(d.value())))
                                   : ClpfdV2Bridge.domainToTerm(d));
            return m.unify(args[1], dom) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class FdSize implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            IntervalDomain d = domainOf(m, args[0], "fd_size/2");
            Term size = (d == null) ? Number.valueOf(1)
                : (d.isFinite() ? Number.valueOf(d.size()) : (Term) new Atom("sup"));
            return m.unify(args[1], size) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class FdBound implements Builtin {
        private final boolean lower;
        FdBound(boolean lower) { this.lower = lower; }
        @Override public Outcome call(Machine m, Term[] args) {
            Term x = m.deref(args[0]);
            IntervalDomain d = domainOf(m, x, lower ? "fd_inf/2" : "fd_sup/2");
            Term b;
            if (d == null) {
                b = x;
            } else {
                long v = lower ? d.min() : d.max();
                b = (v == IntervalDomain.INF) ? new Atom("inf")
                  : (v == IntervalDomain.SUP) ? new Atom("sup") : (Term) Number.valueOf(v);
            }
            return m.unify(args[1], b) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class FdVarB implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Term x = m.deref(args[0]);
            return (x instanceof Variable && ClpfdV2Bridge.cellDomain((Variable) x) != null)
                ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }
    // END_CHANGE: ISS-2025-0649

    // START_CHANGE: ISS-2025-0650 - element/3, tuples_in/2, global_cardinality/2
    private static final class ElementB implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            List<Term> xs = properList(m, args[1], "element/3");
            for (int i = 0; i < xs.size(); i++) xs.set(i, m.resolve(xs.get(i)));
            if (xs.isEmpty()) return Outcome.FAILURE;
            if (!ClpfdV2Bridge.postElement(m.resolve(args[0]), xs, m.resolve(args[2]))) return Outcome.FAILURE;
            return bindDetermined(m);
        }
    }

    private static final class TuplesIn implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            List<Term> tuplesT = properList(m, args[0], "tuples_in/2");
            List<Term> rowsT = properList(m, args[1], "tuples_in/2");
            long[][] rows = new long[rowsT.size()][];
            for (int i = 0; i < rows.length; i++) {
                List<Term> row = properList(m, rowsT.get(i), "tuples_in/2");
                rows[i] = new long[row.size()];
                for (int j = 0; j < row.size(); j++) rows[i][j] = fdInt(m, row.get(j), "tuples_in/2");
            }
            List<List<Term>> tuples = new ArrayList<List<Term>>(tuplesT.size());
            for (Term tt : tuplesT) {
                List<Term> tuple = properList(m, tt, "tuples_in/2");
                for (int j = 0; j < tuple.size(); j++) tuple.set(j, m.resolve(tuple.get(j)));
                for (long[] row : rows) if (row.length != tuple.size()) return Outcome.FAILURE;
                tuples.add(tuple);
            }
            if (!ClpfdV2Bridge.postTuples(tuples, rows)) return Outcome.FAILURE;
            return bindDetermined(m);
        }
    }

    private static final class Gcc implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            List<Term> vs = properList(m, args[0], "global_cardinality/2");
            List<Term> pairs = properList(m, args[1], "global_cardinality/2");
            long[] keys = new long[pairs.size()];
            List<Term> counts = new ArrayList<Term>(pairs.size());
            for (int i = 0; i < pairs.size(); i++) {
                Term p = m.deref(pairs.get(i));
                if (p instanceof Variable) throw Errors.instantiation("global_cardinality/2");
                if (!(p instanceof CompoundTerm) || !"-".equals(((CompoundTerm) p).getName())
                        || ((CompoundTerm) p).getArguments().size() != 2) {
                    throw Errors.type("pair", m.resolve(p), "global_cardinality/2");
                }
                keys[i] = fdInt(m, ((CompoundTerm) p).getArguments().get(0), "global_cardinality/2");
                for (int j = 0; j < i; j++) {
                    if (keys[j] == keys[i]) throw Errors.domain("gcc_unique_key_pairs", m.resolve(args[1]), "global_cardinality/2");
                }
                counts.add(m.resolve(((CompoundTerm) p).getArguments().get(1)));
            }
            for (int i = 0; i < vs.size(); i++) vs.set(i, m.resolve(vs.get(i)));
            if (!ClpfdV2Bridge.postGcc(vs, keys, counts)) return Outcome.FAILURE;
            return bindDetermined(m);
        }
    }

    private static long fdInt(Machine m, Term t, String ind) {
        Term x = m.deref(t);
        if (x instanceof Variable) throw Errors.instantiation(ind);
        if (!(x instanceof Number) || !((Number) x).isInteger()) throw Errors.type("integer", m.resolve(x), ind);
        if (((Number) x).bigIntegerValue().bitLength() > 62) throw Errors.representation("max_integer", ind);
        return ((Number) x).longValue();
    }
    // END_CHANGE: ISS-2025-0650

    // ================================================================== labeling
    // START_CHANGE: ISS-2025-0642 - lazy labeling on the machine's choice points.

    /** Branching strategies (SWI labeling/2 option names). */
    private static final int STEP = 0, ENUM = 1, BISECT = 2;

    private static int code(Labeler.VarSel sel, Labeler.ValOrder ord, int branch) {
        return sel.ordinal() + 8 * ord.ordinal() + 16 * branch;
    }

    private static final Atom LABEL_STEP = new Atom("$clpfd_label");

    private static final class Label implements Builtin {
        private final boolean withOptions;
        Label(boolean withOptions) { this.withOptions = withOptions; }

        @Override public Outcome call(Machine m, Term[] args) {
            String ind = withOptions ? "labeling/2" : "label/1";
            // SWI defaults: leftmost, up, step
            Labeler.VarSel varSel = Labeler.VarSel.LEFTMOST;
            Labeler.ValOrder valOrder = Labeler.ValOrder.UP;
            int branch = STEP;
            List<Term> objectives = new ArrayList<Term>();
            List<Term> plain = new ArrayList<Term>();
            if (withOptions) {
                List<Term> opts = list(m, m.deref(args[0]));
                if (opts == null) {
                    Term o = m.deref(args[0]);
                    throw (o instanceof Variable) ? Errors.instantiation(ind)
                                                  : Errors.type("list", m.resolve(o), ind);
                }
                // SWI: one option per category; a repeated one is
                // domain_error(nonrepeating_labeling_options, Opts), a second different one
                // domain_error(consistent_labeling_options, Opts)
                String[] seen = new String[3];                     // selection, order, branching
                for (int i = 0; i < opts.size(); i++) {
                    Term opt = m.deref(opts.get(i));
                    if (opt instanceof Atom) {
                        String n = ((Atom) opt).getName();
                        int cat;
                        if ("leftmost".equals(n)) { varSel = Labeler.VarSel.LEFTMOST; cat = 0; }
                        else if ("ff".equals(n)) { varSel = Labeler.VarSel.FF; cat = 0; }
                        else if ("ffc".equals(n)) { varSel = Labeler.VarSel.FFC; cat = 0; }
                        else if ("min".equals(n)) { varSel = Labeler.VarSel.MIN; cat = 0; }
                        else if ("max".equals(n)) { varSel = Labeler.VarSel.MAX; cat = 0; }
                        else if ("up".equals(n)) { valOrder = Labeler.ValOrder.UP; cat = 1; }
                        else if ("down".equals(n)) { valOrder = Labeler.ValOrder.DOWN; cat = 1; }
                        else if ("step".equals(n)) { branch = STEP; cat = 2; }
                        else if ("enum".equals(n)) { branch = ENUM; cat = 2; }
                        else if ("bisect".equals(n)) { branch = BISECT; cat = 2; }
                        else throw Errors.domain("labeling_option", m.resolve(opt), ind);
                        if (seen[cat] != null) {
                            throw Errors.domain(seen[cat].equals(n) ? "nonrepeating_labeling_options"
                                                                    : "consistent_labeling_options",
                                                m.resolve(args[0]), ind);
                        }
                        seen[cat] = n;
                        plain.add(opt);
                    } else if (opt instanceof CompoundTerm
                            && ((CompoundTerm) opt).getArguments().size() == 1
                            && ("min".equals(((CompoundTerm) opt).getName())
                                || "max".equals(((CompoundTerm) opt).getName()))) {
                        objectives.add(m.resolve(opt));
                    } else if (opt instanceof Variable) {
                        throw Errors.instantiation(ind);
                    } else {
                        throw Errors.domain("labeling_option", m.resolve(opt), ind);
                    }
                }
            }
            Term listArg = m.deref(args[withOptions ? 1 : 0]);
            List<Term> vars = list(m, listArg);
            // START_CHANGE: ISS-2025-0510 - 4.3 wave D: label(a) / labeling(_, a) is
            // instantiation_error or type_error(list, a); the variable list used to fail silently.
            if (vars == null) {
                throw (listArg instanceof Variable) ? Errors.instantiation(ind)
                                                    : Errors.type("list", m.resolve(listArg), ind);
            }
            // END_CHANGE: ISS-2025-0510
            // SWI: every element is an integer or an FD variable with a FINITE domain
            for (int i = 0; i < vars.size(); i++) {
                Term v = m.deref(vars.get(i));
                if (v instanceof Variable) {
                    IntervalDomain d = ClpfdV2Bridge.cellDomain((Variable) v);
                    if (d == null || !d.isFinite()) throw Errors.instantiation(ind);
                } else if (!(v instanceof Number) || !((Number) v).isInteger()) {
                    throw Errors.type("integer", m.resolve(v), ind);
                }
            }
            if (!objectives.isEmpty()) return optimise(m, args[0], listArg, vars, objectives, plain, varSel, valOrder, ind);
            return step(m, listArg, code(varSel, valOrder, branch));
        }
    }

    /** indomain(X) = label([X]). */
    private static final class Indomain implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Term v = m.deref(args[0]);
            if (v instanceof Number && ((Number) v).isInteger()) return Outcome.SUCCESS;
            if (!(v instanceof Variable)) throw Errors.type("integer", m.resolve(v), "indomain/1");
            IntervalDomain d = ClpfdV2Bridge.cellDomain((Variable) v);
            if (d == null || !d.isFinite()) throw Errors.instantiation("indomain/1");
            Term listT = new CompoundTerm(new Atom("."), Arrays.asList(v, (Term) new Atom("[]")));
            return step(m, listT, code(Labeler.VarSel.LEFTMOST, Labeler.ValOrder.UP, STEP));
        }
    }

    /** {@code '$clpfd_label'(Vars, Code)}: the continuation of a labeling step. */
    private static final class LabelStep implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Term c = m.deref(args[1]);
            return step(m, m.deref(args[0]), (int) ((Number) c).longValue());
        }
    }

    /**
     * One labeling step: bind what propagation has determined, select the next variable, push the
     * continuation and a generator over its branches. SUCCESS once every variable is bound.
     */
    private static Builtin.Outcome step(Machine m, Term listT, int code) {
        if (bindDetermined(m) == Builtin.Outcome.FAILURE) return Builtin.Outcome.FAILURE;
        Labeler.VarSel sel = Labeler.VarSel.values()[code % 8];
        final boolean up = ((code / 8) % 2) == 0;
        int branch = code / 16;
        Variable best = null;
        IntervalDomain bestDom = null;
        int bestDeg = -1;
        Term cur = m.deref(listT);
        int n = 0;
        while (cur instanceof CompoundTerm && ((CompoundTerm) cur).getArguments().size() == 2) {
            Term e = m.deref(((CompoundTerm) cur).getArguments().get(0));
            cur = m.deref(((CompoundTerm) cur).getArguments().get(1));
            if ((++n & 0x3FF) == 0) m.guard().step();
            if (!(e instanceof Variable)) continue;
            Variable v = (Variable) e;
            IntervalDomain d = ClpfdV2Bridge.cellDomain(v);
            if (d == null || !d.isFinite()) throw Errors.instantiation("labeling/2");
            if (best == null) {
                best = v; bestDom = d;
                if (sel == Labeler.VarSel.LEFTMOST) break;
                if (sel == Labeler.VarSel.FFC) bestDeg = ClpfdV2Bridge.cellDegree(v);
                continue;
            }
            boolean better;
            switch (sel) {
                case FF: better = d.size() < bestDom.size(); break;
                case FFC: {
                    long a = d.size(), b = bestDom.size();
                    int deg = (a <= b) ? ClpfdV2Bridge.cellDegree(v) : -1;
                    better = a < b || (a == b && deg > bestDeg);
                    if (better) bestDeg = deg;
                    break;
                }
                case MIN: better = d.min() < bestDom.min(); break;
                case MAX: better = d.max() > bestDom.max(); break;
                default: better = false; break;
            }
            if (better) { best = v; bestDom = d; }
        }
        if (best == null) return Builtin.Outcome.SUCCESS;                // everything is labeled
        m.pushGoal(new CompoundTerm(LABEL_STEP, Arrays.asList(listT, (Term) Number.valueOf(code))));
        Generator g;
        if (bestDom.isSingleton() || branch == ENUM) g = new EnumGen(best, bestDom, up);
        else if (branch == BISECT) g = new BisectGen(best, bestDom, up);
        else g = new StepGen(best, bestDom, up);
        return m.pushGenerator(g) ? Builtin.Outcome.SUSPENDED : Builtin.Outcome.FAILURE;
    }

    /** {@code X = V1 ; X = V2 ; ...} over a snapshot of the domain. */
    private static final class EnumGen implements Generator {
        private final Variable var;
        private final long[][] ranges;
        private final boolean up;
        private int ri;
        private long next;
        private boolean done;

        EnumGen(Variable var, IntervalDomain d, boolean up) {
            this.var = var;
            this.ranges = d.rangeArray();
            this.up = up;
            if (ranges.length == 0) { done = true; return; }
            ri = up ? 0 : ranges.length - 1;
            next = up ? ranges[0][0] : ranges[ranges.length - 1][1];
        }

        private void advance() {
            if (up) {
                if (next < ranges[ri][1]) { next++; return; }
                if (++ri >= ranges.length) { done = true; return; }
                next = ranges[ri][0];
            } else {
                if (next > ranges[ri][0]) { next--; return; }
                if (--ri < 0) { done = true; return; }
                next = ranges[ri][1];
            }
        }

        @Override public boolean next(Machine m) {
            while (!done) {
                long v = next;
                advance();
                if (done) m.lastSolution();
                if (m.unifyOrUndo(var, Number.valueOf(v))) return true;   // invariant: undo a failed try
            }
            return false;
        }
    }

    /** {@code X = V ; X #\= V} with V the smallest (up) or largest (down) value. */
    private static final class StepGen implements Generator {
        private final Variable var;
        private final IntervalDomain dom;
        private final long value;
        private int state;

        StepGen(Variable var, IntervalDomain d, boolean up) {
            this.var = var;
            this.dom = d;
            this.value = up ? d.min() : d.max();
        }

        @Override public boolean next(Machine m) {
            if (state == 0) {
                state = 1;
                return m.unify(var, Number.valueOf(value));
            }
            m.lastSolution();
            return ClpfdV2Bridge.narrowCell(var, dom.removeValue(value));
        }
    }

    /** {@code X #=< Mid ; X #> Mid} (reversed for down). */
    private static final class BisectGen implements Generator {
        private final Variable var;
        private final IntervalDomain low, high;
        private int state;

        BisectGen(Variable var, IntervalDomain d, boolean up) {
            this.var = var;
            long lo = d.min(), hi = d.max();
            long mid = (lo >> 1) + (hi >> 1) + (lo & hi & 1);          // floor((lo+hi)/2), no overflow
            IntervalDomain a = d.removeAbove(mid), b = d.removeBelow(mid + 1);
            this.low = up ? a : b;
            this.high = up ? b : a;
        }

        @Override public boolean next(Machine m) {
            while (state < 2) {
                IntervalDomain d = (state == 0) ? low : high;
                state++;
                if (state == 2) m.lastSolution();
                if (ClpfdV2Bridge.narrowCell(var, d)) return true;       // a failed post leaves nothing
            }
            return false;
        }
    }
    // END_CHANGE: ISS-2025-0642

    // START_CHANGE: ISS-2025-0643 - min(Expr)/max(Expr) by branch and bound, then SWI's order:
    // (Expr #= Opt, labeling(Rest, Vars) ; Expr #\= Opt, labeling(Opts, Vars))
    private static Builtin.Outcome optimise(Machine m, Term optsT, Term listArg, List<Term> vars,
                                            List<Term> objectives, List<Term> plain,
                                            Labeler.VarSel varSel, Labeler.ValOrder valOrder, String ind) {
        CompoundTerm what = (CompoundTerm) objectives.get(0);
        boolean minimize = "min".equals(what.getName());
        Term expr = what.getArguments().get(0);
        List<Term> resolvedVars = new ArrayList<Term>(vars.size());
        for (Term v : vars) resolvedVars.add(m.deref(v));
        BigInteger best = ClpfdV2Bridge.optimum(resolvedVars, expr, minimize, varSel, valOrder);
        if (best == null) return Builtin.Outcome.FAILURE;
        List<Term> rest = new ArrayList<Term>(objectives.subList(1, objectives.size()));
        rest.addAll(plain);
        Term restList = new Atom("[]");
        for (int i = rest.size() - 1; i >= 0; i--) {
            restList = new CompoundTerm(new Atom("."), Arrays.asList(rest.get(i), restList));
        }
        Term opt = num(best);
        Term take = new CompoundTerm(new Atom(","), Arrays.asList(
            (Term) new CompoundTerm(new Atom("#="), Arrays.asList(expr, opt)),
            new CompoundTerm(new Atom("labeling"), Arrays.asList(restList, listArg))));
        Term skip = new CompoundTerm(new Atom(","), Arrays.asList(
            (Term) new CompoundTerm(new Atom("#\\="), Arrays.asList(expr, opt)),
            new CompoundTerm(new Atom("labeling"), Arrays.asList(m.resolve(optsT), listArg))));
        m.pushGoal(new CompoundTerm(new Atom(";"), Arrays.asList(take, skip)));
        return Builtin.Outcome.SUCCESS;
    }
    // END_CHANGE: ISS-2025-0643

    /** A proper list's elements, or null. */
    private static List<Term> list(Machine m, Term t) {
        List<Term> out = new ArrayList<Term>();
        Term cur = Unify.deref(t);
        int n = 0;
        while (cur instanceof CompoundTerm && ".".equals(((CompoundTerm) cur).getName())
                && ((CompoundTerm) cur).getArguments().size() == 2) {
            out.add(((CompoundTerm) cur).getArguments().get(0));
            cur = Unify.deref(((CompoundTerm) cur).getArguments().get(1));
            if ((++n & 0x3FF) == 0) m.guard().step();
        }
        return (cur instanceof Atom && "[]".equals(((Atom) cur).getName())) ? out : null;
    }

    /** A proper list's elements; instantiation_error / type_error(list, T) otherwise. */
    private static List<Term> properList(Machine m, Term t, String ind) {
        List<Term> out = list(m, t);
        if (out != null) return out;
        Term d = m.deref(t);
        if (d instanceof Variable) throw Errors.instantiation(ind);
        Term cur = d;
        while (cur instanceof CompoundTerm && ".".equals(((CompoundTerm) cur).getName())
                && ((CompoundTerm) cur).getArguments().size() == 2) {
            cur = m.deref(((CompoundTerm) cur).getArguments().get(1));
        }
        if (cur instanceof Variable) throw Errors.instantiation(ind);
        throw Errors.type("list", m.resolve(d), ind);
    }
}
// END_CHANGE: ISS-2025-0471
