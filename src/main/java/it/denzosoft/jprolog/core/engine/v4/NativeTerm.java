package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.ControlFlow;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0498 - 4.1 wave B: term construction/inspection, the remaining type
// checks, succ/2, plus/3 and unify_with_occurs_check/2 on the v4 SPI (design B.5).
/**
 * {@code functor/3}, {@code arg/3}, {@code =../2}, {@code atom_to_term/3}, the type checks that
 * are not inline in the machine ({@code is_list/1}, {@code proper_list/1}, {@code partial_list/1},
 * {@code simple/1}, {@code string/1}, {@code must_be/2}), {@code succ/2}, {@code plus/3} and
 * {@code unify_with_occurs_check/2}.
 *
 * <h3>Two behaviour corrections, both deliberate</h3>
 * <ul>
 *   <li>{@code functor(f(X), N, A)} raised {@code instantiation_error}: the registry version chose
 *       its mode with {@code Term.isGround()}, so a compound holding a variable was treated as the
 *       CONSTRUCT mode with two unbound outputs. ISO 8.5.1 decomposes any non-variable first
 *       argument, and that is what the native does — {@code N = f, A = 1}.</li>
 *   <li>{@code arg(N, T, A)} with {@code N} unbound raised {@code instantiation_error}; ISO 8.5.2
 *       and every other system ENUMERATE the arguments. It is a lazy {@link Generator} now
 *       (one argument per redo), which is also what makes {@code arg/3} usable in a loop.</li>
 * </ul>
 * Everything else — every error term, every failure mode — is what the registry versions produced.
 */
final class NativeTerm {

    private NativeTerm() {}

    private static final Atom NIL = new Atom("[]");

    static void register(BuiltinTable t) {
        t.register("functor", 3, new FunctorB());
        t.register("arg", 3, new ArgB());
        t.register("=..", 2, new UnivB());
        t.register("atom_to_term", 3, new AtomToTermB());
        t.register("succ", 2, new SuccB());
        t.register("plus", 3, new PlusB());
        t.register("is_list", 1, new ListCheckB(true));
        t.register("proper_list", 1, new ListCheckB(true));
        t.register("partial_list", 1, new ListCheckB(false));
        t.register("simple", 1, new SimpleB());
        t.register("string", 1, new StringB());
        t.register("must_be", 2, new MustBeB());
        t.register("unify_with_occurs_check", 2, new UnifyOccursB());
    }

    // ------------------------------------------------------------------ functor/3

    private static final class FunctorB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term t = m.deref(args[0]);
            if (!(t instanceof Variable)) {                        // ISO 8.5.1: decompose
                Term name;
                long arity;
                if (t instanceof CompoundTerm) {
                    name = ((CompoundTerm) t).getFunctor();
                    arity = ((CompoundTerm) t).getArguments().size();
                } else {
                    name = t;                                      // atom, number or string
                    arity = 0;
                }
                if (!m.unifyOrUndo(args[1], name)) return Outcome.FAILURE;
                return m.unify(args[2], Number.valueOf(arity)) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            Term name = m.deref(args[1]);
            Term ar = m.deref(args[2]);
            if (name instanceof Variable || ar instanceof Variable) {
                throw new PrologException(ISOErrorTerms.instantiationError("functor/3"));
            }
            if (!(ar instanceof Number) || !((Number) ar).isInteger()) {
                throw new PrologException(ISOErrorTerms.typeError("integer", ar, "functor/3"));
            }
            int n = (int) ((Number) ar).longValue();
            if (n < 0) {
                throw new PrologException(ISOErrorTerms.domainError("not_less_than_zero", ar, "functor/3"));
            }
            if (n == 0) {
                if (name instanceof Atom || name instanceof Number) {
                    return m.unify(args[0], name) ? Outcome.SUCCESS : Outcome.FAILURE;
                }
                throw new PrologException(ISOErrorTerms.typeError("atomic", m.resolve(name), "functor/3"));
            }
            if (!(name instanceof Atom || name instanceof Number || name instanceof PrologString)) {
                throw new PrologException(ISOErrorTerms.typeError("atomic", m.resolve(name), "functor/3"));
            }
            if (!(name instanceof Atom)) {
                throw new PrologException(ISOErrorTerms.typeError("atom", name, "functor/3"));
            }
            List<Term> fresh = new ArrayList<Term>(n);
            for (int i = 0; i < n; i++) fresh.add(new Variable());
            return m.unify(args[0], new CompoundTerm((Atom) name, fresh))
                ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ arg/3

    private static final class ArgB implements Builtin {
        @Override
        public Outcome call(Machine m, final Term[] args) {
            Term idx = m.deref(args[0]);
            Term t = m.deref(args[1]);
            if (t instanceof Variable) {
                throw new PrologException(ISOErrorTerms.instantiationError("arg/3"));
            }
            if (!(t instanceof CompoundTerm)) {
                throw new PrologException(ISOErrorTerms.typeError("compound", m.resolve(t), "arg/3"));
            }
            final List<Term> as = ((CompoundTerm) t).getArguments();
            if (idx instanceof Variable) {
                // ISS-2025-0498: enumerate, one argument per redo.
                final int max = as.size();
                final int[] i = {0};
                Generator gen = new Generator() {
                    @Override public boolean next(Machine mm) {
                        while (i[0] < max) {
                            int k = i[0]++;
                            if (i[0] >= max) mm.lastSolution();
                            Bindings b = mm.bindings();
                            int mark = b.mark();
                            b.forceTrail++;
                            boolean ok;
                            try {
                                ok = Unify.unify(args[0], Number.valueOf((long) (k + 1)), b)
                                  && Unify.unify(args[2], as.get(k), b);
                                if (!ok) b.undo(mark);             // ISS-2025-0448 ordering
                            } finally {
                                b.forceTrail--;
                            }
                            if (ok) return true;
                            mm.guard().step();
                        }
                        return false;
                    }
                };
                return m.pushGenerator(gen) ? Outcome.SUSPENDED : Outcome.FAILURE;
            }
            if (!(idx instanceof Number) || !((Number) idx).isInteger()) {
                throw new PrologException(ISOErrorTerms.typeError("integer", idx, "arg/3"));
            }
            long n = ((Number) idx).longValue();
            if (n < 0) {
                throw new PrologException(ISOErrorTerms.domainError("not_less_than_zero", idx, "arg/3"));
            }
            if (n < 1 || n > as.size()) return Outcome.FAILURE;    // ISO 8.5.2.1: plain failure
            return m.unify(args[2], as.get((int) n - 1)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ =../2

    private static final class UnivB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term t = m.deref(args[0]);
            if (!(t instanceof Variable)) {
                List<Term> parts = new ArrayList<Term>();
                if (t instanceof CompoundTerm) {
                    parts.add(((CompoundTerm) t).getFunctor());
                    parts.addAll(((CompoundTerm) t).getArguments());
                } else if (t instanceof Atom || t instanceof Number || t instanceof PrologString) {
                    parts.add(t);
                } else {
                    return Outcome.FAILURE;
                }
                return m.unify(args[1], NativeLibrary.listOf(parts, NIL))
                    ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            List<Term> elems = new ArrayList<Term>();
            Term tail = NativeLibrary.spineTail(args[1], elems, m.guard());
            if (tail instanceof Variable) {
                throw new PrologException(ISOErrorTerms.instantiationError("=../2"));
            }
            if (tail == null || !NativeLibrary.isNil(tail)) {
                throw new PrologException(ISOErrorTerms.typeError("list", m.resolve(args[1]), "=../2"));
            }
            if (elems.isEmpty()) {
                throw new PrologException(
                    ISOErrorTerms.domainError("non_empty_list", m.resolve(args[1]), "=../2"));
            }
            Term head = m.deref(elems.get(0));
            if (head instanceof Variable) {
                throw new PrologException(ISOErrorTerms.instantiationError("=../2"));
            }
            Term built;
            if (elems.size() == 1) {
                if (!(head instanceof Atom) && !(head instanceof Number)
                        && !(head instanceof PrologString)) {
                    throw new PrologException(ISOErrorTerms.typeError("atomic", m.resolve(head), "=../2"));
                }
                built = head;
            } else {
                if (!(head instanceof Atom)) {
                    throw new PrologException(ISOErrorTerms.typeError("atom", m.resolve(head), "=../2"));
                }
                built = new CompoundTerm((Atom) head,
                    new ArrayList<Term>(elems.subList(1, elems.size())));
            }
            return m.unify(args[0], built) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ atom_to_term/3

    private static final class AtomToTermB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term a = m.deref(args[0]);
            if (!(a instanceof Atom)) {
                throw new PrologEvaluationException("type_error(atom, " + m.resolve(a) + ")");
            }
            Term parsed;
            try {
                parsed = new it.denzosoft.jprolog.core.parser.Parser().parseTerm(((Atom) a).getName());
            } catch (PrologException pe) {
                throw pe;
            } catch (StackOverflowError so) {
                throw new PrologException(ISOErrorTerms.resourceError("parser_nesting", "atom_to_term/3"));
            } catch (RuntimeException e) {
                ControlFlow.rethrowIfControl(e);
                throw new PrologEvaluationException("syntax_error(" + e.getMessage() + ")");
            }
            if (parsed == null) return Outcome.FAILURE;
            Map<String, Variable> named = new LinkedHashMap<String, Variable>();
            collectNamed(parsed, named);
            List<Term> pairs = new ArrayList<Term>(named.size());
            for (Map.Entry<String, Variable> e : named.entrySet()) {
                pairs.add(new CompoundTerm(new Atom("="),
                    Arrays.asList((Term) new Atom(e.getKey()), (Term) e.getValue())));
            }
            if (!m.unifyOrUndo(args[1], parsed)) return Outcome.FAILURE;
            return m.unify(args[2], NativeLibrary.listOf(pairs, NIL)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }

        private void collectNamed(Term t, Map<String, Variable> out) {
            ArrayList<Term> work = new ArrayList<Term>();
            work.add(t);
            while (!work.isEmpty()) {
                Term x = work.remove(work.size() - 1);
                if (x instanceof Variable) {
                    String n = ((Variable) x).getName();
                    if (n != null && !n.startsWith("_") && !out.containsKey(n)) out.put(n, (Variable) x);
                } else if (x instanceof CompoundTerm) {
                    List<Term> as = ((CompoundTerm) x).getArguments();
                    for (int i = as.size() - 1; i >= 0; i--) work.add(as.get(i));
                }
            }
        }
    }

    // ------------------------------------------------------------------ succ/2, plus/3

    private static final class SuccB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term a = m.deref(args[0]), b = m.deref(args[1]);
            boolean av = a instanceof Variable, bv = b instanceof Variable;
            if (av && bv) {
                throw new PrologEvaluationException("succ/2: at least one argument must be instantiated");
            }
            if (!av && !bv) {
                if (!(a instanceof Number) || !(b instanceof Number)) return Outcome.FAILURE;
                double v1 = ((Number) a).getValue(), v2 = ((Number) b).getValue();
                if (v1 != Math.floor(v1) || v2 != Math.floor(v2) || v1 < 0 || v2 < 0) return Outcome.FAILURE;
                return ((long) v2 == (long) v1 + 1) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (av) {
                if (!(b instanceof Number)) return Outcome.FAILURE;
                double v2 = ((Number) b).getValue();
                if (v2 != Math.floor(v2) || v2 < 1) return Outcome.FAILURE;
                return m.unify(args[0], Number.valueOf((long) v2 - 1)) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (!(a instanceof Number)) return Outcome.FAILURE;
            double v1 = ((Number) a).getValue();
            if (v1 != Math.floor(v1) || v1 < 0) return Outcome.FAILURE;
            return m.unify(args[1], Number.valueOf((long) v1 + 1)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class PlusB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term a = m.deref(args[0]), b = m.deref(args[1]), c = m.deref(args[2]);
            boolean av = a instanceof Variable, bv = b instanceof Variable, cv = c instanceof Variable;
            int vars = (av ? 1 : 0) + (bv ? 1 : 0) + (cv ? 1 : 0);
            if (vars > 1) {
                throw new PrologEvaluationException("plus/3: at most one argument can be uninstantiated");
            }
            if (vars == 0) {
                if (!(a instanceof Number) || !(b instanceof Number) || !(c instanceof Number)) {
                    return Outcome.FAILURE;
                }
                Number n1 = (Number) a, n2 = (Number) b, n3 = (Number) c;
                if (n1.isInteger() && n2.isInteger() && n3.isInteger()) {
                    return (n1.longValue() + n2.longValue() == n3.longValue())
                        ? Outcome.SUCCESS : Outcome.FAILURE;
                }
                return (Double.compare(n1.getValue() + n2.getValue(), n3.getValue()) == 0)
                    ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (av) {
                if (!(b instanceof Number) || !(c instanceof Number)) return Outcome.FAILURE;
                return m.unify(args[0], sub((Number) c, (Number) b)) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (bv) {
                if (!(a instanceof Number) || !(c instanceof Number)) return Outcome.FAILURE;
                return m.unify(args[1], sub((Number) c, (Number) a)) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            if (!(a instanceof Number) || !(b instanceof Number)) return Outcome.FAILURE;
            Number n1 = (Number) a, n2 = (Number) b;
            Term r = (n1.isInteger() && n2.isInteger())
                ? Number.valueOf(n1.longValue() + n2.longValue())
                : new Number(n1.getValue() + n2.getValue());
            return m.unify(args[2], r) ? Outcome.SUCCESS : Outcome.FAILURE;
        }

        private Term sub(Number x, Number y) {
            return (x.isInteger() && y.isInteger())
                ? Number.valueOf(x.longValue() - y.longValue())
                : new Number(x.getValue() - y.getValue());
        }
    }

    // ------------------------------------------------------------------ the remaining type checks

    private static final class ListCheckB implements Builtin {
        private final boolean proper;
        ListCheckB(boolean proper) { this.proper = proper; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            Term tail = NativeLibrary.spineTail(args[0], null, m.guard());
            if (tail == null) return Outcome.FAILURE;              // cyclic spine
            boolean ok = proper ? NativeLibrary.isNil(tail) : (tail instanceof Variable);
            return ok ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class SimpleB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term t = m.deref(args[0]);
            boolean ok = (t instanceof Atom) || (t instanceof Number) || (t instanceof PrologString);
            return ok ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class StringB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            return (m.deref(args[0]) instanceof PrologString) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class MustBeB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term ty = m.deref(args[0]);
            Term v = m.deref(args[1]);
            if (!(ty instanceof Atom)) {
                throw new PrologException(ISOErrorTerms.typeError("atom", m.resolve(ty), "must_be/2"));
            }
            String type = ((Atom) ty).getName();
            if (v instanceof Variable && !"var".equals(type) && !"nonvar".equals(type)) {
                throw new PrologException(ISOErrorTerms.instantiationError("must_be/2"));
            }
            boolean ok;
            if ("atom".equals(type))                   ok = v instanceof Atom;
            else if ("atomic".equals(type))            ok = (v instanceof Atom) || (v instanceof Number);
            else if ("number".equals(type))            ok = v instanceof Number;
            else if ("integer".equals(type))           ok = (v instanceof Number) && ((Number) v).isInteger();
            else if ("float".equals(type))             ok = (v instanceof Number) && !((Number) v).isInteger();
            else if ("compound".equals(type))          ok = v instanceof CompoundTerm;
            else if ("callable".equals(type))          ok = (v instanceof Atom) || (v instanceof CompoundTerm);
            else if ("var".equals(type))               ok = v instanceof Variable;
            else if ("nonvar".equals(type))            ok = !(v instanceof Variable);
            else if ("ground".equals(type))            ok = Unify.isGround(v, m.guard());
            else if ("list".equals(type))              ok = NativeLibrary.elements(v, m.guard()) != null;
            else if ("boolean".equals(type)) {
                ok = (v instanceof Atom)
                  && ("true".equals(((Atom) v).getName()) || "false".equals(((Atom) v).getName()));
            } else if ("positive_integer".equals(type)) {
                ok = (v instanceof Number) && ((Number) v).isInteger() && ((Number) v).longValue() > 0;
            } else if ("nonneg".equals(type)) {
                ok = (v instanceof Number) && ((Number) v).isInteger() && ((Number) v).longValue() >= 0;
            } else {
                throw new PrologException(ISOErrorTerms.domainError("type", ty, "must_be/2"));
            }
            if (!ok) throw new PrologException(ISOErrorTerms.typeError(type, m.resolve(v), "must_be/2"));
            return Outcome.SUCCESS;
        }
    }

    // ------------------------------------------------------------------ unify_with_occurs_check/2

    /**
     * ISO 8.2.2. The engine supports rational trees, so a plain unification would SUCCEED on
     * {@code X = f(X)}: the native unifies inside a mark/undo extent and rejects the result when
     * it became cyclic. That is the occurs check, expressed as a property of the answer rather
     * than as a per-binding test — and it keeps the check out of {@link Unify}'s binding path,
     * which every other unification in the engine goes through.
     */
    private static final class UnifyOccursB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Bindings b = m.bindings();
            int mark = b.mark();
            b.forceTrail++;
            boolean ok;
            try {
                ok = Unify.unify(args[0], args[1], b);
                if (ok && (Unify.isCyclic(args[0], m.guard()) || Unify.isCyclic(args[1], m.guard()))) {
                    ok = false;
                }
                if (!ok) b.undo(mark);                             // ISS-2025-0448 ordering
            } finally {
                b.forceTrail--;
            }
            return ok ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }
}
// END_CHANGE: ISS-2025-0498
