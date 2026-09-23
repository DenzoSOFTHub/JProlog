package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.List;

// START_CHANGE: ISS-2025-0443 - engine v4, design B.4/B.5 (the first natives on the v4 SPI).
/**
 * The built-ins that must be native on v4 because they depend on the cell model, plus the term
 * walkers of {@link Unify} that are now cycle-safe.
 *
 * <ul>
 *   <li>{@code setarg/3} and {@code nb_setarg/3} mutate a bound compound in place, so they need the
 *       ACTUAL term objects. On v3.8.0 they were the one exception to the resolved-goal handoff
 *       (ISS-2025-0317, an unresolved goal plus a copy of the whole binding map); with cells there
 *       is no binding map at all, so design B.5 makes them native in wave W1.</li>
 *   <li>{@code cyclic_term/1} / {@code acyclic_term/1} are real tests now that rational trees are
 *       supported, instead of the {@code representation_error} the old walkers raised.</li>
 *   <li>{@code term_variables/2}, {@code ground/1}, {@code numbervars/3}, {@code subsumes_term/2},
 *       {@code compare/3} are the cycle-safe, stack-safe versions from {@link Unify}.</li>
 * </ul>
 */
final class NativeBuiltins {

    private NativeBuiltins() {}

    static void register(BuiltinTable t) {
        t.register("setarg", 3, new SetArg(true));
        t.register("nb_setarg", 3, new SetArg(false));
        t.register("cyclic_term", 1, new Cyclic(true));
        t.register("acyclic_term", 1, new Cyclic(false));
        t.register("term_variables", 2, new TermVariables());
        t.register("ground", 1, new Ground());
        t.register("numbervars", 3, new NumberVars());
        t.register("subsumes_term", 2, new Subsumes());
        t.register("compare", 3, new Compare());
        // START_CHANGE: ISS-2025-0451/0452/0453 - wave W3: the control/collection built-ins and the
        // lazy library generators.
        NativeControl.register(t);
        NativeLibrary.register(t);
        // END_CHANGE: ISS-2025-0451/0452/0453
        // START_CHANGE: ISS-2025-0457/0458 - wave W4: attributed variables and the wake queue.
        Coroutining.register(t);
        // END_CHANGE: ISS-2025-0457/0458
        // START_CHANGE: ISS-2025-0464 - wave W5: the tabling built-ins on the v4 answer store.
        Tabling.register(t);
        // END_CHANGE: ISS-2025-0464
        // START_CHANGE: ISS-2025-0470 - wave W6: current_module/1 and the module properties of
        // predicate_property/2 on the v4 Modules owner.
        ModuleBuiltins.register(t);
        // END_CHANGE: ISS-2025-0470
        // START_CHANGE: ISS-2025-0471 - wave W6: label/1 and labeling/2 as cell-based generators.
        ClpfdNative.register(t);
        // ISS-2025-0486 - wave W9: sort/4, predsort/3, max_list/2, min_list/2, current_op/3,
        // nb_getval/2, b_getval/2 (the last of LIM-037's eager list)
        NativeMisc.register(t);
        // END_CHANGE: ISS-2025-0471
        // START_CHANGE: ISS-2025-0496 - 4.1 wave B: the io family (format, the write family, the
        // character I/O predicates) as natives over Streams/Writer.
        NativeIo.register(t);
        // END_CHANGE: ISS-2025-0496
        // START_CHANGE: ISS-2025-0566 - 4.5 wave P3: the term readers and in-memory input (v2 parser)
        NativeRead.register(t);
        NativeExpand.register(t);   // ISS-2025-0571
        // END_CHANGE: ISS-2025-0566
        // START_CHANGE: ISS-2025-0497 - 4.1 wave B: the atom/string/character/conversion families
        // and the last three eager list built-ins.
        NativeText.register(t);
        // END_CHANGE: ISS-2025-0497
        // START_CHANGE: ISS-2025-0498 - 4.1 wave B: functor/arg/univ, the remaining type checks,
        // succ/plus and unify_with_occurs_check.
        NativeTerm.register(t);
        t.register("number_vars", 3, new NumberVars());     // the legacy alias of numbervars/3
        // END_CHANGE: ISS-2025-0498
        // START_CHANGE: ISS-2025-0499 - 4.1 wave B: the database, global-variable, flag and halt
        // built-ins, plus findall/4.
        NativeDb.register(t);

        // START_CHANGE: ISS-2025-0503 - 4.2 wave C: char_type/2 and code_type/2, the last eager
        // enumeration of the ISO-core set, are generators with the SWI parametric forms.
        NativeChars.register(t);
        // END_CHANGE: ISS-2025-0503
        // END_CHANGE: ISS-2025-0499
    }

    /** {@code setarg(+N, +Compound, +Value)} — backtrackable; {@code nb_setarg/3} is not. */
    private static final class SetArg implements Builtin {
        private final boolean backtrackable;
        SetArg(boolean backtrackable) { this.backtrackable = backtrackable; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            Term nt = m.deref(args[0]);
            Term ct = m.deref(args[1]);
            Term vt = m.deref(args[2]);
            String ctx = (backtrackable ? "setarg/3" : "nb_setarg/3");
            if (nt instanceof Variable || ct instanceof Variable) throw Errors.instantiation(ctx);
            if (!(nt instanceof Number) || !((Number) nt).isInteger()) throw Errors.type("integer", nt, ctx);
            if (!(ct instanceof CompoundTerm)) throw Errors.type("compound", ct, ctx);
            final CompoundTerm c = (CompoundTerm) ct;
            int n = (int) ((Number) nt).longValue();
            if (n < 1 || n > c.getArguments().size()) return Outcome.FAILURE;
            final int idx = n;
            final Term value = backtrackable ? vt : m.copy(vt);
            final Term old = c.setArgument(idx, value);
            if (backtrackable) {
                m.bindings().pushUndo(new Runnable() {
                    @Override public void run() { c.setArgument(idx, old); }
                });
            }
            return Outcome.SUCCESS;
        }
    }

    private static final class Cyclic implements Builtin {
        private final boolean wantCyclic;
        Cyclic(boolean wantCyclic) { this.wantCyclic = wantCyclic; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            boolean c = Unify.isCyclic(args[0], m.guard());
            return (c == wantCyclic) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class TermVariables implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            List<Variable> vars = new ArrayList<Variable>();
            Unify.termVariables(args[0], vars, m.guard());
            Term list = new Atom("[]");
            for (int i = vars.size() - 1; i >= 0; i--) {
                List<Term> cell = new ArrayList<Term>(2);
                cell.add(vars.get(i));
                cell.add(list);
                list = new CompoundTerm(new Atom("."), cell);
            }
            return m.unify(args[1], list) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class Ground implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            return Unify.isGround(args[0], m.guard()) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class NumberVars implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term start = m.deref(args[1]);
            if (start instanceof Variable) throw Errors.instantiation("numbervars/3");
            if (!(start instanceof Number) || !((Number) start).isInteger()) {
                throw Errors.type("integer", start, "numbervars/3");
            }
            int end = Unify.numberVars(args[0], (int) ((Number) start).longValue(), m.bindings());
            return m.unify(args[2], Number.valueOf(end)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class Subsumes implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            return Unify.subsumes(args[0], args[1], m.bindings()) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class Compare implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term order = m.deref(args[0]);                 // ISO 8.4.2.3: validate Order first
            if (!(order instanceof Variable)) {
                if (!(order instanceof Atom)) throw Errors.type("atom", order, "compare/3");
                String o = ((Atom) order).getName();
                if (!"<".equals(o) && !">".equals(o) && !"=".equals(o)) {
                    throw Errors.domain("order", order, "compare/3");
                }
            }
            int c = Unify.compareTerms(args[1], args[2], m.guard());
            return m.unify(args[0], new Atom(c < 0 ? "<" : (c > 0 ? ">" : "="))) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }
}
// END_CHANGE: ISS-2025-0443
