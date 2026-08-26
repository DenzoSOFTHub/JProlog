package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.builtin.io.StreamManager;
import it.denzosoft.jprolog.core.engine.BuiltInRegistry;
import it.denzosoft.jprolog.core.engine.KnowledgeBase;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.system.PrologFlags;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0499 - 4.1 wave B: the database, global-variable, flag and halt built-ins
// on the v4 SPI (design B.5).
/**
 * {@code current_predicate/1}, {@code retractall/1}, {@code abolish/1}, {@code dynamic/1},
 * {@code listing/0,1}, the global variables ({@code nb_setval/2}, {@code b_setval/2},
 * {@code nb_current/2}, {@code nb_delete/1}), {@code current_prolog_flag/2},
 * {@code set_prolog_flag/2}, {@code halt/0,1} and {@code findall/4}.
 *
 * <h3>What this wave fixes</h3>
 * <ul>
 *   <li><b>{@code listing/1} worked at all only by accident, and it did not.</b>
 *       {@code BuiltInFactory} registers ONE implementation per name, and the name
 *       {@code listing} was bound to {@code Listing0}, whose first statement rejects any argument:
 *       {@code listing(foo/1)} raised "listing/0 takes no arguments". The v4 table is keyed by
 *       {@code (name, arity)}, so {@code listing/0} and {@code listing/1} are two entries and
 *       {@code listing/1} does what its documentation always said.</li>
 *   <li><b>{@code current_predicate/1} and {@code nb_current/2} are lazy.</b> They built one
 *       solution map per predicate / per global variable before the first was looked at;
 *       {@code once(current_predicate(_))} now stops at the first.</li>
 *   <li><b>{@code b_setval/2} no longer goes through {@link Undo}</b> — it pushes its restore
 *       action straight onto the running machine's trail, which is what {@code Undo} was a doorway
 *       to. (The CLP(FD) bridge is the last external user; see LIM-037.)</li>
 * </ul>
 * Everything else keeps the error terms and the failure modes of the registry versions, including
 * their non-ISO {@code PrologEvaluationException}s for the flag predicates.
 */
final class NativeDb {

    private NativeDb() {}

    private static final Atom NIL = new Atom("[]");

    static void register(BuiltinTable t) {
        t.register("current_predicate", 1, new CurrentPredicateB());
        t.register("retractall", 1, new RetractallB());
        t.register("abolish", 1, new AbolishB());
        t.register("dynamic", 1, new DynamicB());
        t.register("listing", 0, new ListingB(0));
        t.register("listing", 1, new ListingB(1));
        t.register("nb_setval", 2, new SetvalB(false));
        t.register("b_setval", 2, new SetvalB(true));
        t.register("nb_current", 2, new NbCurrentB());
        t.register("nb_delete", 1, new NbDeleteB());
        t.register("current_prolog_flag", 2, new CurrentFlagB());
        t.register("set_prolog_flag", 2, new SetFlagB());
        t.register("halt", 0, new HaltB(0));
        t.register("halt", 1, new HaltB(1));
        // START_CHANGE: ISS-2025-0499 - findall/4 is NEW: findall/3 with an open tail (SWI/ISO cor.2)
        t.register("findall", 4, new Findall4B());
        // END_CHANGE: ISS-2025-0499
    }

    // ------------------------------------------------------------------ helpers

    private static Prolog prolog(Machine m, String ctx) {
        Prolog p = m.engine().prolog();
        if (p == null) {
            throw new PrologException(ISOErrorTerms.error(new Atom("system_error"),
                new Atom(ctx + ": cannot access clause database")));
        }
        return p;
    }

    /**
     * The permission check {@code builtin.database.DatabaseValidation} did, widened by
     * ISS-2025-0501 to the v4 native table and the prelude exports (see
     * {@code Machine.isProtectedProcedure}).
     */
    private static void checkModifiable(Machine m, String functor, int arity, String ctx) {
        if (m.isProtectedProcedure(functor, arity)) {
            throw new PrologException(ISOErrorTerms.permissionError(
                "modify", "static_procedure", Machine.indicator(functor, arity), ctx));
        }
    }

    // ------------------------------------------------------------------ current_predicate/1

    private static final class CurrentPredicateB implements Builtin {
        @Override
        public Outcome call(Machine m, final Term[] args) {
            Term pi = m.deref(args[0]);
            if (!(pi instanceof Variable)) {
                boolean valid = false;
                if (pi instanceof CompoundTerm) {
                    CompoundTerm c = (CompoundTerm) pi;
                    if ("/".equals(c.getName()) && c.getArguments().size() == 2) {
                        Term name = m.deref(c.getArguments().get(0));
                        Term ar = m.deref(c.getArguments().get(1));
                        boolean nameOk = (name instanceof Variable) || (name instanceof Atom);
                        boolean arOk = (ar instanceof Variable)
                            || (ar instanceof Number && ((Number) ar).isInteger()
                                && ((Number) ar).getValue() >= 0);
                        valid = nameOk && arOk;
                    }
                }
                if (!valid) {
                    throw new PrologException(ISOErrorTerms.typeError(
                        "predicate_indicator", m.resolve(pi), "current_predicate/1"));
                }
            }
            final List<Term> all = new ArrayList<Term>();
            for (String s : prolog(m, "current_predicate/1").getCurrentPredicates()) {
                int slash = s.lastIndexOf('/');
                if (slash <= 0) continue;
                int arity;
                try { arity = Integer.parseInt(s.substring(slash + 1)); }
                catch (NumberFormatException e) { continue; }
                all.add(new CompoundTerm(new Atom("/"), Arrays.asList(
                    (Term) new Atom(s.substring(0, slash)), (Term) Number.valueOf((long) arity))));
            }
            final int max = all.size();
            final int[] i = {0};
            Generator gen = new Generator() {
                @Override public boolean next(Machine mm) {
                    while (i[0] < max) {
                        Term cand = all.get(i[0]++);
                        if (i[0] >= max) mm.lastSolution();
                        if (mm.unifyOrUndo(args[0], cand)) return true;
                        mm.guard().step();
                    }
                    return false;
                }
            };
            return m.pushGenerator(gen) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ retractall/1, abolish/1

    private static final class RetractallB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term head = m.deref(args[0]);
            if (head instanceof Variable) {
                throw new PrologException(ISOErrorTerms.instantiationError(
                    "retractall/1: clause head must be instantiated"));
            }
            if (!(head instanceof Atom) && !(head instanceof CompoundTerm)) {
                throw new PrologException(
                    ISOErrorTerms.typeError("callable", m.resolve(head), "retractall/1"));
            }
            String f = (head instanceof Atom) ? ((Atom) head).getName() : ((CompoundTerm) head).getName();
            int n = (head instanceof Atom) ? 0 : ((CompoundTerm) head).getArguments().size();
            checkModifiable(m, f, n, "retractall/1");
            prolog(m, "retractall/1").retractAllClauses(m.resolve(head));
            return Outcome.SUCCESS;
        }
    }

    private static final class AbolishB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term pi = m.deref(args[0]);
            if (pi instanceof Variable) {
                throw new PrologException(ISOErrorTerms.instantiationError(
                    "abolish/1: predicate indicator must be instantiated"));
            }
            if (!(pi instanceof CompoundTerm) || !"/".equals(((CompoundTerm) pi).getName())
                    || ((CompoundTerm) pi).getArguments().size() != 2) {
                throw new PrologException(ISOErrorTerms.typeError("predicate_indicator",
                    m.resolve(pi), "abolish/1: argument must be Functor/Arity"));
            }
            Term ft = m.deref(((CompoundTerm) pi).getArguments().get(0));
            Term at = m.deref(((CompoundTerm) pi).getArguments().get(1));
            // START_CHANGE: ISS-2025-0508 - ISO 8.9.4.3 (a): an UNBOUND half of the indicator is
            // instantiation_error, not type_error(atom, _G17)/type_error(integer, _G17) — the old
            // culprit was a fresh variable, which no catcher can usefully match.
            if (ft instanceof Variable || at instanceof Variable) {
                throw Errors.instantiation("abolish/1");
            }
            // END_CHANGE: ISS-2025-0508
            if (!(ft instanceof Atom)) {
                throw new PrologException(ISOErrorTerms.typeError("atom", m.resolve(ft),
                    "abolish/1: functor must be an atom"));
            }
            if (!(at instanceof Number)) {
                throw new PrologException(ISOErrorTerms.typeError("integer", m.resolve(at),
                    "abolish/1: arity must be an integer"));
            }
            int arity = (int) Math.round(((Number) at).getValue());
            if (arity < 0) {
                throw new PrologException(ISOErrorTerms.domainError("not_less_than_zero", at,
                    "abolish/1: arity must be non-negative"));
            }
            String functor = ((Atom) ft).getName();
            checkModifiable(m, functor, arity, "abolish/1");
            prolog(m, "abolish/1").abolishPredicate(functor, arity);
            return Outcome.SUCCESS;
        }
    }

    // ------------------------------------------------------------------ dynamic/1

    private static final class DynamicB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            declare(m, m.engine().kb(), args[0]);
            return Outcome.SUCCESS;
        }

        private void declare(Machine m, KnowledgeBase kb, Term specRaw) {
            Term spec = m.deref(specRaw);
            if (spec instanceof Variable) {
                throw new PrologException(ISOErrorTerms.instantiationError("dynamic/1"));
            }
            if (spec instanceof CompoundTerm) {
                CompoundTerm c = (CompoundTerm) spec;
                String f = c.getName();
                int n = c.getArguments().size();
                if ((",".equals(f) || ".".equals(f)) && n == 2) {
                    declare(m, kb, c.getArguments().get(0));
                    declare(m, kb, c.getArguments().get(1));
                    return;
                }
                if ("/".equals(f) && n == 2) {
                    Term name = m.deref(c.getArguments().get(0));
                    Term ar = m.deref(c.getArguments().get(1));
                    if (name instanceof Variable || ar instanceof Variable) {
                        throw new PrologException(ISOErrorTerms.instantiationError("dynamic/1"));
                    }
                    if (name instanceof Atom && ar instanceof Number
                            && ((Number) ar).isInteger() && ((Number) ar).getValue() >= 0) {
                        kb.markDynamic(((Atom) name).getName(),
                            (int) Math.round(((Number) ar).getValue()));
                        return;
                    }
                    throw new PrologException(ISOErrorTerms.typeError(
                        "predicate_indicator", m.resolve(spec), "dynamic/1"));
                }
                throw new PrologException(ISOErrorTerms.typeError(
                    "predicate_indicator", m.resolve(spec), "dynamic/1"));
            }
            if (spec instanceof Atom) {
                String name = ((Atom) spec).getName();
                if ("[]".equals(name)) return;
                kb.markDynamic(name, 0);
                return;
            }
            throw new PrologException(ISOErrorTerms.typeError(
                "predicate_indicator", m.resolve(spec), "dynamic/1"));
        }
    }

    // ------------------------------------------------------------------ listing/0,1

    private static final class ListingB implements Builtin {
        private final int arity;
        ListingB(int arity) { this.arity = arity; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            Prolog p = prolog(m, "listing/" + arity);
            if (arity == 0) {
                p.listing();
                return Outcome.SUCCESS;
            }
            Term spec = m.deref(args[0]);
            // START_CHANGE: ISS-2025-0508
            if (!Unify.isGround(spec, m.guard())) throw Errors.instantiation("listing/1");
            // END_CHANGE: ISS-2025-0508
            // A bare name lists EVERY arity, as the reference has always documented
            // (`listing(parent)`); Name/Arity lists exactly one.
            if (spec instanceof Atom) {
                String name = ((Atom) spec).getName();
                java.util.List<String> hits = new ArrayList<String>();
                for (String s : p.getCurrentPredicates()) {
                    int slash = s.lastIndexOf('/');
                    if (slash > 0 && s.substring(0, slash).equals(name)) hits.add(s);
                }
                java.util.Collections.sort(hits);
                if (hits.isEmpty()) {
                    StreamManager.out().println("% No clauses found for " + name);
                } else {
                    for (int i = 0; i < hits.size(); i++) p.listing(hits.get(i));
                }
                return Outcome.SUCCESS;
            }
            p.listing(Writer.format(spec, Writer.Options.write(), 1200));
            return Outcome.SUCCESS;
        }
    }

    // ------------------------------------------------------------------ the global variables

    private static final class SetvalB implements Builtin {
        private final boolean backtrackable;
        SetvalB(boolean backtrackable) { this.backtrackable = backtrackable; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            final String ctx = backtrackable ? "b_setval/2" : "nb_setval/2";
            Term nameT = m.deref(args[0]);
            if (nameT instanceof Variable) throw new PrologException(ISOErrorTerms.instantiationError(ctx));
            if (!(nameT instanceof Atom)) {
                throw new PrologException(ISOErrorTerms.typeError("atom", m.resolve(nameT), ctx));
            }
            final String name = ((Atom) nameT).getName();
            final Prolog p = prolog(m, ctx);
            if (backtrackable) {
                final Term old = p.nbGetval(name);
                // ISS-2025-0499: straight onto the running machine's trail — no Undo doorway.
                m.bindings().pushUndo(new Runnable() {
                    @Override public void run() {
                        if (old == null) p.nbDelete(name); else p.nbSetval(name, old);
                    }
                });
            }
            p.nbSetval(name, m.resolve(args[1]));
            return Outcome.SUCCESS;
        }
    }

    private static final class NbCurrentB implements Builtin {
        @Override
        public Outcome call(Machine m, final Term[] args) {
            Term nameT = m.deref(args[0]);
            Map<String, Term> all = prolog(m, "nb_current/2").nbCurrentAll();
            if (nameT instanceof Atom) {
                Term v = all.get(((Atom) nameT).getName());
                if (v == null) return Outcome.FAILURE;
                return m.unify(args[1], v) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            final List<Map.Entry<String, Term>> entries =
                new ArrayList<Map.Entry<String, Term>>(all.entrySet());
            final int max = entries.size();
            final int[] i = {0};
            Generator gen = new Generator() {
                @Override public boolean next(Machine mm) {
                    while (i[0] < max) {
                        Map.Entry<String, Term> e = entries.get(i[0]++);
                        if (i[0] >= max) mm.lastSolution();
                        Bindings b = mm.bindings();
                        int mark = b.mark();
                        b.forceTrail++;
                        boolean ok;
                        try {
                            ok = Unify.unify(args[0], new Atom(e.getKey()), b)
                              && Unify.unify(args[1], e.getValue(), b);
                            if (!ok) b.undo(mark);                 // ISS-2025-0448 ordering
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

    private static final class NbDeleteB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term nameT = m.deref(args[0]);
            if (nameT instanceof Variable) {
                throw new PrologException(ISOErrorTerms.instantiationError("nb_delete/1"));
            }
            if (!(nameT instanceof Atom)) {
                throw new PrologException(
                    ISOErrorTerms.typeError("atom", m.resolve(nameT), "nb_delete/1"));
            }
            prolog(m, "nb_delete/1").nbDelete(((Atom) nameT).getName());
            return Outcome.SUCCESS;
        }
    }

    // ------------------------------------------------------------------ the ISO flags

    private static final class CurrentFlagB implements Builtin {
        @Override
        public Outcome call(Machine m, final Term[] args) {
            Term flagT = m.deref(args[0]);
            if (flagT instanceof Atom) {
                // START_CHANGE: ISS-2025-0508 - ISO 8.17.2.3 (b): an atom that is not a flag of
                // this implementation is domain_error(prolog_flag, F), not a silent failure.
                Term v = PrologFlags.getFlag(((Atom) flagT).getName());
                if (v == null) throw Errors.domain("prolog_flag", flagT, "current_prolog_flag/2");
                // END_CHANGE: ISS-2025-0508
                return m.unify(args[1], v) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            // START_CHANGE: ISS-2025-0508 - ISO 8.17.2.3 (a): type_error(atom, F).
            if (!(flagT instanceof Variable)) {
                throw Errors.type("atom", m.resolve(flagT), "current_prolog_flag/2");
            }
            // END_CHANGE: ISS-2025-0508
            final List<String> names = new ArrayList<String>(PrologFlags.getAllFlagNames());
            final int max = names.size();
            final int[] i = {0};
            Generator gen = new Generator() {
                @Override public boolean next(Machine mm) {
                    while (i[0] < max) {
                        String name = names.get(i[0]++);
                        if (i[0] >= max) mm.lastSolution();
                        Term v = PrologFlags.getFlag(name);
                        if (v == null) continue;
                        Bindings b = mm.bindings();
                        int mark = b.mark();
                        b.forceTrail++;
                        boolean ok;
                        try {
                            ok = Unify.unify(args[0], new Atom(name), b)
                              && Unify.unify(args[1], v, b);
                            if (!ok) b.undo(mark);                 // ISS-2025-0448 ordering
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

    private static final class SetFlagB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            // START_CHANGE: ISS-2025-0508 - ISO 8.17.1.3, all five clauses.
            Term flagT = m.deref(args[0]);
            Term valT = m.deref(args[1]);
            if (flagT instanceof Variable || valT instanceof Variable) {
                throw Errors.instantiation("set_prolog_flag/2");
            }
            if (!(flagT instanceof Atom)) {
                throw Errors.type("atom", m.resolve(flagT), "set_prolog_flag/2");
            }
            String name = ((Atom) flagT).getName();
            if (!PrologFlags.hasFlag(name)) {
                throw Errors.domain("prolog_flag", flagT, "set_prolog_flag/2");
            }
            if (PrologFlags.isReadOnly(name)) {
                throw Errors.permission("modify", "flag", flagT, "set_prolog_flag/2");
            }
            Term value = m.resolve(args[1]);
            if (PrologFlags.setFlag(name, value)) return Outcome.SUCCESS;
            throw Errors.domain("flag_value",
                new CompoundTerm(new Atom("+"), java.util.Arrays.asList(flagT, value)),
                "set_prolog_flag/2");
            // END_CHANGE: ISS-2025-0508
        }
    }

    // ------------------------------------------------------------------ halt/0,1

    private static final class HaltB implements Builtin {
        private final int arity;
        HaltB(int arity) { this.arity = arity; }

        @Override
        public Outcome call(Machine m, Term[] args) {
            if (arity == 0) throw new PrologException(0);
            Term code = m.deref(args[0]);
            if (code instanceof Variable) {
                throw new PrologException(ISOErrorTerms.instantiationError(
                    "halt/1: exit code must be instantiated"));
            }
            if (!(code instanceof Number)) {
                throw new PrologException(ISOErrorTerms.typeError("integer", m.resolve(code),
                    "halt/1: exit code must be an integer"));
            }
            throw new PrologException((int) Math.round(((Number) code).getValue()));
        }
    }

    // ------------------------------------------------------------------ findall/4

    /** {@code findall(Template, Goal, List, Tail)}: like findall/3 but List ends in Tail. */
    private static final class Findall4B implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term goal = m.deref(args[1]);
            if (goal instanceof Variable) throw Errors.instantiation("findall/4");
            if (!(goal instanceof Atom) && !(goal instanceof CompoundTerm)) {
                throw Errors.type("callable", m.resolve(goal), "findall/4");
            }
            List<Term> found = m.findAll(args[0], args[1]);
            Term list = args[3];
            for (int i = found.size() - 1; i >= 0; i--) {
                list = new CompoundTerm(new Atom("."), Arrays.asList(found.get(i), list));
            }
            return m.unify(args[2], list) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }
}
// END_CHANGE: ISS-2025-0499
