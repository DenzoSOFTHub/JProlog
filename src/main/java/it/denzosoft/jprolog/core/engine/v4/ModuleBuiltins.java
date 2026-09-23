package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

// START_CHANGE: ISS-2025-0470 - engine v4 wave W6, design B.10: the module-facing built-ins.
/**
 * {@code current_module/1} and the module properties of {@code predicate_property/2}, on the v4
 * {@link Modules} owner.
 *
 * <p>Both are v4-only, like {@code current_table/2} (W5) and {@code unifiable/3} (W4): the legacy
 * and v2 engines resolve through {@code core.module.ModuleManager}, which has no notion of a
 * library module and no autoload, so there is nothing there for them to report.
 */
final class ModuleBuiltins {

    private ModuleBuiltins() {}

    static void register(BuiltinTable t) {
        t.register("current_module", 1, new CurrentModule());
        t.register("predicate_property", 2, new PredicateProperty());
    }

    /** {@code current_module(?Module)} — enumeration by unification, like {@code current_op/3}. */
    private static final class CurrentModule implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            final Term q = m.deref(args[0]);
            final List<Term> names = m.engine().modules4().currentModuleTerms();
            if (q instanceof Atom) {
                String n = ((Atom) q).getName();
                for (int i = 0; i < names.size(); i++) {
                    if (n.equals(((Atom) names.get(i)).getName())) return Outcome.SUCCESS;
                }
                return Outcome.FAILURE;
            }
            if (!(q instanceof Variable)) throw Errors.type("atom", m.resolve(q), "current_module/1");
            final int[] i = {0};
            Generator gen = new Generator() {
                @Override public boolean next(Machine mm) {
                    while (i[0] < names.size()) {
                        Term name = names.get(i[0]++);
                        if (i[0] >= names.size()) mm.lastSolution();
                        if (mm.unifyOrUndo(q, name)) return true;
                    }
                    return false;
                }
            };
            return m.pushGenerator(gen) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }
    }

    /**
     * {@code predicate_property(?Head, ?Property)} with the three module properties design B.10
     * asks for — {@code exported}, {@code imported_from(M)} and {@code defined_in(M)} — layered
     * over the existing registry implementation, which answers everything else
     * ({@code built_in}, {@code dynamic}, {@code static}, {@code defined}, …) unchanged.
     */
    private static final class PredicateProperty implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Term head = m.deref(args[0]);
            Term prop = m.deref(args[1]);
            String f = null;
            int n = 0;
            if (head instanceof Atom) { f = ((Atom) head).getName(); n = 0; }
            else if (head instanceof CompoundTerm) {
                f = ((CompoundTerm) head).getName();
                n = ((CompoundTerm) head).getArguments().size();
            }
            if (f != null && isModuleProperty(prop)) {
                return moduleProperty(m, f, n, prop) ? Outcome.SUCCESS : Outcome.FAILURE;
            }
            // START_CHANGE: ISS-2025-0610 - P4.16: a bound head is answered here, from the three
            // stores (registry/native table, the KnowledgeBase with its dynamic flag, the module
            // owner). A declared-but-empty dynamic predicate is `dynamic` and `defined`; consulted
            // clauses are `static`; number_of_clauses(N) is reported; a predicate nothing defines
            // FAILS (SWI) instead of answering `undefined` — which also stops a module-exported
            // predicate from being `exported` and `undefined` at once.
            if (f != null) {
                final List<Term> props = propertiesOf(m, f, n);
                if (props == null) return Outcome.FAILURE;
                if (!(prop instanceof Variable)) {
                    for (int i = 0; i < props.size(); i++) {
                        if (m.unifyOrUndo(prop, props.get(i))) return Outcome.SUCCESS;
                    }
                    return Outcome.FAILURE;
                }
                final int[] i = {0};
                Generator gen = new Generator() {
                    @Override public boolean next(Machine mm) {
                        while (i[0] < props.size()) {
                            Term p = props.get(i[0]++);
                            if (i[0] >= props.size()) mm.lastSolution();
                            if (mm.unifyOrUndo(prop, p)) return true;
                        }
                        return false;
                    }
                };
                return m.pushGenerator(gen) ? Outcome.SUSPENDED : Outcome.FAILURE;
            }
            // END_CHANGE: ISS-2025-0610
            int r = LegacyBuiltinAdapter.run(m, rebuild(args), "predicate_property", 2);
            return (r == 1) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }

        private static Term rebuild(Term[] args) {
            return new CompoundTerm(new Atom("predicate_property"), Arrays.asList(args[0], args[1]));
        }

        /** The module properties first (as their own choice point), then everything the registry
         *  knows — pushed as a residual goal so the two enumerations concatenate. */
        private static Outcome enumerate(final Machine m, final Term[] args, final Term prop,
                                         final List<Term> extra) {
            final int[] i = {0};
            final Term goal = rebuild(args);
            Generator gen = new Generator() {
                @Override public boolean next(Machine mm) {
                    while (i[0] < extra.size()) {
                        Term p = extra.get(i[0]++);
                        if (mm.unifyOrUndo(prop, p)) return true;
                    }
                    if (i[0] == extra.size()) {                 // then the registry's properties
                        i[0]++;
                        mm.lastSolution();
                        return LegacyBuiltinAdapter.run(mm, goal, "predicate_property", 2) == 1;
                    }
                    return false;
                }
            };
            return m.pushGenerator(gen) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }

        // START_CHANGE: ISS-2025-0610
        /** The properties of f/n, or null when nothing defines it. */
        private static List<Term> propertiesOf(Machine m, String f, int n) {
            List<Term> props = new ArrayList<Term>();
            addModuleProperties(m, f, n, props);
            boolean inModule = !props.isEmpty();
            String key = f + "/" + n;
            Engine e = m.engine();
            boolean builtin = (e.registry() != null && e.registry().isBuiltIn(f, n)) || e.natives().isNativeKey(key);
            it.denzosoft.jprolog.core.engine.KnowledgeBase kb = e.kb();
            List<it.denzosoft.jprolog.core.engine.Rule> rules = (kb == null) ? null : kb.getRulesForPredicate(f, n);
            int clauses = (rules == null) ? 0 : rules.size();
            boolean dyn = kb != null && kb.isDynamic(f, n);
            boolean user = clauses > 0 || dyn;
            boolean lib = e.modules4().isLibraryIndicatorKey(key);
            if (!builtin && !user && !lib && !inModule) return null;
            if (builtin && !user) {
                props.add(new Atom("built_in"));
                props.add(new Atom("system"));
            }
            props.add(new Atom("defined"));
            props.add(new Atom("visible"));
            if (user) {
                props.add(new Atom(dyn ? "dynamic" : "static"));
                props.add(new CompoundTerm(new Atom("number_of_clauses"),
                    Arrays.<Term>asList(Number.valueOf((long) clauses))));
            } else {
                props.add(new Atom("static"));
            }
            // ISS-2025-0572: predicate_property(P, tabled)
            if (e.tables() != null && e.tables().isTabled(f, n)) props.add(new Atom("tabled"));
            return props;
        }
        // END_CHANGE: ISS-2025-0610

        private static boolean isModuleProperty(Term p) {
            if (p instanceof Atom) return "exported".equals(((Atom) p).getName());
            if (p instanceof CompoundTerm) {
                CompoundTerm c = (CompoundTerm) p;
                if (c.getArguments().size() != 1) return false;
                String n = c.getName();
                return "imported_from".equals(n) || "defined_in".equals(n) || "module".equals(n);
            }
            return false;
        }

        private static void addModuleProperties(Machine m, String f, int n, List<Term> out) {
            Modules ms = m.engine().modules4();
            String ctx = m.contextModule();
            String def = ms.definingModule(ctx, f, n);
            if (def == null) return;
            out.add(new CompoundTerm(new Atom("defined_in"), Arrays.<Term>asList(new Atom(def))));
            if (ms.exports(def, f, n)) out.add(new Atom("exported"));
            if (!def.equals(ctx)) {
                out.add(new CompoundTerm(new Atom("imported_from"), Arrays.<Term>asList(new Atom(def))));
            }
        }

        private static boolean moduleProperty(Machine m, String f, int n, Term prop) {
            List<Term> props = new ArrayList<Term>();
            addModuleProperties(m, f, n, props);
            for (int i = 0; i < props.size(); i++) {
                if (m.unifyOrUndo(prop, props.get(i))) return true;
            }
            return false;
        }
    }
}
// END_CHANGE: ISS-2025-0470
