package it.denzosoft.jprolog.builtin.clpfd.v2;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Engine-facing built-ins backed by the clean-room {@link ClpStore} (via {@link ClpfdV2Bridge}).
 * Register them over the legacy CLP(FD) names with {@link #register(Prolog)} to drive the engine's
 * {@code in/2}, {@code #=}/{@code #<}/…, {@code all_different/1} and {@code label/1} through the v2
 * solver. The per-query store is reset by {@code Prolog.solve} so nothing leaks between queries.
 */
public final class ClpfdV2Builtins {

    private ClpfdV2Builtins() {}

    /** Register the v2-backed CLP(FD) built-ins (overriding the legacy ones). */
    public static void register(Prolog prolog) {
        prolog.registerBuiltInPredicate("in", new In());
        prolog.registerBuiltInPredicate("#=", new Cmp(Constraint.Rel.EQ));
        prolog.registerBuiltInPredicate("#\\=", new Cmp(Constraint.Rel.NE));
        prolog.registerBuiltInPredicate("#<", new Cmp(Constraint.Rel.LT));
        prolog.registerBuiltInPredicate("#>", new Cmp(Constraint.Rel.GT));
        prolog.registerBuiltInPredicate("#=<", new Cmp(Constraint.Rel.LE));
        prolog.registerBuiltInPredicate("#>=", new Cmp(Constraint.Rel.GE));
        prolog.registerBuiltInPredicate("all_different", new AllDifferent());
        prolog.registerBuiltInPredicate("all_distinct", new AllDifferent());
        prolog.registerBuiltInPredicate("label", new Label());
        prolog.registerBuiltInPredicate("labeling", new Label());
        prolog.registerBuiltInPredicate("indomain", new Indomain());
        prolog.registerBuiltInPredicate("fd_dom", new FdDom());
        prolog.registerBuiltInPredicate("fd_size", new FdSize());
    }

    // ---- in(Var, Lo..Hi) ----
    public static final class In implements BuiltIn {
        @Override public boolean execute(Term q, Map<String, Term> b, List<Map<String, Term>> sols) {
            List<Term> a = q.getArguments();
            if (a.size() != 2) return false;
            Term dom = a.get(1).resolveBindings(b);
            if (!(dom instanceof CompoundTerm) || !"..".equals(((CompoundTerm) dom).getName())) return false;
            CompoundTerm range = (CompoundTerm) dom;
            Term loT = range.getArguments().get(0).resolveBindings(b);
            Term hiT = range.getArguments().get(1).resolveBindings(b);
            if (!(loT instanceof Number) || !(hiT instanceof Number)) return false;
            if (ClpfdV2Bridge.postIn(a.get(0), ((Number) loT).longValue(), ((Number) hiT).longValue(), b)) {
                // START_CHANGE: ISS-2025-0357 - propagation that fixed a domain binds the variable
                Map<String, Term> nb = new HashMap<>(b);
                ClpfdV2Bridge.exportSingletons(nb);
                sols.add(nb);
                // END_CHANGE: ISS-2025-0357
                return true;
            }
            return false;
        }
    }

    // ---- X #rel Y ----
    public static final class Cmp implements BuiltIn {
        private final Constraint.Rel rel;
        public Cmp(Constraint.Rel rel) { this.rel = rel; }
        @Override public boolean execute(Term q, Map<String, Term> b, List<Map<String, Term>> sols) {
            List<Term> a = q.getArguments();
            if (a.size() != 2) return false;
            if (ClpfdV2Bridge.postCmp(a.get(0), rel, a.get(1), b)) {
                // START_CHANGE: ISS-2025-0357 - propagation that fixed a domain binds the variable
                Map<String, Term> nb = new HashMap<>(b);
                ClpfdV2Bridge.exportSingletons(nb);
                sols.add(nb);
                // END_CHANGE: ISS-2025-0357
                return true;
            }
            return false;
        }
    }

    // ---- all_different(List) ----
    public static final class AllDifferent implements BuiltIn {
        @Override public boolean execute(Term q, Map<String, Term> b, List<Map<String, Term>> sols) {
            List<Term> a = q.getArguments();
            if (a.size() != 1) return false;
            List<Term> elems = toList(a.get(0).resolveBindings(b));
            if (elems == null) return false;
            if (ClpfdV2Bridge.postAllDifferent(elems, b)) {
                // START_CHANGE: ISS-2025-0357 - propagation that fixed a domain binds the variable
                Map<String, Term> nb = new HashMap<>(b);
                ClpfdV2Bridge.exportSingletons(nb);
                sols.add(nb);
                // END_CHANGE: ISS-2025-0357
                return true;
            }
            return false;
        }
    }

    // ---- label(List) / labeling(Opts, List) ----
    public static final class Label implements BuiltIn {
        @Override public boolean execute(Term q, Map<String, Term> b, List<Map<String, Term>> sols) {
            List<Term> a = q.getArguments();
            Term listArg = (a.size() == 2) ? a.get(1) : (a.size() == 1 ? a.get(0) : null);
            if (listArg == null) return false;
            List<Term> vars = toList(listArg.resolveBindings(b));
            if (vars == null) return false;
            List<Map<String, Term>> labeled = ClpfdV2Bridge.label(vars, b);
            sols.addAll(labeled);
            return !labeled.isEmpty();
        }
    }

    // ---- indomain(Var) : label a single variable ----
    public static final class Indomain implements BuiltIn {
        @Override public boolean execute(Term q, Map<String, Term> b, List<Map<String, Term>> sols) {
            List<Term> a = q.getArguments();
            if (a.size() != 1) return false;
            List<Map<String, Term>> labeled = ClpfdV2Bridge.label(java.util.Collections.singletonList(a.get(0)), b);
            sols.addAll(labeled);
            return !labeled.isEmpty();
        }
    }

    // ---- fd_dom(Var, Dom) ----
    public static final class FdDom implements BuiltIn {
        @Override public boolean execute(Term q, Map<String, Term> b, List<Map<String, Term>> sols) {
            List<Term> a = q.getArguments();
            if (a.size() != 2) return false;
            Term dom = ClpfdV2Bridge.domainTerm(a.get(0), b);
            if (dom == null) return false;
            Map<String, Term> nb = new HashMap<>(b);
            if (a.get(1).unify(dom, nb)) { sols.add(nb); return true; }
            return false;
        }
    }

    // ---- fd_size(Var, N) ----
    public static final class FdSize implements BuiltIn {
        @Override public boolean execute(Term q, Map<String, Term> b, List<Map<String, Term>> sols) {
            List<Term> a = q.getArguments();
            if (a.size() != 2) return false;
            long n = ClpfdV2Bridge.domainSize(a.get(0), b);
            Map<String, Term> nb = new HashMap<>(b);
            if (a.get(1).unify(new Number(n), nb)) { sols.add(nb); return true; }
            return false;
        }
    }

    /** Walk a proper '.'/2 list into a Java list, or null if not a proper list. */
    private static List<Term> toList(Term t) {
        List<Term> out = new ArrayList<>();
        Term cur = t;
        while (cur instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) cur;
            if (!".".equals(c.getName()) || c.getArguments().size() != 2) return null;
            out.add(c.getArguments().get(0));
            cur = c.getArguments().get(1);
        }
        return (cur instanceof Atom && "[]".equals(((Atom) cur).getName())) ? out : null;
    }
}
