package it.denzosoft.jprolog.core.engine;

import java.util.Collections;
import java.util.List;
import java.util.HashSet;
import java.util.Set;

// START_CHANGE: ISS-2025-0092 - Tabling (memoization) support
// START_CHANGE: ISS-2025-0491 - 4.1 wave A: the ANSWER tables, the in-progress set, the partial
// cache, the goal normaliser and the one-thread evaluation claim all belonged to the v2 engine's
// variant-tabling driver and are DELETED with it. What survives is
// the one thing the v4 engine reads from here: the set of `:- table f/n` DECLARATIONS, written by
// the consult-time directive (Prolog.processTableDirective) and by the table/1, abolish_table/1
// and abolish_all_tables/0 built-ins. The answer tables of the live engine are
// {@code core.engine.v4.Tabling} (linear tabling with completion, its own claim protocol).
/**
 * The {@code :- table f/n} declarations of one {@code Prolog}.
 *
 * <p>Not a cache: {@code core.engine.v4.Tabling} owns the answer tries. This is the small,
 * engine-neutral registry the consult directive writes and {@code Machine.isTabled} reads.
 */
public class TableStore {

    /** Set of tabled predicate indicators: "fib/2", "path/2", etc. */
    private final Set<String> tabledPredicates = java.util.concurrent.ConcurrentHashMap.newKeySet();   // ISS-2025-0752: read by every thread

    // START_CHANGE: ISS-2025-0542 - wave P2.3: isTabled runs on every user-predicate call;
    // a program that tables nothing (the common case) answers without building "name/arity", and
    // modCount lets the machine's call-site cache notice a new declaration.
    private volatile int modCount;

    /** Bumped by every declaration change. */
    public int modCount() { return modCount; }

    public void declareTable(String functor, int arity) {
        tabledPredicates.add(functor + "/" + arity);
        modCount++;
    }

    public boolean isTabled(String functor, int arity) {
        if (tabledPredicates.isEmpty()) return false;
        return tabledPredicates.contains(functor + "/" + arity);
    }
    // END_CHANGE: ISS-2025-0542

    /** Drop every answer table. Declarations survive (v4 semantics, and XSB's). */
    public void abolishAllTables() {
        // no answer tables live here any more (ISS-2025-0491); v4's Tabling.abolishAll does the work
    }

    // START_CHANGE: ISS-2025-0124 - abolish_table/1 support
    /**
     * Remove a predicate from the tabled set (its answer tables are dropped by
     * {@code core.engine.v4.Tabling}).
     */
    public void abolishTable(String functor, int arity) {
        tabledPredicates.remove(functor + "/" + arity);
        sharedPredicates.remove(functor + "/" + arity);                  // ISS-2025-0753
        modes.remove(functor + "/" + arity);                           // ISS-2025-0572
        modCount++;                                                    // ISS-2025-0542
    }
    // END_CHANGE: ISS-2025-0124

    // START_CHANGE: ISS-2025-0572 - 4.5 wave P3.3: every form of the table directive, and
    // mode-directed tabling (answer subsumption on the moded arguments).
    // START_CHANGE: ISS-2025-0754 - 4.6 wave Q4.4: every SWI mode, lattice(PI) and po(PI) included.
    /** One argument's table mode. */
    public static final class ModeSpec {
        /** index, first, last, min, max, sum, lattice or po. */
        public final String kind;
        /** The lattice/po predicate's name, and its module (null == user). */
        public final String pred;
        public final String module;

        ModeSpec(String kind, String pred, String module) {
            this.kind = kind; this.pred = pred; this.module = module;
        }

        public boolean isIndex() { return "index".equals(kind); }
    }

    private static final ModeSpec INDEX = new ModeSpec("index", null, null);
    // END_CHANGE: ISS-2025-0754

    /** Per-argument modes of a mode-directed table. */
    private final java.util.Map<String, ModeSpec[]> modes = new java.util.concurrent.ConcurrentHashMap<>();

    /** Is any table mode-directed? (the per-call fast path) */
    public boolean hasModes() { return !modes.isEmpty(); }

    public void declareTable(String functor, int arity, ModeSpec[] argModes) {
        String key = functor + "/" + arity;
        tabledPredicates.add(key);
        if (argModes == null) modes.remove(key); else modes.put(key, argModes);
        modCount++;
    }

    /** The argument modes of {@code indicator} ("name/arity"), or null for variant tabling. */
    public ModeSpec[] getModes(String indicator) {
        return modes.isEmpty() ? null : modes.get(indicator);
    }

    /**
     * Declare every table of a {@code table/1} specification: {@code Name/Arity},
     * {@code Name//Arity}, a {@code ','}-sequence or a list of them, {@code Spec as Options}
     * (the options are accepted and ignored: tables are variant tables), or a mode-directed head
     * such as {@code path(_,_,min)}. A mode JProlog does not implement raises
     * {@code domain_error(table_mode, M)}, never a silent no-op.
     */
    public List<String> declareSpec(it.denzosoft.jprolog.core.terms.Term spec, String ctx) {
        List<String> warnings = new java.util.ArrayList<String>();
        declareSpec(spec, ctx, null, warnings);
        return warnings;
    }

    // START_CHANGE: ISS-2025-0753 - 4.6 wave Q4 (extra): `Spec as Options`.
    /** Predicates declared {@code as shared} (ISS-2025-0752): their complete tables are published. */
    private final Set<String> sharedPredicates = java.util.concurrent.ConcurrentHashMap.newKeySet();

    /** Is {@code indicator} ("name/arity") declared {@code as shared}? */
    public boolean isShared(String indicator) {
        return !sharedPredicates.isEmpty() && sharedPredicates.contains(indicator);
    }

    /** The options JProlog accepts but does not implement; each one is reported as a warning. */
    private static final java.util.Set<String> IGNORED_OPTIONS = new java.util.HashSet<String>(java.util.Arrays.asList(
        "subsumptive", "incremental", "opaque", "monotonic", "lazy", "dynamic", "tnot"));

    /**
     * Read the options of {@code Spec as Options} (one option, a {@code ,}-sequence or a list):
     * {@code variant} (the default), {@code shared} and {@code private} are implemented;
     * {@code subsumptive}, {@code incremental}, {@code opaque}, {@code monotonic}, {@code lazy},
     * {@code dynamic}, {@code max_answers/1}, {@code subgoal_abstract/1} and
     * {@code answer_abstract/1} are accepted with a warning (variant tabling is used, which gives
     * the same answers; what is lost is reuse or the incremental re-evaluation); anything else is
     * {@code domain_error(table_option, O)}. Returns TRUE for shared, FALSE for private, null for
     * neither.
     */
    private static Boolean tableOptions(it.denzosoft.jprolog.core.terms.Term opts, String ctx, List<String> warnings) {
        it.denzosoft.jprolog.core.terms.Term o = it.denzosoft.jprolog.core.engine.v4.Unify.deref(opts);
        if (o instanceof it.denzosoft.jprolog.core.terms.Variable) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError(ctx));
        }
        if (o instanceof it.denzosoft.jprolog.core.terms.CompoundTerm
                && ((it.denzosoft.jprolog.core.terms.CompoundTerm) o).getArguments().size() == 2
                && (",".equals(o.getName()) || ".".equals(o.getName()))) {
            it.denzosoft.jprolog.core.terms.CompoundTerm c = (it.denzosoft.jprolog.core.terms.CompoundTerm) o;
            Boolean a = tableOptions(c.getArguments().get(0), ctx, warnings);
            it.denzosoft.jprolog.core.terms.Term rest = it.denzosoft.jprolog.core.engine.v4.Unify.deref(c.getArguments().get(1));
            Boolean b = (rest instanceof it.denzosoft.jprolog.core.terms.Atom && "[]".equals(rest.getName()))
                ? null : tableOptions(rest, ctx, warnings);
            return (b != null) ? b : a;
        }
        if (o instanceof it.denzosoft.jprolog.core.terms.Atom) {
            String n = o.getName();
            if ("shared".equals(n)) return Boolean.TRUE;
            if ("private".equals(n)) return Boolean.FALSE;
            if ("variant".equals(n) || "[]".equals(n)) return null;
            if (IGNORED_OPTIONS.contains(n)) {
                warnings.add("table option " + n + " is not implemented and was ignored (variant tabling is used)");
                return null;
            }
        } else if (o instanceof it.denzosoft.jprolog.core.terms.CompoundTerm
                && ((it.denzosoft.jprolog.core.terms.CompoundTerm) o).getArguments().size() == 1) {
            String n = o.getName();
            if ("max_answers".equals(n) || "subgoal_abstract".equals(n) || "answer_abstract".equals(n)) {
                warnings.add("table option " + n + "/1 is not implemented and was ignored");
                return null;
            }
        }
        throw new it.denzosoft.jprolog.core.exceptions.PrologException(
            it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.domainError("table_option", o, ctx));
    }

    private void applyShare(String functor, int arity, Boolean share) {
        if (share == null) return;
        if (share.booleanValue()) sharedPredicates.add(functor + "/" + arity);
        else sharedPredicates.remove(functor + "/" + arity);
    }
    // END_CHANGE: ISS-2025-0753

    private void declareSpec(it.denzosoft.jprolog.core.terms.Term spec, String ctx, Boolean share, List<String> warnings) {
        it.denzosoft.jprolog.core.terms.Term s = it.denzosoft.jprolog.core.engine.v4.Unify.deref(spec);
        if (s instanceof it.denzosoft.jprolog.core.terms.Variable) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError(ctx));
        }
        if (s instanceof it.denzosoft.jprolog.core.terms.CompoundTerm) {
            it.denzosoft.jprolog.core.terms.CompoundTerm c = (it.denzosoft.jprolog.core.terms.CompoundTerm) s;
            String n = c.getName();
            int ar = c.getArguments().size();
            if ((",".equals(n) || ".".equals(n)) && ar == 2) {
                declareSpec(c.getArguments().get(0), ctx, share, warnings);
                it.denzosoft.jprolog.core.terms.Term rest = it.denzosoft.jprolog.core.engine.v4.Unify.deref(c.getArguments().get(1));
                if (!(rest instanceof it.denzosoft.jprolog.core.terms.Atom && "[]".equals(rest.getName()))) {
                    declareSpec(rest, ctx, share, warnings);
                }
                return;
            }
            if ("as".equals(n) && ar == 2) {                                   // ISS-2025-0753
                Boolean sh = tableOptions(c.getArguments().get(1), ctx, warnings);
                declareSpec(c.getArguments().get(0), ctx, sh != null ? sh : share, warnings);
                return;
            }
            if (("/".equals(n) || "//".equals(n)) && ar == 2) {
                it.denzosoft.jprolog.core.terms.Term f = it.denzosoft.jprolog.core.engine.v4.Unify.deref(c.getArguments().get(0));
                it.denzosoft.jprolog.core.terms.Term a = it.denzosoft.jprolog.core.engine.v4.Unify.deref(c.getArguments().get(1));
                if (f instanceof it.denzosoft.jprolog.core.terms.Atom && a instanceof it.denzosoft.jprolog.core.terms.Number
                        && ((it.denzosoft.jprolog.core.terms.Number) a).isInteger()) {
                    int arity = (int) ((it.denzosoft.jprolog.core.terms.Number) a).longValue() + ("//".equals(n) ? 2 : 0);
                    declareTable(f.getName(), arity, null);
                    applyShare(f.getName(), arity, share);                        // ISS-2025-0753
                    return;
                }
                throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                    it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("predicate_indicator", s, ctx));
            }
            // a mode-directed head: p(_, _, min)
            ModeSpec[] m = new ModeSpec[ar];
            boolean moded = false;
            for (int i = 0; i < ar; i++) {
                it.denzosoft.jprolog.core.terms.Term a = it.denzosoft.jprolog.core.engine.v4.Unify.deref(c.getArguments().get(i));
                ModeSpec mode = tableMode(a, ctx);
                m[i] = mode;
                if (!mode.isIndex()) moded = true;
            }
            declareTable(n, ar, moded ? m : null);
            applyShare(n, ar, share);                                          // ISS-2025-0753
            return;
        }
        if (s instanceof it.denzosoft.jprolog.core.terms.Atom && !"[]".equals(s.getName())) {
            declareTable(s.getName(), 0, null);
            applyShare(s.getName(), 0, share);                                 // ISS-2025-0753
            return;
        }
        if (s instanceof it.denzosoft.jprolog.core.terms.Atom) return;      // []
        throw new it.denzosoft.jprolog.core.exceptions.PrologException(
            it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("callable", s, ctx));
    }

    // START_CHANGE: ISS-2025-0754 - SWI's modes (boot/tabling.pl): index/+/Var, first/-, last,
    // min, max (standard order of terms), sum, lattice(PI) with PI of arity 3 and po(PI) of arity
    // 2, as Name/Arity, Name, Module:Name/Arity, Module:Name or (lattice) a compound head.
    // Anything else: domain_error(tabled_mode, M) — SWI's name (4.5 used table_mode).
    private static ModeSpec tableMode(it.denzosoft.jprolog.core.terms.Term a, String ctx) {
        if (a instanceof it.denzosoft.jprolog.core.terms.Variable) return INDEX;
        if (a instanceof it.denzosoft.jprolog.core.terms.Atom) {
            switch (a.getName()) {
                case "index": case "+": return INDEX;
                case "-": case "first": return new ModeSpec("first", null, null);
                case "last": return new ModeSpec("last", null, null);
                case "min": return new ModeSpec("min", null, null);
                case "max": return new ModeSpec("max", null, null);
                case "sum": return new ModeSpec("sum", null, null);
                default: break;
            }
        }
        if (a instanceof it.denzosoft.jprolog.core.terms.CompoundTerm
                && ((it.denzosoft.jprolog.core.terms.CompoundTerm) a).getArguments().size() == 1
                && ("lattice".equals(a.getName()) || "po".equals(a.getName()))) {
            String kind = a.getName();
            int want = "lattice".equals(kind) ? 3 : 2;
            it.denzosoft.jprolog.core.terms.Term pi = it.denzosoft.jprolog.core.engine.v4.Unify.deref(
                ((it.denzosoft.jprolog.core.terms.CompoundTerm) a).getArguments().get(0));
            String module = null;
            if (pi instanceof it.denzosoft.jprolog.core.terms.CompoundTerm && ":".equals(pi.getName())
                    && ((it.denzosoft.jprolog.core.terms.CompoundTerm) pi).getArguments().size() == 2) {
                it.denzosoft.jprolog.core.terms.Term mt = it.denzosoft.jprolog.core.engine.v4.Unify.deref(
                    ((it.denzosoft.jprolog.core.terms.CompoundTerm) pi).getArguments().get(0));
                if (!(mt instanceof it.denzosoft.jprolog.core.terms.Atom)) throw modeError(a, ctx);
                module = mt.getName();
                pi = it.denzosoft.jprolog.core.engine.v4.Unify.deref(
                    ((it.denzosoft.jprolog.core.terms.CompoundTerm) pi).getArguments().get(1));
            }
            String name;
            int arity;
            if (pi instanceof it.denzosoft.jprolog.core.terms.Atom) {
                name = pi.getName();
                arity = want;
            } else if (pi instanceof it.denzosoft.jprolog.core.terms.CompoundTerm && "/".equals(pi.getName())
                    && ((it.denzosoft.jprolog.core.terms.CompoundTerm) pi).getArguments().size() == 2) {
                it.denzosoft.jprolog.core.terms.Term n = it.denzosoft.jprolog.core.engine.v4.Unify.deref(
                    ((it.denzosoft.jprolog.core.terms.CompoundTerm) pi).getArguments().get(0));
                it.denzosoft.jprolog.core.terms.Term ar = it.denzosoft.jprolog.core.engine.v4.Unify.deref(
                    ((it.denzosoft.jprolog.core.terms.CompoundTerm) pi).getArguments().get(1));
                if (!(n instanceof it.denzosoft.jprolog.core.terms.Atom)
                        || !(ar instanceof it.denzosoft.jprolog.core.terms.Number)
                        || !((it.denzosoft.jprolog.core.terms.Number) ar).isInteger()) {
                    throw modeError(a, ctx);
                }
                name = n.getName();
                arity = (int) ((it.denzosoft.jprolog.core.terms.Number) ar).longValue();
            } else if ("lattice".equals(kind) && pi instanceof it.denzosoft.jprolog.core.terms.CompoundTerm) {
                name = pi.getName();
                arity = ((it.denzosoft.jprolog.core.terms.CompoundTerm) pi).getArguments().size();
            } else {
                throw modeError(a, ctx);
            }
            if (arity != want) {
                throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                    it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.domainError(
                        kind + "_arity", it.denzosoft.jprolog.core.terms.Number.valueOf(arity), ctx));
            }
            return new ModeSpec(kind, name, module);
        }
        throw modeError(a, ctx);
    }

    private static it.denzosoft.jprolog.core.exceptions.PrologException modeError(
            it.denzosoft.jprolog.core.terms.Term a, String ctx) {
        return new it.denzosoft.jprolog.core.exceptions.PrologException(
            it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.domainError("tabled_mode", a, ctx));
    }
    // END_CHANGE: ISS-2025-0754
    // END_CHANGE: ISS-2025-0572

    public Set<String> getTabledPredicates() {
        return Collections.unmodifiableSet(tabledPredicates);
    }
}
// END_CHANGE: ISS-2025-0491
// END_CHANGE: ISS-2025-0092
