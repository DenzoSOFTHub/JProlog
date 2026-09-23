package it.denzosoft.jprolog.core.engine;

import java.util.Collections;
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
    private final Set<String> tabledPredicates = new HashSet<>();

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
        modes.remove(functor + "/" + arity);                           // ISS-2025-0572
        modCount++;                                                    // ISS-2025-0542
    }
    // END_CHANGE: ISS-2025-0124

    // START_CHANGE: ISS-2025-0572 - 4.5 wave P3.3: every form of the table directive, and
    // mode-directed tabling (answer subsumption on the moded arguments).
    /** Per-argument modes of a mode-directed table ("index", "first", "last", "min", "max"). */
    private final java.util.Map<String, String[]> modes = new java.util.concurrent.ConcurrentHashMap<>();

    public void declareTable(String functor, int arity, String[] argModes) {
        String key = functor + "/" + arity;
        tabledPredicates.add(key);
        if (argModes == null) modes.remove(key); else modes.put(key, argModes);
        modCount++;
    }

    /** The argument modes of {@code indicator} ("name/arity"), or null for variant tabling. */
    public String[] getModes(String indicator) {
        return modes.isEmpty() ? null : modes.get(indicator);
    }

    /**
     * Declare every table of a {@code table/1} specification: {@code Name/Arity},
     * {@code Name//Arity}, a {@code ','}-sequence or a list of them, {@code Spec as Options}
     * (the options are accepted and ignored: tables are variant tables), or a mode-directed head
     * such as {@code path(_,_,min)}. A mode JProlog does not implement raises
     * {@code domain_error(table_mode, M)}, never a silent no-op.
     */
    public void declareSpec(it.denzosoft.jprolog.core.terms.Term spec, String ctx) {
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
                declareSpec(c.getArguments().get(0), ctx);
                it.denzosoft.jprolog.core.terms.Term rest = it.denzosoft.jprolog.core.engine.v4.Unify.deref(c.getArguments().get(1));
                if (!(rest instanceof it.denzosoft.jprolog.core.terms.Atom && "[]".equals(rest.getName()))) {
                    declareSpec(rest, ctx);
                }
                return;
            }
            if ("as".equals(n) && ar == 2) { declareSpec(c.getArguments().get(0), ctx); return; }
            if (("/".equals(n) || "//".equals(n)) && ar == 2) {
                it.denzosoft.jprolog.core.terms.Term f = it.denzosoft.jprolog.core.engine.v4.Unify.deref(c.getArguments().get(0));
                it.denzosoft.jprolog.core.terms.Term a = it.denzosoft.jprolog.core.engine.v4.Unify.deref(c.getArguments().get(1));
                if (f instanceof it.denzosoft.jprolog.core.terms.Atom && a instanceof it.denzosoft.jprolog.core.terms.Number
                        && ((it.denzosoft.jprolog.core.terms.Number) a).isInteger()) {
                    int arity = (int) ((it.denzosoft.jprolog.core.terms.Number) a).longValue() + ("//".equals(n) ? 2 : 0);
                    declareTable(f.getName(), arity, null);
                    return;
                }
                throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                    it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("predicate_indicator", s, ctx));
            }
            // a mode-directed head: p(_, _, min)
            String[] m = new String[ar];
            boolean moded = false;
            for (int i = 0; i < ar; i++) {
                it.denzosoft.jprolog.core.terms.Term a = it.denzosoft.jprolog.core.engine.v4.Unify.deref(c.getArguments().get(i));
                String mode = tableMode(a, ctx);
                m[i] = mode;
                if (!"index".equals(mode)) moded = true;
            }
            declareTable(n, ar, moded ? m : null);
            return;
        }
        if (s instanceof it.denzosoft.jprolog.core.terms.Atom && !"[]".equals(s.getName())) {
            declareTable(s.getName(), 0, null);
            return;
        }
        if (s instanceof it.denzosoft.jprolog.core.terms.Atom) return;      // []
        throw new it.denzosoft.jprolog.core.exceptions.PrologException(
            it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("callable", s, ctx));
    }

    private static String tableMode(it.denzosoft.jprolog.core.terms.Term a, String ctx) {
        if (a instanceof it.denzosoft.jprolog.core.terms.Variable) return "index";
        if (a instanceof it.denzosoft.jprolog.core.terms.Atom) {
            switch (a.getName()) {
                case "index": case "+": return "index";
                case "-": case "first": return "first";
                case "last": return "last";
                case "min": return "min";
                case "max": return "max";
                default: break;
            }
        }
        // lattice(PI), po(PI) and anything else: not implemented -> an error, never a hang
        throw new it.denzosoft.jprolog.core.exceptions.PrologException(
            it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.domainError("table_mode", a, ctx));
    }
    // END_CHANGE: ISS-2025-0572

    public Set<String> getTabledPredicates() {
        return Collections.unmodifiableSet(tabledPredicates);
    }
}
// END_CHANGE: ISS-2025-0491
// END_CHANGE: ISS-2025-0092
