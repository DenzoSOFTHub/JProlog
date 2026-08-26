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

    public void declareTable(String functor, int arity) {
        tabledPredicates.add(functor + "/" + arity);
    }

    public boolean isTabled(String functor, int arity) {
        return tabledPredicates.contains(functor + "/" + arity);
    }

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
    }
    // END_CHANGE: ISS-2025-0124

    public Set<String> getTabledPredicates() {
        return Collections.unmodifiableSet(tabledPredicates);
    }
}
// END_CHANGE: ISS-2025-0491
// END_CHANGE: ISS-2025-0092
