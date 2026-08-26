// START_CHANGE: ISS-2025-0242 - operator-aware term formatter for write/1, writeq/1, format ~w/~q
package it.denzosoft.jprolog.core.util;

import it.denzosoft.jprolog.core.operator.OperatorTable;
import it.denzosoft.jprolog.core.terms.Term;


/**
 * Operator-aware term-to-string formatter.
 *
 * format(term, quoted, ignoreOps, numbervars, maxContextPrecedence):
 *  - quoted: atoms quoted as needed (for re-readability)
 *  - ignoreOps: never use operator syntax
 *  - numbervars: render '$VAR'(N) as A, B, ..., Z, A1, ...
 *  - maxContextPrecedence: outer precedence (1200 by default)
 */
public final class TermFormatter {

    private TermFormatter() {}

    public static String format(Term t, boolean quoted, boolean ignoreOps, boolean numbervars, int contextPrec) {
        // ISS-2025-0475: null means "the operator store of the engine current on this thread"
        // (was the process-wide OperatorTable.getDefault(), i.e. whichever engine booted first).
        return format(t, quoted, ignoreOps, numbervars, contextPrec, null);
    }

    // START_CHANGE: ISS-2025-0475 - engine v4 wave W7 (design B.12): the rendering itself now lives
    // in core.engine.v4.Writer, which is fully iterative (a 1 M-element list, a 1 M-deep operator
    // chain), cycle-safe (a rational tree terminates instead of looping) and implements the complete
    // write_term/2,3 option set. This class stays as the entry point the ~20 existing call sites use
    // (write/1,2, writeln, writeq, print, format ~w/~q/~p, term_to_atom, the two engines' trace
    // output), so both engines get the new writer with no change of their own. The operator table
    // now defaults to the ENGINE's store rather than the process-wide OperatorTable.getDefault().
    public static String format(Term t, boolean quoted, boolean ignoreOps, boolean numbervars, int contextPrec, OperatorTable opTable) {
        it.denzosoft.jprolog.core.engine.v4.Writer.Options o =
            new it.denzosoft.jprolog.core.engine.v4.Writer.Options();
        o.quoted = quoted;
        o.ignoreOps = ignoreOps;
        o.numbervars = numbervars;
        o.ops = opTable;
        return it.denzosoft.jprolog.core.engine.v4.Writer.format(t, o, contextPrec);
    }
    // END_CHANGE: ISS-2025-0475

    /** Atom needs quoting if not simple lowercase identifier and not a recognized symbolic operator atom. */
    public static boolean needsQuoting(String name) {
        return it.denzosoft.jprolog.core.engine.v4.Writer.needsQuoting(name);
    }

    /** Quote an atom, escaping what ISO 6.4.2 requires. */
    public static String quoteAtom(String name) {
        return it.denzosoft.jprolog.core.engine.v4.Writer.quoteAtom(name);
    }

}
// END_CHANGE: ISS-2025-0242
