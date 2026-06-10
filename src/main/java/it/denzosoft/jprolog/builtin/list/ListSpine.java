// START_CHANGE: ISS-2025-0380 - Tail-aware list spine walker for partial-list handling
package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;

/**
 * Helper for walking the '.'/2 spine of a (binding-resolved) list term.
 * Unlike {@code ListUtils.extractElements}, callers also learn what the
 * final spine tail is, so partial lists ([a|T] with T unbound) and improper
 * lists ([a|b]) can be handled soundly instead of being silently truncated.
 */
final class ListSpine {

    private ListSpine() {}

    /**
     * Walk the '.'/2 spine of {@code list}, appending the prefix elements to
     * {@code prefixOut} (ignored when null), and return the final spine tail:
     * the atom [] for proper lists, an unbound Variable for partial lists,
     * or any other term for improper lists / non-lists.
     *
     * @param list      The (already binding-resolved) term to walk
     * @param prefixOut Receives the prefix elements in order (may be null)
     * @return The final spine tail term
     */
    static Term tail(Term list, List<Term> prefixOut) {
        Term current = list;
        while (current instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) current;
            if (".".equals(c.getName()) && c.getArguments().size() == 2) {
                if (prefixOut != null) {
                    prefixOut.add(c.getArguments().get(0));
                }
                current = c.getArguments().get(1);
            } else {
                break;
            }
        }
        return current;
    }
}
// END_CHANGE: ISS-2025-0380
