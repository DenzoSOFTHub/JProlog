package it.denzosoft.jprolog.builtin.string;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;

// START_CHANGE: ISS-2025-0405 - shared SWI-style text extraction for atom/string interop
/**
 * Helper for SWI-style text interop: atoms and strings are interchangeable "text" in the
 * text-typed argument positions of the atom_* and string_* predicate families.
 */
public final class TextTerm {

    private TextTerm() {
    }

    /**
     * Return the text of a term: the name of an {@link Atom} or the value of a
     * {@link PrologString}; {@code null} for any other term.
     */
    public static String textOf(Term t) {
        if (t instanceof Atom) {
            return ((Atom) t).getName();
        }
        if (t instanceof PrologString) {
            return ((PrologString) t).getStringValue();
        }
        return null;
    }
}
// END_CHANGE: ISS-2025-0405
