package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Term;

// START_CHANGE: ISS-2025-0442 - engine v4, design B.6 (ISO errors carrying the machine's context).
/**
 * ISO error construction for the v4 machine.
 *
 * <p>Every error the machine raises is stamped with the predicate indicator of the goal being
 * executed, so {@code catch(G, error(E, Context), true)} tells the user WHERE the error came from
 * rather than the generic tag the built-in happened to pass. The machine keeps the current
 * indicator in {@link Machine#currentContext()}.
 */
public final class Errors {

    private Errors() {}

    public static PrologException instantiation(String context) {
        return new PrologException(ISOErrorTerms.instantiationError(context));
    }

    public static PrologException type(String expected, Term culprit, String context) {
        return new PrologException(ISOErrorTerms.typeError(expected, culprit, context));
    }

    public static PrologException domain(String domain, Term culprit, String context) {
        return new PrologException(ISOErrorTerms.domainError(domain, culprit, context));
    }

    public static PrologException existence(String kind, Term culprit, String context) {
        return new PrologException(ISOErrorTerms.existenceError(kind, culprit, context));
    }

    public static PrologException permission(String op, String kind, Term culprit, String context) {
        return new PrologException(ISOErrorTerms.permissionError(op, kind, culprit, context));
    }

    public static PrologException representation(String flag, String context) {
        return new PrologException(ISOErrorTerms.representationError(flag, context));
    }

    public static PrologException resource(String what, String context) {
        return new PrologException(ISOErrorTerms.resourceError(what, context));
    }

    public static PrologException system(String what, String context) {
        return new PrologException(ISOErrorTerms.systemError(what, context));
    }
}
// END_CHANGE: ISS-2025-0442
