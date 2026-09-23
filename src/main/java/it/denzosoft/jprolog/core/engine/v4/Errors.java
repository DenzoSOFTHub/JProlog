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

    // START_CHANGE: ISS-2025-0504 - 4.3 wave D: the three formals the natives still had to build
    // by hand (or, worse, raise as a PrologEvaluationException carrying a message atom), plus the
    // one culprit shape ISO asks for over and over: a predicate indicator Name/Arity.
    /** {@code error(evaluation_error(What), Context)} — ISO 7.12.2 h. */
    public static PrologException evaluation(String what, String context) {
        return new PrologException(ISOErrorTerms.evaluationError(what, context));
    }

    // START_CHANGE: ISS-2025-0569
    /** {@code error(uninstantiation_error(Culprit), Context)} — ISO Cor.2 7.12.2 k. */
    public static PrologException uninstantiation(Term culprit, String context) {
        return new PrologException(new it.denzosoft.jprolog.core.terms.CompoundTerm(
            new it.denzosoft.jprolog.core.terms.Atom("error"), java.util.Arrays.<Term>asList(
                new it.denzosoft.jprolog.core.terms.CompoundTerm(
                    new it.denzosoft.jprolog.core.terms.Atom("uninstantiation_error"),
                    java.util.Arrays.<Term>asList(culprit)),
                new it.denzosoft.jprolog.core.terms.Atom(context))));
    }
    // END_CHANGE: ISS-2025-0569

    /** {@code error(syntax_error(What), Context)} — ISO 7.12.2 j. */
    public static PrologException syntax(String what, String context) {
        return new PrologException(ISOErrorTerms.syntaxError(what, context));
    }

    /**
     * The ISO culprit for a procedure: {@code Name/Arity}. Built here rather than in five natives
     * so that {@code existence_error(procedure, foo/1)} and
     * {@code permission_error(modify, static_procedure, foo/1)} can never disagree on the shape.
     */
    public static Term pi(String name, int arity) {
        return new it.denzosoft.jprolog.core.terms.CompoundTerm(
            new it.denzosoft.jprolog.core.terms.Atom("/"),
            java.util.Arrays.<Term>asList(new it.denzosoft.jprolog.core.terms.Atom(name),
                                          it.denzosoft.jprolog.core.terms.Number.valueOf(arity)));
    }
    // END_CHANGE: ISS-2025-0504
}
// END_CHANGE: ISS-2025-0442
