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

    // START_CHANGE: ISS-2025-0681 - wave Q1.1: the error terms of the bridged EXTENDED LIBRARIES
    // (LIM-038). Until 4.5.0 those raised a bare message atom ('xml_parse: argument must be an
    // atom.'), which only a variable catcher could see. They now raise
    //     error(Formal, context(Name/Arity, Message))
    // as SWI-Prolog does: the formal is the ISO one (so catch(G, error(type_error(_, _), _), R)
    // matches), and the English sentence survives as the context's message. A HOST failure (a file,
    // a socket, a database, a process) is not an argument fault: host(...) maps it to the SWI
    // formals existence_error/2, permission_error/3, io_error/2 or system_error/1.

    /** {@code context(Name/Arity, Message)}; a null message leaves the second argument unbound. */
    public static Term context(String name, int arity, String message) {
        return new it.denzosoft.jprolog.core.terms.CompoundTerm(
            new it.denzosoft.jprolog.core.terms.Atom("context"), java.util.Arrays.<Term>asList(
                pi(name, arity),
                message == null ? new it.denzosoft.jprolog.core.terms.Variable("_")
                                : new it.denzosoft.jprolog.core.terms.Atom(message)));
    }

    /** {@code error(Formal, context(Name/Arity, Message))}. */
    public static PrologException error(Term formal, String name, int arity, String message) {
        return new PrologException(ISOErrorTerms.error(formal, context(name, arity, message)));
    }

    private static Term f(String functor, Term... args) {
        return new it.denzosoft.jprolog.core.terms.CompoundTerm(
            new it.denzosoft.jprolog.core.terms.Atom(functor), java.util.Arrays.asList(args));
    }

    private static Term a(String name) { return new it.denzosoft.jprolog.core.terms.Atom(name); }

    public static PrologException instantiation(String name, int arity, String message) {
        return error(a("instantiation_error"), name, arity, message);
    }

    public static PrologException type(String type, Term culprit, String name, int arity, String message) {
        return error(f("type_error", a(type), culprit), name, arity, message);
    }

    public static PrologException domain(String domain, Term culprit, String name, int arity, String message) {
        return error(f("domain_error", a(domain), culprit), name, arity, message);
    }

    public static PrologException existence(String kind, Term culprit, String name, int arity, String message) {
        return error(f("existence_error", a(kind), culprit), name, arity, message);
    }

    public static PrologException permission(String action, String type, Term culprit, String name,
                                             int arity, String message) {
        return error(f("permission_error", a(action), a(type), culprit), name, arity, message);
    }

    public static PrologException representation(String what, String name, int arity, String message) {
        return error(f("representation_error", a(what)), name, arity, message);
    }

    public static PrologException resource(String what, String name, int arity, String message) {
        return error(f("resource_error", a(what)), name, arity, message);
    }

    public static PrologException syntax(String what, String name, int arity, String message) {
        return error(f("syntax_error", a(what)), name, arity, message);
    }

    /** SWI's {@code io_error(Op, Culprit)}: a read/write/open on a host channel failed. */
    public static PrologException ioError(String op, Term culprit, String name, int arity, String message) {
        return error(f("io_error", a(op), culprit), name, arity, message);
    }

    /** {@code system_error(Message)}: the host failed in a way no finer formal describes. */
    public static PrologException systemError(String message, String name, int arity) {
        String m = message == null ? "unknown" : message;
        return error(f("system_error", a(m)), name, arity, m);
    }

    /**
     * Map a HOST failure (never an argument fault) to its SWI formal. A {@link PrologException}
     * passes through unchanged. Control exceptions must be rethrown by the caller first
     * ({@code ControlFlow.rethrowIfControl}).
     *
     * @param op      the operation, for io_error/permission_error (read, write, open, create...)
     * @param kind    the kind of object, for existence_error/permission_error (file, directory,
     *                host, url, class, connection...)
     * @param culprit the object the operation was applied to (a path, a URL, a handle), or null
     */
    public static PrologException host(Throwable e, String op, String kind, Term culprit,
                                       String name, int arity) {
        if (e instanceof PrologException) return (PrologException) e;
        if (e instanceof InterruptedException) {                // a Stop, never a Prolog error
            Thread.currentThread().interrupt();
            throw new it.denzosoft.jprolog.core.engine.QueryCancelledException();
        }
        Term c = culprit != null ? culprit : new it.denzosoft.jprolog.core.terms.Variable("_");
        String msg = e.getMessage() != null ? e.getMessage() : e.getClass().getSimpleName();
        if (e instanceof java.io.FileNotFoundException
                || e instanceof java.nio.file.NoSuchFileException
                || e instanceof java.nio.file.NotDirectoryException) {
            return existence(kind, c, name, arity, msg);
        }
        if (e instanceof java.nio.file.FileAlreadyExistsException) {
            return permission("create", kind, c, name, arity, msg);
        }
        if (e instanceof java.nio.file.DirectoryNotEmptyException) {
            return permission("delete", kind, c, name, arity, msg);
        }
        if (e instanceof java.nio.file.AccessDeniedException || e instanceof SecurityException) {
            return permission(op, kind, c, name, arity, msg);
        }
        if (e instanceof java.net.UnknownHostException) {
            return existence("host", c, name, arity, msg);
        }
        if (e instanceof ClassNotFoundException) {
            return existence("class", c, name, arity, msg);
        }
        if (e instanceof java.io.IOException || e instanceof java.io.UncheckedIOException) {
            return ioError(op, c, name, arity, msg);
        }
        return systemError(e.getClass().getSimpleName() + ": " + msg, name, arity);
    }
    // END_CHANGE: ISS-2025-0681
}
// END_CHANGE: ISS-2025-0442
