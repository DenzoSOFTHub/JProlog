package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.io.PrintStream;
import java.util.Map;

// START_CHANGE: ISS-2025-0373 - shared stream-argument resolution for the I/O built-ins
/**
 * Helpers shared by the stream-argument forms of the I/O built-ins
 * (write/2, writeln/2, nl/1, put_char/2, tab/2, write_term/3, print/2, format/3).
 */
public final class IOStreamUtils {

    private IOStreamUtils() {
    }

    /**
     * Extract the stream alias from a stream argument: either an atom alias
     * (e.g. {@code stream_1001}, {@code user_output}) or a {@code stream(Alias)} wrapper.
     *
     * @param streamTerm the resolved stream argument
     * @return the alias, or null if the term has no recognizable alias
     */
    public static String streamAlias(Term streamTerm) {
        if (streamTerm instanceof Atom) {
            return ((Atom) streamTerm).getName();
        }
        if (streamTerm instanceof CompoundTerm && "stream".equals(streamTerm.getName())
                && streamTerm.getArguments() != null && streamTerm.getArguments().size() == 1) {
            Term inner = streamTerm.getArguments().get(0);
            if (inner instanceof Atom) {
                return ((Atom) inner).getName();
            }
        }
        return null;
    }

    /**
     * Resolve a stream argument to a writable {@link PrintStream}, raising the ISO errors
     * mandated for stream arguments (8.14.2.3): instantiation_error for an unbound argument,
     * domain_error(stream_or_alias, S) for a non-stream term, existence_error(stream, S)
     * for an alias that names no open output stream.
     *
     * @param streamArg the raw stream argument (will be resolved against bindings)
     * @param bindings  current variable bindings
     * @param context   predicate indicator for the error context (e.g. "write/2")
     * @return the resolved PrintStream (never null)
     */
    public static PrintStream resolveOutputStream(Term streamArg, Map<String, Term> bindings, String context) {
        Term streamTerm = streamArg.resolveBindings(bindings);
        if (streamTerm instanceof Variable) {
            throw new PrologException(ISOErrorTerms.instantiationError(context));
        }
        String alias = streamAlias(streamTerm);
        if (alias == null) {
            throw new PrologException(ISOErrorTerms.domainError("stream_or_alias", streamTerm, context));
        }
        PrintStream out = StreamManager.resolveOutput(alias);
        if (out == null) {
            throw new PrologException(ISOErrorTerms.existenceError("stream", streamTerm, context));
        }
        return out;
    }
}
// END_CHANGE: ISS-2025-0373
