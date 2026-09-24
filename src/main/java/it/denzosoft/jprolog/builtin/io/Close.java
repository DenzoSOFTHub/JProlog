package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.PrologStream;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.List;
import java.util.Map;

/**
 * close/1 - close(+Stream)
 * close/2 - close(+Stream, +Options) — supports force(true|false)
 */
public class Close implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = query.getArguments().size();
        if (arity != 1 && arity != 2) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0697
        }

        Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);
        // START_CHANGE: ISS-2025-0377 - ISO error/2 terms (8.11.6)
        String ctx = "close/" + arity;
        if (streamTerm instanceof Variable) {
            throw new PrologException(ISOErrorTerms.instantiationError(ctx));
        }
        if (IOStreamUtils.streamAlias(streamTerm) == null && !isStreamTerm(streamTerm)) {
            throw new PrologException(ISOErrorTerms.domainError("stream_or_alias", streamTerm, ctx));
        }
        // END_CHANGE: ISS-2025-0377

        // START_CHANGE: ISS-2025-0253 - parse force option
        boolean force = false;
        if (arity == 2) {
            Term opts = query.getArguments().get(1).resolveBindings(bindings);
            List<Term> optList = ListUtils.extractElements(opts);
            if (optList != null) {
                for (Term opt : optList) {
                    if (opt instanceof CompoundTerm) {
                        CompoundTerm c = (CompoundTerm) opt;
                        if ("force".equals(c.getName()) && c.getArguments() != null && c.getArguments().size() == 1) {
                            Term v = c.getArguments().get(0);
                            if (v instanceof Atom && "true".equals(((Atom) v).getName())) force = true;
                        }
                    }
                }
            }
        }
        // END_CHANGE: ISS-2025-0253

        // START_CHANGE: ISS-2025-0472 - close the engine's own stream object
        PrologStream s = StreamManager.stream(streamTerm);
        if (s != null && StreamManager.streams().close(s)) {
            solutions.add(bindings);
            return true;
        }
        if (force) {
            solutions.add(bindings);
            return true;
        }
        if (s == null) {
            throw new PrologException(ISOErrorTerms.existenceError("stream", streamTerm, ctx));
        }
        throw new PrologException(ISOErrorTerms.permissionError("close", "stream", streamTerm, ctx));   // ISS-2025-0697
        // END_CHANGE: ISS-2025-0472
    }

    private static boolean isStreamTerm(Term t) {
        return t instanceof CompoundTerm && "$stream".equals(t.getName())
            && t.getArguments() != null && t.getArguments().size() == 1;
    }
}
