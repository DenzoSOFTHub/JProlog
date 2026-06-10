package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
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
            throw new PrologEvaluationException("close/1 or close/2 expected.");
        }

        Term streamTerm = query.getArguments().get(0).resolveBindings(bindings);
        // START_CHANGE: ISS-2025-0377 - raise ISO error/2 terms (8.11.6): instantiation_error for an
        // unbound stream, domain_error(stream_or_alias, S) for a non-stream term.
        String ctx = "close/" + arity;
        if (streamTerm instanceof it.denzosoft.jprolog.core.terms.Variable) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError(ctx));
        }
        String streamAlias = IOStreamUtils.streamAlias(streamTerm);
        if (streamAlias == null) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.domainError("stream_or_alias", streamTerm, ctx));
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

        if (StreamManager.closeStream(streamAlias)) {
            solutions.add(bindings);
            return true;
        }
        if (force) {
            // force(true): succeed even if stream not found / already closed
            solutions.add(bindings);
            return true;
        }
        // START_CHANGE: ISS-2025-0377 - an alias that names no open stream is the ISO
        // error(existence_error(stream, S), _) (8.11.6); only the system-stream / I/O-failure
        // case keeps the implementation-specific message.
        if (!StreamManager.hasStream(streamAlias)) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.existenceError("stream", streamTerm, ctx));
        }
        // END_CHANGE: ISS-2025-0377
        throw new PrologEvaluationException("close: Cannot close stream '" + streamAlias + "' (not found or system stream).");
    }
}