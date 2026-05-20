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
        if (!(streamTerm instanceof Atom)) {
            throw new PrologEvaluationException("close: Stream must be an atom.");
        }
        String streamAlias = ((Atom) streamTerm).getName();

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
        throw new PrologEvaluationException("close: Cannot close stream '" + streamAlias + "' (not found or system stream).");
    }
}