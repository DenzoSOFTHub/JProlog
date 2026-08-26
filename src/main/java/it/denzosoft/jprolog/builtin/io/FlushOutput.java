package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.PrologStream;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

/**
 * flush_output/0 and flush_output/1 - ISO 8.11.7.
 */
public class FlushOutput implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = (query.getArguments() == null) ? 0 : query.getArguments().size();
        if (arity != 0 && arity != 1) {
            throw new PrologEvaluationException("flush_output expects 0 or 1 arguments, got " + arity);
        }
        // START_CHANGE: ISS-2025-0472 - flush the engine's own stream (and the thread-local capture)
        if (arity == 0) {
            StreamManager.out().flush();
        } else {
            PrologStream s = IOStreamUtils.outputStream(query.getArguments().get(0), bindings, "flush_output/1");
            StreamManager.streams().writerFor(s).flush();
            s.flush();
        }
        solutions.add(bindings);
        return true;
        // END_CHANGE: ISS-2025-0472
    }
}
