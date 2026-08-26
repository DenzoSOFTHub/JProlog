// START_CHANGE: ISS-2025-0047 - Implement write_canonical/1 and write_canonical/2
package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.Writer;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.PrintStream;
import java.util.List;
import java.util.Map;

/**
 * write_canonical/1 and write_canonical/2 - ISO Prolog I/O predicates.
 * {@code write_term(T, [quoted(true), ignore_ops(true)])}: every operator in functional notation.
 *
 * <p>ISS-2025-0475 (wave W7): renders through {@link Writer}, so the output is the same
 * canonicalisation the rest of the write family uses and a cyclic term terminates.
 */
public class WriteCanonical implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = query.getArguments().size();
        if (arity != 1 && arity != 2) {
            throw new PrologEvaluationException("write_canonical expects 1 or 2 arguments, got " + arity);
        }
        PrintStream out;
        Term term;
        if (arity == 1) {
            out = StreamManager.out();
            term = query.getArguments().get(0);
        } else {
            out = IOStreamUtils.resolveOutputStream(query.getArguments().get(0), bindings, "write_canonical/2");
            term = query.getArguments().get(1);
        }
        out.print(Writer.format(term.resolveBindings(bindings), Writer.Options.canonical(), 1200));
        out.flush();
        solutions.add(bindings);
        return true;
    }
}
// END_CHANGE: ISS-2025-0047
