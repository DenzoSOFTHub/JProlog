package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0378 - print/1,2 (universal SWI/GNU/SICStus practice)
/**
 * print(+Term) - write Term to the current output with numbervars(true) semantics.
 * print(+Stream, +Term) - write Term to Stream.
 *
 * Equivalent to write_term(Term, [numbervars(true)]) (the portray/1 hook is not supported).
 */
public class Print implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = (query.getArguments() == null) ? 0 : query.getArguments().size();
        if (arity != 1 && arity != 2) {
            throw new PrologEvaluationException("print/1 or print/2 expected.");
        }

        java.io.PrintStream out;
        Term termToWrite;
        if (arity == 1) {
            out = StreamManager.out();
            termToWrite = query.getArguments().get(0);
        } else {
            out = IOStreamUtils.resolveOutputStream(query.getArguments().get(0), bindings, "print/2");
            termToWrite = query.getArguments().get(1);
        }
        Term resolvedTerm = termToWrite.resolveBindings(bindings);
        // write semantics (unquoted, operators honoured) with numbervars(true)
        out.print(it.denzosoft.jprolog.core.util.TermFormatter.format(resolvedTerm, false, false, true, 1200));
        out.flush();

        solutions.add(bindings);
        return true;
    }
}
// END_CHANGE: ISS-2025-0378
