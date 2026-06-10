package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

public class Writeln implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        // START_CHANGE: ISS-2025-0373 - writeln/2: write to the given stream
        int arity = query.getArguments().size();
        if (arity != 1 && arity != 2) {
            throw new PrologEvaluationException("writeln/1 or writeln/2 expected.");
        }

        java.io.PrintStream out;
        Term termToWrite;
        if (arity == 1) {
            out = StreamManager.out();
            termToWrite = query.getArguments().get(0);
        } else {
            out = IOStreamUtils.resolveOutputStream(query.getArguments().get(0), bindings, "writeln/2");
            termToWrite = query.getArguments().get(1);
        }
        Term resolvedTerm = termToWrite.resolveBindings(bindings);
        // START_CHANGE: ISS-2025-0242 - operator-aware formatting
        // START_CHANGE: ISS-2025-0389 - ISO 8.14.2: writeln follows write/1 = numbervars(true)
        out.println(it.denzosoft.jprolog.core.util.TermFormatter.format(resolvedTerm, false, false, true, 1200));
        out.flush();
        // END_CHANGE: ISS-2025-0389
        // END_CHANGE: ISS-2025-0242
        // END_CHANGE: ISS-2025-0373

        solutions.add(bindings);
        return true;
    }
}
