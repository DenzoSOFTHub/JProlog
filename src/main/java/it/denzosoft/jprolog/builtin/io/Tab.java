package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * tab(+N) - Write N space characters.
 * tab(+Stream, +N) - Write N space characters to Stream.
 */
public class Tab implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        // START_CHANGE: ISS-2025-0373 - tab/2: write the spaces to the given stream
        int arity = query.getArguments().size();
        if (arity != 1 && arity != 2) return false;

        java.io.PrintStream out;
        Term nTerm;
        if (arity == 1) {
            out = StreamManager.out();
            nTerm = query.getArguments().get(0).resolveBindings(bindings);
        } else {
            out = IOStreamUtils.resolveOutputStream(query.getArguments().get(0), bindings, "tab/2");
            nTerm = query.getArguments().get(1).resolveBindings(bindings);
        }
        // END_CHANGE: ISS-2025-0373
        if (!(nTerm instanceof Number)) return false;

        // START_CHANGE: ISS-2025-0192 - Validate N >= 0
        int n = ((Number) nTerm).getValue().intValue();
        if (n < 0) return false;
        // END_CHANGE: ISS-2025-0192
        for (int i = 0; i < n; i++) {
            out.print(' ');
        }
        // START_CHANGE: ISS-2025-0373 - flush stream output
        out.flush();
        // END_CHANGE: ISS-2025-0373

        solutions.add(new HashMap<>(bindings));
        return true;
    }
}
