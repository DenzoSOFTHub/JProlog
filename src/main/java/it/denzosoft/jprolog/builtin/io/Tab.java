package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * tab(+N) - Write N space characters.
 */
public class Tab implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) return false;

        Term nTerm = query.getArguments().get(0).resolveBindings(bindings);
        if (!(nTerm instanceof Number)) return false;

        // START_CHANGE: ISS-2025-0192 - Validate N >= 0
        int n = ((Number) nTerm).getValue().intValue();
        if (n < 0) return false;
        // END_CHANGE: ISS-2025-0192
        for (int i = 0; i < n; i++) {
            System.out.print(' ');
        }

        solutions.add(new HashMap<>(bindings));
        return true;
    }
}
