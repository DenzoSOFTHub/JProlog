// START_CHANGE: CR-2025-0009 - debugging/0 builtin
package it.denzosoft.jprolog.builtin.debug;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * debugging/0 — reports current debug state to stdout.
 *
 * Outputs:
 * - Whether trace mode is on
 * - List of spy points (predicates being spied)
 * Always succeeds.
 */
public class Debugging implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        boolean tracing = Trace.isTracingEnabled();
        java.util.Set<String> spies = Spy.getSpyPoints();
        StringBuilder sb = new StringBuilder();
        sb.append(tracing ? "Tracing is ON\n" : "Tracing is OFF\n");
        if (spies.isEmpty()) {
            sb.append("No spy points.\n");
        } else {
            sb.append("Spy points:\n");
            for (String s : spies) sb.append("  ").append(s).append('\n');
        }
        System.out.print(sb);
        solutions.add(new HashMap<>(bindings));
        return true;
    }
}
// END_CHANGE: CR-2025-0009
