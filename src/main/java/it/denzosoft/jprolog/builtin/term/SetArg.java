// START_CHANGE: R1 - setarg/3 destructive arg replacement
package it.denzosoft.jprolog.builtin.term;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.Trail;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * setarg(+Index, +Term, +NewArg)
 *
 * Destructively replaces the Index-th argument (1-based) of compound Term with NewArg.
 * Replacement is recorded on the Trail and undone on backtracking.
 */
public class SetArg implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 3) {
            throw new PrologEvaluationException("setarg/3 requires exactly 3 arguments");
        }
        Term idxT = query.getArguments().get(0).resolveBindings(bindings);
        Term termT = query.getArguments().get(1).resolveBindings(bindings);
        Term newT = query.getArguments().get(2).resolveBindings(bindings);

        if (!(idxT instanceof Number) || !((Number) idxT).isInteger()) {
            throw new PrologEvaluationException("type_error(integer, " + idxT + ")");
        }
        if (!(termT instanceof CompoundTerm)) {
            throw new PrologEvaluationException("type_error(compound, " + termT + ")");
        }
        int idx = (int) ((Number) idxT).longValue();
        final CompoundTerm c = (CompoundTerm) termT;
        try {
            final Term old = c.setArgument(idx, newT);
            // Record undo: restore old arg on backtrack
            Trail.record(() -> c.setArgument(idx, old));
            solutions.add(new HashMap<>(bindings));
            return true;
        } catch (IndexOutOfBoundsException e) {
            throw new PrologEvaluationException("domain_error(argument_index, " + idx + ")");
        }
    }
}
// END_CHANGE: R1
