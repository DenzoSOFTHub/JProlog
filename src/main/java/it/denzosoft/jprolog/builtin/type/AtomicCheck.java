package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
// START_CHANGE: ISS-2025-0348 - strings are atomic
import it.denzosoft.jprolog.core.terms.PrologString;
// END_CHANGE: ISS-2025-0348
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;


public class AtomicCheck implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("atomic/1 requires exactly one argument.");
        }

        // START_CHANGE: ISS-2025-0080 - Resolve bindings before type/ground checks
        Term termArg = query.getArguments().get(0).resolveBindings(bindings);
        // END_CHANGE: ISS-2025-0080

        if (termArg.isGround()) {
            // START_CHANGE: ISS-2025-0348 - strings are atomic terms (SWI semantics for the
            // default double_quotes=string flag)
            boolean isAtomic = (termArg instanceof Atom) || (termArg instanceof Number)
                || (termArg instanceof PrologString);
            // END_CHANGE: ISS-2025-0348

            if (isAtomic) {
                solutions.add(bindings);
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }
}
