// START_CHANGE: CR-2025-0009 - spying/1 builtin
package it.denzosoft.jprolog.builtin.debug;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * spying(?PredicateIndicator) — enumerate active spy points.
 *
 * Each spy point is unified successively with PredicateIndicator
 * which has form Name/Arity. Backtracks over all registered spy points.
 */
public class Spying implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 1) {
            throw new PrologEvaluationException("spying/1 requires exactly 1 argument");
        }
        Term piTerm = query.getArguments().get(0);
        boolean any = false;
        for (String spyKey : Spy.getSpyPoints()) {
            int slash = spyKey.lastIndexOf('/');
            if (slash <= 0) continue;
            String name = spyKey.substring(0, slash);
            int arity;
            try { arity = Integer.parseInt(spyKey.substring(slash + 1)); }
            catch (NumberFormatException e) { continue; }
            Term pi = new CompoundTerm(new Atom("/"),
                Arrays.asList(new Atom(name), new Number((long) arity)));
            Map<String, Term> nb = new HashMap<>(bindings);
            if (piTerm.unify(pi, nb)) {
                solutions.add(nb);
                any = true;
            }
        }
        return any;
    }
}
// END_CHANGE: CR-2025-0009
