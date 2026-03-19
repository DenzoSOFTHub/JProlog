package it.denzosoft.jprolog.builtin.term;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.*;

/**
 * numbervars(+Term, +Start, -End) - Number unbound variables in Term.
 * Variables are bound to '$VAR'(N) terms, N starting from Start.
 * End is unified with Start + number_of_variables.
 */
public class NumberVars implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) return false;

        Term term = query.getArguments().get(0).resolveBindings(bindings);
        Term startTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term endArg = query.getArguments().get(2);

        if (!(startTerm instanceof Number)) return false;

        int start = ((Number) startTerm).getValue().intValue();
        Map<String, Term> newBindings = new HashMap<>(bindings);

        int[] counter = {start};
        numberVarsWalk(term, newBindings, counter, new HashSet<>());

        if (endArg.unify(new Number(counter[0]), newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    private void numberVarsWalk(Term term, Map<String, Term> bindings, int[] counter, Set<String> seen) {
        if (term instanceof Variable) {
            String name = ((Variable) term).getName();
            Term resolved = term.resolveBindings(bindings);
            if (resolved instanceof Variable) {
                String resolvedName = ((Variable) resolved).getName();
                if (!seen.contains(resolvedName)) {
                    seen.add(resolvedName);
                    // Bind to '$VAR'(N)
                    Term varTerm = new CompoundTerm(new Atom("$VAR"),
                        Collections.singletonList(new Number(counter[0])));
                    bindings.put(resolvedName, varTerm);
                    counter[0]++;
                }
            }
        } else if (term instanceof CompoundTerm) {
            for (Term arg : term.getArguments()) {
                numberVarsWalk(arg, bindings, counter, seen);
            }
        }
    }
}
