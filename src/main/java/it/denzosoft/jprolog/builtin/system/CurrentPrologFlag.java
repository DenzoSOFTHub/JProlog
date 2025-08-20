package it.denzosoft.jprolog.builtin.system;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.system.PrologFlags;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * current_prolog_flag/2 - current_prolog_flag(?Flag, ?Value)
 * Gets or checks Prolog system flags.
 */
public class CurrentPrologFlag implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("current_prolog_flag/2 requires exactly 2 arguments: current_prolog_flag(?Flag, ?Value).");
        }

        Term flagTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term valueTerm = query.getArguments().get(1).resolveBindings(bindings);

        if (flagTerm instanceof Variable) {
            // Generate all flag-value pairs
            boolean foundSolution = false;
            for (String flagName : PrologFlags.getAllFlagNames()) {
                Term flagValue = PrologFlags.getFlag(flagName);
                
                // Try to unify with both flag name and value
                Atom flagAtom = new Atom(flagName);
                Map<String, Term> flagBinding = new HashMap<>(bindings);
                if (flagTerm.unify(flagAtom, flagBinding)) {
                    Map<String, Term> valueBinding = new HashMap<>(flagBinding);
                    if (valueTerm.unify(flagValue, valueBinding)) {
                        solutions.add(valueBinding);
                        foundSolution = true;
                    }
                }
            }
            return foundSolution;
        } else if (flagTerm instanceof Atom) {
            // Specific flag requested
            String flagName = ((Atom) flagTerm).getName();
            Term flagValue = PrologFlags.getFlag(flagName);
            
            if (flagValue != null) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (valueTerm.unify(flagValue, newBindings)) {
                    solutions.add(newBindings);
                    return true;
                }
            }
            return false;
        } else {
            throw new PrologEvaluationException("current_prolog_flag/2: Flag must be a variable or atom.");
        }
    }
}