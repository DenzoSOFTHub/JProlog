package it.denzosoft.jprolog.builtin.term;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.*;

/**
 * term_variables/2 - ISO Prolog predicate for term variable extraction
 * term_variables(+Term, ?Variables)
 * 
 * Unifies Variables with a list of all variables appearing in Term,
 * in the order of their first occurrence (left-to-right, depth-first).
 */
public class TermVariables implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("term_variables/2 requires exactly 2 arguments");
        }

        Term inputTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term variablesTerm = query.getArguments().get(1);

        // Collect variables in order of first occurrence
        List<Variable> variables = collectVariables(inputTerm);

        // Create a Prolog list of the variables
        Term variablesList = createPrologList(variables);

        // Try to unify with the variables term
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (variablesTerm.unify(variablesList, newBindings)) {
            solutions.add(newBindings);
            return true;
        }

        return false;
    }

    /**
     * Collect all variables from a term in order of first occurrence.
     */
    private List<Variable> collectVariables(Term term) {
        Set<String> seenVariables = new LinkedHashSet<>(); // Maintains insertion order
        List<Variable> variables = new ArrayList<>();
        
        collectVariablesHelper(term, seenVariables, variables);
        
        return variables;
    }

    /**
     * Recursively collect variables from a term.
     */
    private void collectVariablesHelper(Term term, Set<String> seenVariables, List<Variable> variables) {
        if (term instanceof Variable) {
            Variable var = (Variable) term;
            String varName = var.getName();
            if (!seenVariables.contains(varName)) {
                seenVariables.add(varName);
                variables.add(var);
            }
        } else if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            if (compound.getArguments() != null) {
                for (Term arg : compound.getArguments()) {
                    collectVariablesHelper(arg, seenVariables, variables);
                }
            }
        }
        // Atoms and Numbers contain no variables
    }

    /**
     * Create a Prolog list from a Java list of terms.
     */
    private Term createPrologList(List<? extends Term> terms) {
        Term result = new Atom("[]"); // Empty list
        
        // Build list from right to left
        for (int i = terms.size() - 1; i >= 0; i--) {
            result = new CompoundTerm(
                new Atom("."), 
                Arrays.asList(terms.get(i), result)
            );
        }
        
        return result;
    }
}