package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.List;
import java.util.Map;

/**
 * partial_list/1 - Succeeds if the argument is a partial list.
 * A partial list is a list structure that ends with an unbound variable
 * instead of the empty list [].
 */
public class PartialListCheck implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("partial_list/1 requires exactly one argument.");
        }

        Term termArg = query.getArguments().get(0);
        
        boolean isPartialList = isPartialListTerm(termArg, bindings);

        if (isPartialList) {
            solutions.add(bindings);
            return true;
        } else {
            return false;
        }
    }
    
    /**
     * Recursively checks if a term is a partial list.
     * A partial list is a list structure that ends with an unbound variable.
     */
    private boolean isPartialListTerm(Term term, Map<String, Term> bindings) {
        if (term instanceof Variable) {
            Variable var = (Variable) term;
            Term boundValue = bindings.get(var.getName());
            if (boundValue == null) {
                return true; // Unbound variable at the end makes it a partial list
            }
            return isPartialListTerm(boundValue, bindings);
        }
        
        if (term instanceof Atom) {
            // If it's [], then it's a proper list, not partial
            return false;
        }
        
        if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            if (".".equals(compound.getName()) && compound.getArguments().size() == 2) {
                // Check the tail (second argument)
                return isPartialListTerm(compound.getArguments().get(1), bindings);
            }
        }
        
        return false;
    }
}