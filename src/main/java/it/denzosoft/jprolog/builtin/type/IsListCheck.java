package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

/**
 * is_list/1 - Succeeds if the argument is a proper list.
 * A proper list is either [] (empty list) or a compound term with functor ./2 
 * where the second argument is also a proper list.
 */
public class IsListCheck implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("is_list/1 requires exactly one argument.");
        }

        Term termArg = query.getArguments().get(0);
        
        boolean isList = isProperList(termArg);

        if (isList) {
            solutions.add(bindings);
            return true;
        } else {
            return false;
        }
    }
    
    /**
     * Recursively checks if a term is a proper list.
     * A proper list is either:
     * - An atom [] (empty list)
     * - A compound term with functor ./2 where the tail is also a proper list
     */
    private boolean isProperList(Term term) {
        if (term instanceof Atom) {
            return "[]".equals(((Atom) term).getName());
        }
        
        if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            if (".".equals(compound.getName()) && compound.getArguments().size() == 2) {
                // Check if the tail (second argument) is also a proper list
                return isProperList(compound.getArguments().get(1));
            }
        }
        
        return false;
    }
}