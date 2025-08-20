package it.denzosoft.jprolog.builtin.type;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.List;
import java.util.Map;

/**
 * ground/1 - Succeeds if the argument is ground (contains no unbound variables).
 * A term is ground if it contains no unbound variables anywhere in its structure.
 */
public class GroundCheck implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("ground/1 requires exactly one argument.");
        }

        Term termArg = query.getArguments().get(0);
        
        boolean isGround = isGroundTerm(termArg, bindings);

        if (isGround) {
            solutions.add(bindings);
            return true;
        } else {
            return false;
        }
    }
    
    /**
     * Recursively checks if a term is ground (contains no unbound variables).
     */
    private boolean isGroundTerm(Term term, Map<String, Term> bindings) {
        if (term instanceof Variable) {
            Variable var = (Variable) term;
            Term boundValue = bindings.get(var.getName());
            if (boundValue == null) {
                return false; // Unbound variable found
            }
            // Check if the bound value is also ground
            return isGroundTerm(boundValue, bindings);
        }
        
        if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            // All arguments must be ground
            for (Term arg : compound.getArguments()) {
                if (!isGroundTerm(arg, bindings)) {
                    return false;
                }
            }
        }
        
        // Atoms, numbers, and strings are always ground
        return true;
    }
}