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
    
    // START_CHANGE: ISS-2025-0428 - ENG-09: iterative with an explicit work stack. The recursive
    // walk needed one Java frame per list cell, so ground/1 on a list of a few tens of thousands of
    // elements raised resource_error(stack_overflow).
    /**
     * Checks if a term is ground (contains no unbound variables), dereferencing through
     * {@code bindings}. A {@code seen} set makes circular variable chains terminate.
     */
    private boolean isGroundTerm(Term term, Map<String, Term> bindings) {
        java.util.ArrayDeque<Term> work = new java.util.ArrayDeque<>();
        java.util.Set<String> seen = null;                     // allocated only if variables appear
        work.push(term);
        while (!work.isEmpty()) {
            Term t = work.pop();
            if (t instanceof Variable) {
                String name = ((Variable) t).getName();
                Term boundValue = bindings.get(name);
                if (boundValue == null) return false;          // unbound variable found
                if (seen == null) seen = new java.util.HashSet<>();
                if (!seen.add(name)) continue;                 // already expanded / circular chain
                work.push(boundValue);
            } else if (t instanceof CompoundTerm) {
                for (Term arg : ((CompoundTerm) t).getArguments()) work.push(arg);
            }
            // Atoms, numbers, and strings are always ground
        }
        return true;
    }
    // END_CHANGE: ISS-2025-0428
}