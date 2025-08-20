package it.denzosoft.jprolog.builtin.control;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Atom;

import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class UnifyWithOccursCheck implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("unify_with_occurs_check/2 requires exactly 2 arguments.");
        }

        Term term1 = query.getArguments().get(0);
        Term term2 = query.getArguments().get(1);

        // Create a copy of bindings for unification
        Map<String, Term> newBindings = new HashMap<>(bindings);
        
        // Attempt unification with occurs check
        if (unifyWithOccursCheck(term1, term2, newBindings)) {
            solutions.add(new HashMap<>(newBindings));
            return true;
        }
        
        return false;
    }

    private boolean unifyWithOccursCheck(Term term1, Term term2, Map<String, Term> bindings) {
        // If either term is a variable, check for occurs before unifying
        if (term1 instanceof Variable) {
            Variable var = (Variable) term1;
            if (occursCheck(var, term2, bindings)) {
                return false; // Occurs check failed
            }
            bindings.put(var.getName(), term2);
            return true;
        } else if (term2 instanceof Variable) {
            Variable var = (Variable) term2;
            if (occursCheck(var, term1, bindings)) {
                return false; // Occurs check failed
            }
            bindings.put(var.getName(), term1);
            return true;
        } else if (term1 instanceof CompoundTerm && term2 instanceof CompoundTerm) {
            CompoundTerm ct1 = (CompoundTerm) term1;
            CompoundTerm ct2 = (CompoundTerm) term2;
            
            if (!ct1.getName().equals(ct2.getName()) || ct1.getArguments().size() != ct2.getArguments().size()) {
                return false;
            }
            
            for (int i = 0; i < ct1.getArguments().size(); i++) {
                if (!unifyWithOccursCheck(ct1.getArguments().get(i), ct2.getArguments().get(i), bindings)) {
                    return false;
                }
            }
            return true;
        } else {
            // For atoms and numbers, use regular unification
            return term1.unify(term2, bindings);
        }
    }

    private boolean occursCheck(Variable variable, Term term, Map<String, Term> bindings) {
        if (term instanceof Variable) {
            Variable var = (Variable) term;
            if (variable.getName().equals(var.getName())) {
                return true;
            }
            // Check if this variable is bound and recursively check
            if (bindings.containsKey(var.getName())) {
                return occursCheck(variable, bindings.get(var.getName()), bindings);
            }
            return false;
        } else if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            for (Term arg : compound.getArguments()) {
                if (occursCheck(variable, arg, bindings)) {
                    return true;
                }
            }
            return false;
        }
        return false;
    }
}
