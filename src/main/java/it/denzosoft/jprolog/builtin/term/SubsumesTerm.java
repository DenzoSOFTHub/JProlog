package it.denzosoft.jprolog.builtin.term;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * subsumes_term/2 - ISO Prolog predicate for term subsumption
 * subsumes_term(@General, @Specific)
 * 
 * Succeeds if General subsumes Specific, i.e., if there exists a substitution
 * such that General becomes identical to Specific, without binding any variables
 * in Specific.
 * 
 * Term T1 subsumes term T2 if T1 is more general than T2.
 * Examples:
 * - f(X, Y) subsumes f(a, b) (X=a, Y=b)
 * - f(X, X) subsumes f(a, a) (X=a)  
 * - f(X, X) does not subsume f(a, b) (X cannot be both a and b)
 * - f(a, b) does not subsume f(X, Y) (specific cannot subsume general)
 */
public class SubsumesTerm implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("subsumes_term/2 requires exactly 2 arguments");
        }

        Term general = query.getArguments().get(0).resolveBindings(bindings);
        Term specific = query.getArguments().get(1).resolveBindings(bindings);

        // Check if general subsumes specific
        if (subsumes(general, specific)) {
            solutions.add(new HashMap<>(bindings));
            return true;
        }

        return false;
    }

    /**
     * Check if the general term subsumes the specific term.
     * 
     * @param general The general term (may contain variables to be bound)
     * @param specific The specific term (variables should not be bound)
     * @return true if general subsumes specific
     */
    private boolean subsumes(Term general, Term specific) {
        Map<String, Term> generalBindings = new HashMap<>();
        return unifyForSubsumption(general, specific, generalBindings);
    }

    /**
     * Attempt to unify general with specific for subsumption check.
     * Only allows binding variables in the general term.
     * 
     * @param general The general term
     * @param specific The specific term  
     * @param generalBindings Bindings for variables in general term
     * @return true if unification succeeds with subsumption constraints
     */
    private boolean unifyForSubsumption(Term general, Term specific, Map<String, Term> generalBindings) {
        // If general is a variable
        if (general instanceof Variable) {
            Variable generalVar = (Variable) general;
            String varName = generalVar.getName();
            
            if (generalBindings.containsKey(varName)) {
                // Variable already bound - check consistency
                Term existingBinding = generalBindings.get(varName);
                return termEquals(existingBinding, specific);
            } else {
                // Bind the variable to the specific term
                generalBindings.put(varName, specific);
                return true;
            }
        }
        
        // If specific is a variable, general cannot subsume it
        // (we don't allow binding variables in the specific term)
        if (specific instanceof Variable) {
            return false;
        }
        
        // If both are atoms
        if (general instanceof Atom && specific instanceof Atom) {
            return ((Atom) general).getName().equals(((Atom) specific).getName());
        }
        
        // If both are numbers
        if (general instanceof it.denzosoft.jprolog.core.terms.Number && 
            specific instanceof it.denzosoft.jprolog.core.terms.Number) {
            return ((it.denzosoft.jprolog.core.terms.Number) general).getValue() ==
                   ((it.denzosoft.jprolog.core.terms.Number) specific).getValue();
        }
        
        // If both are compound terms
        if (general instanceof CompoundTerm && specific instanceof CompoundTerm) {
            CompoundTerm generalComp = (CompoundTerm) general;
            CompoundTerm specificComp = (CompoundTerm) specific;
            
            // Check functor names match
            if (!generalComp.getName().equals(specificComp.getName())) {
                return false;
            }
            
            // Check arity matches
            List<Term> generalArgs = generalComp.getArguments();
            List<Term> specificArgs = specificComp.getArguments();
            
            int generalArity = (generalArgs != null) ? generalArgs.size() : 0;
            int specificArity = (specificArgs != null) ? specificArgs.size() : 0;
            
            if (generalArity != specificArity) {
                return false;
            }
            
            // Recursively check arguments
            if (generalArgs != null && specificArgs != null) {
                for (int i = 0; i < generalArgs.size(); i++) {
                    if (!unifyForSubsumption(generalArgs.get(i), specificArgs.get(i), generalBindings)) {
                        return false;
                    }
                }
            }
            
            return true;
        }
        
        // Different term types - no subsumption
        return false;
    }

    /**
     * Check if two terms are structurally equal.
     */
    private boolean termEquals(Term term1, Term term2) {
        if (term1.getClass() != term2.getClass()) {
            return false;
        }
        
        if (term1 instanceof Atom) {
            return ((Atom) term1).getName().equals(((Atom) term2).getName());
        }
        
        if (term1 instanceof it.denzosoft.jprolog.core.terms.Number) {
            return ((it.denzosoft.jprolog.core.terms.Number) term1).getValue() ==
                   ((it.denzosoft.jprolog.core.terms.Number) term2).getValue();
        }
        
        if (term1 instanceof Variable) {
            return ((Variable) term1).getName().equals(((Variable) term2).getName());
        }
        
        if (term1 instanceof CompoundTerm) {
            CompoundTerm comp1 = (CompoundTerm) term1;
            CompoundTerm comp2 = (CompoundTerm) term2;
            
            if (!comp1.getName().equals(comp2.getName())) {
                return false;
            }
            
            List<Term> args1 = comp1.getArguments();
            List<Term> args2 = comp2.getArguments();
            
            if (args1 == null && args2 == null) return true;
            if (args1 == null || args2 == null) return false;
            if (args1.size() != args2.size()) return false;
            
            for (int i = 0; i < args1.size(); i++) {
                if (!termEquals(args1.get(i), args2.get(i))) {
                    return false;
                }
            }
            
            return true;
        }
        
        return false;
    }
}