package it.denzosoft.jprolog.terms;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;



public class CompoundTerm extends Term {

    private Atom functor;
    private List<Term> arguments;

    public CompoundTerm(Atom functor, List<Term> arguments) {
        this.functor = functor;
        this.arguments = new ArrayList<>(arguments); // Make a copy
    }

    public Atom getFunctor() {
        return functor;
    }

    @Override
    public List<Term> getArguments() {
        return new ArrayList<>(arguments); // Return a copy
    }

    @Override
    public String getName() {
        return functor.getName();
    }

    @Override
    public boolean unify(Term term, Map<String, Term> substitution) {
        if (term instanceof Variable) {
            return term.unify(this, substitution);
        } else if (term instanceof CompoundTerm) {
            CompoundTerm otherCompound = (CompoundTerm) term;
            if (!this.functor.getName().equals(otherCompound.functor.getName()) || 
                this.arguments.size() != otherCompound.arguments.size()) {
                return false;
            }

            // Create a working copy of the substitution
            Map<String, Term> workingSubstitution = new HashMap<>(substitution);
            
            for (int i = 0; i < this.arguments.size(); i++) {
                if (!this.arguments.get(i).unify(otherCompound.arguments.get(i), workingSubstitution)) {
                    return false;
                }
            }
            
            // If we get here, all arguments unified successfully
            substitution.putAll(workingSubstitution);
            return true;
        } else {
            return false;
        }
    }

    @Override
    public boolean isGround() {
        for (Term arg : arguments) {
            if (!arg.isGround()) {
                return false;
            }
        }
        return true;
    }

    @Override
    public String toString() {
        if (arguments.isEmpty()) {
            return functor.toString();
        }
        return functor.getName() + "(" + arguments.stream().map(Term::toString).collect(Collectors.joining(", ")) + ")";
    }

    @Override
    public Term copy() {
        List<Term> copiedArguments = new ArrayList<>();
        for (Term arg : arguments) {
            copiedArguments.add(arg.copy());
        }
        return new CompoundTerm(functor, copiedArguments);
    }
    
    @Override
    public Term resolveBindings(Map<String, Term> bindings) {
        List<Term> resolvedArguments = new ArrayList<>();
        for (Term arg : arguments) {
            resolvedArguments.add(arg.resolveBindings(bindings));
        }
        return new CompoundTerm(functor, resolvedArguments);
    }
}
