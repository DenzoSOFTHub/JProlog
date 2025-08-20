package it.denzosoft.jprolog.core.terms;

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
        
        // START_CHANGE: ISS-2025-0019 - Improved list representation with ISO-compliant formatting
        // Special formatting for Prolog lists (functor "." with 2 arguments)
        if (".".equals(functor.getName()) && arguments.size() == 2) {
            return formatAsList();
        }
        // END_CHANGE: ISS-2025-0019
        
        return functor.getName() + "(" + arguments.stream().map(Term::toString).collect(Collectors.joining(", ")) + ")";
    }
    
    // START_CHANGE: ISS-2025-0019 - Helper method for formatting lists in ISO-compliant way
    /**
     * Format this compound term as an ISO-compliant Prolog list [a,b,c]
     * Only called when this is a list structure (functor "." with 2 args)
     */
    private String formatAsList() {
        List<String> elements = new ArrayList<>();
        Term current = this;
        
        // Traverse the list structure collecting elements
        while (current instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) current;
            if (!".".equals(compound.functor.getName()) || compound.arguments.size() != 2) {
                break; // Not a proper list structure
            }
            
            elements.add(compound.arguments.get(0).toString());
            current = compound.arguments.get(1);
        }
        
        // Handle the tail
        if (current instanceof Atom && "[]".equals(((Atom) current).getName())) {
            // Proper list ending with []
            return "[" + String.join(", ", elements) + "]";
        } else {
            // Improper list with non-empty tail  
            return "[" + String.join(", ", elements) + "|" + current.toString() + "]";
        }
    }
    // END_CHANGE: ISS-2025-0019

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
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        CompoundTerm that = (CompoundTerm) obj;
        return functor.equals(that.functor) && arguments.equals(that.arguments);
    }
    
    @Override
    public int hashCode() {
        return java.util.Objects.hash(functor, arguments);
    }
}
