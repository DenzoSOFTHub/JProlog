package it.denzosoft.jprolog.terms;

import java.util.Map;


public class Variable extends Term {

    private String name;

    public Variable(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

   @Override
   public boolean unify(Term term, Map<String, Term> substitution) {
        if (substitution.containsKey(this.name)) {
            return substitution.get(this.name).unify(term, substitution);
        } else {
            // Occurs check: prevent infinite recursion
            if (occurs(this, term, substitution)) {
                return false; // Unification fails if variable occurs in the term
            }
            substitution.put(this.name, term);
            return true;
        }
   }

    // Helper method to check if a variable occurs within a term
    private boolean occurs(Variable variable, Term term, Map<String, Term> substitution) {
        if (term instanceof Variable) {
            if (variable.getName().equals(((Variable) term).getName())) {
                return true;
            } else if (substitution.containsKey(((Variable) term).getName())) {
                // Resolve the variable and check again
                return occurs(variable, substitution.get(((Variable) term).getName()), substitution);
            } else {
                return false;
            }
        } else if (term instanceof CompoundTerm) {
            CompoundTerm compoundTerm = (CompoundTerm) term;
            for (Term arg : compoundTerm.getArguments()) {
                if (occurs(variable, arg, substitution)) {
                    return true;
                }
            }
            return false;
        } else {
            return false; // Atoms, Numbers, and Lists cannot contain variables
        }
    }


    @Override
    public boolean isGround() {
        return false; // Variables are never ground until they are substituted
    }

     @Override
    public String toString() {
        return name;
    }

    @Override
    public Term copy() {
        return new Variable(this.name);
    }
}
