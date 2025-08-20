package it.denzosoft.jprolog.core.terms;

import java.util.Map;


public class Atom extends Term {

    private String name;

    public Atom(String name) {
        this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public boolean unify(Term term, Map<String, Term> substitution) {
        if (term instanceof Variable) {
            return term.unify(this, substitution);
        } else if (term instanceof Atom) {
            return this.name.equals(((Atom) term).name);
        }
        return false;
    }

    @Override
    public boolean isGround() {
        return true;  // Atoms are always ground
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public Term copy() {
        return new Atom(this.name); // Return a new Atom with the same name
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        Atom atom = (Atom) obj;
        return name != null ? name.equals(atom.name) : atom.name == null;
    }
    
    @Override
    public int hashCode() {
        return name != null ? name.hashCode() : 0;
    }
}
