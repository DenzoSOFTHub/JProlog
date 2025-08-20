package it.denzosoft.jprolog.core.utils;

import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.Map;




public class Substitution  {

    private Map<String, Term> bindings = new HashMap<>();

    public Substitution() {
    }

    public Substitution(Map<String, Term> initialBindings) {
        this.bindings.putAll(initialBindings);
    }

    public Term get(String variableName) {
        Term term = bindings.get(variableName);
        return resolveTerm(term);
    }

    public void put(String variableName, Term term) {
        bindings.put(variableName, term);
    }

    public boolean containsKey(String variableName) {
        return bindings.containsKey(variableName);
    }

    public Substitution copy() {
        return new Substitution(new HashMap<>(this.bindings));
    }

    public boolean isInstantiated(Variable var) {
        return bindings.containsKey(var.getName());
    }

    private Term resolveTerm(Term term) {
        if (term == null) {
            return null;
        }

        if (term instanceof Variable) {
            String variableName = ((Variable) term).getName();
            if (bindings.containsKey(variableName)) {
                Term boundTerm = bindings.get(variableName);
                return resolveTerm(boundTerm); // Recursively resolve
            }
        }
        return term; // Return the term if it's not a bound variable or is not a variable
    }

    @Override
	public String toString() {
		return "Substitution [bindings=" + bindings + "]";
	}
}
