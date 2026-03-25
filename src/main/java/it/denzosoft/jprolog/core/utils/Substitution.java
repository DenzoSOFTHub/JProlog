package it.denzosoft.jprolog.core.utils;

import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;




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

    // START_CHANGE: ISS-2025-0192 - Recursively resolve compound terms containing variables
    private Term resolveTerm(Term term) {
        if (term == null) {
            return null;
        }

        Set<String> visited = new HashSet<>();
        Term current = term;
        while (current instanceof Variable) {
            String variableName = ((Variable) current).getName();
            if (visited.contains(variableName)) {
                // Cycle detected - return the variable as-is to prevent infinite recursion
                return current;
            }
            if (!bindings.containsKey(variableName)) {
                return current;
            }
            visited.add(variableName);
            current = bindings.get(variableName);
        }
        return current;
    }
    // END_CHANGE: ISS-2025-0192

    @Override
	public String toString() {
		return "Substitution [bindings=" + bindings + "]";
	}
}
