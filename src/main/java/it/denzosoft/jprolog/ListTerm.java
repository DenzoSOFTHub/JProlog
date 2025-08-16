package it.denzosoft.jprolog;

import it.denzosoft.jprolog.terms.Atom;
import it.denzosoft.jprolog.terms.CompoundTerm;
import it.denzosoft.jprolog.terms.Term;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;





public class ListTerm extends Term {

    private final List<Term> elements;

    public ListTerm() {
        this(new ArrayList<>());
    }

    public ListTerm(List<Term> elements) {
        this.elements = new ArrayList<>(elements);
    }

    public List<Term> getElements() {
        return new ArrayList<>(elements); // Return a copy
    }

    @Override
    public boolean unify(Term term, Map<String, Term> substitution) {
        if (term instanceof ListTerm) {
            ListTerm otherList = (ListTerm) term;
            if (this.elements.size() != otherList.elements.size()) {
            	System.out.println("List sizes differ, unification failed: " + this.elements.size() + " != " + otherList.elements.size());
                return false;
            }
            for (int i = 0; i < this.elements.size(); i++) {
                if (!this.elements.get(i).unify(otherList.elements.get(i), substitution)) {
                	System.out.println("Element " + i + " failed to unify: " + this.elements.get(i) + " with " + otherList.elements.get(i));
                    return false;
                }
            }
            return true;
        } else {
            return term.unify(this, substitution);
        }
    }

    @Override
    public boolean isGround() {
        for (Term element : elements) {
            if (!element.isGround()) {
                return false;
            }
        }
        return true;
    }

    @Override
    public String toString() {
        return "[" + elements.stream().map(Term::toString).collect(Collectors.joining(", ")) + "]";
    }

    @Override
    public String getName() {
        return null;
    }

    @Override
    public List<Term> getArguments() {
        return new ArrayList<>(elements); // Return a copy
    }

    @Override
    public Term resolveBindings(Map<String, Term> bindings) {
        List<Term> resolvedElements = new ArrayList<>();
        for (Term element : elements) {
            resolvedElements.add(element.resolveBindings(bindings));
        }
        return new ListTerm(resolvedElements);
    }

    @Override
    public Term copy() {
        List<Term> copiedElements = new ArrayList<>();
        for (Term element : elements) {
            copiedElements.add(element.copy());
        }
        return new ListTerm(copiedElements);
    }

    // Helper function to create a list term from a list of terms.
    public static Term createListTerm(List<Term> terms) {
        if (terms == null || terms.isEmpty()) {
            return new Atom("[]");  // Empty list represented as an atom "[]"
        } else {
            List<Term> args = new ArrayList<>();
            args.add(terms.get(0));

            List<Term> sublist = terms.subList(1, terms.size());
            Term tail = createListTerm(sublist);
            args.add(tail);
            return new CompoundTerm(new Atom("."), args); // List constructor
        }
    }
}
