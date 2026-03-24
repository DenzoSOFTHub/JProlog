package it.denzosoft.jprolog.core.utils;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.util.stream.Collectors;





public class ListTerm extends Term {

    private static final Logger LOGGER = Logger.getLogger(ListTerm.class.getName());

    private final List<Term> elements;

    public ListTerm() {
        this(new ArrayList<>());
    }

    public ListTerm(List<Term> elements) {
        this.elements = new ArrayList<>(elements);
    }

    // START_CHANGE: ISS-2025-0190 - Return unmodifiable view instead of copy
    public List<Term> getElements() {
        return Collections.unmodifiableList(elements);
    }
    // END_CHANGE: ISS-2025-0190

    @Override
    public boolean unify(Term term, Map<String, Term> substitution) {
        if (term instanceof ListTerm) {
            ListTerm otherList = (ListTerm) term;
            if (this.elements.size() != otherList.elements.size()) {
            	LOGGER.fine("List sizes differ, unification failed: " + this.elements.size() + " != " + otherList.elements.size());
                return false;
            }
            for (int i = 0; i < this.elements.size(); i++) {
                if (!this.elements.get(i).unify(otherList.elements.get(i), substitution)) {
                	LOGGER.fine("Element " + i + " failed to unify: " + this.elements.get(i) + " with " + otherList.elements.get(i));
                    return false;
                }
            }
            return true;
        // START_CHANGE: ISS-2025-0186 - Debug, utility, and list predicate fixes
        } else if (term instanceof CompoundTerm && ".".equals(term.getName())) {
            // Unify with standard Prolog "." list by converting this ListTerm
            Term standardList = createListTerm(this.elements);
            return standardList.unify(term, substitution);
        } else if (term instanceof Atom && "[]".equals(((Atom) term).getName()) && this.elements.isEmpty()) {
            return true;
        // END_CHANGE: ISS-2025-0186
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

    // START_CHANGE: ISS-2025-0190 - Return unmodifiable view instead of copy
    @Override
    public List<Term> getArguments() {
        return Collections.unmodifiableList(elements);
    }
    // END_CHANGE: ISS-2025-0190

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

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        ListTerm listTerm = (ListTerm) obj;
        return elements.equals(listTerm.elements);
    }
    
    @Override
    public int hashCode() {
        return elements.hashCode();
    }

    // START_CHANGE: ISS-2025-0191 - Iterative list construction to avoid stack overflow for large lists
    public static Term createListTerm(List<Term> terms) {
        if (terms == null || terms.isEmpty()) {
            return new Atom("[]");
        }
        Term result = new Atom("[]");
        for (int i = terms.size() - 1; i >= 0; i--) {
            List<Term> args = new ArrayList<>(2);
            args.add(terms.get(i));
            args.add(result);
            result = new CompoundTerm(new Atom("."), args);
        }
        return result;
    }
    // END_CHANGE: ISS-2025-0191
}
