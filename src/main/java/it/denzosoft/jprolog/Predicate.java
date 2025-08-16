package it.denzosoft.jprolog;

import it.denzosoft.jprolog.terms.Term;

import java.util.List;


public class Predicate {
    private String name;
    private List<Term> terms;

    public Predicate(String name, List<Term> terms) {
        this.name = name;
        this.terms = terms;
    }
}
