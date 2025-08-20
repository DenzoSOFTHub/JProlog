package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;


public class Predicate {
    private String name;
    private List<Term> terms;

    public Predicate(String name, List<Term> terms) {
        this.name = name;
        this.terms = terms;
    }
}
