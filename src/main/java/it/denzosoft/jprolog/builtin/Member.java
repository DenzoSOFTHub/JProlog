package it.denzosoft.jprolog.builtin;

import it.denzosoft.jprolog.BuiltIn;
import it.denzosoft.jprolog.PrologEvaluationException;
import it.denzosoft.jprolog.terms.Atom;
import it.denzosoft.jprolog.terms.CompoundTerm;
import it.denzosoft.jprolog.terms.Term;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class Member implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("member/2 requires exactly 2 arguments.");
        }

        Term element = query.getArguments().get(0);
        Term list = query.getArguments().get(1);

        if (list.isGround()) {
            // Find all elements in the ground list and try to unify with the given element
            List<Term> elements = extractElements(list);
            boolean found = false;
            
            for (Term listItem : elements) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (element.unify(listItem.copy(), newBindings)) {
                    solutions.add(new HashMap<>(newBindings));
                    found = true;
                }
            }
            
            return found;
        } else {
            throw new PrologEvaluationException("member/2 with non-ground list not implemented.");
        }
    }

    private List<Term> extractElements(Term list) {
        List<Term> elements = new ArrayList<>();
        Term current = list;
        
        while (current instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) current;
            if (compound.getName().equals(".") && compound.getArguments().size() == 2) {
                elements.add(compound.getArguments().get(0));
                current = compound.getArguments().get(1);
            } else {
                break;
            }
        }
        
        return elements;
    }
}
