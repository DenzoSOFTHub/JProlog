package it.denzosoft.jprolog.builtin;

import it.denzosoft.jprolog.BuiltIn;
import it.denzosoft.jprolog.PrologEvaluationException;
import it.denzosoft.jprolog.terms.Atom;
import it.denzosoft.jprolog.terms.CompoundTerm;
import it.denzosoft.jprolog.terms.Term;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class Sort implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("sort/2 requires exactly 2 arguments.");
        }

        Term inputList = query.getArguments().get(0);
        Term sortedList = query.getArguments().get(1);

        if (inputList.isGround()) {
            // Extract elements from the input list
            List<Term> elements = extractElements(inputList);
            
            // Remove duplicates and sort
            List<Term> uniqueElements = new ArrayList<>();
            for (Term term : elements) {
                if (!uniqueElements.contains(term)) {
                    uniqueElements.add(term);
                }
            }
            
            Collections.sort(uniqueElements, (t1, t2) -> t1.toString().compareTo(t2.toString()));
            
            // Create the sorted list term
            Term sortedListTerm = createList(uniqueElements);
            
            // Unify with the output list
            if (sortedList.unify(sortedListTerm, bindings)) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            return false;
        } else {
            throw new PrologEvaluationException("sort/2 requires a ground input list.");
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

    private Term createList(List<Term> elements) {
        Term result = new Atom("[]");
        for (int i = elements.size() - 1; i >= 0; i--) {
            List<Term> args = new ArrayList<>();
            args.add(elements.get(i));
            args.add(result);
            result = new CompoundTerm(new Atom("."), args);
        }
        return result;
    }
}
