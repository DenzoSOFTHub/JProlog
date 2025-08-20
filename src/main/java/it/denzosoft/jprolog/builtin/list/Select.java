package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class Select implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("select/3 requires exactly 3 arguments.");
        }

        Term element = query.getArguments().get(0);
        Term inputList = query.getArguments().get(1);
        Term remainderList = query.getArguments().get(2);

        if (inputList.isGround()) {
            // Extract elements from the input list
            List<Term> elements = extractElements(inputList);
            boolean found = false;
            
            // Try removing each element from the list
            for (int i = 0; i < elements.size(); i++) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                
                // Try to unify the selected element
                if (element.unify(elements.get(i).copy(), newBindings)) {
                    // Create remainder list without the selected element
                    List<Term> remainderElements = new ArrayList<>(elements);
                    remainderElements.remove(i);
                    Term remainderListTerm = createList(remainderElements);
                    
                    // Unify with the remainder list
                    if (remainderList.unify(remainderListTerm, newBindings)) {
                        solutions.add(new HashMap<>(newBindings));
                        found = true;
                    }
                }
            }
            
            return found;
        } else {
            throw new PrologEvaluationException("select/3 requires a ground input list.");
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
