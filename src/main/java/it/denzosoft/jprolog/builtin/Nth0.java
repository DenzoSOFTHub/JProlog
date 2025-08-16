package it.denzosoft.jprolog.builtin;

import it.denzosoft.jprolog.BuiltIn;
import it.denzosoft.jprolog.PrologEvaluationException;
import it.denzosoft.jprolog.terms.Atom;
import it.denzosoft.jprolog.terms.CompoundTerm;
import it.denzosoft.jprolog.terms.Number;
import it.denzosoft.jprolog.terms.Term;
import it.denzosoft.jprolog.terms.Variable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;



public class Nth0 implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("nth0/3 requires exactly 3 arguments.");
        }

        Term indexTerm = query.getArguments().get(0);
        Term list = query.getArguments().get(1);
        Term element = query.getArguments().get(2);

        if (indexTerm.isGround() && list.isGround()) {
            // Case: nth0(GroundIndex, GroundList, Element)
            if (indexTerm instanceof Number) {
                int index = (int) Math.round(((Number) indexTerm).getValue());
                List<Term> elements = extractElements(list);
                
                if (index >= 0 && index < elements.size()) {
                    Term listElement = elements.get(index);
                    if (element.unify(listElement.copy(), bindings)) {
                        solutions.add(new HashMap<>(bindings));
                        return true;
                    }
                }
            }
            return false; // Index out of bounds or invalid index type
        } else if (list.isGround() && element.isGround()) {
            // Case: nth0(Index, GroundList, GroundElement)
            List<Term> elements = extractElements(list);
            
            for (int i = 0; i < elements.size(); i++) {
                if (elements.get(i).unify(element.copy(), new HashMap<>())) {
                    Term indexVar = new Number(i);
                    if (indexTerm.unify(indexVar, bindings)) {
                        solutions.add(new HashMap<>(bindings));
                        return true;
                    }
                }
            }
            return false;
        } else {
            throw new PrologEvaluationException("nth0/3: unsupported argument pattern.");
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
