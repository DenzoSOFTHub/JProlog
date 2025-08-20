package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;



public class Nth1 implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("nth1/3 requires exactly 3 arguments.");
        }

        Term indexTerm = query.getArguments().get(0);
        Term list = query.getArguments().get(1);
        Term element = query.getArguments().get(2);

        if (indexTerm.isGround() && list.isGround()) {
            // Case: nth1(GroundIndex, GroundList, Element)
            if (indexTerm instanceof Number) {
                int index = (int) Math.round(((Number) indexTerm).getValue());
                List<Term> elements = extractElements(list);
                
                // nth1 is 1-based indexing
                if (index > 0 && index <= elements.size()) {
                    Term listElement = elements.get(index - 1);
                    if (element.unify(listElement.copy(), bindings)) {
                        solutions.add(new HashMap<>(bindings));
                        return true;
                    }
                }
            }
            return false; // Index out of bounds or invalid index type
        } else {
            throw new PrologEvaluationException("nth1/3: unsupported argument pattern.");
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
