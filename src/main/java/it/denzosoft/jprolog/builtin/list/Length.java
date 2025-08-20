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



public class Length implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("length/2 requires exactly 2 arguments.");
        }

        Term list = query.getArguments().get(0);
        Term lengthTerm = query.getArguments().get(1);

        if (list.isGround()) {
            // Case: length(GroundList, Length)
            int count = countElements(list);
            Term length = new Number(count);
            if (lengthTerm.unify(length, bindings)) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            return false;
        } else if (lengthTerm.isGround() && lengthTerm instanceof Number) {
            // Case: length(List, GroundInteger)
            int expectedLength = (int) Math.round(((Number) lengthTerm).getValue());
            if (expectedLength < 0) {
                return false; // Can't have negative length lists
            }
            
            Term generatedList = generateList(expectedLength);
            if (list.unify(generatedList, bindings)) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            return false;
        } else {
            throw new PrologEvaluationException("length/2: unsupported argument pattern.");
        }
    }

    private int countElements(Term list) {
        int count = 0;
        Term current = list;
        
        while (current instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) current;
            if (compound.getName().equals(".") && compound.getArguments().size() == 2) {
                count++;
                current = compound.getArguments().get(1);
            } else {
                break;
            }
        }
        
        // If final term is not the empty list atom [], it's malformed
        if (current instanceof Atom && ((Atom) current).getName().equals("[]")) {
            return count;
        } else {
            return -1; // Indicates malformed list
        }
    }

    private Term generateList(int length) {
        if (length < 0) {
            throw new IllegalArgumentException("List length must be non-negative");
        }
        
        Term current = new Atom("[]");
        for (int i = 0; i < length; i++) {
            List<Term> args = new ArrayList<>();
            args.add(new Variable("_")); // Anonymous variable for list element
            args.add(current);
            current = new CompoundTerm(new Atom("."), args);
        }
        return current;
    }
}
