package it.denzosoft.jprolog.builtin;

import it.denzosoft.jprolog.BuiltIn;
import it.denzosoft.jprolog.PrologEvaluationException;
import it.denzosoft.jprolog.terms.Atom;
import it.denzosoft.jprolog.terms.CompoundTerm;
import it.denzosoft.jprolog.terms.Term;
import it.denzosoft.jprolog.terms.Variable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;





public class Append implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("append/3 requires exactly 3 arguments.");
        }

        Term list1 = query.getArguments().get(0);
        Term list2 = query.getArguments().get(1);
        Term result = query.getArguments().get(2);

        if (list1.isGround() && list2.isGround()) {
            // Case: append(GroundList1, GroundList2, Result)
            Term concatenated = concatenateLists(list1, list2);
            if (result.unify(concatenated, bindings)) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            return false;
        } else if (result.isGround()) {
            // Case: append(List1, List2, GroundResult)
            // Generate all possible splits of the result list
            List<List<Term>> splits = splitList(result);
            boolean found = false;
            for (List<Term> split : splits) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                Term leftList = buildListFromElements(split.subList(0, split.size()/2));
                Term rightList = buildListFromElements(split.subList(split.size()/2, split.size()));
                
                if (list1.unify(leftList, newBindings) && list2.unify(rightList, newBindings)) {
                    solutions.add(new HashMap<>(newBindings));
                    found = true;
                }
            }
            return found;
        } else {
            throw new PrologEvaluationException("append/3 not implemented for this case.");
        }
    }

    private Term concatenateLists(Term list1, Term list2) {
        List<Term> elements1 = new ArrayList<>();
        convertToList(list1, elements1);
        List<Term> elements2 = new ArrayList<>();
        convertToList(list2, elements2);

        List<Term> resultElements = new ArrayList<>(elements1);
        resultElements.addAll(elements2);

        return buildListFromElements(resultElements);
    }

    private void convertToList(Term term, List<Term> elements) {
        if (term instanceof Atom && ((Atom) term).getName().equals("[]")) {
            return;
        } else if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            if (compound.getName().equals(".")) {
                elements.add(compound.getArguments().get(0));
                convertToList(compound.getArguments().get(1), elements);
            }
        }
    }

    private Term buildListFromElements(List<Term> elements) {
        Term result = new Atom("[]");
        for (int i = elements.size() - 1; i >= 0; i--) {
            List<Term> args = new ArrayList<>();
            args.add(elements.get(i));
            args.add(result);
            result = new CompoundTerm(new Atom("."), args);
        }
        return result;
    }

    private List<List<Term>> splitList(Term list) {
        List<Term> elements = new ArrayList<>();
        convertToList(list, elements);
        
        List<List<Term>> splits = new ArrayList<>();

        for (int i = 0; i <= elements.size(); i++) {
            List<Term> left = new ArrayList<>(elements.subList(0, i));
            List<Term> right = new ArrayList<>(elements.subList(i, elements.size()));
            
            List<Term> combined = new ArrayList<>();
            combined.addAll(left);
            combined.addAll(right);
            splits.add(combined);
        }

        return splits;
    }
}
