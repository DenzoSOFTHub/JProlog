package it.denzosoft.jprolog.builtin;

import it.denzosoft.jprolog.ArithmeticEvaluator;
import it.denzosoft.jprolog.BuiltIn;
import it.denzosoft.jprolog.PrologEvaluationException;
import it.denzosoft.jprolog.terms.Number;
import it.denzosoft.jprolog.terms.Term;
import it.denzosoft.jprolog.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class Is implements BuiltIn {
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("is/2 requires exactly 2 arguments.");
        }

        Term variableTerm = query.getArguments().get(0);
        Term expressionTerm = query.getArguments().get(1);

        if (!(variableTerm instanceof Variable)) {
            throw new PrologEvaluationException("First argument of 'is' must be a variable.");
        }

        try {
            double result = ArithmeticEvaluator.evaluate(expressionTerm, bindings);
            Variable variable = (Variable) variableTerm;
            
            // Create a new bindings map with the variable bound to the result
            Map<String, Term> newBindings = new HashMap<>(bindings);
            newBindings.put(variable.getName(), new Number(result));
            
            solutions.add(newBindings);
            return true;
        } catch (IllegalArgumentException e) {
            // Return false for arithmetic errors instead of throwing exception
            return false;
        }
    }
}
