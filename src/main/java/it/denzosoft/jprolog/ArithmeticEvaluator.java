package it.denzosoft.jprolog;

import it.denzosoft.jprolog.terms.Atom;
import it.denzosoft.jprolog.terms.CompoundTerm;
import it.denzosoft.jprolog.terms.Number;
import it.denzosoft.jprolog.terms.Term;
import it.denzosoft.jprolog.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;

public class ArithmeticEvaluator {
    private static final Map<String, BiFunction<Double, Double, Double>> OPERATIONS = new HashMap<>();

    static {
        OPERATIONS.put("+", Double::sum);
        OPERATIONS.put("-", (a, b) -> a - b);
        OPERATIONS.put("*", (a, b) -> a * b);
        OPERATIONS.put("/", (a, b) -> a / b);
    }

    /**
     * Evaluates an arithmetic expression term.
     * 
     * @param term The term to evaluate
     * @param substitution Variable bindings
     * @return The numeric result
     * @throws PrologEvaluationException if evaluation fails
     */
    public static double evaluate(Term term, Map<String, Term> substitution) {
        try {
            return evaluateTerm(term, substitution);
        } catch (Exception e) {
            throw new PrologEvaluationException("Error evaluating arithmetic expression: " + e.getMessage(), e);
        }
    }

    private static double evaluateTerm(Term term, Map<String, Term> substitution) {
        if (term instanceof Number) {
            return ((Number) term).getValue();
        } else if (term instanceof Variable) {
            Term value = resolveVariable((Variable) term, substitution);
            if (value == null) {
                throw new PrologEvaluationException("Unbound variable in arithmetic expression: " + ((Variable) term).getName());
            }
            return evaluateTerm(value, substitution);
        } else if (term instanceof CompoundTerm) {
            return evaluateCompoundTerm((CompoundTerm) term, substitution);
        } else {
            throw new PrologEvaluationException("Cannot evaluate term: " + term);
        }
    }

    private static double evaluateCompoundTerm(CompoundTerm compoundTerm, Map<String, Term> substitution) {
        String functorString = compoundTerm.getFunctor().getName();
        List<Term> args = compoundTerm.getArguments();

        BiFunction<Double, Double, Double> operation = OPERATIONS.get(functorString);
        if (operation != null && args.size() == 2) {
            return operation.apply(evaluateTerm(args.get(0), substitution), evaluateTerm(args.get(1), substitution));
        } else {
            throw new PrologEvaluationException("Unknown arithmetic operator or incorrect arity: " + functorString);
        }
    }

    private static Term resolveVariable(Variable variable, Map<String, Term> substitution) {
        Term value = substitution.get(variable.getName());
        if (value instanceof Variable && value != variable) {
            return resolveVariable((Variable) value, substitution); // Recursive resolution
        }
        return value;
    }
}
