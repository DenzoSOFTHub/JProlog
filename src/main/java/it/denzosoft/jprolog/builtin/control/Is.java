package it.denzosoft.jprolog.builtin.control;

import it.denzosoft.jprolog.core.engine.ArithmeticEvaluator;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

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

        // START_CHANGE: ISS-2025-0074 - Support bound variables and numbers in is/2
        // Resolve bindings on the first argument for ISO compliance:
        // - If unbound Variable, bind it to the result
        // - If bound to a Number (or a Number literal), check equality with result
        variableTerm = variableTerm.resolveBindings(bindings);

        try {
            // START_CHANGE: LIM-008 - Use evaluateToNumber to preserve integer/float type
            Number resultNum = ArithmeticEvaluator.evaluateToNumber(expressionTerm, bindings);
            // END_CHANGE: LIM-008

            if (variableTerm instanceof Variable) {
                // Unbound variable: bind it to the result
                Variable variable = (Variable) variableTerm;
                Map<String, Term> newBindings = new HashMap<>(bindings);
                newBindings.put(variable.getName(), resultNum);
                solutions.add(newBindings);
                return true;
            } else if (variableTerm instanceof Number) {
                // Already bound to a number or a number literal: check equality
                Number existingNum = (Number) variableTerm;
                // START_CHANGE: LIM-008 - Compare with BigInteger awareness
                boolean equal;
                if (existingNum.isBigInteger() || resultNum.isBigInteger()) {
                    if (existingNum.isInteger() && resultNum.isInteger()) {
                        equal = existingNum.bigIntegerValue().equals(resultNum.bigIntegerValue());
                    } else {
                        equal = existingNum.doubleValue() == resultNum.doubleValue();
                    }
                } else {
                    equal = existingNum.doubleValue() == resultNum.doubleValue();
                }
                // END_CHANGE: LIM-008
                if (equal) {
                    solutions.add(new HashMap<>(bindings));
                    return true;
                } else {
                    return false;
                }
            } else {
                // First argument resolved to a non-numeric term: type error per ISO
                throw new PrologEvaluationException(
                    "is/2: first argument must be a variable or a number, got: " + variableTerm);
            }
        // START_CHANGE: ISS-2025-0182 - Built-in predicate bug fixes
        // Re-throw PrologExceptions instead of swallowing them
        } catch (PrologException e) {
            throw e;
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            return false;
        }
        // END_CHANGE: ISS-2025-0182
        // END_CHANGE: ISS-2025-0074
    }
}
