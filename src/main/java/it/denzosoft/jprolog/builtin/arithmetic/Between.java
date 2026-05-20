package it.denzosoft.jprolog.builtin.arithmetic;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of between/3 predicate.
 * 
 * between(+Low, +High, ?Value)
 * 
 * True if Low ≤ Value ≤ High. On backtracking, Value is unified with all 
 * integer values between Low and High (inclusive).
 * 
 * Examples:
 * ?- between(1, 3, X).
 * X = 1 ;
 * X = 2 ;
 * X = 3.
 * 
 * ?- between(1, 3, 2).
 * true.
 * 
 * ?- between(1, 3, 5).
 * false.
 */
public class Between implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 3) {
            throw new PrologEvaluationException("between/3 requires exactly 3 arguments");
        }
        
        Term lowTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term highTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term valueTerm = query.getArguments().get(2);
        
        // START_CHANGE: Round5 - structured ISO error terms via ISOErrorTerms
        if (lowTerm instanceof Variable || highTerm instanceof Variable) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError("between/3"));
        }

        boolean highIsInfinity = false;
        if (highTerm instanceof it.denzosoft.jprolog.core.terms.Atom) {
            String n = ((it.denzosoft.jprolog.core.terms.Atom) highTerm).getName();
            if ("inf".equals(n) || "infinite".equals(n)) {
                highIsInfinity = true;
            } else {
                throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                    it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("integer", highTerm, "between/3"));
            }
        } else if (!(highTerm instanceof it.denzosoft.jprolog.core.terms.Number)) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("integer", highTerm, "between/3"));
        }
        if (!(lowTerm instanceof it.denzosoft.jprolog.core.terms.Number)) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("integer", lowTerm, "between/3"));
        }

        double lowValue = ((it.denzosoft.jprolog.core.terms.Number) lowTerm).getValue();
        if (Double.isInfinite(lowValue) || lowValue != Math.floor(lowValue)) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("integer", lowTerm, "between/3"));
        }
        long low = (long) lowValue;

        long high;
        if (highIsInfinity) {
            high = Long.MAX_VALUE;
        } else {
            double highValue = ((it.denzosoft.jprolog.core.terms.Number) highTerm).getValue();
            if (Double.isInfinite(highValue) || highValue != Math.floor(highValue)) {
                throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                    it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("integer", highTerm, "between/3"));
            }
            high = (long) highValue;
        // END_CHANGE: Round5
        }
        // END_CHANGE: ISS-2025-0209

        if (low > high) {
            return false;
        }

        Term resolvedValueTerm = valueTerm.resolveBindings(bindings);

        if (resolvedValueTerm instanceof Variable) {
            // START_CHANGE: ISS-2025-0209 - safety cap: refuse to materialize unbounded enumeration
            if (highIsInfinity) {
                // Enumeration must be lazy via backtracking; pre-materializing all solutions
                // for inf would exhaust memory. Cap at a configurable practical limit and warn.
                long cap = low + 1_000_000L;
                if (cap < low) cap = Long.MAX_VALUE; // overflow guard
                for (long i = low; i <= cap; i++) {
                    Map<String, Term> newBindings = new HashMap<>(bindings);
                    if (valueTerm.unify(new it.denzosoft.jprolog.core.terms.Number(i), newBindings)) {
                        solutions.add(newBindings);
                    }
                }
                return !solutions.isEmpty();
            }
            // END_CHANGE: ISS-2025-0209
            boolean foundSolution = false;
            for (long i = low; i <= high; i++) {
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (valueTerm.unify(new it.denzosoft.jprolog.core.terms.Number(i), newBindings)) {
                    solutions.add(newBindings);
                    foundSolution = true;
                }
            }
            return foundSolution;
        } else {
            if (!(resolvedValueTerm instanceof it.denzosoft.jprolog.core.terms.Number)) {
                return false;
            }

            double value = ((it.denzosoft.jprolog.core.terms.Number) resolvedValueTerm).getValue();
            if (value != Math.floor(value) || Double.isInfinite(value)) {
                return false;
            }

            long longValue = (long) value;
            if (longValue >= low && longValue <= high) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            return false;
        }
    }
}