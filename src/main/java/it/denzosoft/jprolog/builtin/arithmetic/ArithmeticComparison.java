package it.denzosoft.jprolog.builtin.arithmetic;

import it.denzosoft.jprolog.core.engine.ArithmeticEvaluator;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class ArithmeticComparison implements BuiltIn {
    public enum ComparisonType {
        EQUAL("=:="),
        NOT_EQUAL("=\\="),
        LESS("<"),
        LESS_EQUAL("=<"),
        GREATER(">"),
        GREATER_EQUAL(">=");

        private final String symbol;

        ComparisonType(String symbol) {
            this.symbol = symbol;
        }

        public String getSymbol() {
            return symbol;
        }
    }

    private final ComparisonType type;

    public ArithmeticComparison(ComparisonType type) {
        this.type = type;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException(type.getSymbol() + "/2 requires exactly 2 arguments.");
        }

        Term expr1 = query.getArguments().get(0);
        Term expr2 = query.getArguments().get(1);

        try {
            // START_CHANGE: LIM-008 - Use evaluateToNumber for BigInteger-aware comparison
            Number num1 = ArithmeticEvaluator.evaluateToNumber(expr1, bindings);
            Number num2 = ArithmeticEvaluator.evaluateToNumber(expr2, bindings);

            boolean result;
            // Use BigInteger comparison for large integers
            // START_CHANGE: ISS-2025-0189 - Use exact integer comparison for all integer pairs
            if (num1.isInteger() && num2.isInteger()) {
                int cmp = num1.bigIntegerValue().compareTo(num2.bigIntegerValue());
                switch (type) {
                    case EQUAL:       result = cmp == 0; break;
                    case NOT_EQUAL:   result = cmp != 0; break;
                    case LESS:        result = cmp < 0;  break;
                    case LESS_EQUAL:  result = cmp <= 0; break;
                    case GREATER:     result = cmp > 0;  break;
                    case GREATER_EQUAL: result = cmp >= 0; break;
                    default: throw new PrologEvaluationException("Unknown arithmetic comparison type: " + type);
                }
            } else {
                double value1 = num1.doubleValue();
                double value2 = num2.doubleValue();

                // START_CHANGE: ISS-2025-0182 - Built-in predicate bug fixes
                // ISO Prolog requires exact comparison, not epsilon-based
                switch (type) {
                    // START_CHANGE: ISS-2025-0274 - use IEEE-754 == / != (not Double.compare) so
                    // -0.0 =:= 0.0 succeeds and nan =:= nan fails (Double.compare gives -0.0<0.0
                    // and NaN==NaN).
                    case EQUAL:
                        result = value1 == value2;
                        break;
                    case NOT_EQUAL:
                        result = value1 != value2;
                        break;
                    // END_CHANGE: ISS-2025-0274
                // END_CHANGE: ISS-2025-0182
                    case LESS:
                        result = value1 < value2;
                        break;
                    case LESS_EQUAL:
                        result = value1 <= value2;
                        break;
                    case GREATER:
                        result = value1 > value2;
                        break;
                    case GREATER_EQUAL:
                        result = value1 >= value2;
                        break;
                    default:
                        throw new PrologEvaluationException("Unknown arithmetic comparison type: " + type);
                }
            }
            // END_CHANGE: LIM-008

            if (result) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            return false;
        // START_CHANGE: ISS-2025-0248 - Preserve ISO error terms (instantiation_error,
        // type_error(evaluable,_), evaluation_error(zero_divisor)) raised by the evaluator;
        // the generic catch below would otherwise re-wrap them in a bare-atom message.
        } catch (PrologException e) {
            throw e;
        // END_CHANGE: ISS-2025-0248
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw new PrologEvaluationException("Error in arithmetic comparison: " + e.getMessage(), e);
        }
    }
}
