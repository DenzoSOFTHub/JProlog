package it.denzosoft.jprolog.builtin.arithmetic;

import it.denzosoft.jprolog.core.engine.ArithmeticEvaluator;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
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
            if ((num1.isBigInteger() || num2.isBigInteger()) && num1.isInteger() && num2.isInteger()) {
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

                switch (type) {
                    case EQUAL:
                        result = Math.abs(value1 - value2) < 1e-10; // Use epsilon for double comparison
                        break;
                    case NOT_EQUAL:
                        result = Math.abs(value1 - value2) >= 1e-10;
                        break;
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
        } catch (Exception e) {
            throw new PrologEvaluationException("Error in arithmetic comparison: " + e.getMessage(), e);
        }
    }
}
