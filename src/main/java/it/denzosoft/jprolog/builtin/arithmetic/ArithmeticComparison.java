package it.denzosoft.jprolog.builtin.arithmetic;

import it.denzosoft.jprolog.core.engine.ArithmeticEvaluator;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
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
            double value1 = ArithmeticEvaluator.evaluate(expr1, bindings);
            double value2 = ArithmeticEvaluator.evaluate(expr2, bindings);
            
            boolean result;
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
