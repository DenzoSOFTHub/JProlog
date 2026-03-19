package it.denzosoft.jprolog.builtin.arithmetic;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.exceptions.PrologException;

import java.util.HashMap;
import java.util.Map;
import java.util.function.BinaryOperator;

public class StandardArithmeticOperations {
    private static final Map<String, ArithmeticOperation> OPERATIONS = new HashMap<>();
    
    static {
        registerOperation("+", Double::sum);
        registerOperation("-", (a, b) -> a - b);
        registerOperation("*", (a, b) -> a * b);
        registerOperation("/", (a, b) -> {
            if (b == 0.0) {
                throw new PrologException(ISOErrorTerms.zeroDivisorError("(/)/2"));
            }
            return a / b;
        });
        // START_CHANGE: ISS-2025-0057 - Fix mod vs rem and add integer division //
        // ISO Prolog: mod(X,Y) = X - floor(X/Y) * Y (result has sign of Y)
        registerOperation("mod", (a, b) -> {
            if (b == 0.0) {
                throw new PrologException(ISOErrorTerms.zeroDivisorError("mod/2"));
            }
            return a - Math.floor(a / b) * b;
        });
        registerOperation("**", Math::pow);

        // ISO Prolog: rem(X,Y) = X - truncate(X/Y) * Y (result has sign of X)
        // Java's % operator implements rem semantics
        registerOperation("rem", (a, b) -> {
            if (b == 0.0) {
                throw new PrologException(ISOErrorTerms.zeroDivisorError("rem/2"));
            }
            return a % b;
        });

        // ISO Prolog: X // Y = truncate(X / Y) (integer division)
        registerOperation("//", (a, b) -> {
            if (b == 0.0) {
                throw new PrologException(ISOErrorTerms.zeroDivisorError("(//)/2"));
            }
            double result = a / b;
            return result >= 0 ? Math.floor(result) : Math.ceil(result);
        });
        // END_CHANGE: ISS-2025-0057
        
        // Bitwise operations (convert to int, operate, convert back)
        registerOperation("/\\", (a, b) -> (double)((int)a.doubleValue() & (int)b.doubleValue()));
        registerOperation("\\/", (a, b) -> (double)((int)a.doubleValue() | (int)b.doubleValue()));
        registerOperation("xor", (a, b) -> (double)((int)a.doubleValue() ^ (int)b.doubleValue()));
        
        // Bit shift operations
        registerOperation("<<", (a, b) -> (double)((int)a.doubleValue() << (int)b.doubleValue()));
        registerOperation(">>", (a, b) -> (double)((int)a.doubleValue() >> (int)b.doubleValue()));
        // END_CHANGE: ISS-2025-0017
    }
    
    private static void registerOperation(String symbol, BinaryOperator<Double> function) {
        OPERATIONS.put(symbol, new ArithmeticOperation() {
            @Override
            public double apply(double left, double right) {
                return function.apply(left, right);
            }
            
            @Override
            public String getSymbol() {
                return symbol;
            }
        });
    }
    
    public static ArithmeticOperation getOperation(String symbol) {
        return OPERATIONS.get(symbol);
    }
    
    public static boolean hasOperation(String symbol) {
        return OPERATIONS.containsKey(symbol);
    }
    
    public static void registerCustomOperation(String symbol, ArithmeticOperation operation) {
        OPERATIONS.put(symbol, operation);
    }
}
