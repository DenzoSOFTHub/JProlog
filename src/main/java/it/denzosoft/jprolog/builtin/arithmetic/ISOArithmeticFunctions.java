package it.denzosoft.jprolog.builtin.arithmetic;

import java.util.Random;

/**
 * ISO Prolog arithmetic functions that were missing from the implementation.
 * These functions extend the arithmetic capabilities to be ISO compliant.
 */
public class ISOArithmeticFunctions {
    
    private static final Random RANDOM = new Random();
    
    /**
     * Register all ISO arithmetic functions with the ArithmeticEvaluator
     */
    public static void registerAll() {
        // Already handled by ArithmeticEvaluator, but we add the missing ones
        
        // Binary functions
        registerBinaryFunctions();
        
        // Unary functions
        registerUnaryFunctions();
        
        // Constants (as nullary functions)
        registerConstants();
    }
    
    private static void registerBinaryFunctions() {
        // max/2 - Maximum of two numbers
        StandardArithmeticOperations.registerCustomOperation("max", new ArithmeticOperation() {
            @Override
            public double apply(double left, double right) {
                return Math.max(left, right);
            }
            
            @Override
            public String getSymbol() {
                return "max";
            }
        });
        
        // min/2 - Minimum of two numbers
        StandardArithmeticOperations.registerCustomOperation("min", new ArithmeticOperation() {
            @Override
            public double apply(double left, double right) {
                return Math.min(left, right);
            }
            
            @Override
            public String getSymbol() {
                return "min";
            }
        });
        
        // div/2 - Integer division
        StandardArithmeticOperations.registerCustomOperation("div", new ArithmeticOperation() {
            @Override
            public double apply(double left, double right) {
                if (right == 0) {
                    throw new ArithmeticException("Division by zero");
                }
                return Math.floor(left / right);
            }
            
            @Override
            public String getSymbol() {
                return "div";
            }
        });
        
        // rem/2 - Remainder (different from mod for negative numbers)
        StandardArithmeticOperations.registerCustomOperation("rem", new ArithmeticOperation() {
            @Override
            public double apply(double left, double right) {
                if (right == 0) {
                    throw new ArithmeticException("Division by zero");
                }
                return left - (Math.floor(left / right) * right);
            }
            
            @Override
            public String getSymbol() {
                return "rem";
            }
        });
        
        // atan2/2 - Two-argument arctangent
        StandardArithmeticOperations.registerCustomOperation("atan2", new ArithmeticOperation() {
            @Override
            public double apply(double y, double x) {
                return Math.atan2(y, x);
            }
            
            @Override
            public String getSymbol() {
                return "atan2";
            }
        });
        
        // xor/2 - Bitwise XOR (for integers)
        StandardArithmeticOperations.registerCustomOperation("xor", new ArithmeticOperation() {
            @Override
            public double apply(double left, double right) {
                return (long)left ^ (long)right;
            }
            
            @Override
            public String getSymbol() {
                return "xor";
            }
        });
        
        // Bitwise operations
        StandardArithmeticOperations.registerCustomOperation("\\/", new ArithmeticOperation() {
            @Override
            public double apply(double left, double right) {
                return (long)left | (long)right;
            }
            
            @Override
            public String getSymbol() {
                return "\\/";
            }
        });
        
        StandardArithmeticOperations.registerCustomOperation("/\\", new ArithmeticOperation() {
            @Override
            public double apply(double left, double right) {
                return (long)left & (long)right;
            }
            
            @Override
            public String getSymbol() {
                return "/\\";
            }
        });
        
        StandardArithmeticOperations.registerCustomOperation(">>", new ArithmeticOperation() {
            @Override
            public double apply(double left, double right) {
                return (long)left >> (long)right;
            }
            
            @Override
            public String getSymbol() {
                return ">>";
            }
        });
        
        StandardArithmeticOperations.registerCustomOperation("<<", new ArithmeticOperation() {
            @Override
            public double apply(double left, double right) {
                return (long)left << (long)right;
            }
            
            @Override
            public String getSymbol() {
                return "<<";
            }
        });
    }
    
    private static void registerUnaryFunctions() {
        // These need to be registered with the ArithmeticEvaluator
        // We'll create a separate registrar class for that
    }
    
    private static void registerConstants() {
        // Constants will be handled as special atoms in the ArithmeticEvaluator
    }
    
    /**
     * sign/1 - Sign of a number (-1, 0, or 1)
     */
    public static double sign(double x) {
        return Math.signum(x);
    }
    
    /**
     * float_integer_part/1 - Integer part of a float
     */
    public static double floatIntegerPart(double x) {
        return Math.floor(Math.abs(x)) * Math.signum(x);
    }
    
    /**
     * float_fractional_part/1 - Fractional part of a float
     */
    public static double floatFractionalPart(double x) {
        return x - floatIntegerPart(x);
    }
    
    /**
     * truncate/1 - Truncate towards zero
     */
    public static double truncate(double x) {
        return x < 0 ? Math.ceil(x) : Math.floor(x);
    }
    
    /**
     * random/0 - Random float between 0 and 1
     */
    public static double random() {
        return RANDOM.nextDouble();
    }
    
    /**
     * random_between/2 - Random integer between Low and High (inclusive)
     */
    public static long randomBetween(long low, long high) {
        if (low > high) {
            throw new IllegalArgumentException("Low must be <= High");
        }
        return low + RANDOM.nextLong() % (high - low + 1);
    }
    
    /**
     * Mathematical constants
     */
    public static double pi() {
        return Math.PI;
    }
    
    public static double e() {
        return Math.E;
    }
}