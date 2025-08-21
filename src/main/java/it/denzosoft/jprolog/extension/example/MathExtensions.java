package it.denzosoft.jprolog.extension.example;

import it.denzosoft.jprolog.core.engine.ArithmeticEvaluator;


public class MathExtensions {
    
    /**
     * Registers additional custom mathematical functions.
     * This method can be called at application startup
     * to extend Prolog interpreter capabilities.
     */
    public static void registerCustomFunctions() {
        // Power function (e.g. pow(2, 3) = 8)
        ArithmeticEvaluator.registerBinaryOperation("pow", Math::pow);
        
        // Base 10 logarithm function
        ArithmeticEvaluator.registerUnaryFunction("log10", Math::log10);
        
        // Maximum value function between two numbers
        ArithmeticEvaluator.registerBinaryOperation("max", Math::max);
        
        // Minimum value function between two numbers
        ArithmeticEvaluator.registerBinaryOperation("min", Math::min);
        
        // Factorial function (recursive implementation)
        ArithmeticEvaluator.registerUnaryFunction("fact", x -> {
            if (x < 0) return Double.NaN;
            int n = (int) Math.round(x); // Fixed: properly convert double to int
            double result = 1;
            for (int i = 2; i <= n; i++) {
                result *= i;
            }
            return result;
        });
        
        // Degrees to radians conversion function
        ArithmeticEvaluator.registerUnaryFunction("degToRad", Math::toRadians);
        
        // Radians to degrees conversion function
        ArithmeticEvaluator.registerUnaryFunction("radToDeg", Math::toDegrees);
    }
    
    /**
     * Usage example of mathematical extension.
     * This method shows how to use the registered functions.
     */
    public static void demonstrateUsage() {
        System.out.println("Usage example of mathematical extensions:");
        System.out.println("================================================");
        
        // Note: To actually test these functions,
        // it would be necessary to integrate them with the parser and Prolog engine
        System.out.println("Registered functions:");
        System.out.println("- pow(X, Y): Raises X to the power of Y");
        System.out.println("- log10(X): Base 10 logarithm of X");
        System.out.println("- max(X, Y): Maximum value between X and Y");
        System.out.println("- min(X, Y): Minimum value between X and Y");
        System.out.println("- fact(X): Factorial of X (for integers)");
        System.out.println("- degToRad(X): Converts X from degrees to radians");
        System.out.println("- radToDeg(X): Converts X from radians to degrees");
        
        System.out.println("\nExamples of Prolog queries that could be executed:");
        System.out.println("?- X is pow(2, 3).              % Result: X = 8.0");
        System.out.println("?- X is log10(100).             % Result: X = 2.0");
        System.out.println("?- X is max(5, 12).             % Result: X = 12.0");
        System.out.println("?- X is fact(5).                % Result: X = 120.0");
        System.out.println("?- X is degToRad(180).          % Result: X = 3.14159...");
    }
}
