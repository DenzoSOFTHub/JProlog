package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.builtin.arithmetic.StandardArithmeticOperations;
import it.denzosoft.jprolog.builtin.arithmetic.ArithmeticOperation;
import it.denzosoft.jprolog.builtin.arithmetic.ISOArithmeticFunctions;
import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.logging.Logger;



public class ArithmeticEvaluator {
    private static final Logger LOGGER = Logger.getLogger(ArithmeticEvaluator.class.getName());
    
    // Existing binary operations
    private static final Map<String, BiFunction<Double, Double, Double>> BINARY_OPERATIONS = new HashMap<>();
    
    // Unary mathematical functions
    private static final Map<String, Function<Double, Double>> UNARY_FUNCTIONS = new HashMap<>();

    static {
        // Operatori binari esistenti
        BINARY_OPERATIONS.put("+", Double::sum);
        BINARY_OPERATIONS.put("-", (a, b) -> a - b);
        // START_CHANGE: ISS-2025-0164 - Detect overflow in multiplication
        BINARY_OPERATIONS.put("*", (a, b) -> {
            double result = a * b;
            if (Double.isInfinite(result) && !Double.isInfinite(a) && !Double.isInfinite(b)) {
                throw new PrologException(ISOErrorTerms.floatOverflowError("(*)/2"));
            }
            return result;
        });
        // END_CHANGE: ISS-2025-0164
        BINARY_OPERATIONS.put("/", (a, b) -> {
            if (b == 0.0) {
                throw new PrologException(ISOErrorTerms.zeroDivisorError("(/)/2"));
            }
            double result = a / b;
            // START_CHANGE: ISS-2025-0164 - Detect overflow/NaN in division
            if (Double.isInfinite(result) && !Double.isInfinite(a)) {
                throw new PrologException(ISOErrorTerms.floatOverflowError("(/)/2"));
            }
            // END_CHANGE: ISS-2025-0164
            return result;
        });
        
        // Funzioni matematiche unarie standard
        UNARY_FUNCTIONS.put("sin", Math::sin);
        UNARY_FUNCTIONS.put("cos", Math::cos);
        UNARY_FUNCTIONS.put("tan", Math::tan);
        UNARY_FUNCTIONS.put("asin", x -> {
            if (x < -1.0 || x > 1.0) {
                throw new PrologException(ISOErrorTerms.evaluationError("undefined", "asin/1"));
            }
            return Math.asin(x);
        });
        UNARY_FUNCTIONS.put("acos", x -> {
            if (x < -1.0 || x > 1.0) {
                throw new PrologException(ISOErrorTerms.evaluationError("undefined", "acos/1"));
            }
            return Math.acos(x);
        });
        UNARY_FUNCTIONS.put("atan", Math::atan);
        UNARY_FUNCTIONS.put("sqrt", x -> {
            if (x < 0) {
                throw new PrologException(ISOErrorTerms.evaluationError("undefined", "sqrt/1"));
            }
            return Math.sqrt(x);
        });
        UNARY_FUNCTIONS.put("log", x -> {
            if (x <= 0) {
                throw new PrologException(ISOErrorTerms.evaluationError("undefined", "log/1"));
            }
            return Math.log(x);
        });
        UNARY_FUNCTIONS.put("exp", Math::exp);
        UNARY_FUNCTIONS.put("abs", Math::abs);
        UNARY_FUNCTIONS.put("ceil", Math::ceil);
        UNARY_FUNCTIONS.put("ceiling", Math::ceil);  // ISO standard name
        UNARY_FUNCTIONS.put("floor", Math::floor);
        UNARY_FUNCTIONS.put("round", (x) -> (double) Math.round(x));
        
        // ISO Prolog additional functions
        UNARY_FUNCTIONS.put("sign", Math::signum);
        UNARY_FUNCTIONS.put("truncate", x -> x < 0 ? Math.ceil(x) : Math.floor(x));
        UNARY_FUNCTIONS.put("float_integer_part", x -> Math.floor(Math.abs(x)) * Math.signum(x));
        UNARY_FUNCTIONS.put("float_fractional_part", x -> x - (Math.floor(Math.abs(x)) * Math.signum(x)));
        // START_CHANGE: ISS-2025-0169 - Validate integer type before bitwise NOT to prevent precision loss
        UNARY_FUNCTIONS.put("\\", x -> {
            if (x != Math.floor(x) || Double.isInfinite(x)) {
                throw new PrologException(ISOErrorTerms.typeError("integer",
                    new Number(x), "(\\)/1"));
            }
            return (double)(~(x.longValue()));
        }); // Bitwise NOT
        // END_CHANGE: ISS-2025-0169
        UNARY_FUNCTIONS.put("random", x -> Math.random()); // Random number (ignores argument)
        UNARY_FUNCTIONS.put("float", x -> x.doubleValue()); // ISO: convert to float

        // START_CHANGE: ISS-2025-0170 - Add msb/1, lsb/1, popcount/1 ISO arithmetic functions
        UNARY_FUNCTIONS.put("msb", x -> {
            if (x != Math.floor(x) || Double.isInfinite(x) || x <= 0) {
                throw new PrologException(ISOErrorTerms.typeError("integer", new Atom(String.valueOf(x)), "msb/1"));
            }
            long longVal = x.longValue();
            return (double)(63 - Long.numberOfLeadingZeros(longVal));
        });
        UNARY_FUNCTIONS.put("lsb", x -> {
            if (x != Math.floor(x) || Double.isInfinite(x) || x <= 0) {
                throw new PrologException(ISOErrorTerms.typeError("integer", new Atom(String.valueOf(x)), "lsb/1"));
            }
            long longVal = x.longValue();
            return (double) Long.numberOfTrailingZeros(longVal);
        });
        UNARY_FUNCTIONS.put("popcount", x -> {
            if (x != Math.floor(x) || Double.isInfinite(x) || x < 0) {
                throw new PrologException(ISOErrorTerms.typeError("integer", new Atom(String.valueOf(x)), "popcount/1"));
            }
            long longVal = x.longValue();
            return (double) Long.bitCount(longVal);
        });
        // END_CHANGE: ISS-2025-0170

        // START_CHANGE: ISS-2025-0105 - Unary minus and plus for prefix expressions like -X, +X
        UNARY_FUNCTIONS.put("-", x -> -x);
        UNARY_FUNCTIONS.put("+", x -> x);
        // END_CHANGE: ISS-2025-0105

        // Register ISO arithmetic functions
        ISOArithmeticFunctions.registerAll();
    }

    /**
     * Registra una nuova funzione matematica binaria.
     * This method allows extending arithmetic functionality.
     * 
     * @param name Function name
     * @param function Function implementation
     */
    public static void registerBinaryOperation(String name, BiFunction<Double, Double, Double> function) {
        BINARY_OPERATIONS.put(name, function);
    }

    /**
     * Registra una nuova funzione matematica unaria.
     * This method allows extending arithmetic functionality.
     * 
     * @param name Function name
     * @param function Function implementation
     */
    public static void registerUnaryFunction(String name, Function<Double, Double> function) {
        UNARY_FUNCTIONS.put(name, function);
    }

    /**
     * Evaluates an arithmetic expression term.
     * 
     * @param term The term to evaluate
     * @param substitution Variable bindings
     * @return The numeric result
     * @throws PrologEvaluationException if evaluation fails
     */
    // START_CHANGE: ISS-2025-0163 - Let PrologException pass through for ISO error terms
    public static double evaluate(Term term, Map<String, Term> substitution) throws PrologEvaluationException {
        try {
            return evaluateTerm(term, substitution);
        } catch (PrologException e) {
            throw e; // Preserve ISO error term structure for catch/3
        } catch (ArithmeticException e) {
            throw new PrologEvaluationException("Arithmetic error evaluating expression: " + e.getMessage(), e);
        } catch (Exception e) {
            throw new PrologEvaluationException("Error evaluating arithmetic expression: " + e.getMessage(), e);
        }
    }
    // END_CHANGE: ISS-2025-0163

    private static double evaluateTerm(Term term, Map<String, Term> substitution) throws PrologEvaluationException {
        if (term instanceof Number) {
            return ((Number) term).getValue();
        } else if (term instanceof Variable) {
            Term value = resolveVariable((Variable) term, substitution);
            if (value == null) {
                throw new PrologEvaluationException("Unbound variable in arithmetic expression: " + ((Variable) term).getName());
            }
            return evaluateTerm(value, substitution);
        } else if (term instanceof Atom) {
            // Check for mathematical constants
            String atomName = ((Atom) term).getName();
            if ("pi".equals(atomName)) {
                return Math.PI;
            } else if ("e".equals(atomName)) {
                return Math.E;
            }
            // START_CHANGE: ISS-2025-0163 - Throw type_error for unknown atoms instead of silent 0.0
            if ("inf".equals(atomName) || "infinity".equals(atomName)) {
                return Double.POSITIVE_INFINITY;
            } else if ("nan".equals(atomName)) {
                return Double.NaN;
            }
            // ISO Prolog: unknown atom in arithmetic context is a type_error
            throw new PrologException(ISOErrorTerms.typeError("evaluable", new Atom(atomName + "/0"), "is/2"));
        // START_CHANGE: ISS-2025-0104 - Cache getArguments()/getName() to avoid repeated calls
        } else if (term instanceof CompoundTerm) {
            CompoundTerm compoundTerm = (CompoundTerm) term;
            List<Term> args = compoundTerm.getArguments();
            int arity = args.size();
            String name = compoundTerm.getName();

            // Gestione funzioni unarie
            if (arity == 1) {
                Function<Double, Double> function = UNARY_FUNCTIONS.get(name);
                if (function != null) {
                    double argValue = evaluateTerm(args.get(0), substitution);
                    return function.apply(argValue);
                }
            }

            // Gestione operatori binari
            if (arity == 2) {
                // Check legacy operations first
                BiFunction<Double, Double, Double> operation = BINARY_OPERATIONS.get(name);
                if (operation != null) {
                    double left = evaluateTerm(args.get(0), substitution);
                    double right = evaluateTerm(args.get(1), substitution);
                    return operation.apply(left, right);
                }

                // Check StandardArithmeticOperations
                ArithmeticOperation standardOp = StandardArithmeticOperations.getOperation(name);
                if (standardOp != null) {
                    double left = evaluateTerm(args.get(0), substitution);
                    double right = evaluateTerm(args.get(1), substitution);
                    return standardOp.apply(left, right);
                }
            }

            throw new PrologEvaluationException("Unknown arithmetic function or incorrect arity: " + name);
        // END_CHANGE: ISS-2025-0104
        } else {
            throw new PrologEvaluationException("Cannot evaluate term in arithmetic context: " + term);
        }
    }

    // START_CHANGE: ISS-2025-0091 - Iterative variable resolution to prevent stack overflow
    // START_CHANGE: ISS-2025-0166 - Circular variable binding detection with depth limit
    private static final int MAX_RESOLVE_DEPTH = 64;

    private static Term resolveVariable(Variable variable, Map<String, Term> substitution) {
        Term current = substitution.get(variable.getName());
        int depth = 0;
        while (current instanceof Variable && current != variable) {
            if (++depth > MAX_RESOLVE_DEPTH) {
                throw new PrologException(ISOErrorTerms.resourceError(
                    "circular_binding", "Circular variable binding detected (depth " + MAX_RESOLVE_DEPTH + ") resolving: " + variable.getName()));
            }
            Term next = substitution.get(((Variable) current).getName());
            if (next == null) break;
            current = next;
        }
        return current;
    }
    // END_CHANGE: ISS-2025-0166
    // END_CHANGE: ISS-2025-0091
}
