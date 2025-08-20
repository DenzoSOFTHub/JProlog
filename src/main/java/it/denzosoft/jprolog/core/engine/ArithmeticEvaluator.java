package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.builtin.arithmetic.StandardArithmeticOperations;
import it.denzosoft.jprolog.builtin.arithmetic.ArithmeticOperation;
import it.denzosoft.jprolog.builtin.arithmetic.ISOArithmeticFunctions;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
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
        BINARY_OPERATIONS.put("*", (a, b) -> a * b);
        BINARY_OPERATIONS.put("/", (a, b) -> a / b);
        
        // Funzioni matematiche unarie standard
        UNARY_FUNCTIONS.put("sin", Math::sin);
        UNARY_FUNCTIONS.put("cos", Math::cos);
        UNARY_FUNCTIONS.put("tan", Math::tan);
        UNARY_FUNCTIONS.put("asin", Math::asin);
        UNARY_FUNCTIONS.put("acos", Math::acos);
        UNARY_FUNCTIONS.put("atan", Math::atan);
        UNARY_FUNCTIONS.put("sqrt", Math::sqrt);
        UNARY_FUNCTIONS.put("log", Math::log);
        UNARY_FUNCTIONS.put("exp", Math::exp);
        UNARY_FUNCTIONS.put("abs", Math::abs);
        UNARY_FUNCTIONS.put("ceil", Math::ceil);
        UNARY_FUNCTIONS.put("floor", Math::floor);
        UNARY_FUNCTIONS.put("round", (x) -> (double) Math.round(x));
        
        // ISO Prolog additional functions
        UNARY_FUNCTIONS.put("sign", Math::signum);
        UNARY_FUNCTIONS.put("truncate", x -> x < 0 ? Math.ceil(x) : Math.floor(x));
        UNARY_FUNCTIONS.put("float_integer_part", x -> Math.floor(Math.abs(x)) * Math.signum(x));
        UNARY_FUNCTIONS.put("float_fractional_part", x -> x - (Math.floor(Math.abs(x)) * Math.signum(x)));
        UNARY_FUNCTIONS.put("\\", x -> (double)(~(x.longValue()))); // Bitwise NOT
        UNARY_FUNCTIONS.put("random", x -> Math.random()); // Random number (ignores argument)
        
        // Register ISO arithmetic functions
        ISOArithmeticFunctions.registerAll();
    }

    /**
     * Registra una nuova funzione matematica binaria.
     * This method allows extending arithmetic functionality.
     * 
     * @param name Nome della funzione
     * @param function Implementazione della funzione
     */
    public static void registerBinaryOperation(String name, BiFunction<Double, Double, Double> function) {
        BINARY_OPERATIONS.put(name, function);
    }

    /**
     * Registra una nuova funzione matematica unaria.
     * This method allows extending arithmetic functionality.
     * 
     * @param name Nome della funzione
     * @param function Implementazione della funzione
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
    public static double evaluate(Term term, Map<String, Term> substitution) throws PrologEvaluationException {
        try {
            return evaluateTerm(term, substitution);
        } catch (ArithmeticException e) {
            throw new PrologEvaluationException("Arithmetic error evaluating expression: " + e.getMessage(), e);
        } catch (Exception e) {
            throw new PrologEvaluationException("Error evaluating arithmetic expression: " + e.getMessage(), e);
        }
    }

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
            // Other atoms in arithmetic context are treated as 0.0
            LOGGER.warning("Atom '" + atomName + "' in arithmetic context treated as 0.0");
            return 0.0;
        } else if (term instanceof CompoundTerm) {
            CompoundTerm compoundTerm = (CompoundTerm) term;
            
            // Gestione funzioni unarie
            if (compoundTerm.getArguments().size() == 1) {
                Function<Double, Double> function = UNARY_FUNCTIONS.get(compoundTerm.getName());
                if (function != null) {
                    double argValue = evaluateTerm(compoundTerm.getArguments().get(0), substitution);
                    return function.apply(argValue);
                }
            }
            
            // Gestione operatori binari
            if (compoundTerm.getArguments().size() == 2) {
                // Check legacy operations first
                BiFunction<Double, Double, Double> operation = BINARY_OPERATIONS.get(compoundTerm.getName());
                if (operation != null) {
                    double left = evaluateTerm(compoundTerm.getArguments().get(0), substitution);
                    double right = evaluateTerm(compoundTerm.getArguments().get(1), substitution);
                    return operation.apply(left, right);
                }
                
                // Check StandardArithmeticOperations
                ArithmeticOperation standardOp = StandardArithmeticOperations.getOperation(compoundTerm.getName());
                if (standardOp != null) {
                    double left = evaluateTerm(compoundTerm.getArguments().get(0), substitution);
                    double right = evaluateTerm(compoundTerm.getArguments().get(1), substitution);
                    return standardOp.apply(left, right);
                }
            }
            
            throw new PrologEvaluationException("Unknown arithmetic function or incorrect arity: " + compoundTerm.getName());
        } else {
            throw new PrologEvaluationException("Cannot evaluate term in arithmetic context: " + term);
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
