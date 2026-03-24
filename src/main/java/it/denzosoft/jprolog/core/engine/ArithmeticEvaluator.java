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

import java.math.BigInteger;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.logging.Logger;



public class ArithmeticEvaluator {
    private static final Logger LOGGER = Logger.getLogger(ArithmeticEvaluator.class.getName());

    // Existing binary operations
    private static final Map<String, BiFunction<Double, Double, Double>> BINARY_OPERATIONS = new HashMap<>();

    // Unary mathematical functions
    private static final Map<String, Function<Double, Double>> UNARY_FUNCTIONS = new HashMap<>();

    // START_CHANGE: LIM-008 - Operations that must always produce integer results
    private static final Set<String> INTEGER_BINARY_OPS = new HashSet<>();
    private static final Set<String> INTEGER_UNARY_OPS = new HashSet<>();
    private static final Set<String> FLOAT_UNARY_OPS = new HashSet<>();
    // END_CHANGE: LIM-008

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

        // START_CHANGE: LIM-008 - Classify operations by result type
        // Binary operations that always produce integer results (when inputs are integers)
        INTEGER_BINARY_OPS.add("//");
        INTEGER_BINARY_OPS.add("mod");
        INTEGER_BINARY_OPS.add("rem");
        INTEGER_BINARY_OPS.add("div");
        INTEGER_BINARY_OPS.add("/\\");
        INTEGER_BINARY_OPS.add("\\/");
        INTEGER_BINARY_OPS.add("xor");
        INTEGER_BINARY_OPS.add("<<");
        INTEGER_BINARY_OPS.add(">>");

        // Unary operations that always produce integer results
        INTEGER_UNARY_OPS.add("truncate");
        INTEGER_UNARY_OPS.add("round");
        INTEGER_UNARY_OPS.add("ceiling");
        INTEGER_UNARY_OPS.add("ceil");
        INTEGER_UNARY_OPS.add("floor");
        INTEGER_UNARY_OPS.add("sign");  // sign of integer is integer in ISO
        INTEGER_UNARY_OPS.add("\\");    // bitwise NOT

        // Unary operations that always produce float results
        FLOAT_UNARY_OPS.add("sin");
        FLOAT_UNARY_OPS.add("cos");
        FLOAT_UNARY_OPS.add("tan");
        FLOAT_UNARY_OPS.add("asin");
        FLOAT_UNARY_OPS.add("acos");
        FLOAT_UNARY_OPS.add("atan");
        FLOAT_UNARY_OPS.add("sqrt");
        FLOAT_UNARY_OPS.add("log");
        FLOAT_UNARY_OPS.add("exp");
        FLOAT_UNARY_OPS.add("float");
        FLOAT_UNARY_OPS.add("float_integer_part");
        FLOAT_UNARY_OPS.add("float_fractional_part");
        // END_CHANGE: LIM-008
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

    // START_CHANGE: LIM-008 - New method returning Number with integer/float type preserved
    /**
     * Evaluates an arithmetic expression term and returns a Number that preserves
     * the integer/float distinction. Integer operations on integer operands produce
     * integer results; overflow from long is handled with BigInteger.
     *
     * @param term The term to evaluate
     * @param substitution Variable bindings
     * @return A Number term with the correct integer/float type
     * @throws PrologEvaluationException if evaluation fails
     */
    public static Number evaluateToNumber(Term term, Map<String, Term> substitution) throws PrologEvaluationException {
        try {
            return evaluateTermToNumber(term, substitution);
        } catch (PrologException e) {
            throw e;
        } catch (ArithmeticException e) {
            throw new PrologEvaluationException("Arithmetic error evaluating expression: " + e.getMessage(), e);
        } catch (Exception e) {
            throw new PrologEvaluationException("Error evaluating arithmetic expression: " + e.getMessage(), e);
        }
    }

    /**
     * Core evaluation that returns Number preserving integer/float type.
     */
    private static Number evaluateTermToNumber(Term term, Map<String, Term> substitution) throws PrologEvaluationException {
        if (term instanceof Number) {
            return (Number) term;
        } else if (term instanceof Variable) {
            Term value = resolveVariable((Variable) term, substitution);
            if (value == null) {
                throw new PrologEvaluationException("Unbound variable in arithmetic expression: " + ((Variable) term).getName());
            }
            return evaluateTermToNumber(value, substitution);
        } else if (term instanceof Atom) {
            String atomName = ((Atom) term).getName();
            if ("pi".equals(atomName)) {
                return new Number(Math.PI, false);
            } else if ("e".equals(atomName)) {
                return new Number(Math.E, false);
            }
            if ("inf".equals(atomName) || "infinity".equals(atomName)) {
                return new Number(Double.POSITIVE_INFINITY, false);
            } else if ("nan".equals(atomName)) {
                return new Number(Double.NaN, false);
            }
            throw new PrologException(ISOErrorTerms.typeError("evaluable", new Atom(atomName + "/0"), "is/2"));
        } else if (term instanceof CompoundTerm) {
            CompoundTerm compoundTerm = (CompoundTerm) term;
            List<Term> args = compoundTerm.getArguments();
            int arity = args.size();
            String name = compoundTerm.getName();

            // Handle unary operations
            if (arity == 1) {
                Number argNum = evaluateTermToNumber(args.get(0), substitution);
                return applyUnaryToNumber(name, argNum);
            }

            // Handle binary operations
            if (arity == 2) {
                Number left = evaluateTermToNumber(args.get(0), substitution);
                Number right = evaluateTermToNumber(args.get(1), substitution);
                return applyBinaryToNumber(name, left, right);
            }

            throw new PrologEvaluationException("Unknown arithmetic function or incorrect arity: " + name);
        } else {
            throw new PrologEvaluationException("Cannot evaluate term in arithmetic context: " + term);
        }
    }

    /**
     * Apply a unary operation preserving integer/float type.
     */
    private static Number applyUnaryToNumber(String name, Number arg) {
        // Special handling for unary minus on integers
        if ("-".equals(name) && arg.isInteger()) {
            if (arg.isBigInteger()) {
                return new Number(arg.bigIntegerValue().negate());
            }
            long val = arg.longValue();
            if (val == Long.MIN_VALUE) {
                // Negating Long.MIN_VALUE overflows
                return new Number(BigInteger.valueOf(val).negate());
            }
            return new Number(-val);
        }

        // Special handling for unary plus on integers
        if ("+".equals(name) && arg.isInteger()) {
            return arg;
        }

        // abs preserves integer type
        if ("abs".equals(name) && arg.isInteger()) {
            if (arg.isBigInteger()) {
                return new Number(arg.bigIntegerValue().abs());
            }
            long val = arg.longValue();
            if (val == Long.MIN_VALUE) {
                return new Number(BigInteger.valueOf(val).abs());
            }
            return new Number(Math.abs(val));
        }

        // sign on integer returns integer
        if ("sign".equals(name) && arg.isInteger()) {
            if (arg.isBigInteger()) {
                return new Number((long) arg.bigIntegerValue().signum());
            }
            long val = arg.longValue();
            return new Number(val > 0 ? 1L : (val < 0 ? -1L : 0L));
        }

        // Bitwise NOT on integer
        if ("\\".equals(name) && arg.isInteger()) {
            if (arg.isBigInteger()) {
                return new Number(arg.bigIntegerValue().not());
            }
            return new Number(~arg.longValue());
        }

        // Rounding operations that convert float to integer
        if (INTEGER_UNARY_OPS.contains(name) && !("sign".equals(name) && !arg.isInteger())) {
            Function<Double, Double> function = UNARY_FUNCTIONS.get(name);
            if (function != null) {
                double result = function.apply(arg.doubleValue());
                // Result should be integer
                return new Number((long) result);
            }
        }

        // Float-producing operations
        if (FLOAT_UNARY_OPS.contains(name)) {
            Function<Double, Double> function = UNARY_FUNCTIONS.get(name);
            if (function != null) {
                double result = function.apply(arg.doubleValue());
                return new Number(result, false);
            }
        }

        // Default: use the double-based function
        Function<Double, Double> function = UNARY_FUNCTIONS.get(name);
        if (function != null) {
            double result = function.apply(arg.doubleValue());
            // Preserve integer type for operations not in float set
            if (arg.isInteger() && !FLOAT_UNARY_OPS.contains(name)) {
                if (result == Math.floor(result) && !Double.isInfinite(result)) {
                    return new Number((long) result);
                }
            }
            return new Number(result, false);
        }

        throw new PrologEvaluationException("Unknown unary arithmetic function: " + name);
    }

    /**
     * Apply a binary operation preserving integer/float type.
     */
    private static Number applyBinaryToNumber(String name, Number left, Number right) {
        boolean bothInteger = left.isInteger() && right.isInteger();

        // Integer-preserving operations when both operands are integers
        if (bothInteger) {
            switch (name) {
                case "+":
                    return addIntegers(left, right);
                case "-":
                    return subtractIntegers(left, right);
                case "*":
                    return multiplyIntegers(left, right);
                case "//":
                    return integerDivide(left, right);
                case "mod":
                    return integerMod(left, right);
                case "rem":
                    return integerRem(left, right);
                case "div":
                    return integerDiv(left, right);
                case "/\\":
                    return bitwiseAnd(left, right);
                case "\\/":
                    return bitwiseOr(left, right);
                case "xor":
                    return bitwiseXor(left, right);
                case "<<":
                    return shiftLeft(left, right);
                case ">>":
                    return shiftRight(left, right);
                case "**":
                    return integerPower(left, right);
                case "max":
                    return integerMax(left, right);
                case "min":
                    return integerMin(left, right);
                case "/": {
                    // Integer division: if result is exact, return integer; otherwise float
                    long lv = left.longValue();
                    long rv = right.longValue();
                    if (rv == 0) {
                        throw new PrologException(ISOErrorTerms.zeroDivisorError("(/)/2"));
                    }
                    if (lv % rv == 0) {
                        return new Number(lv / rv);
                    }
                    // Fall through to float division
                    break;
                }
            }
        }

        // For integer-only operations with at least one integer operand
        if (INTEGER_BINARY_OPS.contains(name)) {
            // These operations should produce integer results regardless
            BiFunction<Double, Double, Double> op = BINARY_OPERATIONS.get(name);
            if (op != null) {
                double result = op.apply(left.doubleValue(), right.doubleValue());
                return new Number((long) result);
            }
            ArithmeticOperation standardOp = StandardArithmeticOperations.getOperation(name);
            if (standardOp != null) {
                double result = standardOp.apply(left.doubleValue(), right.doubleValue());
                return new Number((long) result);
            }
        }

        // START_CHANGE: LIM-012 - Rational number support via rdiv
        if ("rdiv".equals(name)) {
            it.denzosoft.jprolog.core.terms.Rational lr = it.denzosoft.jprolog.core.terms.Rational.fromNumber(left);
            it.denzosoft.jprolog.core.terms.Rational rr = it.denzosoft.jprolog.core.terms.Rational.fromNumber(right);
            return lr.divide(rr);
        }
        // END_CHANGE: LIM-012

        // Default: use double-based operations
        // Check legacy operations first
        BiFunction<Double, Double, Double> operation = BINARY_OPERATIONS.get(name);
        if (operation != null) {
            double result = operation.apply(left.doubleValue(), right.doubleValue());
            return new Number(result, false);
        }

        // Check StandardArithmeticOperations
        ArithmeticOperation standardOp = StandardArithmeticOperations.getOperation(name);
        if (standardOp != null) {
            double result = standardOp.apply(left.doubleValue(), right.doubleValue());
            // For max/min, preserve integer type if both inputs are integers
            if (bothInteger && ("max".equals(name) || "min".equals(name))) {
                return new Number((long) result);
            }
            return new Number(result, false);
        }

        throw new PrologEvaluationException("Unknown arithmetic function or incorrect arity: " + name);
    }

    // --- Integer arithmetic helpers with overflow detection ---

    private static Number addIntegers(Number a, Number b) {
        if (a.isBigInteger() || b.isBigInteger()) {
            return normalizeBigInt(a.bigIntegerValue().add(b.bigIntegerValue()));
        }
        try {
            return new Number(Math.addExact(a.longValue(), b.longValue()));
        } catch (ArithmeticException overflow) {
            return normalizeBigInt(a.bigIntegerValue().add(b.bigIntegerValue()));
        }
    }

    private static Number subtractIntegers(Number a, Number b) {
        if (a.isBigInteger() || b.isBigInteger()) {
            return normalizeBigInt(a.bigIntegerValue().subtract(b.bigIntegerValue()));
        }
        try {
            return new Number(Math.subtractExact(a.longValue(), b.longValue()));
        } catch (ArithmeticException overflow) {
            return normalizeBigInt(a.bigIntegerValue().subtract(b.bigIntegerValue()));
        }
    }

    private static Number multiplyIntegers(Number a, Number b) {
        if (a.isBigInteger() || b.isBigInteger()) {
            return normalizeBigInt(a.bigIntegerValue().multiply(b.bigIntegerValue()));
        }
        try {
            return new Number(Math.multiplyExact(a.longValue(), b.longValue()));
        } catch (ArithmeticException overflow) {
            return normalizeBigInt(a.bigIntegerValue().multiply(b.bigIntegerValue()));
        }
    }

    private static Number integerDivide(Number a, Number b) {
        if (a.isBigInteger() || b.isBigInteger()) {
            BigInteger bv = b.bigIntegerValue();
            if (bv.signum() == 0) {
                throw new PrologException(ISOErrorTerms.zeroDivisorError("(//)/2"));
            }
            // ISO: truncate towards zero
            BigInteger[] qr = a.bigIntegerValue().divideAndRemainder(bv);
            return normalizeBigInt(qr[0]);
        }
        long lv = a.longValue();
        long rv = b.longValue();
        if (rv == 0) {
            throw new PrologException(ISOErrorTerms.zeroDivisorError("(//)/2"));
        }
        return new Number(lv / rv);  // Java long division truncates towards zero
    }

    private static Number integerMod(Number a, Number b) {
        if (a.isBigInteger() || b.isBigInteger()) {
            BigInteger bv = b.bigIntegerValue();
            if (bv.signum() == 0) {
                throw new PrologException(ISOErrorTerms.zeroDivisorError("mod/2"));
            }
            // ISO: mod(X,Y) = X - floor(X/Y) * Y
            BigInteger av = a.bigIntegerValue();
            BigInteger result = av.mod(bv.abs());
            if (bv.signum() < 0 && result.signum() > 0) {
                result = result.add(bv);
            }
            return normalizeBigInt(result);
        }
        long lv = a.longValue();
        long rv = b.longValue();
        if (rv == 0) {
            throw new PrologException(ISOErrorTerms.zeroDivisorError("mod/2"));
        }
        // ISO: mod(X,Y) = X - floor(X/Y) * Y (result has sign of Y)
        long result = lv % rv;
        if (result != 0 && ((result ^ rv) < 0)) {
            result += rv;
        }
        return new Number(result);
    }

    private static Number integerRem(Number a, Number b) {
        if (a.isBigInteger() || b.isBigInteger()) {
            BigInteger bv = b.bigIntegerValue();
            if (bv.signum() == 0) {
                throw new PrologException(ISOErrorTerms.zeroDivisorError("rem/2"));
            }
            return normalizeBigInt(a.bigIntegerValue().remainder(bv));
        }
        long lv = a.longValue();
        long rv = b.longValue();
        if (rv == 0) {
            throw new PrologException(ISOErrorTerms.zeroDivisorError("rem/2"));
        }
        return new Number(lv % rv);
    }

    private static Number integerDiv(Number a, Number b) {
        // div/2 is floor division
        if (a.isBigInteger() || b.isBigInteger()) {
            BigInteger bv = b.bigIntegerValue();
            if (bv.signum() == 0) {
                throw new PrologException(ISOErrorTerms.zeroDivisorError("div/2"));
            }
            BigInteger av = a.bigIntegerValue();
            // Floor division: for negative quotients, round down
            BigInteger[] qr = av.divideAndRemainder(bv);
            if (qr[1].signum() != 0 && (qr[1].signum() ^ bv.signum()) < 0) {
                return normalizeBigInt(qr[0].subtract(BigInteger.ONE));
            }
            return normalizeBigInt(qr[0]);
        }
        long lv = a.longValue();
        long rv = b.longValue();
        if (rv == 0) {
            throw new PrologException(ISOErrorTerms.zeroDivisorError("div/2"));
        }
        return new Number(Math.floorDiv(lv, rv));
    }

    private static Number bitwiseAnd(Number a, Number b) {
        if (a.isBigInteger() || b.isBigInteger()) {
            return normalizeBigInt(a.bigIntegerValue().and(b.bigIntegerValue()));
        }
        return new Number(a.longValue() & b.longValue());
    }

    private static Number bitwiseOr(Number a, Number b) {
        if (a.isBigInteger() || b.isBigInteger()) {
            return normalizeBigInt(a.bigIntegerValue().or(b.bigIntegerValue()));
        }
        return new Number(a.longValue() | b.longValue());
    }

    private static Number bitwiseXor(Number a, Number b) {
        if (a.isBigInteger() || b.isBigInteger()) {
            return normalizeBigInt(a.bigIntegerValue().xor(b.bigIntegerValue()));
        }
        return new Number(a.longValue() ^ b.longValue());
    }

    private static Number shiftLeft(Number a, Number b) {
        if (a.isBigInteger()) {
            return normalizeBigInt(a.bigIntegerValue().shiftLeft((int) b.longValue()));
        }
        return new Number(a.longValue() << b.longValue());
    }

    private static Number shiftRight(Number a, Number b) {
        if (a.isBigInteger()) {
            return normalizeBigInt(a.bigIntegerValue().shiftRight((int) b.longValue()));
        }
        return new Number(a.longValue() >> b.longValue());
    }

    private static Number integerPower(Number base, Number exp) {
        long ev = exp.longValue();
        if (ev < 0) {
            // Negative exponent: result is float
            return new Number(Math.pow(base.doubleValue(), exp.doubleValue()), false);
        }
        if (ev == 0) {
            return new Number(1L);
        }
        if (base.isBigInteger() || ev > 62) {
            // Use BigInteger power for large exponents
            if (ev > Integer.MAX_VALUE) {
                throw new PrologEvaluationException("Exponent too large for integer power");
            }
            return normalizeBigInt(base.bigIntegerValue().pow((int) ev));
        }
        // Try with long, detect overflow
        try {
            long result = 1;
            long b = base.longValue();
            long e = ev;
            while (e > 0) {
                if ((e & 1) == 1) {
                    result = Math.multiplyExact(result, b);
                }
                e >>= 1;
                if (e > 0) {
                    b = Math.multiplyExact(b, b);
                }
            }
            return new Number(result);
        } catch (ArithmeticException overflow) {
            return normalizeBigInt(base.bigIntegerValue().pow((int) ev));
        }
    }

    private static Number integerMax(Number a, Number b) {
        if (a.isBigInteger() || b.isBigInteger()) {
            BigInteger av = a.bigIntegerValue();
            BigInteger bv = b.bigIntegerValue();
            return normalizeBigInt(av.compareTo(bv) >= 0 ? av : bv);
        }
        return new Number(Math.max(a.longValue(), b.longValue()));
    }

    private static Number integerMin(Number a, Number b) {
        if (a.isBigInteger() || b.isBigInteger()) {
            BigInteger av = a.bigIntegerValue();
            BigInteger bv = b.bigIntegerValue();
            return normalizeBigInt(av.compareTo(bv) <= 0 ? av : bv);
        }
        return new Number(Math.min(a.longValue(), b.longValue()));
    }

    /**
     * Normalize a BigInteger result: if it fits in a long, use long representation.
     */
    private static Number normalizeBigInt(BigInteger value) {
        if (value.bitLength() < 63) {
            return new Number(value.longValueExact());
        }
        return new Number(value);
    }
    // END_CHANGE: LIM-008

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
