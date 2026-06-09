package it.denzosoft.jprolog.core.arith.v2;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Map;

/**
 * Clean-room ISO arithmetic evaluator — a single evaluation path that replaces the legacy
 * {@code ArithmeticEvaluator}'s dual {@code evaluateToNumber}/{@code evaluateTerm} (double) code.
 *
 * <p>Type rules (matching the v3 fixes): {@code + - *} keep integers exact (BigInteger), mixed →
 * float; {@code //}/{@code mod}/{@code rem}/{@code div}/bitwise/shift require integers (else
 * {@code type_error(integer,_)}); {@code (**)/2} is the float power, {@code (^)/2} integer power;
 * {@code min/max} return the selected operand preserving its type; rounding functions promote to
 * BigInteger past long range. Errors are ISO terms (instantiation_error, type_error(evaluable,N/A),
 * type_error(integer,_), evaluation_error(zero_divisor|undefined)).
 */
public final class ArithEvaluator {

    private final Map<String, Term> bindings;

    public ArithEvaluator(Map<String, Term> bindings) { this.bindings = bindings; }

    public static Number eval(Term expr, Map<String, Term> bindings) {
        return new ArithEvaluator(bindings).evaluate(expr);
    }

    public Number evaluate(Term term) {
        Term t = (bindings != null) ? term.resolveBindings(bindings) : term;
        if (t instanceof Number) return (Number) t;
        if (t instanceof Variable) throw new PrologException(ISOErrorTerms.instantiationError("is/2"));
        if (t instanceof Atom) return constant(((Atom) t).getName());
        if (t instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) t;
            int n = c.getArguments().size();
            if (n == 1) return unary(c.getName(), evaluate(c.getArguments().get(0)));
            if (n == 2) return binary(c.getName(), evaluate(c.getArguments().get(0)), evaluate(c.getArguments().get(1)));
            throw evaluableError(c.getName(), n);
        }
        throw new PrologException(ISOErrorTerms.typeError("evaluable", t, "is/2"));
    }

    // ----------------------------------------------------------------- constants
    private Number constant(String name) {
        switch (name) {
            case "pi": return f(Math.PI);
            case "e": return f(Math.E);
            case "inf": case "infinite": return f(Double.POSITIVE_INFINITY);
            case "nan": return f(Double.NaN);
            case "epsilon": return f(Math.ulp(1.0));
            case "max_tagged_integer": return i(Long.MAX_VALUE);
            case "min_tagged_integer": return i(Long.MIN_VALUE);
            case "random": return f(Math.random());
            default: throw evaluableError(name, 0);
        }
    }

    // ----------------------------------------------------------------- unary
    private Number unary(String op, Number x) {
        switch (op) {
            case "-": return x.isInteger() ? big(x.bigIntegerValue().negate()) : f(-x.doubleValue());
            case "+": return x;
            case "abs": return x.isInteger() ? big(x.bigIntegerValue().abs()) : f(Math.abs(x.doubleValue()));
            case "sign":
                if (x.isInteger()) return i(x.bigIntegerValue().signum());
                return f(Math.signum(x.doubleValue()));
            case "min": case "max": return x; // unary degenerate
            case "sqrt": return f(checkDomain(x.doubleValue() >= 0, "sqrt/1") ? Math.sqrt(x.doubleValue()) : 0);
            case "sin": return f(Math.sin(x.doubleValue()));
            case "cos": return f(Math.cos(x.doubleValue()));
            case "tan": return f(Math.tan(x.doubleValue()));
            case "asin": return f(Math.asin(inUnit(x.doubleValue(), "asin/1")));
            case "acos": return f(Math.acos(inUnit(x.doubleValue(), "acos/1")));
            case "atan": return f(Math.atan(x.doubleValue()));
            case "exp": return f(Math.exp(x.doubleValue()));
            case "log": checkDomain(x.doubleValue() > 0, "log/1"); return f(Math.log(x.doubleValue()));
            case "sinh": return f(Math.sinh(x.doubleValue()));
            case "cosh": return f(Math.cosh(x.doubleValue()));
            case "tanh": return f(Math.tanh(x.doubleValue()));
            case "asinh": return f(Math.log(x.doubleValue() + Math.sqrt(x.doubleValue() * x.doubleValue() + 1)));
            case "acosh": checkDomain(x.doubleValue() >= 1, "acosh/1"); return f(Math.log(x.doubleValue() + Math.sqrt(x.doubleValue() * x.doubleValue() - 1)));
            case "atanh": checkDomain(Math.abs(x.doubleValue()) < 1, "atanh/1"); return f(0.5 * Math.log((1 + x.doubleValue()) / (1 - x.doubleValue())));
            case "cbrt": return f(Math.cbrt(x.doubleValue()));
            case "float": return f(x.doubleValue());
            case "integer": case "truncate": return roundToInt(x.doubleValue() < 0 ? Math.ceil(x.doubleValue()) : Math.floor(x.doubleValue()), op);
            case "floor": return x.isInteger() ? x : roundToInt(Math.floor(x.doubleValue()), op);
            case "ceiling": return x.isInteger() ? x : roundToInt(Math.ceil(x.doubleValue()), op);
            case "round": return x.isInteger() ? x : roundToInt(Math.floor(x.doubleValue() + 0.5), op);
            case "float_integer_part": return f((double) (long) x.doubleValue());
            case "float_fractional_part": { double v = x.doubleValue(); return f(v - (long) v); }
            case "\\": requireInt(x, "(\\)/1"); return big(x.bigIntegerValue().not());
            case "msb":
                requireInt(x, "msb/1");
                if (x.bigIntegerValue().signum() <= 0) throw new PrologException(ISOErrorTerms.evaluationError("undefined", "msb/1"));
                return i(x.bigIntegerValue().bitLength() - 1);
            case "succ": requireInt(x, "succ/1"); return big(x.bigIntegerValue().add(BigInteger.ONE));
            default: throw evaluableError(op, 1);
        }
    }

    // ----------------------------------------------------------------- binary
    private Number binary(String op, Number a, Number b) {
        boolean bothInt = a.isInteger() && b.isInteger();
        switch (op) {
            case "+": return bothInt ? big(a.bigIntegerValue().add(b.bigIntegerValue())) : f(a.doubleValue() + b.doubleValue());
            case "-": return bothInt ? big(a.bigIntegerValue().subtract(b.bigIntegerValue())) : f(a.doubleValue() - b.doubleValue());
            case "*": return bothInt ? big(a.bigIntegerValue().multiply(b.bigIntegerValue())) : f(a.doubleValue() * b.doubleValue());
            case "/":
                if (bothInt) {
                    if (b.bigIntegerValue().signum() == 0) throw new PrologException(ISOErrorTerms.zeroDivisorError("(/)/2"));
                    BigInteger[] qr = a.bigIntegerValue().divideAndRemainder(b.bigIntegerValue());
                    if (qr[1].signum() == 0) return big(qr[0]);              // exact -> integer
                }
                if (b.doubleValue() == 0.0) throw new PrologException(ISOErrorTerms.zeroDivisorError("(/)/2"));
                return f(a.doubleValue() / b.doubleValue());
            case "//": requireInt(a, "(//)/2"); requireInt(b, "(//)/2"); checkNonZero(b, "(//)/2");
                return big(a.bigIntegerValue().divide(b.bigIntegerValue()));            // truncate toward zero
            case "div": requireInt(a, "(div)/2"); requireInt(b, "(div)/2"); checkNonZero(b, "(div)/2");
                return big(floorDiv(a.bigIntegerValue(), b.bigIntegerValue()));         // floor division
            case "mod": requireInt(a, "(mod)/2"); requireInt(b, "(mod)/2"); checkNonZero(b, "(mod)/2");
                return big(floorMod(a.bigIntegerValue(), b.bigIntegerValue()));
            case "rem": requireInt(a, "(rem)/2"); requireInt(b, "(rem)/2"); checkNonZero(b, "(rem)/2");
                return big(a.bigIntegerValue().remainder(b.bigIntegerValue()));
            case "**":                                                                 // float power (ISO §9.3.1)
                if (a.doubleValue() == 0.0 && b.doubleValue() < 0.0) throw new PrologException(ISOErrorTerms.evaluationError("undefined", "(**)/2"));
                return f(Math.pow(a.doubleValue(), b.doubleValue()));
            case "^":                                                                  // integer power (ISO §9.3.10)
                if (bothInt) {
                    int exp = b.bigIntegerValue().intValueExact();
                    if (exp >= 0) return big(a.bigIntegerValue().pow(exp));
                    if (a.bigIntegerValue().abs().equals(BigInteger.ONE)) return big(a.bigIntegerValue().pow(-exp).equals(BigInteger.ONE) ? BigInteger.ONE : a.bigIntegerValue());
                    throw new PrologException(ISOErrorTerms.typeError("float", a, "(^)/2"));
                }
                return f(Math.pow(a.doubleValue(), b.doubleValue()));
            case "min": return compareNum(a, b) <= 0 ? a : b;                          // preserve operand type
            case "max": return compareNum(a, b) >= 0 ? a : b;
            case "gcd": requireInt(a, "gcd/2"); requireInt(b, "gcd/2");
                return big(a.bigIntegerValue().gcd(b.bigIntegerValue()));
            case ">>": requireInt(a, "(>>)/2"); requireInt(b, "(>>)/2"); requireNonNegShift(b); return big(a.bigIntegerValue().shiftRight(b.bigIntegerValue().intValueExact()));
            case "<<": requireInt(a, "(<<)/2"); requireInt(b, "(<<)/2"); requireNonNegShift(b); return big(a.bigIntegerValue().shiftLeft(b.bigIntegerValue().intValueExact()));
            case "/\\": requireInt(a, "(/\\)/2"); requireInt(b, "(/\\)/2"); return big(a.bigIntegerValue().and(b.bigIntegerValue()));
            case "\\/": requireInt(a, "(\\/)/2"); requireInt(b, "(\\/)/2"); return big(a.bigIntegerValue().or(b.bigIntegerValue()));
            case "xor": requireInt(a, "xor/2"); requireInt(b, "xor/2"); return big(a.bigIntegerValue().xor(b.bigIntegerValue()));
            case "atan": case "atan2": return f(Math.atan2(a.doubleValue(), b.doubleValue()));
            case "copysign": return f(Math.copySign(a.doubleValue(), b.doubleValue()));
            case "log": checkDomain(a.doubleValue() > 0 && b.doubleValue() > 0, "log/2"); return f(Math.log(b.doubleValue()) / Math.log(a.doubleValue()));
            case "truncate": return roundToInt(a.doubleValue() < 0 ? Math.ceil(a.doubleValue()) : Math.floor(a.doubleValue()), op);
            default: throw evaluableError(op, 2);
        }
    }

    // ----------------------------------------------------------------- helpers
    private static Number i(long v) { return new Number(v); }
    private static Number big(BigInteger v) {
        return (v.bitLength() <= 63) ? new Number(v.longValueExact()) : new Number(v);
    }
    private static Number f(double v) { return new Number(v, false); }

    private static int compareNum(Number a, Number b) {
        if (a.isInteger() && b.isInteger()) return a.bigIntegerValue().compareTo(b.bigIntegerValue());
        return Double.compare(a.doubleValue(), b.doubleValue());
    }

    private static final double LONG_RANGE = 9.223372036854776E18;
    private Number roundToInt(double v, String op) {
        if (Double.isNaN(v) || Double.isInfinite(v)) throw new PrologException(ISOErrorTerms.evaluationError("undefined", "(" + op + ")/1"));
        if (v >= -LONG_RANGE && v < LONG_RANGE) return i((long) v);
        return big(new BigDecimal(v).toBigInteger());
    }

    private void requireInt(Number n, String ctx) {
        if (!n.isInteger()) throw new PrologException(ISOErrorTerms.typeError("integer", n, ctx));
    }
    private void requireNonNegShift(Number n) {
        if (n.bigIntegerValue().signum() < 0) throw new PrologException(ISOErrorTerms.evaluationError("negative_shift", "shift/2"));
    }
    private void checkNonZero(Number n, String ctx) {
        if (n.isInteger() && n.bigIntegerValue().signum() == 0) throw new PrologException(ISOErrorTerms.zeroDivisorError(ctx));
    }
    private boolean checkDomain(boolean ok, String ctx) {
        if (!ok) throw new PrologException(ISOErrorTerms.evaluationError("undefined", ctx));
        return true;
    }
    private double inUnit(double v, String ctx) { checkDomain(v >= -1 && v <= 1, ctx); return v; }

    private static BigInteger floorDiv(BigInteger a, BigInteger b) {
        BigInteger[] qr = a.divideAndRemainder(b);
        if (qr[1].signum() != 0 && (qr[1].signum() != b.signum())) return qr[0].subtract(BigInteger.ONE);
        return qr[0];
    }
    private static BigInteger floorMod(BigInteger a, BigInteger b) {
        BigInteger r = a.mod(b.abs());
        return (b.signum() < 0 && r.signum() != 0) ? r.subtract(b.abs()) : r;
    }

    private PrologException evaluableError(String name, int arity) {
        Term pi = new CompoundTerm(new Atom("/"), Arrays.asList(new Atom(name), new Number((long) arity)));
        return new PrologException(ISOErrorTerms.typeError("evaluable", pi, "is/2"));
    }
}
