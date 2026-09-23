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
    // START_CHANGE: ISS-2025-0434 - ENG-14: an O(1) per-node dereference hook. The map-based path
    // called term.resolveBindings(bindings) at EVERY node, and resolveBindings walks the whole
    // sub-term — so evaluating an expression of n nodes cost O(n^2) term traversals plus a rebuilt
    // copy of the expression. A deref function only looks through variable bindings at the current
    // node; evaluate() already recurses into the arguments, so one level is all that is needed.
    private final java.util.function.UnaryOperator<Term> derefFn;

    public ArithEvaluator(Map<String, Term> bindings) { this.bindings = bindings; this.derefFn = null; }

    private ArithEvaluator(java.util.function.UnaryOperator<Term> derefFn) {
        this.bindings = null; this.derefFn = derefFn;
    }

    public static Number eval(Term expr, Map<String, Term> bindings) {
        return new ArithEvaluator(bindings).evaluate(expr);
    }

    /** Evaluate {@code expr} dereferencing each node through {@code deref} (the v2 machine's
     *  binding store) instead of deep-copying the expression first. */
    public static Number evalDeref(Term expr, java.util.function.UnaryOperator<Term> deref) {
        Term t = deref.apply(expr);
        if (t instanceof Number) return (Number) t;           // ISS-2025-0594: no evaluator needed
        return new ArithEvaluator(deref).evaluate(t);
    }
    // END_CHANGE: ISS-2025-0434

    // START_CHANGE: ISS-2025-0594 - P4.5: the error context is the predicate that evaluated the
    // expression ('=:='/2, '<'/2, ...), not always is/2.
    private String ctxPI;                 // a full indicator ("sum_list/2"), or null
    private String ctxOp;                 // a comparison functor ("=:="), or null

    /** The predicate indicator errors name; built only when an error is raised. */
    private String ctx() {
        if (ctxPI != null) return ctxPI;
        if (ctxOp != null) return ctxOp + "/2";
        return "is/2";
    }

    /** As {@link #evalDeref(Term, java.util.function.UnaryOperator)}, reporting errors in the
     *  context {@code context} (a predicate indicator such as {@code "sum_list/2"}). */
    public static Number evalDeref(Term expr, java.util.function.UnaryOperator<Term> deref, String context) {
        ArithEvaluator ev = new ArithEvaluator(deref);
        ev.ctxPI = context;
        return ev.evaluate(expr);
    }

    /** One side of the arithmetic comparison {@code op}/2 (errors name {@code op/2}). */
    public static Number evalCompare(Term expr, java.util.function.UnaryOperator<Term> deref, String op) {
        Term t = deref.apply(expr);
        if (t instanceof Number) return (Number) t;           // the common case: no evaluator
        ArithEvaluator ev = new ArithEvaluator(deref);
        ev.ctxOp = op;
        return ev.evaluate(t);
    }
    // END_CHANGE: ISS-2025-0594

    private Term derefNode(Term term) {
        return (derefFn != null) ? derefFn.apply(term)
             : (bindings != null) ? term.resolveBindings(bindings) : term;
    }

    // START_CHANGE: ISS-2025-0592 - P4.3: deep expressions. The recursive walk is kept for the
    // common shallow case (it is the fast path of is/2); past RECURSION_LIMIT nesting levels the
    // sub-expression is evaluated by an explicit-stack walker, so a left-deep sum of 10^5 terms
    // no longer overflows the Java stack.
    private static final int RECURSION_LIMIT = 400;

    public Number evaluate(Term term) {
        return evalRec(term, 0);
    }

    private Number evalRec(Term term, int depth) {
        Term t = derefNode(term);
        if (t instanceof Number) return (Number) t;
        if (t instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) t;
            int n = c.arity();
            if (n == 2) {
                String name = c.getName();
                if (!isListName(name)) {
                    if (depth > RECURSION_LIMIT) return evalIter(c);
                    Number a = evalRec(c.arg(0), depth + 1);
                    return binary(name, a, evalRec(c.arg(1), depth + 1));
                }
            } else if (n == 1) {
                if (depth > RECURSION_LIMIT) return evalIter(c);
                return unary(c.getName(), evalRec(c.arg(0), depth + 1));
            }
        }
        return leaf(t);
    }

    private static boolean isListCell(CompoundTerm c) {
        return c.arity() == 2 && isListName(c.getName());
    }

    private static boolean isListName(String name) {
        return ".".equals(name) || "[|]".equals(name);
    }

    /** A term that is not an evaluable unary/binary compound: a number, a variable, a constant,
     *  a one-element list, a one-character string, or an error. */
    private Number leaf(Term t) {
        if (t instanceof Number) return (Number) t;
        if (t instanceof Variable) throw new PrologException(ISOErrorTerms.instantiationError(ctx()));
        if (t instanceof Atom) return constant(((Atom) t).getName());
        if (t instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) t;
            // START_CHANGE: ISS-2025-0594 - P4.5: [X] evaluates X (SWI); a longer list is not
            // evaluable and is reported as '[|]'/2, not as its tail's []/0.
            if (isListCell(c)) {
                Term tail = derefNode(c.arg(1));
                if (tail instanceof Atom && "[]".equals(((Atom) tail).getName())) return evaluate(c.arg(0));
                throw evaluableError("[|]", 2);
            }
            // END_CHANGE: ISS-2025-0594
            throw evaluableError(c.getName(), c.arity());
        }
        if (t instanceof it.denzosoft.jprolog.core.terms.PrologString) {
            String s = ((it.denzosoft.jprolog.core.terms.PrologString) t).getStringValue();
            if (s.codePointCount(0, s.length()) == 1) return i(s.codePointAt(0));   // SWI: "a" is 97
            throw new PrologException(ISOErrorTerms.typeError("evaluable", t, ctx()));
        }
        throw new PrologException(ISOErrorTerms.typeError("evaluable", t, ctx()));
    }

    /** Explicit-stack post-order evaluation of a (deep) evaluable compound. */
    private Number evalIter(CompoundTerm root) {
        CompoundTerm[] frames = new CompoundTerm[64];
        Number[] firsts = new Number[64];
        byte[] state = new byte[64];          // 0 = evaluating arg 0, 1 = evaluating arg 1
        int sp = 0;
        Term cur = root;
        Number result;
        while (true) {
            // descend along argument 0 until a leaf
            while (true) {
                Term t = derefNode(cur);
                if (t instanceof CompoundTerm) {
                    CompoundTerm c = (CompoundTerm) t;
                    int n = c.arity();
                    if (n == 1 || (n == 2 && !isListCell(c))) {
                        if (sp == frames.length) {
                            frames = Arrays.copyOf(frames, sp * 2);
                            firsts = Arrays.copyOf(firsts, sp * 2);
                            state = Arrays.copyOf(state, sp * 2);
                        }
                        frames[sp] = c; firsts[sp] = null; state[sp] = 0; sp++;
                        cur = c.arg(0);
                        continue;
                    }
                }
                result = leaf(t);
                break;
            }
            // ascend, combining results, until a frame needs its second argument
            while (true) {
                if (sp == 0) return result;
                CompoundTerm c = frames[sp - 1];
                if (c.arity() == 1) {
                    sp--; frames[sp] = null;
                    result = unary(c.getName(), result);
                } else if (state[sp - 1] == 0) {
                    firsts[sp - 1] = result; state[sp - 1] = 1;
                    cur = c.arg(1);
                    break;
                } else {
                    sp--; frames[sp] = null;
                    Number a = firsts[sp]; firsts[sp] = null;
                    result = binary(c.getName(), a, result);
                }
            }
        }
    }
    // END_CHANGE: ISS-2025-0592

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
            // START_CHANGE: ISS-2025-0359 / ISS-2025-0360 - computed floats go through fc() so
            // Infinity raises float_overflow and NaN raises undefined (e.g. sin(inf), exp(1000))
            case "sin": return fc(Math.sin(x.doubleValue()), "sin/1", x);
            case "cos": return fc(Math.cos(x.doubleValue()), "cos/1", x);
            case "tan": return fc(Math.tan(x.doubleValue()), "tan/1", x);
            case "asin": return f(Math.asin(inUnit(x.doubleValue(), "asin/1")));
            case "acos": return f(Math.acos(inUnit(x.doubleValue(), "acos/1")));
            case "atan": return f(Math.atan(x.doubleValue()));
            case "exp": return fc(Math.exp(x.doubleValue()), "exp/1", x);
            case "log": checkDomain(x.doubleValue() > 0, "log/1"); return f(Math.log(x.doubleValue()));
            case "sinh": return fc(Math.sinh(x.doubleValue()), "sinh/1", x);
            case "cosh": return fc(Math.cosh(x.doubleValue()), "cosh/1", x);
            case "tanh": return f(Math.tanh(x.doubleValue()));
            case "asinh": return fc(Math.log(x.doubleValue() + Math.sqrt(x.doubleValue() * x.doubleValue() + 1)), "asinh/1", x);
            case "acosh": checkDomain(x.doubleValue() >= 1, "acosh/1"); return fc(Math.log(x.doubleValue() + Math.sqrt(x.doubleValue() * x.doubleValue() - 1)), "acosh/1", x);
            case "atanh": checkDomain(Math.abs(x.doubleValue()) < 1, "atanh/1"); return f(0.5 * Math.log((1 + x.doubleValue()) / (1 - x.doubleValue())));
            case "cbrt": return f(Math.cbrt(x.doubleValue()));
            // START_CHANGE: ISS-2025-0613 - documented evaluables the v2 evaluator never had
            case "cot": return fc(1.0 / Math.tan(x.doubleValue()), "cot/1", x);
            case "acot": return f(Math.atan(1.0 / x.doubleValue()));
            case "lsb":
                requireInt(x, "lsb/1");
                if (x.bigIntegerValue().signum() <= 0) throw new PrologException(ISOErrorTerms.evaluationError("undefined", "lsb/1"));
                return i(x.bigIntegerValue().getLowestSetBit());
            case "popcount":
                requireInt(x, "popcount/1");
                if (x.bigIntegerValue().signum() < 0) throw new PrologException(ISOErrorTerms.typeError("not_less_than_zero", x, "popcount/1"));
                return i(x.bigIntegerValue().bitCount());
            // END_CHANGE: ISS-2025-0613
            case "float": return fc(x.doubleValue(), "float/1", x);
            // END_CHANGE: ISS-2025-0359 / ISS-2025-0360
            // START_CHANGE: ISS-2025-0590 - P4.1: an integer argument is returned unchanged (the
            // old path went through doubleValue() and lost big integers); integer/1 ROUNDS like
            // round/1 (SWI); round is half away from zero without the floor(x+0.5) double error
            // (round(0.49999999999999994) was 1, round(-2.5) was -2).
            case "truncate": return x.isInteger() ? x : roundToInt(truncTowardZero(x.doubleValue()), op);
            case "integer": case "round": return x.isInteger() ? x : roundToInt(roundHalfAway(x.doubleValue()), op);
            // END_CHANGE: ISS-2025-0590
            case "floor": return x.isInteger() ? x : roundToInt(Math.floor(x.doubleValue()), op);
            case "ceiling": return x.isInteger() ? x : roundToInt(Math.ceil(x.doubleValue()), op);
            // START_CHANGE: ISS-2025-0407 - truncate toward zero in double math: the old (long)
            // cast saturated at +/-2^63, silently corrupting results for |x| >= 2^63
            case "float_integer_part": { double v = x.doubleValue(); return f(v < 0 ? Math.ceil(v) : Math.floor(v)); }
            case "float_fractional_part": { double v = x.doubleValue(); return f(v - (v < 0 ? Math.ceil(v) : Math.floor(v))); }
            // END_CHANGE: ISS-2025-0407
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
        // START_CHANGE: ISS-2025-0434 - ENG-14: primitive long fast path for + - *. The BigInteger
        // path allocated THREE BigIntegers per operation (two operands plus the result) even for
        // single-digit integers, which dominated arithmetic-heavy programs. Math.*Exact throws on
        // overflow, so the exact BigInteger path below still handles everything outside long range.
        if (bothInt && a.fitsInLong() && b.fitsInLong()
                && ("+".equals(op) || "-".equals(op) || "*".equals(op))) {
            long x = a.longValue(), y = b.longValue();
            try {
                switch (op) {
                    case "+": return Number.valueOf(Math.addExact(x, y));
                    case "-": return Number.valueOf(Math.subtractExact(x, y));
                    default:  return Number.valueOf(Math.multiplyExact(x, y));
                }
            } catch (ArithmeticException overflow) {
                // fall through to the exact BigInteger path
            }
        }
        // END_CHANGE: ISS-2025-0434
        switch (op) {
            // START_CHANGE: ISS-2025-0359 / ISS-2025-0360 - computed floats go through fc()
            case "+": return bothInt ? big(a.bigIntegerValue().add(b.bigIntegerValue())) : fc(a.doubleValue() + b.doubleValue(), "(+)/2", a, b);
            case "-": return bothInt ? big(a.bigIntegerValue().subtract(b.bigIntegerValue())) : fc(a.doubleValue() - b.doubleValue(), "(-)/2", a, b);
            case "*": return bothInt ? big(a.bigIntegerValue().multiply(b.bigIntegerValue())) : fc(a.doubleValue() * b.doubleValue(), "(*)/2", a, b);
            case "/":
                if (bothInt) {
                    if (b.bigIntegerValue().signum() == 0) throw new PrologException(ISOErrorTerms.zeroDivisorError("(/)/2"));
                    BigInteger[] qr = a.bigIntegerValue().divideAndRemainder(b.bigIntegerValue());
                    if (qr[1].signum() == 0) return big(qr[0]);              // exact -> integer
                    // START_CHANGE: ISS-2025-0590 - P4.1: an operand past 2^53 has no exact double
                    // image (10^400 is Infinity): divide exactly in decimal, then round once.
                    if (a.bigIntegerValue().bitLength() > 53 || b.bigIntegerValue().bitLength() > 53) {
                        double q = new BigDecimal(a.bigIntegerValue())
                                .divide(new BigDecimal(b.bigIntegerValue()), java.math.MathContext.DECIMAL128).doubleValue();
                        return fc(q, "(/)/2", a, b);
                    }
                    // END_CHANGE: ISS-2025-0590
                }
                if (b.doubleValue() == 0.0) throw new PrologException(ISOErrorTerms.zeroDivisorError("(/)/2"));
                return fc(a.doubleValue() / b.doubleValue(), "(/)/2", a, b);
            // END_CHANGE: ISS-2025-0359 / ISS-2025-0360
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
                // START_CHANGE: ISS-2025-0359 / ISS-2025-0360 - 2.0 ** 10000 -> float_overflow; (-2.0) ** 0.5 -> undefined
                return fc(Math.pow(a.doubleValue(), b.doubleValue()), "(**)/2", a, b);
                // END_CHANGE: ISS-2025-0359 / ISS-2025-0360
            case "^":                                                                  // integer power (ISO §9.3.10)
                if (bothInt) {
                    BigInteger base = a.bigIntegerValue();
                    BigInteger bexp = b.bigIntegerValue();
                    if (bexp.signum() < 0) {
                        // START_CHANGE: ISS-2025-0362 - 0 ^ negative is evaluation_error(zero_divisor) (ISO 9.3.10.3)
                        if (base.signum() == 0) throw new PrologException(ISOErrorTerms.zeroDivisorError("(^)/2"));
                        // END_CHANGE: ISS-2025-0362
                        // |base| == 1: (-1)^odd = -1, otherwise 1 (computable for any exponent size)
                        if (base.abs().equals(BigInteger.ONE)) return big(bexp.testBit(0) ? base : BigInteger.ONE);
                        throw new PrologException(ISOErrorTerms.typeError("float", a, "(^)/2"));
                    }
                    // START_CHANGE: ISS-2025-0361 - an exponent beyond int range must raise a catchable
                    // ISO error, not a raw java.lang.ArithmeticException from intValueExact(); bases in
                    // {-1, 0, 1} stay exactly computable for any exponent size.
                    if (bexp.bitLength() > 31) {
                        if (base.signum() == 0) return i(0);
                        if (base.abs().equals(BigInteger.ONE)) return big(base.signum() > 0 || !bexp.testBit(0) ? BigInteger.ONE : base);
                        throw new PrologException(ISOErrorTerms.resourceError("memory", "(^)/2"));
                    }
                    // END_CHANGE: ISS-2025-0361
                    return big(base.pow(bexp.intValueExact()));
                }
                // START_CHANGE: ISS-2025-0359 / ISS-2025-0360 - 2.0 ^ 10000 -> float_overflow; -2 ^ 0.5 -> undefined
                return fc(Math.pow(a.doubleValue(), b.doubleValue()), "(^)/2", a, b);
                // END_CHANGE: ISS-2025-0359 / ISS-2025-0360
            case "min": return compareNum(a, b) <= 0 ? a : b;                          // preserve operand type
            case "max": return compareNum(a, b) >= 0 ? a : b;
            case "gcd": requireInt(a, "gcd/2"); requireInt(b, "gcd/2");
                return big(a.bigIntegerValue().gcd(b.bigIntegerValue()));
            // START_CHANGE: ISS-2025-0361 - shift counts beyond int range must not raise a raw
            // java.lang.ArithmeticException from intValueExact(): (>>) has the exact mathematical
            // result (the sign extension), (<<) raises a catchable ISO resource_error.
            // START_CHANGE: ISS-2025-0591 - P4.2: a negative shift count shifts the other way
            // (SWI: 1 << -1 =:= 0, 8 >> -2 =:= 32) instead of raising the non-ISO
            // evaluation_error(negative_shift).
            case ">>": requireInt(a, "(>>)/2"); requireInt(b, "(>>)/2");
                return shift(a, b, false, "(>>)/2");
            case "<<": requireInt(a, "(<<)/2"); requireInt(b, "(<<)/2");
                return shift(a, b, true, "(<<)/2");
            // END_CHANGE: ISS-2025-0591
            // END_CHANGE: ISS-2025-0361
            case "/\\": requireInt(a, "(/\\)/2"); requireInt(b, "(/\\)/2"); return big(a.bigIntegerValue().and(b.bigIntegerValue()));
            case "\\/": requireInt(a, "(\\/)/2"); requireInt(b, "(\\/)/2"); return big(a.bigIntegerValue().or(b.bigIntegerValue()));
            case "xor": requireInt(a, "xor/2"); requireInt(b, "xor/2"); return big(a.bigIntegerValue().xor(b.bigIntegerValue()));
            case "atan": case "atan2": return f(Math.atan2(a.doubleValue(), b.doubleValue()));
            case "copysign": return f(Math.copySign(a.doubleValue(), b.doubleValue()));
            // START_CHANGE: ISS-2025-0359 - log(1, X) divides by log(1) = 0 -> float_overflow, not Infinity
            case "log": checkDomain(a.doubleValue() > 0 && b.doubleValue() > 0, "log/2"); return fc(Math.log(b.doubleValue()) / Math.log(a.doubleValue()), "log/2", a, b);
            // END_CHANGE: ISS-2025-0359
            case "truncate": return roundToInt(a.doubleValue() < 0 ? Math.ceil(a.doubleValue()) : Math.floor(a.doubleValue()), op);
            default: throw evaluableError(op, 2);
        }
    }

    // ----------------------------------------------------------------- helpers
    private static Number i(long v) { return Number.valueOf(v); }   // ISS-2025-0434 - ENG-14
    private static Number big(BigInteger v) {
        return (v.bitLength() <= 63) ? Number.valueOf(v.longValueExact()) : new Number(v);   // ISS-2025-0434
    }
    private static Number f(double v) { return new Number(v, false); }

    // START_CHANGE: ISS-2025-0359 / ISS-2025-0360 - a COMPUTED float result must raise
    // evaluation_error(float_overflow) instead of silently returning Infinity (ISO 9.1.4.1) and
    // evaluation_error(undefined) instead of NaN (ISO 9.3.1.3), unless an operand was already
    // exceptional (so inf + 1 stays inf and nan + 1 stays nan; the inf/nan CONSTANTS still build
    // their value directly via f() in constant()). Integer operands are mathematically finite even
    // when their double image saturates (e.g. float(10^400) -> float_overflow).
    private static Number fc(double v, String ctx, Number... ops) {
        if (Double.isInfinite(v)) {
            for (Number op : ops) {
                if (!op.isInteger() && Double.isInfinite(op.doubleValue())) return new Number(v, false);
            }
            throw new PrologException(ISOErrorTerms.evaluationError("float_overflow", ctx));
        }
        if (Double.isNaN(v)) {
            for (Number op : ops) {
                if (!op.isInteger() && Double.isNaN(op.doubleValue())) return new Number(v, false);
            }
            throw new PrologException(ISOErrorTerms.evaluationError("undefined", ctx));
        }
        return new Number(v, false);
    }
    // END_CHANGE: ISS-2025-0359 / ISS-2025-0360

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
    // START_CHANGE: ISS-2025-0591 - shift left by a signed count (negative = right shift)
    private static Number shift(Number a, Number b, boolean left, String ctx) {
        if (a.fitsInLong() && b.fitsInLong()) {              // primitive fast path
            long x = a.longValue(), n = b.longValue();
            long c = left ? n : -n;
            if (c == Long.MIN_VALUE) c = -Long.MAX_VALUE;
            if (c <= 0) {
                long r = -c;
                return i(r >= 63 ? (x < 0 ? -1 : 0) : x >> r);
            }
            if (c < 63 && (x == 0 || Long.numberOfLeadingZeros(x < 0 ? ~x : x) > c + 1)) return i(x << c);
        }
        return shift(a.bigIntegerValue(), left ? b.bigIntegerValue() : b.bigIntegerValue().negate(), ctx);
    }

    private static Number shift(BigInteger v, BigInteger count, String ctx) {
        if (count.bitLength() > 31) {                        // |count| beyond int range
            if (count.signum() < 0) return i(v.signum() < 0 ? -1 : 0);
            if (v.signum() == 0) return i(0);
            throw new PrologException(ISOErrorTerms.resourceError("memory", ctx));
        }
        int c = count.intValue();
        if (c < 0 && v.bitLength() <= 62) {
            long x = v.longValue();
            return i(c <= -63 ? (x < 0 ? -1 : 0) : x >> -c);
        }
        return big(v.shiftLeft(c));
    }
    // END_CHANGE: ISS-2025-0591

    // START_CHANGE: ISS-2025-0590 - exact float -> integral-float conversions
    private static final double TWO_52 = 4503599627370496.0;
    private static double truncTowardZero(double v) { return v < 0 ? Math.ceil(v) : Math.floor(v); }
    private static double roundHalfAway(double v) {
        if (Double.isNaN(v) || Double.isInfinite(v)) return v;
        double a = Math.abs(v);
        if (a >= TWO_52) return v;                           // already integral
        double t = Math.floor(a);
        if (a - t >= 0.5) t += 1.0;                          // a - t is exact below 2^52
        return v < 0 ? -t : t;
    }
    // END_CHANGE: ISS-2025-0590
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
        return new PrologException(ISOErrorTerms.typeError("evaluable", pi, ctx()));
    }
}
