package it.denzosoft.jprolog.core.terms;

// START_CHANGE: LIM-012 - Rational number support
import java.math.BigInteger;
import java.util.Map;

/**
 * Represents an exact rational number (numerator/denominator).
 * Stored in lowest terms with positive denominator.
 * Extends Number for compatibility with arithmetic operations.
 */
public class Rational extends Number {

    private final BigInteger numerator;
    private final BigInteger denominator;

    // START_CHANGE: ISS-2025-0424 - ENG-02: shared by the constructor's two super() arguments
    private static double ratioAsDouble(BigInteger numerator, BigInteger denominator) {
        if (denominator.signum() == 0) return throwZeroDenominator();
        // START_CHANGE: ISS-2025-0712 - the quotient of the two doubles overflows (10^400r3 was
        // NaN) and rounds twice; a 40-digit decimal quotient rounds once, for any magnitude.
        if (numerator.bitLength() <= 53 && denominator.bitLength() <= 53) {
            return numerator.doubleValue() / denominator.doubleValue();   // both exact: one rounding
        }
        return new java.math.BigDecimal(numerator).divide(new java.math.BigDecimal(denominator),
            new java.math.MathContext(40, java.math.RoundingMode.HALF_EVEN)).doubleValue();
        // END_CHANGE: ISS-2025-0712
    }
    // END_CHANGE: ISS-2025-0424

    /**
     * Create a rational from numerator and denominator.
     * Automatically reduces to lowest terms.
     */
    public Rational(long numerator, long denominator) {
        this(BigInteger.valueOf(numerator), BigInteger.valueOf(denominator));
    }

    /**
     * Create a rational from BigInteger numerator and denominator.
     */
    public Rational(BigInteger numerator, BigInteger denominator) {
        // START_CHANGE: ISS-2025-0185 - Check zero denominator before division
        // START_CHANGE: ISS-2025-0424 - ENG-02: Number(double) no longer auto-classifies integral
        // values as ISO integers, but a whole rational (6 rdiv 2 = 3) IS an exact integer, so keep
        // the legacy classification explicitly here via the two-argument constructor.
        super(ratioAsDouble(numerator, denominator),
              isIntegralDouble(ratioAsDouble(numerator, denominator)));
        // END_CHANGE: ISS-2025-0424
        // END_CHANGE: ISS-2025-0185
        // Normalize: GCD reduction and positive denominator
        BigInteger gcd = numerator.gcd(denominator);
        this.numerator = denominator.signum() < 0
            ? numerator.negate().divide(gcd)
            : numerator.divide(gcd);
        this.denominator = denominator.abs().divide(gcd);
    }

    public BigInteger getNumerator() {
        return numerator;
    }

    public BigInteger getDenominator() {
        return denominator;
    }

    /**
     * Check if this rational is an integer (denominator == 1).
     */
    public boolean isWholeNumber() {
        return denominator.equals(BigInteger.ONE);
    }

    /**
     * Add two rationals.
     */
    public Rational add(Rational other) {
        BigInteger num = this.numerator.multiply(other.denominator)
                         .add(other.numerator.multiply(this.denominator));
        BigInteger den = this.denominator.multiply(other.denominator);
        return new Rational(num, den);
    }

    /**
     * Subtract two rationals.
     */
    public Rational subtract(Rational other) {
        BigInteger num = this.numerator.multiply(other.denominator)
                         .subtract(other.numerator.multiply(this.denominator));
        BigInteger den = this.denominator.multiply(other.denominator);
        return new Rational(num, den);
    }

    /**
     * Multiply two rationals.
     */
    public Rational multiply(Rational other) {
        return new Rational(
            this.numerator.multiply(other.numerator),
            this.denominator.multiply(other.denominator)
        );
    }

    /**
     * Divide two rationals.
     */
    public Rational divide(Rational other) {
        if (other.numerator.signum() == 0) {
            throw new ArithmeticException("Rational: division by zero");
        }
        return new Rational(
            this.numerator.multiply(other.denominator),
            this.denominator.multiply(other.numerator)
        );
    }

    /**
     * Negate this rational.
     */
    public Rational negate() {
        return new Rational(numerator.negate(), denominator);
    }

    /**
     * Absolute value.
     */
    public Rational abs() {
        return new Rational(numerator.abs(), denominator);
    }

    /**
     * Compare to another rational.
     */
    public int compareTo(Rational other) {
        BigInteger lhs = this.numerator.multiply(other.denominator);
        BigInteger rhs = other.numerator.multiply(this.denominator);
        return lhs.compareTo(rhs);
    }

    /**
     * Convert integer Number to Rational.
     */
    public static Rational fromNumber(Number num) {
        if (num instanceof Rational) return (Rational) num;
        if (num.isInteger()) {
            return new Rational(num.bigIntegerValue(), BigInteger.ONE);
        }
        // For floats, approximate as rational (not exact)
        double d = num.doubleValue();
        long denom = 1000000000L; // 10^9 precision
        long numer = Math.round(d * denom);
        return new Rational(numer, denom);
    }

    @Override
    public boolean isInteger() {
        return isWholeNumber();
    }

    // START_CHANGE: ISS-2025-0712 - wave Q2.3: rationals are a real number kind of the v4 engine
    // (SWI-Prolog 9 semantics, prefer_rationals = false). The engine only ever builds them through
    // of(), which normalises (lowest terms, denominator > 0) and answers a plain integer Number
    // when the denominator is 1 — so a Rational the machine sees is never whole, never a float.

    /** A rational is not a float: {@code float(1r3)} fails (Number's default is !isInteger()). */
    @Override
    public boolean isFloat() {
        return false;
    }

    /** Truncation toward zero (the integer part), not the double image's. */
    @Override
    public BigInteger bigIntegerValue() {
        return numerator.divide(denominator);
    }

    @Override
    public long longValue() {
        return bigIntegerValue().longValue();
    }

    @Override
    public boolean isLongInteger() {
        return isWholeNumber() && numerator.bitLength() < 64;
    }

    @Override
    public boolean fitsInLong() {
        return isWholeNumber() && numerator.bitLength() <= 63;
    }

    @Override
    public boolean isBigInteger() {
        return isWholeNumber() && numerator.bitLength() > 63;
    }

    /**
     * The normalised value {@code n/d}: an integer {@link Number} when {@code d} divides {@code n},
     * a {@code Rational} in lowest terms with a positive denominator otherwise.
     *
     * @throws ArithmeticException when {@code d} is zero
     */
    public static Number of(BigInteger n, BigInteger d) {
        if (d.signum() == 0) throwZeroDenominator();
        if (d.signum() < 0) { n = n.negate(); d = d.negate(); }
        BigInteger g = n.gcd(d);
        if (!g.equals(BigInteger.ONE) && g.signum() != 0) { n = n.divide(g); d = d.divide(g); }
        if (d.equals(BigInteger.ONE)) {
            return (n.bitLength() <= 63) ? Number.valueOf(n.longValue()) : new Number(n);
        }
        return new Rational(n, d, true);
    }

    /** Already normalised (lowest terms, d > 1): no gcd. */
    private Rational(BigInteger n, BigInteger d, boolean normalised) {
        super(ratioAsDouble(n, d), false);
        this.numerator = n;
        this.denominator = d;
    }

    /** The numerator of an integer or rational (not a float). */
    public static BigInteger numeratorOf(Number x) {
        return (x instanceof Rational) ? ((Rational) x).numerator : x.bigIntegerValue();
    }

    /** The denominator of an integer or rational (not a float): 1 for an integer. */
    public static BigInteger denominatorOf(Number x) {
        return (x instanceof Rational) ? ((Rational) x).denominator : BigInteger.ONE;
    }

    /** Exact comparison of two integers/rationals (neither may be a float). */
    public static int compareExact(Number a, Number b) {
        if (!(a instanceof Rational) && !(b instanceof Rational)) {
            return a.bigIntegerValue().compareTo(b.bigIntegerValue());
        }
        return numeratorOf(a).multiply(denominatorOf(b)).compareTo(numeratorOf(b).multiply(denominatorOf(a)));
    }

    /** SWI-Prolog's syntax: {@code 1r3}, {@code -1r3}. */
    @Override
    public String toString() {
        if (isWholeNumber()) {
            return numerator.toString();
        }
        return numerator.toString() + "r" + denominator.toString();
    }

    /** Immutable, like every number. */
    @Override
    public Term copy() {
        return this;
    }
    // END_CHANGE: ISS-2025-0712

    // START_CHANGE: ISS-2025-0189 - Exact rational unification instead of double comparison
    @Override
    public boolean unify(Term term, Map<String, Term> substitution) {
        if (term instanceof Variable) {
            return term.unify(this, substitution);
        }
        if (term instanceof Rational) {
            Rational other = (Rational) term;
            return this.numerator.equals(other.numerator) && this.denominator.equals(other.denominator);
        }
        if (term instanceof Number) {
            Number other = (Number) term;
            // A rational with denominator 1 can unify with an integer
            if (this.isWholeNumber() && other.isInteger()) {
                return this.numerator.equals(other.bigIntegerValue());
            }
            return false;
        }
        return false;
    }
    // END_CHANGE: ISS-2025-0189

    // START_CHANGE: ISS-2025-0190 - Fix equals/hashCode contract violation
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj instanceof Rational) {
            Rational other = (Rational) obj;
            return this.numerator.equals(other.numerator) && this.denominator.equals(other.denominator);
        }
        // Do NOT delegate to super.equals(Number) — different hashCode would violate contract
        return false;
    }

    @Override
    public int hashCode() {
        return 31 * numerator.hashCode() + denominator.hashCode();
    }
    // END_CHANGE: ISS-2025-0190

    // START_CHANGE: ISS-2025-0185 - Helper for zero denominator check before super()
    private static double throwZeroDenominator() {
        throw new ArithmeticException("Rational: division by zero");
    }
    // END_CHANGE: ISS-2025-0185
}
// END_CHANGE: LIM-012
