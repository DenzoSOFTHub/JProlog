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
        return numerator.doubleValue() / denominator.doubleValue();
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

    @Override
    public String toString() {
        if (isWholeNumber()) {
            return numerator.toString();
        }
        return numerator.toString() + " rdiv " + denominator.toString();
    }

    @Override
    public Term copy() {
        return new Rational(numerator, denominator);
    }

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
