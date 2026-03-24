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
        super(numerator.doubleValue() / denominator.doubleValue());
        if (denominator.signum() == 0) {
            throw new ArithmeticException("Rational: division by zero");
        }
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

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj instanceof Rational) {
            Rational other = (Rational) obj;
            return this.numerator.equals(other.numerator) && this.denominator.equals(other.denominator);
        }
        if (obj instanceof Number) {
            return super.equals(obj);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 31 * numerator.hashCode() + denominator.hashCode();
    }
}
// END_CHANGE: LIM-012
