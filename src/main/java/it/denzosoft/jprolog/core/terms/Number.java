package it.denzosoft.jprolog.core.terms;

import java.math.BigInteger;
import java.util.Map;

// START_CHANGE: LIM-008 - Arbitrary precision integers with dual representation (long/BigInteger/double)
public class Number extends Term {

    private final double doubleValue;
    private final long longValue;
    private final BigInteger bigIntValue;  // null unless needed for values outside long range
    private final boolean isInteger;  // true for integer, false for float

    // START_CHANGE: ISS-2025-0424 - ENG-02: a double NEVER auto-classifies as an ISO integer.
    // Previously this constructor decided "integer if the value has no fractional part", so any
    // built-in that computed a double and wrapped it here silently returned an ISO *integer*
    // whenever the result happened to be integral (sum_list([1.5,1.5],S) gave S = 3, JSON/CSV 1.0
    // parsed to 1). ISO 9.1.3 / 7.1.2: a float never becomes an integer implicitly.
    /**
     * Create a floating-point Number. The value is ALWAYS a float, even when it is integral
     * ({@code new Number(3.0).isInteger()} is {@code false}) — use {@link #Number(long)} /
     * {@link #ofLong(long)} when an ISO integer is intended.
     */
    public Number(double value) {
        this.doubleValue = value;
        this.isInteger = false;
        this.longValue = 0;
        this.bigIntValue = null;
    }

    /** Explicit factory for an ISO integer (equivalent to {@code new Number(long)}). */
    public static Number ofLong(long value) {
        return valueOf(value);
    }

    // START_CHANGE: ISS-2025-0434 - ENG-14: small-integer cache. Arithmetic, list indices, character
    // codes and counters allocate a fresh 48-byte Number for values that are overwhelmingly small;
    // -128..1024 covers character codes, list lengths and loop counters in most programs.
    private static final int CACHE_LOW = -128, CACHE_HIGH = 1024;
    private static final Number[] SMALL = new Number[CACHE_HIGH - CACHE_LOW + 1];
    static {
        for (int v = CACHE_LOW; v <= CACHE_HIGH; v++) SMALL[v - CACHE_LOW] = new Number((long) v);
    }

    /** An ISO integer, reusing a cached instance for small values. Numbers are immutable, so
     *  sharing is invisible to the engine (identity is never significant for a Number). */
    public static Number valueOf(long value) {
        if (value >= CACHE_LOW && value <= CACHE_HIGH) return SMALL[(int) (value - CACHE_LOW)];
        return new Number(value);
    }
    // END_CHANGE: ISS-2025-0434

    /** Explicit factory for an ISO float (equivalent to {@code new Number(double)}). */
    public static Number ofDouble(double value) {
        return new Number(value);
    }

    /**
     * Legacy "classify a double" helper, kept ONLY for the few callers that must reproduce the
     * pre-ISS-2025-0424 behaviour (notably {@link Rational}, whose whole-number values are exact
     * integers). New code must pick {@link #ofLong} or {@link #ofDouble} deliberately.
     */
    public static boolean isIntegralDouble(double value) {
        return (value == Math.floor(value)) && !Double.isInfinite(value)
               && Math.abs(value) <= Long.MAX_VALUE;
    }
    // END_CHANGE: ISS-2025-0424

    /**
     * Create a Number explicitly marked as integer or float.
     */
    public Number(double value, boolean isInteger) {
        this.doubleValue = value;
        this.isInteger = isInteger;
        if (isInteger) {
            this.longValue = (long) value;
            this.bigIntValue = null;
        } else {
            this.longValue = 0;
            this.bigIntValue = null;
        }
    }

    /**
     * Create a Number from a long value (always integer).
     */
    public Number(long value) {
        this.longValue = value;
        this.doubleValue = (double) value;
        this.bigIntValue = null;
        this.isInteger = true;
    }

    /**
     * Create a Number from a BigInteger (always integer).
     */
    public Number(BigInteger value) {
        if (value == null) {
            throw new IllegalArgumentException("BigInteger value cannot be null");
        }
        this.bigIntValue = value;
        this.doubleValue = value.doubleValue();
        // START_CHANGE: ISS-2025-0188 - Fix bitLength threshold: <= 63 covers all long values
        // Try to fit in long
        if (value.bitLength() <= 63) {
            this.longValue = value.longValueExact();
        } else {
            this.longValue = 0; // overflow marker, use bigIntValue
        }
        this.isInteger = true;
    }

    @Override
    public Double getValue() {
        return doubleValue;
    }

    /**
     * Check if this number represents an integer value.
     */
    public boolean isInteger() {
        return isInteger;
    }

    /**
     * Check if this number represents a float value.
     */
    public boolean isFloat() {
        return !isInteger;
    }

    /**
     * Check if this number is stored as an exact integer (long or BigInteger).
     * Same as isInteger() but makes intent clearer.
     */
    public boolean isExactInteger() {
        return isInteger;
    }

    /**
     * Get the value as a long integer. For BigInteger values that overflow long,
     * this returns the low-order 64 bits (same as BigInteger.longValue()).
     */
    public long longValue() {
        if (bigIntValue != null) {
            return bigIntValue.longValue();
        }
        if (isInteger) {
            return longValue;
        }
        return (long) doubleValue;
    }

    /**
     * Get the value as a BigInteger. Works for all integer types.
     * For float values, truncates to BigInteger.
     */
    public BigInteger bigIntegerValue() {
        if (bigIntValue != null) {
            return bigIntValue;
        }
        if (isInteger) {
            return BigInteger.valueOf(longValue);
        }
        // Float: truncate
        return BigInteger.valueOf((long) doubleValue);
    }

    /**
     * Get the double value.
     */
    public double doubleValue() {
        return doubleValue;
    }

    /**
     * Returns true if this integer requires BigInteger representation
     * (i.e., value is outside long range).
     */
    public boolean isBigInteger() {
        return bigIntValue != null && bigIntValue.bitLength() > 63;
    }

    /**
     * Returns true if this integer fits in a long.
     */
    public boolean fitsInLong() {
        if (!isInteger) return false;
        if (bigIntValue != null) {
            return bigIntValue.bitLength() <= 63;
            // END_CHANGE: ISS-2025-0188
        }
        return true;
    }

   @Override
   public boolean unify(Term term, Map<String, Term> substitution) {
		if (term instanceof Variable) {
			return term.unify(this, substitution);
		} else if (term instanceof Number) {
            Number other = (Number) term;
            // START_CHANGE: ISS-2025-0261 - ISO standard order of terms: integers and floats are
            // DISTINCT terms, so 1 does not unify with 1.0 (and 1 \== 1.0). Require the same type
            // AND the same value. Integers compare exactly via BigInteger (also fixes >2^53 longs).
            if (this.isInteger != other.isInteger) {
                return false;
            }
            if (this.isInteger) {
                return this.bigIntegerValue().equals(other.bigIntegerValue());
            }
            return Double.compare(this.doubleValue, other.doubleValue) == 0;
            // END_CHANGE: ISS-2025-0261
        } else {
        	return false;
        }
   }

    @Override
    public boolean isGround() {
        return true;
    }

    @Override
    public String toString() {
        if (isInteger) {
            if (bigIntValue != null && bigIntValue.bitLength() > 63) {
                return bigIntValue.toString();
            }
            return Long.toString(longValue);
        }
        // START_CHANGE: ISS-2025-0390 - ISO float syntax (6.4.5): lowercase exponent 'e' instead
        // of Java's 'E', and SWI-style inf/-inf/nan instead of 'Infinity'/'NaN' (which re-read
        // as fresh VARIABLES). Java's Double.toString only emits 'E' as the exponent marker,
        // so the blanket replace is safe.
        if (Double.isNaN(doubleValue)) return "nan";
        if (Double.isInfinite(doubleValue)) return doubleValue > 0 ? "inf" : "-inf";
        return Double.toString(doubleValue).replace('E', 'e');
        // END_CHANGE: ISS-2025-0390
    }

    @Override
    public Term copy() {
        return this;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        Number number = (Number) obj;
        // START_CHANGE: ISS-2025-0261 - consistent with unify: integers and floats are distinct.
        if (this.isInteger != number.isInteger) {
            return false;
        }
        if (this.isInteger) {
            return this.bigIntegerValue().equals(number.bigIntegerValue());
        }
        return Double.compare(number.doubleValue, doubleValue) == 0;
        // END_CHANGE: ISS-2025-0261
    }

    // START_CHANGE: ISS-2025-0191 - Fix NaN hashCode consistency
    // START_CHANGE: ISS-2025-0261 - keep hashCode consistent with the type-aware equals: an
    // integer and a numerically-equal float are now unequal, so they hash differently.
    @Override
    public int hashCode() {
        if (isInteger) {
            return bigIntegerValue().hashCode();
        }
        // Normalize -0.0 to 0.0 and canonicalize NaN for hashCode consistency with equals
        double val = Double.isNaN(doubleValue) ? Double.NaN : (doubleValue == 0.0) ? 0.0 : doubleValue;
        long temp = Double.doubleToLongBits(val);
        return (int) (temp ^ (temp >>> 32));
    }
    // END_CHANGE: ISS-2025-0261
    // END_CHANGE: ISS-2025-0191
}
// END_CHANGE: LIM-008
