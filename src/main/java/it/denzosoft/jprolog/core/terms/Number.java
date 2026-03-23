package it.denzosoft.jprolog.core.terms;

import java.math.BigInteger;
import java.util.Map;

// START_CHANGE: LIM-008 - Arbitrary precision integers with dual representation (long/BigInteger/double)
public class Number extends Term {

    private final double doubleValue;
    private final long longValue;
    private final BigInteger bigIntValue;  // null unless needed for values outside long range
    private final boolean isInteger;  // true for integer, false for float

    /**
     * Create a Number from a double. If the value has no fractional part and fits
     * in a long, it is treated as an integer.
     */
    public Number(double value) {
        this.doubleValue = value;
        // Determine if value is an integer: no fractional part and within long range
        this.isInteger = (value == Math.floor(value)) && !Double.isInfinite(value)
                         && Math.abs(value) <= Long.MAX_VALUE;
        if (this.isInteger) {
            this.longValue = (long) value;
            this.bigIntValue = null;
        } else {
            this.longValue = 0;
            this.bigIntValue = null;
        }
    }

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
        // Try to fit in long
        if (value.bitLength() < 63) {
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
        return bigIntValue != null && bigIntValue.bitLength() >= 63;
    }

    /**
     * Returns true if this integer fits in a long.
     */
    public boolean fitsInLong() {
        if (!isInteger) return false;
        if (bigIntValue != null) {
            return bigIntValue.bitLength() < 63;
        }
        return true;
    }

   @Override
   public boolean unify(Term term, Map<String, Term> substitution) {
		if (term instanceof Variable) {
			return term.unify(this, substitution);
		} else if (term instanceof Number) {
            Number other = (Number) term;
            // For BigInteger values, compare BigIntegers
            if (this.isBigInteger() || other.isBigInteger()) {
                if (this.isInteger && other.isInteger) {
                    return this.bigIntegerValue().equals(other.bigIntegerValue());
                }
                return this.doubleValue == other.doubleValue;
            }
            return this.doubleValue == other.doubleValue;
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
            if (bigIntValue != null && bigIntValue.bitLength() >= 63) {
                return bigIntValue.toString();
            }
            return Long.toString(longValue);
        }
        return Double.toString(doubleValue);
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
        // For BigInteger values, compare BigIntegers
        if (this.isBigInteger() || number.isBigInteger()) {
            if (this.isInteger && number.isInteger) {
                return this.bigIntegerValue().equals(number.bigIntegerValue());
            }
        }
        return Double.compare(number.doubleValue, doubleValue) == 0;
    }

    @Override
    public int hashCode() {
        if (isBigInteger()) {
            return bigIntValue.hashCode();
        }
        long temp = Double.doubleToLongBits(doubleValue);
        return (int) (temp ^ (temp >>> 32));
    }
}
// END_CHANGE: LIM-008
