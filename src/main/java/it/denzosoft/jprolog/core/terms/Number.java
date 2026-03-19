package it.denzosoft.jprolog.core.terms;

import java.util.Map;

// START_CHANGE: ISS-2025-0056 - Distinguish integer and float types for ISO compliance
public class Number extends Term {

    private double value;
    private boolean isInteger;

    public Number(double value) {
        this.value = value;
        // Determine if value is an integer: no fractional part and within long range
        this.isInteger = (value == Math.floor(value)) && !Double.isInfinite(value)
                         && Math.abs(value) <= Long.MAX_VALUE;
    }

    /**
     * Create a Number explicitly marked as integer or float.
     */
    public Number(double value, boolean isInteger) {
        this.value = value;
        this.isInteger = isInteger;
    }

    @Override
    public Double getValue() {
        return value;
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
     * Get the value as a long integer (truncating any fractional part).
     */
    public long longValue() {
        return (long) value;
    }
    // END_CHANGE: ISS-2025-0056

   @Override
   public boolean unify(Term term, Map<String, Term> substitution) {
		if (term instanceof Variable) {
			return term.unify(this, substitution);
		} else if (term instanceof Number) {
            return this.value == ((Number) term).value;
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
        // START_CHANGE: ISS-2025-0056 - Display integers without decimal point
        if (isInteger && value == Math.floor(value) && !Double.isInfinite(value)) {
            long lv = (long) value;
            return Long.toString(lv);
        }
        return Double.toString(value);
        // END_CHANGE: ISS-2025-0056
    }

    @Override
    public Term copy() {
        return new Number(this.value, this.isInteger);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        Number number = (Number) obj;
        return Double.compare(number.value, value) == 0;
    }

    @Override
    public int hashCode() {
        long temp = Double.doubleToLongBits(value);
        return (int) (temp ^ (temp >>> 32));
    }
}
