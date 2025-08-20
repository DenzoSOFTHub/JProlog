package it.denzosoft.jprolog.core.terms;

import java.util.Map;



public class Number extends Term {

    private double value;

    public Number(double value) {
        this.value = value;
    }

    @Override
    public Double getValue() {
        return value;
    }

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
        return Double.toString(value);
    }

    @Override
    public Term copy() {
        return new Number(this.value);
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
