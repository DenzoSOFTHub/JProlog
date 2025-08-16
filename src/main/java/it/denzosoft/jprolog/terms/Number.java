package it.denzosoft.jprolog.terms;

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
}
