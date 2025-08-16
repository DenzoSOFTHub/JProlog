package it.denzosoft.jprolog.builtin.term;

import it.denzosoft.jprolog.BuiltIn;
import it.denzosoft.jprolog.PrologEvaluationException;
import it.denzosoft.jprolog.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class TermComparison implements BuiltIn {
    public enum ComparisonType {
        AT_LESS_EQUAL("@=<"),
        AT_LESS("@<"),
        AT_GREATER("@>"),
        AT_GREATER_EQUAL("@>="),
        TERM_EQUAL("=="),
        TERM_NOT_EQUAL("\\==");
        
        private final String symbol;
        
        ComparisonType(String symbol) {
            this.symbol = symbol;
        }
        
        public String getSymbol() {
            return symbol;
        }
    }
    
    private final ComparisonType type;
    
    public TermComparison(ComparisonType type) {
        this.type = type;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException(type.getSymbol() + "/2 requires exactly 2 arguments.");
        }

        Term term1 = query.getArguments().get(0);
        Term term2 = query.getArguments().get(1);
        
        Term resolvedTerm1 = term1.resolveBindings(bindings);
        Term resolvedTerm2 = term2.resolveBindings(bindings);
        
        boolean result;
        switch (type) {
            case AT_LESS_EQUAL:
                result = compareTerms(resolvedTerm1, resolvedTerm2) <= 0;
                break;
            case AT_LESS:
                result = compareTerms(resolvedTerm1, resolvedTerm2) < 0;
                break;
            case AT_GREATER:
                result = compareTerms(resolvedTerm1, resolvedTerm2) > 0;
                break;
            case AT_GREATER_EQUAL:
                result = compareTerms(resolvedTerm1, resolvedTerm2) >= 0;
                break;
            case TERM_EQUAL:
                result = termsIdentical(resolvedTerm1, resolvedTerm2);
                break;
            case TERM_NOT_EQUAL:
                result = !termsIdentical(resolvedTerm1, resolvedTerm2);
                break;
            default:
                throw new PrologEvaluationException("Unknown comparison type: " + type);
        }
        
        if (result) {
            solutions.add(new HashMap<>(bindings));
            return true;
        }
        return false;
    }
    
    private int compareTerms(Term term1, Term term2) {
        // Simple implementation - for a full implementation,
        // we would need to follow Prolog standard term ordering
        return term1.toString().compareTo(term2.toString());
    }
    
    private boolean termsIdentical(Term term1, Term term2) {
        return term1.toString().equals(term2.toString());
    }
}
