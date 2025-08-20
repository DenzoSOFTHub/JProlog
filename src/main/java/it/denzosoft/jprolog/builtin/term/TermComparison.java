package it.denzosoft.jprolog.builtin.term;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;

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
        // Use ISO 13211-1 standard term ordering
        return StandardTermOrdering.compare(term1, term2);
    }
    
    private boolean termsIdentical(Term term1, Term term2) {
        // Use standard term ordering for identity check
        return StandardTermOrdering.identical(term1, term2);
    }
}
