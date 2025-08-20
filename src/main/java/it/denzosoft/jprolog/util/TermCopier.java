package it.denzosoft.jprolog.util;

import it.denzosoft.jprolog.core.terms.*;
import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;

/**
 * Utility class for copying terms while preserving variable sharing.
 * When a rule like digits([], S, S) is copied, both occurrences of S
 * must map to the same new variable instance.
 */
public class TermCopier {
    
    /**
     * Copy a term while preserving variable sharing relationships.
     * Variables that appear multiple times in the original will map to
     * the same variable instance in the copy.
     * 
     * @param term The term to copy
     * @return A fresh copy with preserved variable sharing
     */
    public static Term copyWithSharedVariables(Term term) {
        Map<String, Variable> variableMap = new HashMap<>();
        return copyTermInternal(term, variableMap, "");
    }
    
    /**
     * Copy a list of terms while preserving variable sharing across all terms.
     * This is important for copying rules where variables may appear in both
     * the head and body.
     * 
     * @param terms The list of terms to copy
     * @return A fresh copy with preserved variable sharing
     */
    public static List<Term> copyWithSharedVariables(List<Term> terms) {
        Map<String, Variable> variableMap = new HashMap<>();
        List<Term> result = new ArrayList<>();
        for (Term term : terms) {
            result.add(copyTermInternal(term, variableMap, ""));
        }
        return result;
    }
    
    /**
     * Copy a rule (head and body) while preserving variable sharing.
     * Variables are renamed to avoid conflicts with query variables.
     * 
     * @param head The rule head
     * @param body The rule body
     * @return A pair of copied head and body with shared variables preserved
     */
    public static RuleCopy copyRule(Term head, List<Term> body) {
        Map<String, Variable> variableMap = new HashMap<>();
        long timestamp = System.nanoTime(); // Use timestamp to ensure uniqueness
        Term copiedHead = copyTermInternal(head, variableMap, "_R" + timestamp + "_");
        List<Term> copiedBody = new ArrayList<>();
        for (Term term : body) {
            copiedBody.add(copyTermInternal(term, variableMap, "_R" + timestamp + "_"));
        }
        return new RuleCopy(copiedHead, copiedBody);
    }
    
    /**
     * Helper class to return both head and body of a copied rule.
     */
    public static class RuleCopy {
        public final Term head;
        public final List<Term> body;
        
        public RuleCopy(Term head, List<Term> body) {
            this.head = head;
            this.body = body;
        }
    }
    
    private static Term copyTermInternal(Term term, Map<String, Variable> variableMap, String prefix) {
        if (term instanceof Variable) {
            Variable var = (Variable) term;
            String name = var.getName();
            
            // For anonymous variables, always create a new instance
            if (name.equals("_")) {
                return new Variable("_");
            }
            
            // For named variables, reuse the same instance for the same name
            if (!variableMap.containsKey(name)) {
                String uniqueName = prefix + name;
                variableMap.put(name, new Variable(uniqueName));
            }
            return variableMap.get(name);
            
        } else if (term instanceof Atom) {
            // Atoms are immutable, just return a new instance
            return new Atom(((Atom) term).getName());
            
        } else if (term instanceof it.denzosoft.jprolog.core.terms.Number) {
            // Numbers are immutable, just return a new instance
            return new it.denzosoft.jprolog.core.terms.Number(((it.denzosoft.jprolog.core.terms.Number) term).getValue());
            
        } else if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            List<Term> copiedArgs = new ArrayList<>();
            for (Term arg : compound.getArguments()) {
                copiedArgs.add(copyTermInternal(arg, variableMap, prefix));
            }
            return new CompoundTerm(new Atom(compound.getName()), copiedArgs);
            
        } else {
            // Fallback to the term's own copy method
            return term.copy();
        }
    }
}