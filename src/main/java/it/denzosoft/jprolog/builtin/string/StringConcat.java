package it.denzosoft.jprolog.builtin.string;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of string_concat/3 predicate.
 * 
 * string_concat(?String1, ?String2, ?String3)
 * 
 * True if String3 is the concatenation of String1 and String2.
 * Can work in multiple modes depending on which arguments are instantiated.
 */
public class StringConcat implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<java.lang.String, Term> bindings, List<Map<java.lang.String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("string_concat/3 requires exactly 3 arguments.");
        }
        
        Term string1Term = query.getArguments().get(0).resolveBindings(bindings);
        Term string2Term = query.getArguments().get(1).resolveBindings(bindings);
        Term string3Term = query.getArguments().get(2).resolveBindings(bindings);
        
        boolean s1IsVar = string1Term instanceof Variable;
        boolean s2IsVar = string2Term instanceof Variable;
        boolean s3IsVar = string3Term instanceof Variable;
        
        Map<java.lang.String, Term> newBindings = new HashMap<>(bindings);
        
        if (!s1IsVar && !s2IsVar && !s3IsVar) {
            // All instantiated - check if concatenation holds
            return checkConcatenation(string1Term, string2Term, string3Term, newBindings, solutions);
            
        } else if (!s1IsVar && !s2IsVar && s3IsVar) {
            // Concatenate mode: string_concat(+String1, +String2, -String3)
            return concatenateMode(string1Term, string2Term, string3Term, newBindings, solutions);
            
        } else if (!s1IsVar && s2IsVar && !s3IsVar) {
            // Extract suffix mode: string_concat(+String1, -String2, +String3)
            return extractSuffixMode(string1Term, string2Term, string3Term, newBindings, solutions);
            
        } else if (s1IsVar && !s2IsVar && !s3IsVar) {
            // Extract prefix mode: string_concat(-String1, +String2, +String3)
            return extractPrefixMode(string1Term, string2Term, string3Term, newBindings, solutions);
            
        } else if (s1IsVar && s2IsVar && !s3IsVar) {
            // Generate all possible splits: string_concat(-String1, -String2, +String3)
            return generateSplitsMode(string1Term, string2Term, string3Term, newBindings, solutions);
            
        } else {
            throw new PrologEvaluationException("string_concat/3: at least two arguments must be instantiated.");
        }
    }
    
    private boolean checkConcatenation(Term s1, Term s2, Term s3, Map<java.lang.String, Term> bindings, List<Map<java.lang.String, Term>> solutions) {
        if (!(s1 instanceof PrologString) || !(s2 instanceof PrologString) || !(s3 instanceof PrologString)) {
            return false;
        }
        
        java.lang.String str1 = ((PrologString) s1).getStringValue();
        java.lang.String str2 = ((PrologString) s2).getStringValue();
        java.lang.String str3 = ((PrologString) s3).getStringValue();
        
        if ((str1 + str2).equals(str3)) {
            solutions.add(new HashMap<>(bindings));
            return true;
        }
        
        return false;
    }
    
    private boolean concatenateMode(Term s1, Term s2, Term s3Var, Map<java.lang.String, Term> bindings, List<Map<java.lang.String, Term>> solutions) {
        if (!(s1 instanceof PrologString) || !(s2 instanceof PrologString)) {
            return false;
        }
        
        java.lang.String str1 = ((PrologString) s1).getStringValue();
        java.lang.String str2 = ((PrologString) s2).getStringValue();
        java.lang.String result = str1 + str2;
        
        PrologString resultString = new PrologString(result);
        if (s3Var.unify(resultString, bindings)) {
            solutions.add(new HashMap<>(bindings));
            return true;
        }
        
        return false;
    }
    
    private boolean extractSuffixMode(Term s1, Term s2Var, Term s3, Map<java.lang.String, Term> bindings, List<Map<java.lang.String, Term>> solutions) {
        if (!(s1 instanceof PrologString) || !(s3 instanceof PrologString)) {
            return false;
        }
        
        java.lang.String str1 = ((PrologString) s1).getStringValue();
        java.lang.String str3 = ((PrologString) s3).getStringValue();
        
        if (str3.startsWith(str1)) {
            java.lang.String suffix = str3.substring(str1.length());
            PrologString suffixString = new PrologString(suffix);
            
            Map<java.lang.String, Term> newBindings = new HashMap<>(bindings);
            if (s2Var.unify(suffixString, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
        }
        
        return false;
    }
    
    private boolean extractPrefixMode(Term s1Var, Term s2, Term s3, Map<java.lang.String, Term> bindings, List<Map<java.lang.String, Term>> solutions) {
        if (!(s2 instanceof PrologString) || !(s3 instanceof PrologString)) {
            return false;
        }
        
        java.lang.String str2 = ((PrologString) s2).getStringValue();
        java.lang.String str3 = ((PrologString) s3).getStringValue();
        
        if (str3.endsWith(str2)) {
            java.lang.String prefix = str3.substring(0, str3.length() - str2.length());
            PrologString prefixString = new PrologString(prefix);
            
            Map<java.lang.String, Term> newBindings = new HashMap<>(bindings);
            if (s1Var.unify(prefixString, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
        }
        
        return false;
    }
    
    private boolean generateSplitsMode(Term s1Var, Term s2Var, Term s3, Map<java.lang.String, Term> bindings, List<Map<java.lang.String, Term>> solutions) {
        if (!(s3 instanceof PrologString)) {
            return false;
        }
        
        java.lang.String str3 = ((PrologString) s3).getStringValue();
        boolean foundSolution = false;
        
        // Generate all possible splits
        for (int i = 0; i <= str3.length(); i++) {
            java.lang.String prefix = str3.substring(0, i);
            java.lang.String suffix = str3.substring(i);
            
            PrologString prefixString = new PrologString(prefix);
            PrologString suffixString = new PrologString(suffix);
            
            Map<java.lang.String, Term> newBindings = new HashMap<>(bindings);
            if (s1Var.unify(prefixString, newBindings) && s2Var.unify(suffixString, newBindings)) {
                solutions.add(newBindings);
                foundSolution = true;
            }
        }
        
        return foundSolution;
    }
}