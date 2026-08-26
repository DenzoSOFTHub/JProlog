package it.denzosoft.jprolog.builtin.string;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of sub_string/5 predicate.
 * 
 * sub_string(+String, ?Before, ?Length, ?After, ?SubString)
 * 
 * True if SubString is a substring of String, where:
 * - Before is the number of characters before the substring
 * - Length is the length of the substring
 * - After is the number of characters after the substring
 * - Before + Length + After = length of String
 */
public class SubString implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<java.lang.String, Term> bindings, List<Map<java.lang.String, Term>> solutions) {
        if (query.getArguments().size() != 5) {
            throw new PrologEvaluationException("sub_string/5 requires exactly 5 arguments.");
        }
        
        Term stringTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term beforeTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term lengthTerm = query.getArguments().get(2).resolveBindings(bindings);
        Term afterTerm = query.getArguments().get(3).resolveBindings(bindings);
        Term subStringTerm = query.getArguments().get(4).resolveBindings(bindings);
        
        // First argument must be a string
        if (stringTerm instanceof Variable) {
            throw new PrologEvaluationException("sub_string/5: first argument must be instantiated to a string.");
        }
        
        if (!(stringTerm instanceof PrologString)) {
            return false;
        }
        
        java.lang.String mainString = ((PrologString) stringTerm).getStringValue();
        int mainLength = mainString.length();

        // START_CHANGE: ISS-2025-0172 - Optimize sub_string/5 from O(n^2) to constraint-aware
        // Get bound values if instantiated
        Integer beforeVal = getIntegerValue(beforeTerm);
        Integer lengthVal = getIntegerValue(lengthTerm);
        Integer afterVal = getIntegerValue(afterTerm);
        java.lang.String subStringVal = getStringValue(subStringTerm);

        boolean foundSolution = false;

        if (beforeVal != null && lengthVal != null) {
            // Both Before and Length bound: direct check, no loop
            int b = beforeVal, len = lengthVal;
            int a = mainLength - b - len;
            if (b >= 0 && len >= 0 && b + len <= mainLength && (afterVal == null || afterVal == a)) {
                java.lang.String sub = mainString.substring(b, b + len);
                if (subStringVal == null || subStringVal.equals(sub)) {
                    foundSolution = trySolution(b, len, a, sub, beforeTerm, lengthTerm, afterTerm, subStringTerm, bindings, solutions);
                }
            }
        } else if (beforeVal != null) {
            // Before bound: single loop over length
            int b = beforeVal;
            if (b >= 0 && b <= mainLength) {
                for (int len = 0; len <= mainLength - b; len++) {
                    int a = mainLength - b - len;
                    if (afterVal != null && afterVal != a) continue;
                    java.lang.String sub = mainString.substring(b, b + len);
                    if (subStringVal != null && !subStringVal.equals(sub)) continue;
                    if (trySolution(b, len, a, sub, beforeTerm, lengthTerm, afterTerm, subStringTerm, bindings, solutions)) {
                        foundSolution = true;
                    }
                }
            }
        } else if (lengthVal != null) {
            // Length bound: single loop over before
            int len = lengthVal;
            if (len >= 0 && len <= mainLength) {
                for (int b = 0; b <= mainLength - len; b++) {
                    int a = mainLength - b - len;
                    if (afterVal != null && afterVal != a) continue;
                    java.lang.String sub = mainString.substring(b, b + len);
                    if (subStringVal != null && !subStringVal.equals(sub)) continue;
                    if (trySolution(b, len, a, sub, beforeTerm, lengthTerm, afterTerm, subStringTerm, bindings, solutions)) {
                        foundSolution = true;
                    }
                }
            }
        } else if (subStringVal != null) {
            // SubString bound: search for matching positions
            int subLen = subStringVal.length();
            int idx = 0;
            while ((idx = mainString.indexOf(subStringVal, idx)) != -1) {
                int b = idx, len = subLen, a = mainLength - b - len;
                if (afterVal != null && afterVal != a) { idx++; continue; }
                if (trySolution(b, len, a, subStringVal, beforeTerm, lengthTerm, afterTerm, subStringTerm, bindings, solutions)) {
                    foundSolution = true;
                }
                idx++;
            }
            if (subStringVal.isEmpty()) {
                for (int b = 0; b <= mainLength; b++) {
                    int a = mainLength - b;
                    if (afterVal != null && afterVal != a) continue;
                    if (trySolution(b, 0, a, "", beforeTerm, lengthTerm, afterTerm, subStringTerm, bindings, solutions)) {
                        foundSolution = true;
                    }
                }
            }
        } else {
            // Fallback: fully unbound - double loop
            for (int before = 0; before <= mainLength; before++) {
                for (int length = 0; length <= mainLength - before; length++) {
                    int after = mainLength - before - length;
                    if (afterVal != null && afterVal != after) continue;
                    java.lang.String substring = mainString.substring(before, before + length);
                    if (trySolution(before, length, after, substring, beforeTerm, lengthTerm, afterTerm, subStringTerm, bindings, solutions)) {
                        foundSolution = true;
                    }
                }
            }
        }
        // END_CHANGE: ISS-2025-0172

        return foundSolution;
    }

    // START_CHANGE: ISS-2025-0172 - Helper methods for constraint-aware sub_string/5
    private boolean trySolution(int b, int len, int a, java.lang.String sub,
                                Term beforeTerm, Term lengthTerm, Term afterTerm, Term subStringTerm,
                                Map<java.lang.String, Term> bindings, List<Map<java.lang.String, Term>> solutions) {
        Map<java.lang.String, Term> newBindings = new HashMap<>(bindings);
        boolean ok = true;
        if (ok) ok = beforeTerm.unify(new Number((long) b)   /* ISS-2025-0424 */, newBindings);
        if (ok) ok = lengthTerm.unify(new Number((long) len)   /* ISS-2025-0424 */, newBindings);
        if (ok) ok = afterTerm.unify(new Number((long) a)   /* ISS-2025-0424 */, newBindings);
        if (ok) ok = subStringTerm.unify(new PrologString(sub), newBindings);
        if (ok) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    private Integer getIntegerValue(Term term) {
        if (term instanceof Number) {
            double value = ((Number) term).getValue();
            if (value == Math.floor(value) && value >= 0) {
                return (int) value;
            }
        }
        return null;
    }

    private java.lang.String getStringValue(Term term) {
        if (term instanceof PrologString) {
            return ((PrologString) term).getStringValue();
        }
        return null;
    }
    // END_CHANGE: ISS-2025-0172
}