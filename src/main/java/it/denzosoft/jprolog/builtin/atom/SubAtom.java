package it.denzosoft.jprolog.builtin.atom;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Implementation of sub_atom/5 predicate.
 * 
 * sub_atom(+Atom, ?Before, ?Length, ?After, ?SubAtom)
 * 
 * True if SubAtom is a sub-atom of Atom that begins Before characters from the start,
 * has Length characters, and is followed by After characters.
 * 
 * Examples:
 * ?- sub_atom(hello, 1, 3, 1, X).
 * X = ell.
 * 
 * ?- sub_atom(hello, X, 2, Y, el).
 * X = 1, Y = 2.
 * 
 * ?- sub_atom(hello, _, _, _, X).
 * X = '' ; X = h ; X = he ; X = hel ; X = hell ; X = hello ; X = e ; X = el ; ...
 */
public class SubAtom implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 5) {
            throw new PrologEvaluationException("sub_atom/5 requires exactly 5 arguments");
        }
        
        Term atomTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term beforeTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term lengthTerm = query.getArguments().get(2).resolveBindings(bindings);
        Term afterTerm = query.getArguments().get(3).resolveBindings(bindings);
        Term subAtomTerm = query.getArguments().get(4).resolveBindings(bindings);
        
        // The main atom must be instantiated
        if (!(atomTerm instanceof Atom)) {
            throw new PrologEvaluationException("sub_atom/5: first argument must be an atom");
        }
        
        String atom = ((Atom) atomTerm).getName();
        int atomLength = atom.length();
        
        // Get values if instantiated, otherwise use variables
        Integer before = getIntegerValue(beforeTerm);
        Integer length = getIntegerValue(lengthTerm);
        Integer after = getIntegerValue(afterTerm);
        String subAtom = getAtomValue(subAtomTerm);
        
        boolean foundSolution = false;

        // START_CHANGE: ISS-2025-0172 - Optimize sub_atom/5 from O(n^2) to constraint-aware
        if (before != null && length != null) {
            // Both Before and Length are bound: single direct check, no loop needed
            int b = before;
            int len = length;
            int a = atomLength - b - len;
            if (b >= 0 && len >= 0 && b + len <= atomLength && (after == null || after == a)) {
                String sub = atom.substring(b, b + len);
                if (subAtom == null || subAtom.equals(sub)) {
                    foundSolution = trySolution(b, len, a, sub, beforeTerm, lengthTerm, afterTerm, subAtomTerm, bindings, solutions);
                }
            }
        } else if (before != null) {
            // Before is bound: single loop over Length
            int b = before;
            if (b >= 0 && b <= atomLength) {
                for (int len = 0; len <= atomLength - b; len++) {
                    int a = atomLength - b - len;
                    if (after != null && after != a) continue;
                    String sub = atom.substring(b, b + len);
                    if (subAtom != null && !subAtom.equals(sub)) continue;
                    if (trySolution(b, len, a, sub, beforeTerm, lengthTerm, afterTerm, subAtomTerm, bindings, solutions)) {
                        foundSolution = true;
                    }
                }
            }
        } else if (length != null) {
            // Length is bound: single loop over Before
            int len = length;
            if (len >= 0 && len <= atomLength) {
                for (int b = 0; b <= atomLength - len; b++) {
                    int a = atomLength - b - len;
                    if (after != null && after != a) continue;
                    String sub = atom.substring(b, b + len);
                    if (subAtom != null && !subAtom.equals(sub)) continue;
                    if (trySolution(b, len, a, sub, beforeTerm, lengthTerm, afterTerm, subAtomTerm, bindings, solutions)) {
                        foundSolution = true;
                    }
                }
            }
        } else if (subAtom != null) {
            // SubAtom is bound (ground): search for matching positions only
            int subLen = subAtom.length();
            int idx = 0;
            while ((idx = atom.indexOf(subAtom, idx)) != -1) {
                int b = idx;
                int len = subLen;
                int a = atomLength - b - len;
                if (after != null && after != a) { idx++; continue; }
                if (trySolution(b, len, a, subAtom, beforeTerm, lengthTerm, afterTerm, subAtomTerm, bindings, solutions)) {
                    foundSolution = true;
                }
                idx++;
            }
            // Also handle empty sub_atom matches if subAtom is empty
            if (subAtom.isEmpty()) {
                for (int b = 0; b <= atomLength; b++) {
                    int a = atomLength - b;
                    if (after != null && after != a) continue;
                    if (trySolution(b, 0, a, "", beforeTerm, lengthTerm, afterTerm, subAtomTerm, bindings, solutions)) {
                        foundSolution = true;
                    }
                }
            }
        } else {
            // Fallback: fully unbound case - double loop
            for (int b = 0; b <= atomLength; b++) {
                for (int len = 0; len <= atomLength - b; len++) {
                    int a = atomLength - b - len;
                    if (after != null && after != a) continue;
                    String sub = atom.substring(b, b + len);
                    if (trySolution(b, len, a, sub, beforeTerm, lengthTerm, afterTerm, subAtomTerm, bindings, solutions)) {
                        foundSolution = true;
                    }
                }
            }
        }
        // END_CHANGE: ISS-2025-0172

        return foundSolution;
    }
    
    // START_CHANGE: ISS-2025-0172 - Helper for constraint-aware sub_atom/5
    private boolean trySolution(int b, int len, int a, String sub,
                                Term beforeTerm, Term lengthTerm, Term afterTerm, Term subAtomTerm,
                                Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        Map<String, Term> newBindings = new HashMap<>(bindings);
        boolean ok = true;
        if (ok) ok = beforeTerm.unify(new it.denzosoft.jprolog.core.terms.Number((long) b)   /* ISS-2025-0424 */, newBindings);
        if (ok) ok = lengthTerm.unify(new it.denzosoft.jprolog.core.terms.Number((long) len)   /* ISS-2025-0424 */, newBindings);
        if (ok) ok = afterTerm.unify(new it.denzosoft.jprolog.core.terms.Number((long) a)   /* ISS-2025-0424 */, newBindings);
        if (ok) ok = subAtomTerm.unify(new Atom(sub), newBindings);
        if (ok) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }
    // END_CHANGE: ISS-2025-0172

    /**
     * Extract integer value from a term if it's a number, null if variable.
     */
    private Integer getIntegerValue(Term term) {
        if (term instanceof it.denzosoft.jprolog.core.terms.Number) {
            double value = ((it.denzosoft.jprolog.core.terms.Number) term).getValue();
            if (value == Math.floor(value) && value >= 0) {
                return (int) value;
            } else {
                throw new PrologEvaluationException("sub_atom/5: numeric arguments must be non-negative integers");
            }
        } else if (term instanceof Variable) {
            return null;
        } else {
            throw new PrologEvaluationException("sub_atom/5: before, length, and after arguments must be integers or variables");
        }
    }
    
    /**
     * Extract atom value from a term if it's an atom, null if variable.
     */
    private String getAtomValue(Term term) {
        if (term instanceof Atom) {
            return ((Atom) term).getName();
        } else if (term instanceof Variable) {
            return null;
        } else {
            throw new PrologEvaluationException("sub_atom/5: sub-atom argument must be an atom or variable");
        }
    }
}