package it.denzosoft.jprolog.util;

import it.denzosoft.jprolog.core.terms.*;
import java.util.HashMap;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;
// START_CHANGE: ISS-2025-0186 - Debug, utility, and list predicate fixes
import java.util.concurrent.atomic.AtomicLong;
// END_CHANGE: ISS-2025-0186

/**
 * Utility class for copying terms while preserving variable sharing.
 * When a rule like digits([], S, S) is copied, both occurrences of S
 * must map to the same new variable instance.
 */
public class TermCopier {

    // START_CHANGE: ISS-2025-0186 - Debug, utility, and list predicate fixes
    private static final AtomicLong COPY_COUNTER = new AtomicLong(0);
    // END_CHANGE: ISS-2025-0186
    
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
        // START_CHANGE: ISS-2025-0186 - Debug, utility, and list predicate fixes
        String prefix = "_R" + COPY_COUNTER.incrementAndGet() + "_";
        // END_CHANGE: ISS-2025-0186
        Term copiedHead = copyTermInternal(head, variableMap, prefix);
        List<Term> copiedBody = new ArrayList<>(body.size());
        for (Term term : body) {
            copiedBody.add(copyTermInternal(term, variableMap, prefix));
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
    
    // START_CHANGE: ISS-2025-0122 - Fresh variable names for copy_term/2
    /**
     * Copy a term creating fresh variable names with a unique prefix.
     * Used by copy_term/2 to ensure copied terms don't share variable
     * names with the original, preventing unification interference.
     *
     * @param term The term to copy
     * @return A copy with all variables renamed using a unique prefix
     */
    public static Term copyWithFreshVariables(Term term) {
        Map<String, Variable> variableMap = new HashMap<>();
        // START_CHANGE: ISS-2025-0186 - Debug, utility, and list predicate fixes
        String prefix = "_R" + COPY_COUNTER.incrementAndGet() + "_";
        // END_CHANGE: ISS-2025-0186
        return copyTermInternal(term, variableMap, prefix);
    }
    // END_CHANGE: ISS-2025-0122

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
            
        // START_CHANGE: ISS-2025-0091 - Reuse immutable Atom and Number instances
        } else if (term instanceof Atom) {
            // Atoms are immutable - reuse the same instance (no copy needed)
            return term;

        } else if (term instanceof it.denzosoft.jprolog.core.terms.Number) {
            // Numbers (including Rational) are immutable - reuse the same instance (no copy needed)
            return term;

        // START_CHANGE: ISS-2025-0192 - Handle PrologString explicitly (immutable, no copy needed)
        } else if (term instanceof PrologString) {
            return term;
        // END_CHANGE: ISS-2025-0192

        // START_CHANGE: ISS-2025-0103 - Fast-path: skip recursion for ground compound terms
        } else if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            // Ground terms contain no variables — safe to reuse directly
            if (compound.isGround()) {
                return compound;
            }
            // START_CHANGE: ISS-2025-0428 - ENG-09: iterative on the LAST argument. A list of N
            // cells is N nested './2' terms, so recursing into the tail needed N Java frames:
            // copy_term/2 (and every rule copy) died with StackOverflowError past ~20-30k elements.
            // Phase 1 collects the last-argument spine, phase 2 rebuilds it bottom-up.
            ArrayList<CompoundTerm> spine = new ArrayList<>();
            CompoundTerm cur = compound;
            while (true) {
                spine.add(cur);
                List<Term> as = cur.getArguments();
                if (as.isEmpty()) break;
                Term last = as.get(as.size() - 1);
                // a ground sub-spine is reused wholesale, so stop descending into it
                if (!(last instanceof CompoundTerm) || ((CompoundTerm) last).isGround()) break;
                cur = (CompoundTerm) last;
            }
            Term below = null;
            for (int k = spine.size() - 1; k >= 0; k--) {
                CompoundTerm node = spine.get(k);
                List<Term> as = node.getArguments();
                int n = as.size();
                List<Term> copiedArgs = new ArrayList<>(n);
                for (int i = 0; i < n - 1; i++) copiedArgs.add(copyTermInternal(as.get(i), variableMap, prefix));
                if (n > 0) {
                    copiedArgs.add(k < spine.size() - 1
                        ? below : copyTermInternal(as.get(n - 1), variableMap, prefix));
                }
                // Reuse the functor Atom since it's immutable
                below = new CompoundTerm(node.getFunctor(), copiedArgs);
            }
            return below;
            // END_CHANGE: ISS-2025-0428
        // END_CHANGE: ISS-2025-0103
            
        } else {
            // Fallback to the term's own copy method
            return term.copy();
        }
    }
}