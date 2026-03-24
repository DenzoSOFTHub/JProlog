package it.denzosoft.jprolog.core.engine;

// START_CHANGE: LIM-015 - Compiled clause cache for faster unification
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.*;

/**
 * Pre-analyzed clause that caches structural information for faster matching.
 * This is a lightweight "compilation" step that avoids full WAM bytecode
 * but provides significant speedup for clause selection and unification.
 *
 * Key optimizations:
 * - Pre-computed head argument count and types
 * - Pre-computed determinism (fact vs rule, ground vs non-ground head)
 * - Head argument type fingerprint for fast rejection
 * - Variable position map for direct binding
 */
public class CompiledClause {

    /** Argument type tags for fingerprinting */
    public enum ArgType { ATOM, NUMBER, COMPOUND, VARIABLE, OTHER }

    private final Rule rule;
    private final int arity;
    private final ArgType[] argTypes;      // Type of each head argument
    private final String[] atomValues;     // For atom arguments, their names (null for non-atoms)
    private final double[] numberValues;   // For number arguments, their values (NaN for non-numbers)
    private final boolean isFact;          // True if rule has no body
    private final boolean isGroundHead;    // True if head has no variables
    private final int variableCount;       // Number of distinct variables in head
    private final String fingerprint;      // Quick rejection key

    public CompiledClause(Rule rule) {
        this.rule = rule;
        Term head = rule.getHead();

        if (head instanceof CompoundTerm) {
            List<Term> args = ((CompoundTerm) head).getArguments();
            this.arity = args != null ? args.size() : 0;
            this.argTypes = new ArgType[this.arity];
            this.atomValues = new String[this.arity];
            this.numberValues = new double[this.arity];
            Set<String> vars = new HashSet<>();

            StringBuilder fp = new StringBuilder();
            for (int i = 0; i < this.arity; i++) {
                Term arg = args.get(i);
                if (arg instanceof Atom) {
                    argTypes[i] = ArgType.ATOM;
                    atomValues[i] = ((Atom) arg).getName();
                    fp.append('a');
                } else if (arg instanceof it.denzosoft.jprolog.core.terms.Number) {
                    argTypes[i] = ArgType.NUMBER;
                    numberValues[i] = ((it.denzosoft.jprolog.core.terms.Number) arg).doubleValue();
                    fp.append('n');
                } else if (arg instanceof Variable) {
                    argTypes[i] = ArgType.VARIABLE;
                    vars.add(((Variable) arg).getName());
                    fp.append('v');
                } else if (arg instanceof CompoundTerm) {
                    argTypes[i] = ArgType.COMPOUND;
                    fp.append('c');
                } else {
                    argTypes[i] = ArgType.OTHER;
                    fp.append('?');
                }
                // START_CHANGE: ISS-2025-0180 - Core engine bug fixes
                // Removed redundant Arrays.fill that overwrote the value already set at line 58
                if (!(arg instanceof it.denzosoft.jprolog.core.terms.Number)) {
                    numberValues[i] = Double.NaN;
                }
                // END_CHANGE: ISS-2025-0180
            }
            this.variableCount = vars.size();
            this.fingerprint = fp.toString();
        } else {
            this.arity = 0;
            this.argTypes = new ArgType[0];
            this.atomValues = new String[0];
            this.numberValues = new double[0];
            this.variableCount = 0;
            this.fingerprint = "";
        }

        this.isFact = rule.getBody() == null || rule.getBody().isEmpty();
        this.isGroundHead = this.variableCount == 0;
    }

    /**
     * Quick rejection check: can this clause possibly match a query with the given args?
     * Returns false if the clause definitely cannot match (avoiding full unification).
     */
    public boolean canMatch(List<Term> queryArgs) {
        if (queryArgs == null || queryArgs.size() != arity) return false;

        for (int i = 0; i < arity; i++) {
            if (argTypes[i] == ArgType.VARIABLE) continue; // Variable matches anything

            Term qArg = queryArgs.get(i);
            if (qArg instanceof Variable) continue; // Query variable matches anything

            switch (argTypes[i]) {
                case ATOM:
                    if (!(qArg instanceof Atom) || !atomValues[i].equals(((Atom) qArg).getName())) {
                        return false;
                    }
                    break;
                // START_CHANGE: ISS-2025-0180 - Core engine bug fixes
                case NUMBER:
                    if (!(qArg instanceof it.denzosoft.jprolog.core.terms.Number) || Double.compare(numberValues[i], ((it.denzosoft.jprolog.core.terms.Number) qArg).doubleValue()) != 0) {
                        return false;
                    }
                    break;
                // END_CHANGE: ISS-2025-0180
                case COMPOUND:
                    if (!(qArg instanceof CompoundTerm)) {
                        return false;
                    }
                    break;
                default:
                    break;
            }
        }
        return true;
    }

    public Rule getRule() { return rule; }
    public int getArity() { return arity; }
    public ArgType[] getArgTypes() { return argTypes; }
    public boolean isFact() { return isFact; }
    public boolean isGroundHead() { return isGroundHead; }
    public int getVariableCount() { return variableCount; }
    public String getFingerprint() { return fingerprint; }

    /**
     * Cache of compiled clauses per predicate.
     */
    private static final Map<Rule, CompiledClause> cache = new WeakHashMap<>();

    public static CompiledClause compile(Rule rule) {
        return cache.computeIfAbsent(rule, CompiledClause::new);
    }

    public static void clearCache() {
        cache.clear();
    }
}
// END_CHANGE: LIM-015
