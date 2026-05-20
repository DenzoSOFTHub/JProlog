package it.denzosoft.jprolog.builtin.system;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.operator.Operator;
import it.denzosoft.jprolog.core.operator.OperatorTable;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Implementation of op/3 predicate for defining operators.
 * 
 * op(+Precedence, +Type, +Name)
 * 
 * Defines an operator with given precedence and associativity type.
 */
public class OperatorDefinition implements BuiltIn {
    
    public enum OperatorType {
        OP("op/3"),
        CURRENT_OP("current_op/3");
        
        private final String name;
        
        OperatorType(String name) {
            this.name = name;
        }
        
        public String getName() {
            return name;
        }
    }
    
    // START_CHANGE: ISS-2025-0177 - Fix dual-arity operator bug: use composite key (name:typeClass)
    // Global operator registry - shared across all instances
    // Key format: "name:typeClass" where typeClass is "prefix", "infix", or "postfix"
    private static final Map<String, OperatorInfo> OPERATORS = new ConcurrentHashMap<>();
    // START_CHANGE: R2 - track which module each operator was defined in
    private static final Map<String, String> OP_MODULE = new ConcurrentHashMap<>();
    private static volatile String currentModuleContext = "user";
    public static void setCurrentModuleContext(String mod) {
        currentModuleContext = (mod == null) ? "user" : mod;
    }
    public static String getCurrentModuleContext() { return currentModuleContext; }
    // END_CHANGE: R2
    // END_CHANGE: ISS-2025-0177

    // START_CHANGE: ISS-2025-0085 - Shared OperatorTable for parser integration
    private static volatile OperatorTable sharedOperatorTable;

    /**
     * Set the shared OperatorTable that op/3 will update.
     * This must be called during engine initialization to connect
     * the op/3 predicate to the parser's operator table.
     */
    public static void setSharedOperatorTable(OperatorTable table) {
        sharedOperatorTable = table;
    }

    /**
     * Get the shared OperatorTable.
     */
    public static OperatorTable getSharedOperatorTable() {
        return sharedOperatorTable;
    }
    // END_CHANGE: ISS-2025-0085

    // Initialize with standard ISO Prolog operators
    static {
        initializeISOOperators();
    }
    
    // START_CHANGE: ISS-2025-0177 - Fix dual-arity operator bug: use composite key
    private static void initializeISOOperators() {
        // Precedence 1200 (lowest binding)
        putOperator(new OperatorInfo(1200, "xfx", ":-"));  // Rule definition (infix)
        putOperator(new OperatorInfo(1200, "fx", ":-"));    // Directive (prefix)
        putOperator(new OperatorInfo(1200, "xfx", "-->"));  // DCG rule
        putOperator(new OperatorInfo(1200, "fx", "?-"));    // Query directive

        // Precedence 1100
        putOperator(new OperatorInfo(1100, "xfy", ";"));   // Disjunction/if-then-else

        // Precedence 1050
        putOperator(new OperatorInfo(1050, "xfy", "->"));  // If-then

        // Precedence 1000
        putOperator(new OperatorInfo(1000, "xfy", ","));   // Conjunction

        // Precedence 900
        putOperator(new OperatorInfo(900, "fy", "\\+"));  // Negation as failure

        // Precedence 700 (comparison and unification)
        putOperator(new OperatorInfo(700, "xfx", "="));
        putOperator(new OperatorInfo(700, "xfx", "\\="));
        putOperator(new OperatorInfo(700, "xfx", "=="));
        putOperator(new OperatorInfo(700, "xfx", "\\=="));
        putOperator(new OperatorInfo(700, "xfx", "@<"));
        putOperator(new OperatorInfo(700, "xfx", "@=<"));
        putOperator(new OperatorInfo(700, "xfx", "@>"));
        putOperator(new OperatorInfo(700, "xfx", "@>="));
        putOperator(new OperatorInfo(700, "xfx", "=.."));
        putOperator(new OperatorInfo(700, "xfx", "is"));
        putOperator(new OperatorInfo(700, "xfx", "=:="));
        putOperator(new OperatorInfo(700, "xfx", "=\\="));
        putOperator(new OperatorInfo(700, "xfx", "<"));
        putOperator(new OperatorInfo(700, "xfx", "=<"));
        putOperator(new OperatorInfo(700, "xfx", ">"));
        putOperator(new OperatorInfo(700, "xfx", ">="));

        // Precedence 600
        putOperator(new OperatorInfo(600, "xfy", ":"));

        // Precedence 500 (addition-like)
        putOperator(new OperatorInfo(500, "yfx", "+"));
        putOperator(new OperatorInfo(500, "yfx", "-"));
        putOperator(new OperatorInfo(500, "yfx", "/\\"));  // Bitwise AND
        putOperator(new OperatorInfo(500, "yfx", "\\/"));  // Bitwise OR
        putOperator(new OperatorInfo(500, "yfx", "xor"));

        // Precedence 400 (multiplication-like)
        putOperator(new OperatorInfo(400, "yfx", "*"));
        putOperator(new OperatorInfo(400, "yfx", "/"));
        putOperator(new OperatorInfo(400, "yfx", "//"));
        putOperator(new OperatorInfo(400, "yfx", "rem"));
        putOperator(new OperatorInfo(400, "yfx", "mod"));
        putOperator(new OperatorInfo(400, "yfx", "<<"));
        putOperator(new OperatorInfo(400, "yfx", ">>"));

        // Precedence 200 (highest binding)
        putOperator(new OperatorInfo(200, "xfx", "**"));
        putOperator(new OperatorInfo(200, "xfy", "^"));   // Power/existential quantification

        // Unary operators (prefix - these no longer overwrite the infix versions)
        putOperator(new OperatorInfo(200, "fy", "+"));   // Unary plus
        putOperator(new OperatorInfo(200, "fy", "-"));   // Unary minus
        putOperator(new OperatorInfo(200, "fy", "\\"));  // Bitwise NOT
    }
    // END_CHANGE: ISS-2025-0177
    
    // START_CHANGE: ISS-2025-0177 - Composite key helpers for dual-arity operator support
    /**
     * Get the type class (prefix, infix, or postfix) for an operator specifier.
     */
    private static String typeClass(String specifier) {
        switch (specifier.toLowerCase()) {
            case "fx": case "fy": return "prefix";
            case "xf": case "yf": return "postfix";
            case "xfx": case "xfy": case "yfx": return "infix";
            default: return "infix";
        }
    }

    /**
     * Build the composite key for the OPERATORS map: "name:typeClass".
     */
    private static String compositeKey(String name, String specifier) {
        return name + ":" + typeClass(specifier);
    }

    /**
     * Store an operator using the composite key.
     */
    private static void putOperator(OperatorInfo info) {
        OPERATORS.put(compositeKey(info.name, info.type), info);
    }
    // END_CHANGE: ISS-2025-0177

    private final OperatorType type;

    public OperatorDefinition(OperatorType type) {
        this.type = type;
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        switch (type) {
            case OP:
                return handleOp(query, bindings, solutions);
            case CURRENT_OP:
                return handleCurrentOp(query, bindings, solutions);
            default:
                return false;
        }
    }
    
    private boolean handleOp(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (!(query instanceof CompoundTerm)) {
            return false;
        }
        
        CompoundTerm compound = (CompoundTerm) query;
        if (compound.getArguments().size() != 3) {
            throw new PrologEvaluationException("op/3 requires exactly 3 arguments.");
        }
        
        Term precedenceTerm = compound.getArguments().get(0);
        Term typeTerm = compound.getArguments().get(1);  
        Term nameTerm = compound.getArguments().get(2);
        
        // Resolve variables
        precedenceTerm = resolveVariable(precedenceTerm, bindings);
        typeTerm = resolveVariable(typeTerm, bindings);
        nameTerm = resolveVariable(nameTerm, bindings);
        
        if (!(precedenceTerm instanceof Number)) {
            throw new PrologEvaluationException("op/3: First argument must be an integer (precedence).");
        }
        
        if (!(typeTerm instanceof Atom)) {
            throw new PrologEvaluationException("op/3: Second argument must be an atom (type).");
        }
        
        if (!(nameTerm instanceof Atom)) {
            throw new PrologEvaluationException("op/3: Third argument must be an atom (name).");
        }
        
        int precedence = (int) Math.round(((Number) precedenceTerm).getValue());
        String operatorType = ((Atom) typeTerm).getName();
        String name = ((Atom) nameTerm).getName();
        
        // START_CHANGE: ISS-2025-0085 - Support precedence 0 for operator removal
        // Validate precedence (0-1200, where 0 means remove)
        if (precedence < 0 || precedence > 1200) {
            throw new PrologEvaluationException("op/3: Precedence must be between 0 and 1200.");
        }

        // Validate operator type
        if (!isValidOperatorType(operatorType)) {
            throw new PrologEvaluationException("op/3: Invalid operator type: " + operatorType);
        }

        if (precedence == 0) {
            // START_CHANGE: ISS-2025-0177 - Remove using composite key
            // Remove operator by composite key (name:typeClass)
            OPERATORS.remove(compositeKey(name, operatorType));
            // START_CHANGE: R2 - drop module assoc
            OP_MODULE.remove(compositeKey(name, operatorType));
            // END_CHANGE: R2
            // END_CHANGE: ISS-2025-0177
            if (sharedOperatorTable != null) {
                // Remove all operators with this name and compatible type
                Operator.Type type = Operator.parseType(operatorType);
                Set<Operator> ops = sharedOperatorTable.getOperators(name);
                for (Operator op : ops) {
                    if (isCompatibleType(op.getType(), type)) {
                        sharedOperatorTable.removeOperator(op.getPrecedence(), op.getType(), name);
                    }
                }
            }
        } else {
            // START_CHANGE: R1 - record trail entry to undo op definition on backtrack
            final String ckey = compositeKey(name, operatorType);
            final OperatorInfo previous = OPERATORS.get(ckey);
            final String prevModule = OP_MODULE.get(ckey);
            it.denzosoft.jprolog.core.engine.Trail.record(() -> {
                if (previous == null) {
                    OPERATORS.remove(ckey);
                    OP_MODULE.remove(ckey);
                } else {
                    OPERATORS.put(ckey, previous);
                    if (prevModule != null) OP_MODULE.put(ckey, prevModule);
                    else OP_MODULE.remove(ckey);
                }
            });
            // END_CHANGE: R1
            // START_CHANGE: ISS-2025-0177 - Register using composite key
            // Register or update the operator
            putOperator(new OperatorInfo(precedence, operatorType, name));
            // START_CHANGE: R2 - tag operator with defining module
            OP_MODULE.put(compositeKey(name, operatorType), currentModuleContext);
            // END_CHANGE: R2
            // END_CHANGE: ISS-2025-0177
            if (sharedOperatorTable != null) {
                Operator.Type type = Operator.parseType(operatorType);
                sharedOperatorTable.defineOperator(precedence, type, name);
            }
        }
        // END_CHANGE: ISS-2025-0085

        // Success - operator defined/removed
        solutions.add(new HashMap<>(bindings));
        return true;
    }
    
    private boolean handleCurrentOp(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (!(query instanceof CompoundTerm)) {
            return false;
        }
        
        CompoundTerm compound = (CompoundTerm) query;
        if (compound.getArguments().size() != 3) {
            throw new PrologEvaluationException("current_op/3 requires exactly 3 arguments.");
        }
        
        Term precedenceTerm = compound.getArguments().get(0);
        Term typeTerm = compound.getArguments().get(1);
        Term nameTerm = compound.getArguments().get(2);
        
        boolean foundSolution = false;
        
        // START_CHANGE: R2 - filter operators by current module (or "user" for global)
        String curMod = currentModuleContext;
        for (Map.Entry<String, OperatorInfo> e : OPERATORS.entrySet()) {
            OperatorInfo opInfo = e.getValue();
            String defMod = OP_MODULE.get(e.getKey());
            // Visibility rule: op is visible if defined in "user" (global) OR in current module
            if (defMod != null && !defMod.equals("user") && !defMod.equals(curMod)) {
                continue;
            }
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (!unifyTerm(precedenceTerm, new Number((double) opInfo.precedence), newBindings)) continue;
            if (!unifyTerm(typeTerm, new Atom(opInfo.type), newBindings)) continue;
            if (!unifyTerm(nameTerm, new Atom(opInfo.name), newBindings)) continue;
            solutions.add(newBindings);
            foundSolution = true;
        }
        // END_CHANGE: R2

        return foundSolution;
    }
    
    private boolean isValidOperatorType(String type) {
        return type.equals("fx") || type.equals("fy") ||
               type.equals("xfx") || type.equals("xfy") || type.equals("yfx") ||
               type.equals("yf") || type.equals("xf");
    }

    // START_CHANGE: ISS-2025-0085 - Helper for operator removal
    /**
     * Check if two operator types are compatible (same position class).
     * For removal: infix types match infix, prefix match prefix, postfix match postfix.
     */
    private boolean isCompatibleType(Operator.Type existing, Operator.Type requested) {
        if (existing == requested) return true;
        // Infix types are interchangeable for removal
        if (existing.name().contains("F") && existing.name().length() == 3 &&
            requested.name().contains("F") && requested.name().length() == 3) {
            return true; // both are XFX, XFY, or YFX
        }
        // Prefix types
        if ((existing == Operator.Type.FX || existing == Operator.Type.FY) &&
            (requested == Operator.Type.FX || requested == Operator.Type.FY)) {
            return true;
        }
        // Postfix types
        if ((existing == Operator.Type.XF || existing == Operator.Type.YF) &&
            (requested == Operator.Type.XF || requested == Operator.Type.YF)) {
            return true;
        }
        return false;
    }
    // END_CHANGE: ISS-2025-0085
    
    // START_CHANGE: ISS-2025-0177 - Dual-arity aware lookup methods
    /**
     * Get operator information for a given operator name.
     * Returns the infix operator by default (most common usage).
     * Falls back to prefix, then postfix if no infix definition exists.
     */
    public static OperatorInfo getOperator(String name) {
        OperatorInfo info = OPERATORS.get(name + ":infix");
        if (info != null) return info;
        info = OPERATORS.get(name + ":prefix");
        if (info != null) return info;
        return OPERATORS.get(name + ":postfix");
    }

    /**
     * Get the prefix operator for a given name, or null if none.
     */
    public static OperatorInfo getPrefixOperator(String name) {
        return OPERATORS.get(name + ":prefix");
    }

    /**
     * Get the infix operator for a given name, or null if none.
     */
    public static OperatorInfo getInfixOperator(String name) {
        return OPERATORS.get(name + ":infix");
    }

    /**
     * Get the postfix operator for a given name, or null if none.
     */
    public static OperatorInfo getPostfixOperator(String name) {
        return OPERATORS.get(name + ":postfix");
    }

    /**
     * Check if an operator is defined (any type class).
     */
    public static boolean isOperatorDefined(String name) {
        return OPERATORS.containsKey(name + ":infix") ||
               OPERATORS.containsKey(name + ":prefix") ||
               OPERATORS.containsKey(name + ":postfix");
    }

    /**
     * Get all defined operators. Returns a map keyed by composite key (name:typeClass).
     */
    public static Map<String, OperatorInfo> getAllOperators() {
        return new HashMap<>(OPERATORS);
    }
    // END_CHANGE: ISS-2025-0177
    
    private Term resolveVariable(Term term, Map<String, Term> bindings) {
        if (term instanceof it.denzosoft.jprolog.core.terms.Variable) {
            it.denzosoft.jprolog.core.terms.Variable var = (it.denzosoft.jprolog.core.terms.Variable) term;
            Term value = bindings.get(var.getName());
            if (value != null) {
                return resolveVariable(value, bindings);
            }
        }
        return term;
    }
    
    private boolean unifyTerm(Term term1, Term term2, Map<String, Term> bindings) {
        // Simple unification - resolve variables first
        term1 = resolveVariable(term1, bindings);
        term2 = resolveVariable(term2, bindings);
        
        if (term1 instanceof it.denzosoft.jprolog.core.terms.Variable) {
            it.denzosoft.jprolog.core.terms.Variable var = (it.denzosoft.jprolog.core.terms.Variable) term1;
            bindings.put(var.getName(), term2);
            return true;
        }
        
        if (term2 instanceof it.denzosoft.jprolog.core.terms.Variable) {
            it.denzosoft.jprolog.core.terms.Variable var = (it.denzosoft.jprolog.core.terms.Variable) term2;
            bindings.put(var.getName(), term1);
            return true;
        }
        
        return term1.equals(term2);
    }
    
    /**
     * Information about a defined operator.
     */
    public static class OperatorInfo {
        public final int precedence;
        public final String type;  // fx, fy, xfx, xfy, yfx, yf, xf
        public final String name;
        
        public OperatorInfo(int precedence, String type, String name) {
            this.precedence = precedence;
            this.type = type;
            this.name = name;
        }
        
        @Override
        public String toString() {
            return "OperatorInfo{" +
                    "precedence=" + precedence +
                    ", type='" + type + '\'' +
                    ", name='" + name + '\'' +
                    '}';
        }
    }
}