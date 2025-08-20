package it.denzosoft.jprolog.builtin.system;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
    
    // Global operator registry - shared across all instances
    private static final Map<String, OperatorInfo> OPERATORS = new ConcurrentHashMap<>();
    
    // Initialize with standard ISO Prolog operators
    static {
        // Initialize ISO standard operators
        initializeISOOperators();
    }
    
    private static void initializeISOOperators() {
        // Precedence 1200 (lowest binding)
        OPERATORS.put(":-", new OperatorInfo(1200, "fx", ":-"));  // Rule definition
        OPERATORS.put("-->", new OperatorInfo(1200, "xfx", "-->"));  // DCG rule
        
        // Precedence 1150  
        OPERATORS.put(";", new OperatorInfo(1150, "xfy", ";"));   // Disjunction/if-then-else
        
        // Precedence 1100
        OPERATORS.put("->", new OperatorInfo(1100, "xfy", "->"));  // If-then
        
        // Precedence 1050
        OPERATORS.put("*->", new OperatorInfo(1050, "xfy", "*->"));  // Soft cut
        
        // Precedence 1000
        OPERATORS.put(",", new OperatorInfo(1000, "xfy", ","));   // Conjunction
        
        // Precedence 900
        OPERATORS.put("\\+", new OperatorInfo(900, "fy", "\\+"));  // Negation as failure
        
        // Precedence 700 (comparison and unification)
        OPERATORS.put("=", new OperatorInfo(700, "xfx", "="));
        OPERATORS.put("\\=", new OperatorInfo(700, "xfx", "\\="));
        OPERATORS.put("==", new OperatorInfo(700, "xfx", "=="));
        OPERATORS.put("\\==", new OperatorInfo(700, "xfx", "\\=="));
        OPERATORS.put("@<", new OperatorInfo(700, "xfx", "@<"));
        OPERATORS.put("@=<", new OperatorInfo(700, "xfx", "@=<"));
        OPERATORS.put("@>", new OperatorInfo(700, "xfx", "@>"));
        OPERATORS.put("@>=", new OperatorInfo(700, "xfx", "@>="));
        OPERATORS.put("=..", new OperatorInfo(700, "xfx", "=.."));
        OPERATORS.put("is", new OperatorInfo(700, "xfx", "is"));
        OPERATORS.put("=:=", new OperatorInfo(700, "xfx", "=:="));
        OPERATORS.put("=\\=", new OperatorInfo(700, "xfx", "=\\="));
        OPERATORS.put("<", new OperatorInfo(700, "xfx", "<"));
        OPERATORS.put("=<", new OperatorInfo(700, "xfx", "=<"));
        OPERATORS.put(">", new OperatorInfo(700, "xfx", ">"));
        OPERATORS.put(">=", new OperatorInfo(700, "xfx", ">="));
        
        // Precedence 500 (addition-like)
        OPERATORS.put("+", new OperatorInfo(500, "yfx", "+"));
        OPERATORS.put("-", new OperatorInfo(500, "yfx", "-"));
        OPERATORS.put("/\\", new OperatorInfo(500, "yfx", "/\\"));  // Bitwise AND
        OPERATORS.put("\\/", new OperatorInfo(500, "yfx", "\\/"));  // Bitwise OR
        OPERATORS.put("xor", new OperatorInfo(500, "yfx", "xor"));
        
        // Precedence 400 (multiplication-like)
        OPERATORS.put("*", new OperatorInfo(400, "yfx", "*"));
        OPERATORS.put("/", new OperatorInfo(400, "yfx", "/"));
        OPERATORS.put("//", new OperatorInfo(400, "yfx", "//"));
        OPERATORS.put("rem", new OperatorInfo(400, "yfx", "rem"));
        OPERATORS.put("mod", new OperatorInfo(400, "yfx", "mod"));
        OPERATORS.put("<<", new OperatorInfo(400, "yfx", "<<"));
        OPERATORS.put(">>", new OperatorInfo(400, "yfx", ">>"));
        
        // Precedence 200 (highest binding)
        OPERATORS.put("**", new OperatorInfo(200, "xfx", "**"));
        OPERATORS.put("^", new OperatorInfo(200, "xfy", "^"));   // Power/existential quantification
        
        // Unary operators
        OPERATORS.put("+", new OperatorInfo(200, "fy", "+"));   // Unary plus
        OPERATORS.put("-", new OperatorInfo(200, "fy", "-"));   // Unary minus
        OPERATORS.put("\\", new OperatorInfo(200, "fy", "\\"));  // Bitwise NOT
    }
    
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
        
        // Validate precedence (1-1200)
        if (precedence < 1 || precedence > 1200) {
            throw new PrologEvaluationException("op/3: Precedence must be between 1 and 1200.");
        }
        
        // Validate operator type
        if (!isValidOperatorType(operatorType)) {
            throw new PrologEvaluationException("op/3: Invalid operator type: " + operatorType);
        }
        
        // Register or update the operator
        OPERATORS.put(name, new OperatorInfo(precedence, operatorType, name));
        
        // Success - operator defined
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
        
        // Iterate through all defined operators
        for (OperatorInfo opInfo : OPERATORS.values()) {
            Map<String, Term> newBindings = new HashMap<>(bindings);
            
            // Try to unify precedence
            if (!unifyTerm(precedenceTerm, new Number((double) opInfo.precedence), newBindings)) {
                continue;
            }
            
            // Try to unify type
            if (!unifyTerm(typeTerm, new Atom(opInfo.type), newBindings)) {
                continue;
            }
            
            // Try to unify name
            if (!unifyTerm(nameTerm, new Atom(opInfo.name), newBindings)) {
                continue;
            }
            
            // All unified successfully
            solutions.add(newBindings);
            foundSolution = true;
        }
        
        return foundSolution;
    }
    
    private boolean isValidOperatorType(String type) {
        return type.equals("fx") || type.equals("fy") || 
               type.equals("xfx") || type.equals("xfy") || type.equals("yfx") ||
               type.equals("yf") || type.equals("xf");
    }
    
    /**
     * Get operator information for a given operator name.
     */
    public static OperatorInfo getOperator(String name) {
        return OPERATORS.get(name);
    }
    
    /**
     * Check if an operator is defined.
     */
    public static boolean isOperatorDefined(String name) {
        return OPERATORS.containsKey(name);
    }
    
    /**
     * Get all defined operators.
     */
    public static Map<String, OperatorInfo> getAllOperators() {
        return new HashMap<>(OPERATORS);
    }
    
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