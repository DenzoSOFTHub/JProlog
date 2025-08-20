package it.denzosoft.jprolog.core.operator;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Manages operator definitions for ISO Prolog compliance.
 * Maintains operator precedence and associativity information.
 */
public class OperatorTable {
    
    private final Map<String, Set<Operator>> operators;
    private final Map<String, Operator> prefixOperators;
    private final Map<String, Operator> postfixOperators;
    private final Map<String, Operator> infixOperators;
    
    /**
     * Create a new operator table with standard ISO operators.
     */
    public OperatorTable() {
        this.operators = new ConcurrentHashMap<>();
        this.prefixOperators = new ConcurrentHashMap<>();
        this.postfixOperators = new ConcurrentHashMap<>();
        this.infixOperators = new ConcurrentHashMap<>();
        
        initializeStandardOperators();
    }
    
    /**
     * Initialize standard ISO Prolog operators.
     */
    private void initializeStandardOperators() {
        // Standard ISO operators
        defineOperator(1200, Operator.Type.XFX, ":-");
        defineOperator(1200, Operator.Type.XFX, "-->");
        defineOperator(1200, Operator.Type.FX, ":-");
        defineOperator(1200, Operator.Type.FX, "?-");
        
        defineOperator(1100, Operator.Type.XFY, ";");
        defineOperator(1050, Operator.Type.XFY, "->");
        
        defineOperator(1000, Operator.Type.XFY, ",");
        
        defineOperator(900, Operator.Type.FY, "\\+");
        
        defineOperator(700, Operator.Type.XFX, "=");
        defineOperator(700, Operator.Type.XFX, "\\=");
        defineOperator(700, Operator.Type.XFX, "==");
        defineOperator(700, Operator.Type.XFX, "\\==");
        defineOperator(700, Operator.Type.XFX, "@<");
        defineOperator(700, Operator.Type.XFX, "@=<");
        defineOperator(700, Operator.Type.XFX, "@>");
        defineOperator(700, Operator.Type.XFX, "@>=");
        defineOperator(700, Operator.Type.XFX, "=..");
        defineOperator(700, Operator.Type.XFX, "is");
        defineOperator(700, Operator.Type.XFX, "=:=");
        defineOperator(700, Operator.Type.XFX, "=\\=");
        defineOperator(700, Operator.Type.XFX, "<");
        defineOperator(700, Operator.Type.XFX, "=<");
        defineOperator(700, Operator.Type.XFX, ">");
        defineOperator(700, Operator.Type.XFX, ">=");
        
        defineOperator(600, Operator.Type.XFY, ":");
        
        defineOperator(500, Operator.Type.YFX, "+");
        defineOperator(500, Operator.Type.YFX, "-");
        defineOperator(500, Operator.Type.YFX, "/\\");
        defineOperator(500, Operator.Type.YFX, "\\/");
        defineOperator(500, Operator.Type.YFX, "xor");
        
        defineOperator(400, Operator.Type.YFX, "*");
        defineOperator(400, Operator.Type.YFX, "/");
        defineOperator(400, Operator.Type.YFX, "//");
        defineOperator(400, Operator.Type.YFX, "rem");
        defineOperator(400, Operator.Type.YFX, "mod");
        defineOperator(400, Operator.Type.YFX, "<<");
        defineOperator(400, Operator.Type.YFX, ">>");
        
        defineOperator(200, Operator.Type.XFX, "**");
        defineOperator(200, Operator.Type.XFY, "^");
        
        defineOperator(200, Operator.Type.FY, "-");
        defineOperator(200, Operator.Type.FY, "+");
        defineOperator(200, Operator.Type.FY, "\\");
    }
    
    /**
     * Define a new operator.
     * 
     * @param precedence The operator precedence
     * @param type The operator type
     * @param name The operator name
     */
    public void defineOperator(int precedence, Operator.Type type, String name) {
        Operator operator = new Operator(precedence, type, name);
        
        // Add to main operators map
        operators.computeIfAbsent(name, k -> new HashSet<>()).add(operator);
        
        // Add to specialized maps
        if (operator.isPrefix()) {
            prefixOperators.put(name, operator);
        }
        if (operator.isPostfix()) {
            postfixOperators.put(name, operator);
        }
        if (operator.isInfix()) {
            infixOperators.put(name, operator);
        }
    }
    
    /**
     * Remove an operator definition.
     * 
     * @param precedence The operator precedence
     * @param type The operator type
     * @param name The operator name
     * @return true if removed
     */
    public boolean removeOperator(int precedence, Operator.Type type, String name) {
        Operator toRemove = new Operator(precedence, type, name);
        
        Set<Operator> ops = operators.get(name);
        if (ops != null && ops.remove(toRemove)) {
            if (ops.isEmpty()) {
                operators.remove(name);
            }
            
            // Remove from specialized maps if no other operators exist
            if (toRemove.isPrefix() && !hasPrefix(name)) {
                prefixOperators.remove(name);
            }
            if (toRemove.isPostfix() && !hasPostfix(name)) {
                postfixOperators.remove(name);
            }
            if (toRemove.isInfix() && !hasInfix(name)) {
                infixOperators.remove(name);
            }
            
            return true;
        }
        
        return false;
    }
    
    /**
     * Get all operator definitions for a name.
     * 
     * @param name The operator name
     * @return Set of operators, or empty set if none
     */
    public Set<Operator> getOperators(String name) {
        return new HashSet<>(operators.getOrDefault(name, Collections.emptySet()));
    }
    
    /**
     * Get prefix operator for a name.
     * 
     * @param name The operator name
     * @return The prefix operator, or null if none
     */
    public Operator getPrefixOperator(String name) {
        return prefixOperators.get(name);
    }
    
    /**
     * Get postfix operator for a name.
     * 
     * @param name The operator name
     * @return The postfix operator, or null if none
     */
    public Operator getPostfixOperator(String name) {
        return postfixOperators.get(name);
    }
    
    /**
     * Get infix operator for a name.
     * 
     * @param name The operator name
     * @return The infix operator, or null if none
     */
    public Operator getInfixOperator(String name) {
        return infixOperators.get(name);
    }
    
    /**
     * Check if name has a prefix operator.
     * 
     * @param name The operator name
     * @return true if has prefix operator
     */
    public boolean hasPrefix(String name) {
        return operators.getOrDefault(name, Collections.emptySet())
            .stream().anyMatch(Operator::isPrefix);
    }
    
    /**
     * Check if name has a postfix operator.
     * 
     * @param name The operator name
     * @return true if has postfix operator
     */
    public boolean hasPostfix(String name) {
        return operators.getOrDefault(name, Collections.emptySet())
            .stream().anyMatch(Operator::isPostfix);
    }
    
    /**
     * Check if name has an infix operator.
     * 
     * @param name The operator name
     * @return true if has infix operator
     */
    public boolean hasInfix(String name) {
        return operators.getOrDefault(name, Collections.emptySet())
            .stream().anyMatch(Operator::isInfix);
    }
    
    /**
     * Check if a name is defined as any type of operator.
     * 
     * @param name The operator name
     * @return true if defined as operator
     */
    public boolean isOperator(String name) {
        return operators.containsKey(name);
    }
    
    /**
     * Get all defined operator names.
     * 
     * @return Set of all operator names
     */
    public Set<String> getAllOperatorNames() {
        return new HashSet<>(operators.keySet());
    }
    
    /**
     * Get current operator definitions for current_op/3.
     * 
     * @return List of all current operators
     */
    public List<Operator> getCurrentOperators() {
        List<Operator> result = new ArrayList<>();
        for (Set<Operator> ops : operators.values()) {
            result.addAll(ops);
        }
        result.sort(Comparator.comparing(Operator::getPrecedence)
            .thenComparing(op -> op.getType().name())
            .thenComparing(Operator::getName));
        return result;
    }
    
    /**
     * Reset to standard operators only.
     */
    public void reset() {
        operators.clear();
        prefixOperators.clear();
        postfixOperators.clear();
        infixOperators.clear();
        initializeStandardOperators();
    }
    
    @Override
    public String toString() {
        return "OperatorTable{operators=" + operators.size() + "}";
    }
}