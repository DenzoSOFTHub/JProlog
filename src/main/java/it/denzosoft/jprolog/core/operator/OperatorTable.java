package it.denzosoft.jprolog.core.operator;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;
import java.util.logging.Level;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;

/**
 * Manages operator definitions for ISO Prolog compliance.
 * Maintains operator precedence and associativity information.
 */
public class OperatorTable {

    private static final Logger LOGGER = Logger.getLogger(OperatorTable.class.getName());

    // START_CHANGE: ISS-2025-0168 - Standard operator names for redefinition warnings
    private static final Set<String> STANDARD_OPERATORS = new HashSet<>(Arrays.asList(
        ":-", "-->", "?-", ";", "->", ",", "\\+",
        "=", "\\=", "==", "\\==", "@<", "@=<", "@>", "@>=",
        "=..", "is", "=:=", "=\\=", "<", "=<", ">", ">=",
        ":", "+", "-", "/\\", "\\/", "xor",
        "*", "/", "//", "rem", "mod", "div", "rdiv", "<<", ">>",
        "**", "^", "\\"
    ));

    private static final Set<String> VALID_SPECIFIERS = new HashSet<>(Arrays.asList(
        "xf", "yf", "xfx", "xfy", "yfx", "fy", "fx"
    ));
    // END_CHANGE: ISS-2025-0168

    private final Map<String, Set<Operator>> operators;
    private final Map<String, Operator> prefixOperators;
    private final Map<String, Operator> postfixOperators;
    private final Map<String, Operator> infixOperators;
    private boolean initializing = false;

    /**
     * Create a new operator table with standard ISO operators.
     */
    // START_CHANGE: ISS-2025-0242 - shared default instance for write-side operator awareness
    private static volatile OperatorTable defaultInstance;
    public static OperatorTable getDefault() {
        OperatorTable d = defaultInstance;
        if (d == null) {
            synchronized (OperatorTable.class) {
                d = defaultInstance;
                if (d == null) {
                    d = new OperatorTable();
                    // setDefault below handled by constructor publishing
                }
            }
        }
        return d;
    }
    public static void setDefault(OperatorTable t) { defaultInstance = t; }
    // END_CHANGE: ISS-2025-0242

    public OperatorTable() {
        this.operators = new ConcurrentHashMap<>();
        this.prefixOperators = new ConcurrentHashMap<>();
        this.postfixOperators = new ConcurrentHashMap<>();
        this.infixOperators = new ConcurrentHashMap<>();

        initializeStandardOperators();

        // START_CHANGE: ISS-2025-0242 - publish first standard-init instance as default.
        // createEmpty() clears the table after construction; ensure that doesn't poison the default.
        if (defaultInstance == null && !this.infixOperators.isEmpty()) defaultInstance = this;
        // END_CHANGE: ISS-2025-0242
    }

    // START_CHANGE: ISS-2025-0167 - Factory method for empty operator table (per-module scope)
    /**
     * Create an empty operator table without standard ISO operators.
     * Used for per-module local operator definitions.
     *
     * @return An empty OperatorTable
     */
    public static OperatorTable createEmpty() {
        OperatorTable table = new OperatorTable();
        // START_CHANGE: ISS-2025-0242 - if we just published an empty-after-clear as default, undo it
        boolean wasDefault = (defaultInstance == table);
        table.operators.clear();
        table.prefixOperators.clear();
        table.postfixOperators.clear();
        table.infixOperators.clear();
        table.cachedOperatorNames = null;
        if (wasDefault) defaultInstance = null;
        // END_CHANGE: ISS-2025-0242
        return table;
    }
    // END_CHANGE: ISS-2025-0167
    
    /**
     * Initialize standard ISO Prolog operators.
     */
    private void initializeStandardOperators() {
        initializing = true;
        // Standard ISO operators
        defineOperator(1200, Operator.Type.XFX, ":-");
        defineOperator(1200, Operator.Type.XFX, "-->");
        defineOperator(1200, Operator.Type.FX, ":-");
        defineOperator(1200, Operator.Type.FX, "?-");
        // START_CHANGE: R5 - tabling directives: declared as fx 1150 like SWI
        defineOperator(1150, Operator.Type.FX, "table");
        defineOperator(1150, Operator.Type.FX, "dynamic");
        defineOperator(1150, Operator.Type.FX, "discontiguous");
        defineOperator(1150, Operator.Type.FX, "multifile");
        defineOperator(1150, Operator.Type.FX, "meta_predicate");
        defineOperator(1150, Operator.Type.FX, "module_transparent");
        // END_CHANGE: R5
        
        defineOperator(1100, Operator.Type.XFY, ";");
        // START_CHANGE: ISS-2025-0734 - 4.6 wave Q3.5: the bar is the infix operator '|'
        // (SWI-Prolog 9 manual, section 4.25 "Operators", table of system operators:
        // `1105 xfy |` and `700 xfx ... as ...`); `as` is what makes `:- table p/1 as subsumptive`
        // parse.
        defineOperator(1105, Operator.Type.XFY, "|");
        defineOperator(700, Operator.Type.XFX, "as");
        // END_CHANGE: ISS-2025-0734
        defineOperator(1050, Operator.Type.XFY, "->");
        // START_CHANGE: ISS-2025-0201 - soft-cut operator
        defineOperator(1050, Operator.Type.XFY, "*->");
        // END_CHANGE: ISS-2025-0201
        
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
        // START_CHANGE: Round5 minor - CLP(FD) operators (SWI-compat)
        defineOperator(700, Operator.Type.XFX, "in");
        defineOperator(700, Operator.Type.XFX, "ins");
        defineOperator(700, Operator.Type.XFX, "#=");
        defineOperator(700, Operator.Type.XFX, "#\\=");
        defineOperator(700, Operator.Type.XFX, "#<");
        defineOperator(700, Operator.Type.XFX, "#>");
        defineOperator(700, Operator.Type.XFX, "#=<");
        defineOperator(700, Operator.Type.XFX, "#>=");
        defineOperator(450, Operator.Type.XFX, "..");
        // END_CHANGE: Round5 minor
        // START_CHANGE: ISS-2025-0652 - the CLP(FD) reification connectives, SWI priorities
        defineOperator(760, Operator.Type.YFX, "#<==>");
        defineOperator(750, Operator.Type.XFY, "#==>");
        defineOperator(750, Operator.Type.YFX, "#<==");
        defineOperator(740, Operator.Type.YFX, "#\\/");
        defineOperator(730, Operator.Type.YFX, "#\\");
        defineOperator(720, Operator.Type.YFX, "#/\\");
        defineOperator(710, Operator.Type.FY, "#\\");
        // END_CHANGE: ISS-2025-0652
        
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
        defineOperator(400, Operator.Type.YFX, "div");   // ISS-2025-0300: ISO 400 yfx (was missing)
        defineOperator(400, Operator.Type.YFX, "rdiv");
        defineOperator(400, Operator.Type.YFX, "<<");
        defineOperator(400, Operator.Type.YFX, ">>");
        
        defineOperator(200, Operator.Type.XFX, "**");
        defineOperator(200, Operator.Type.XFY, "^");
        
        defineOperator(200, Operator.Type.FY, "-");
        defineOperator(200, Operator.Type.FY, "+");
        defineOperator(200, Operator.Type.FY, "\\");
        initializing = false;
    }
    
    /**
     * Define a new operator with ISO 13211-1 Section 6.3.4 validation.
     *
     * @param precedence The operator precedence (0-1200; 0 removes the operator)
     * @param type The operator type
     * @param name The operator name
     * @throws PrologException if precedence or specifier is invalid
     */
    // START_CHANGE: ISS-2025-0168 - Operator precedence validation per ISO 13211-1 Section 6.3.4
    public void defineOperator(int precedence, Operator.Type type, String name) {
        // Validate precedence range (0-1200)
        if (precedence < 0 || precedence > 1200) {
            throw new PrologException(
                new CompoundTerm(new Atom("error"), Arrays.asList(
                    new CompoundTerm(new Atom("domain_error"), Arrays.asList(
                        new Atom("operator_priority"),
                        new it.denzosoft.jprolog.core.terms.Number((double) precedence, true)
                    )),
                    new CompoundTerm(new Atom("/"), Arrays.asList(
                        new Atom("op"),
                        new it.denzosoft.jprolog.core.terms.Number(3.0, true)
                    ))
                ))
            );
        }

        // Validate specifier
        if (!VALID_SPECIFIERS.contains(type.name().toLowerCase())) {
            throw new PrologException(
                new CompoundTerm(new Atom("error"), Arrays.asList(
                    new CompoundTerm(new Atom("domain_error"), Arrays.asList(
                        new Atom("operator_specifier"),
                        new Atom(type.name().toLowerCase())
                    )),
                    new CompoundTerm(new Atom("/"), Arrays.asList(
                        new Atom("op"),
                        new it.denzosoft.jprolog.core.terms.Number(3.0, true)
                    ))
                ))
            );
        }

        // ISO standard: precedence 0 removes the operator
        if (precedence == 0) {
            removeOperatorByNameAndClass(type, name);
            return;
        }

        // Warn if redefining a standard operator (skip during initialization)
        if (!initializing && STANDARD_OPERATORS.contains(name) && operators.containsKey(name)) {
            LOGGER.log(Level.WARNING, "Redefining standard operator: " + name +
                " with op(" + precedence + ", " + type.name().toLowerCase() + ", " + name + ")");
        }

        Operator operator = new Operator(precedence, type, name);

        // Add to main operators map
        operators.computeIfAbsent(name, k -> new HashSet<>()).add(operator);
        cachedOperatorNames = null; // Invalidate cache

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
     * Remove all operator definitions for a name matching the given type class
     * (prefix, infix, or postfix).
     */
    private void removeOperatorByNameAndClass(Operator.Type type, String name) {
        Set<Operator> ops = operators.get(name);
        if (ops == null) return;

        Iterator<Operator> it = ops.iterator();
        while (it.hasNext()) {
            Operator op = it.next();
            boolean match = false;
            if (type == Operator.Type.FX || type == Operator.Type.FY) {
                match = op.isPrefix();
            } else if (type == Operator.Type.XF || type == Operator.Type.YF) {
                match = op.isPostfix();
            } else {
                match = op.isInfix();
            }
            if (match) {
                it.remove();
                if (op.isPrefix()) prefixOperators.remove(name);
                if (op.isPostfix()) postfixOperators.remove(name);
                if (op.isInfix()) infixOperators.remove(name);
            }
        }
        if (ops.isEmpty()) {
            operators.remove(name);
        }
        cachedOperatorNames = null;
    }
    // END_CHANGE: ISS-2025-0168
    
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
            cachedOperatorNames = null; // Invalidate cache
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
    
    // START_CHANGE: ISS-2025-0091 - Cache operator names set; invalidate on define/remove
    private volatile Set<String> cachedOperatorNames = null;

    /**
     * Get all defined operator names.
     * Returns a cached unmodifiable view; invalidated when operators change.
     *
     * @return Set of all operator names
     */
    public Set<String> getAllOperatorNames() {
        Set<String> cached = cachedOperatorNames;
        if (cached == null) {
            cached = Collections.unmodifiableSet(new HashSet<>(operators.keySet()));
            cachedOperatorNames = cached;
        }
        return cached;
    }
    // END_CHANGE: ISS-2025-0091
    
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
        cachedOperatorNames = null;
        initializeStandardOperators();
    }
    
    @Override
    public String toString() {
        return "OperatorTable{operators=" + operators.size() + "}";
    }
}