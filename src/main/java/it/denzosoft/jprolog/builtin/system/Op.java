package it.denzosoft.jprolog.builtin.system;

import it.denzosoft.jprolog.builtin.AbstractBuiltInWithContext;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.operator.Operator;
import it.denzosoft.jprolog.core.operator.OperatorTable;
import it.denzosoft.jprolog.core.terms.*;
import it.denzosoft.jprolog.util.TermUtils;

import java.util.Map;
import java.util.List;

/**
 * Implementation of op/3 predicate for operator definition.
 * 
 * op(+Precedence, +Type, +Name) - Define an operator
 */
public class Op extends AbstractBuiltInWithContext {
    
    private final OperatorTable operatorTable;
    
    /**
     * Create op/3 predicate.
     * 
     * @param solver The query solver
     */
    public Op(SolverContext solver) {
        super(solver);
        // START_CHANGE: R1/R2 - use the shared default OperatorTable (was creating a private one)
        this.operatorTable = OperatorTable.getDefault();
        // END_CHANGE: R1/R2
    }
    
    @Override
    public boolean execute(Term term, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        // Delegate to solve method for compatibility
        return solve(solver, bindings);
    }
    
    @Override
    public boolean solve(SolverContext solver, Map<String, Term> bindings) {
        Term[] args = getArguments();
        if (args.length != 3) {
            return false;
        }
        
        try {
            Term precedenceTerm = args[0];
            Term typeTerm = args[1];
            Term nameTerm = args[2];
            
            // Extract precedence
            if (!(precedenceTerm instanceof it.denzosoft.jprolog.core.terms.Number)) {
                return false;
            }
            int precedence = ((it.denzosoft.jprolog.core.terms.Number) precedenceTerm).getValue().intValue();
            
            // Extract type
            if (!(typeTerm instanceof Atom)) {
                return false;
            }
            String typeStr = ((Atom) typeTerm).getName();
            Operator.Type type;
            try {
                type = Operator.parseType(typeStr);
            } catch (IllegalArgumentException e) {
                return false; // Invalid operator type
            }
            
            // Extract name(s)
            if (nameTerm instanceof Atom) {
                // Single operator name
                String name = ((Atom) nameTerm).getName();
                return defineOrRemoveOperator(precedence, type, name);
                
            } else if (nameTerm instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(nameTerm))) {
                // List of operator names
                return defineOrRemoveOperatorList(precedence, type, nameTerm);
            }
            
            return false;
            
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            return false;
        }
    }
    
    /**
     * Define or remove a single operator.
     * 
     * @param precedence The precedence (0 means remove)
     * @param type The operator type
     * @param name The operator name
     * @return true if successful
     */
    private boolean defineOrRemoveOperator(int precedence, Operator.Type type, String name) {
        try {
            // START_CHANGE: R1 - record current op state for trail-based undo
            final Operator existing = findExisting(type, name);
            final OperatorTable t = operatorTable;
            if (precedence == 0) {
                if (existing != null) {
                    it.denzosoft.jprolog.core.engine.v4.Undo.record(() -> {
                        t.defineOperator(existing.getPrecedence(), existing.getType(), existing.getName());
                    });
                }
                return operatorTable.removeOperator(0, type, name) ||
                       removeAllOperators(name, type);
            } else {
                if (precedence < 1 || precedence > 1200) return false;
                if (!isValidOperatorDefinition(precedence, type, name)) return false;
                if (existing != null) {
                    it.denzosoft.jprolog.core.engine.v4.Undo.record(() -> {
                        t.defineOperator(existing.getPrecedence(), existing.getType(), existing.getName());
                    });
                } else {
                    final Operator.Type capturedType = type;
                    final String capturedName = name;
                    it.denzosoft.jprolog.core.engine.v4.Undo.record(() -> {
                        t.removeOperator(0, capturedType, capturedName);
                    });
                }
                operatorTable.defineOperator(precedence, type, name);
                return true;
            }
            // END_CHANGE: R1
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            return false;
        }
    }

    // START_CHANGE: R1 - lookup existing op by type+name
    private Operator findExisting(Operator.Type type, String name) {
        for (Operator op : operatorTable.getOperators(name)) {
            if (op.getType() == type) return op;
        }
        return null;
    }
    // END_CHANGE: R1
    
    /**
     * Define or remove operators from a list.
     * 
     * @param precedence The precedence
     * @param type The operator type
     * @param list The list of operator names
     * @return true if all successful
     */
    private boolean defineOrRemoveOperatorList(int precedence, Operator.Type type, Term list) {
        java.util.List<Term> names = extractListElements(list);
        
        for (Term nameTerm : names) {
            if (!(nameTerm instanceof Atom)) {
                return false; // All names must be atoms
            }
            
            String name = ((Atom) nameTerm).getName();
            if (!defineOrRemoveOperator(precedence, type, name)) {
                return false; // If any fails, fail the whole operation
            }
        }
        
        return true;
    }
    
    /**
     * Remove all operators with given name and type.
     * 
     * @param name The operator name
     * @param type The operator type
     * @return true if any removed
     */
    private boolean removeAllOperators(String name, Operator.Type type) {
        boolean removed = false;
        
        // Find all operators with this name and type
        for (Operator op : operatorTable.getOperators(name)) {
            if (op.getType() == type) {
                operatorTable.removeOperator(op.getPrecedence(), op.getType(), op.getName());
                removed = true;
            }
        }
        
        return removed;
    }
    
    /**
     * Validate that an operator definition is legal.
     * 
     * @param precedence The precedence
     * @param type The operator type
     * @param name The operator name
     * @return true if valid
     */
    private boolean isValidOperatorDefinition(int precedence, Operator.Type type, String name) {
        // Check for reserved operators that cannot be redefined
        if (isReservedOperator(name)) {
            return false;
        }
        
        // Check precedence limits
        if (precedence < 1 || precedence > 1200) {
            return false;
        }
        
        // Validate operator name
        if (!isValidOperatorName(name)) {
            return false;
        }
        
        // Check for conflicts - can't have same name with conflicting types
        // at same precedence level in some cases
        return !hasConflictingDefinition(precedence, type, name);
    }
    
    /**
     * Check if an operator name is reserved and cannot be redefined.
     * 
     * @param name The operator name
     * @return true if reserved
     */
    private boolean isReservedOperator(String name) {
        // Some operators cannot be redefined
        switch (name) {
            case ",":  // Conjunction is fundamental
            case ";":  // Disjunction structure
            case "!":  // Cut cannot be redefined
                return true;
            default:
                return false;
        }
    }
    
    /**
     * Validate operator name according to ISO Prolog rules.
     * 
     * @param name The operator name
     * @return true if valid
     */
    private boolean isValidOperatorName(String name) {
        if (name == null || name.isEmpty()) {
            return false;
        }
        
        // Operator names must be atoms
        // Cannot be just a variable name pattern
        if (Character.isUpperCase(name.charAt(0)) || name.charAt(0) == '_') {
            return false;
        }
        
        return true;
    }
    
    /**
     * Check for conflicting operator definitions.
     * 
     * @param precedence The precedence
     * @param type The operator type
     * @param name The operator name
     * @return true if conflicts exist
     */
    private boolean hasConflictingDefinition(int precedence, Operator.Type type, String name) {
        // Generally, multiple definitions are allowed, but some combinations
        // might be problematic for parsing
        return false; // Simplified - in full implementation, check parse conflicts
    }
    
    /**
     * Extract elements from a Prolog list.
     * 
     * @param list The list term
     * @return Java list of elements
     */
    private java.util.List<Term> extractListElements(Term list) {
        java.util.List<Term> elements = new java.util.ArrayList<>();
        Term current = list;
        
        while (current instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(current)) && TermUtils.getArity(current) == 2) {
            CompoundTerm cons = (CompoundTerm) current;
            elements.add(TermUtils.getArgument(cons, 0));
            current = TermUtils.getArgument(cons, 1);
        }
        
        return elements;
    }
}