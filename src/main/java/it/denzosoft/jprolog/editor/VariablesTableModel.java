package it.denzosoft.jprolog.editor;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import javax.swing.table.AbstractTableModel;
import javax.swing.tree.DefaultMutableTreeNode;
import java.util.ArrayList;
import java.util.List;

/**
 * Model for the variables view in the debug panel.
 *
 * <p>Originally a flat table model; it is retained (all public methods preserved)
 * for backward compatibility, and additionally provides {@link #buildTreeNode}
 * which constructs an expandable {@link DefaultMutableTreeNode} from a real
 * {@link Term} so the debugger can show structure-aware variable trees
 * (M14 VARIABLES TREE).</p>
 */
public class VariablesTableModel extends AbstractTableModel {
    
    private static final String[] COLUMN_NAMES = {"Variable", "Value", "Type"};
    private List<VariableEntry> variables;
    
    public VariablesTableModel() {
        this.variables = new ArrayList<>();
    }
    
    @Override
    public int getRowCount() {
        return variables.size();
    }
    
    @Override
    public int getColumnCount() {
        return COLUMN_NAMES.length;
    }
    
    @Override
    public String getColumnName(int column) {
        return COLUMN_NAMES[column];
    }
    
    @Override
    public Object getValueAt(int rowIndex, int columnIndex) {
        if (rowIndex < 0 || rowIndex >= variables.size()) {
            return null;
        }
        
        VariableEntry entry = variables.get(rowIndex);
        
        switch (columnIndex) {
            case 0: return entry.getName();
            case 1: return entry.getValue();
            case 2: return entry.getType();
            default: return null;
        }
    }
    
    @Override
    public boolean isCellEditable(int rowIndex, int columnIndex) {
        // For now variables are read-only
        return false;
    }
    
    /**
     * Adds a variable to the table.
     */
    public void addVariable(String name, String value) {
        String type = determineType(value);
        variables.add(new VariableEntry(name, value, type));
        fireTableRowsInserted(variables.size() - 1, variables.size() - 1);
    }
    
    /**
     * Updates the value of an existing variable or adds it if it doesn't exist.
     */
    public void updateVariable(String name, String value) {
        for (int i = 0; i < variables.size(); i++) {
            VariableEntry entry = variables.get(i);
            if (entry.getName().equals(name)) {
                entry.setValue(value);
                entry.setType(determineType(value));
                fireTableRowsUpdated(i, i);
                return;
            }
        }
        // If not found, add as new variable
        addVariable(name, value);
    }
    
    /**
     * Removes a variable from the table.
     */
    public void removeVariable(String name) {
        for (int i = 0; i < variables.size(); i++) {
            if (variables.get(i).getName().equals(name)) {
                variables.remove(i);
                fireTableRowsDeleted(i, i);
                break;
            }
        }
    }
    
    /**
     * Clears all variables.
     */
    public void clear() {
        int oldSize = variables.size();
        variables.clear();
        if (oldSize > 0) {
            fireTableRowsDeleted(0, oldSize - 1);
        }
    }
    
    /**
     * Determines the type of a variable based on its value.
     */
    private String determineType(String value) {
        if (value == null) {
            return "null";
        }
        
        // Check if it's a number
        try {
            if (value.contains(".")) {
                Double.parseDouble(value);
                return "Float";
            } else {
                Integer.parseInt(value);
                return "Integer";
            }
        } catch (NumberFormatException e) {
            // Not a number
        }
        
        // Check if it's a Prolog list
        if (value.startsWith("[") && value.endsWith("]")) {
            return "List";
        }
        
        // Check if it's a compound term
        if (value.contains("(") && value.contains(")")) {
            return "Compound";
        }
        
        // Check if it's an uninstantiated variable
        if (value.equals("_") || (value.startsWith("_") && value.length() > 1)) {
            return "Var";
        }
        
        // Default: atom
        return "Atom";
    }
    
    /**
     * Get all variables.
     */
    public List<VariableEntry> getVariables() {
        return new ArrayList<>(variables);
    }

    // ===================== M14 VARIABLES TREE (structure-aware) =====================

    /**
     * Build an expandable tree node for a single {@code Name = Value} binding,
     * backed by the real {@link Term}. A CompoundTerm node shows its functor and
     * expands to its argument children; a list expands to its elements; atoms,
     * numbers and unbound variables are leaves rendered as {@code Name = Value}.
     *
     * @param name  the variable name (label prefix)
     * @param value the (already resolved) term value
     * @return a tree node whose children describe the structure of {@code value}
     */
    public static DefaultMutableTreeNode buildTreeNode(String name, Term value) {
        DefaultMutableTreeNode node = new DefaultMutableTreeNode(name + " = " + safeToString(value));
        addChildren(node, value);
        return node;
    }

    /**
     * Recursively append structural children of {@code term} to {@code parent}.
     * Lists expand element-by-element; other compound terms expand argument-by-argument.
     * Leaves (atoms, numbers, variables) get no children.
     */
    private static void addChildren(DefaultMutableTreeNode parent, Term term) {
        if (!(term instanceof CompoundTerm)) {
            return; // leaf: atom, number or variable
        }
        CompoundTerm c = (CompoundTerm) term;

        // Prolog list ('.'/2): expand element by element down the spine.
        if (".".equals(c.getName()) && c.getArguments() != null && c.getArguments().size() == 2) {
            int index = 0;
            Term current = term;
            while (current instanceof CompoundTerm
                    && ".".equals(((CompoundTerm) current).getName())
                    && ((CompoundTerm) current).getArguments().size() == 2) {
                CompoundTerm cell = (CompoundTerm) current;
                Term head = cell.getArguments().get(0);
                DefaultMutableTreeNode elem =
                    new DefaultMutableTreeNode("[" + index + "] = " + safeToString(head));
                addChildren(elem, head);
                parent.add(elem);
                current = cell.getArguments().get(1);
                index++;
            }
            // Non-[] tail (improper list or unbound tail variable).
            if (!(current instanceof Atom && "[]".equals(((Atom) current).getName()))) {
                DefaultMutableTreeNode tail =
                    new DefaultMutableTreeNode("|tail = " + safeToString(current));
                addChildren(tail, current);
                parent.add(tail);
            }
            return;
        }

        // Generic compound term: one child per argument, labelled arg(i).
        List<Term> args = c.getArguments();
        if (args != null) {
            for (int i = 0; i < args.size(); i++) {
                Term arg = args.get(i);
                DefaultMutableTreeNode child =
                    new DefaultMutableTreeNode("arg" + (i + 1) + " = " + safeToString(arg));
                addChildren(child, arg);
                parent.add(child);
            }
        }
    }

    private static String safeToString(Term t) {
        if (t == null) return "_";
        if (t instanceof Variable) {
            String n = t.getName();
            return (n != null) ? n : t.toString();
        }
        return t.toString();
    }

    /**
     * Represents an entry in the variables table.
     */
    public static class VariableEntry {
        private String name;
        private String value;
        private String type;
        
        public VariableEntry(String name, String value, String type) {
            this.name = name;
            this.value = value;
            this.type = type;
        }
        
        public String getName() {
            return name;
        }
        
        public String getValue() {
            return value;
        }
        
        public void setValue(String value) {
            this.value = value;
        }
        
        public String getType() {
            return type;
        }
        
        public void setType(String type) {
            this.type = type;
        }
        
        @Override
        public String toString() {
            return name + " = " + value + " (" + type + ")";
        }
    }
}