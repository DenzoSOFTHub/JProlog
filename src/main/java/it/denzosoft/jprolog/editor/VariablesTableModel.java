package it.denzosoft.jprolog.editor;

import javax.swing.table.AbstractTableModel;
import java.util.ArrayList;
import java.util.List;

/**
 * Model for the variables table in the debug panel.
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
        // Per ora le variabili sono read-only
        return false;
    }
    
    /**
     * Aggiunge una variabile alla tabella.
     */
    public void addVariable(String name, String value) {
        String type = determineType(value);
        variables.add(new VariableEntry(name, value, type));
        fireTableRowsInserted(variables.size() - 1, variables.size() - 1);
    }
    
    /**
     * Aggiorna il valore di una variabile esistente o la aggiunge se non esiste.
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
        // Se non trovata, aggiungi come nuova variabile
        addVariable(name, value);
    }
    
    /**
     * Rimuove una variabile dalla tabella.
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
     * Pulisce tutte le variabili.
     */
    public void clear() {
        int oldSize = variables.size();
        variables.clear();
        if (oldSize > 0) {
            fireTableRowsDeleted(0, oldSize - 1);
        }
    }
    
    /**
     * Determina il tipo di una variabile basandosi sul suo valore.
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
     * Ottieni tutte le variabili.
     */
    public List<VariableEntry> getVariables() {
        return new ArrayList<>(variables);
    }
    
    /**
     * Rappresenta una entry nella tabella delle variabili.
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