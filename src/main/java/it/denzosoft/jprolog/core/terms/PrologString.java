package it.denzosoft.jprolog.core.terms;

import java.util.Map;

/**
 * Represents a string term in Prolog.
 * Strings are atomic terms that contain text enclosed in double quotes.
 * 
 * Examples: "hello", "world", "Hello, World!"
 */
public class PrologString extends Term {
    private final java.lang.String value;
    
    /**
     * Creates a new string term with the given value.
     * 
     * @param value the string value (without quotes)
     */
    public PrologString(java.lang.String value) {
        this.value = value != null ? value : "";
    }
    
    /**
     * Gets the string value.
     * 
     * @return the string value
     */
    public java.lang.String getStringValue() {
        return value;
    }
    
    @Override
    public boolean isGround() {
        return true; // Strings are always ground
    }
    
    @Override
    public Term copy() {
        return new PrologString(value); // Create a copy with the same value
    }
    
    @Override
    public Term resolveBindings(Map<java.lang.String, Term> bindings) {
        return this; // Strings don't have variables to resolve
    }
    
    @Override
    public boolean unify(Term other, Map<java.lang.String, Term> bindings) {
        if (other instanceof Variable) {
            return other.unify(this, bindings);
        }
        
        if (other instanceof PrologString) {
            return this.value.equals(((PrologString) other).value);
        }
        
        return false;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!(obj instanceof PrologString)) return false;
        
        PrologString other = (PrologString) obj;
        return value.equals(other.value);
    }
    
    @Override
    public int hashCode() {
        return value.hashCode();
    }
    
    @Override
    public java.lang.String toString() {
        // Return the string with quotes for display
        return "\"" + escapeString(value) + "\"";
    }
    
    /**
     * Returns the raw string value without quotes.
     * Useful for internal processing.
     * 
     * @return the raw string value
     */
    public java.lang.String toRawString() {
        return value;
    }
    
    /**
     * Escapes special characters in the string for display.
     * 
     * @param str the string to escape
     * @return the escaped string
     */
    private java.lang.String escapeString(java.lang.String str) {
        return str.replace("\\", "\\\\")
                  .replace("\"", "\\\"")
                  .replace("\n", "\\n")
                  .replace("\t", "\\t")
                  .replace("\r", "\\r");
    }
    
    /**
     * Unescapes special characters in a string.
     * Used when parsing string literals.
     * 
     * @param str the string to unescape
     * @return the unescaped string
     */
    public static java.lang.String unescapeString(java.lang.String str) {
        return str.replace("\\\"", "\"")
                  .replace("\\\\", "\\")
                  .replace("\\n", "\n")
                  .replace("\\t", "\t")
                  .replace("\\r", "\r");
    }
}