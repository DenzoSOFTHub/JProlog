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
    
    // START_CHANGE: ISS-2025-0101 - PrologString is immutable, reuse instance
    @Override
    public Term copy() {
        return this;
    }
    // END_CHANGE: ISS-2025-0101
    
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
    // START_CHANGE: ISS-2025-0190 - Add missing escape sequences for full round-trip symmetry
    private java.lang.String escapeString(java.lang.String str) {
        return str.replace("\\", "\\\\")
                  .replace("\"", "\\\"")
                  .replace("\u0007", "\\a")
                  .replace("\b", "\\b")
                  .replace("\f", "\\f")
                  .replace("\u000B", "\\v")
                  .replace("\n", "\\n")
                  .replace("\t", "\\t")
                  .replace("\r", "\\r");
    }
    // END_CHANGE: ISS-2025-0190
    
    /**
     * Unescapes special characters in a string.
     * Used when parsing string literals.
     * 
     * @param str the string to unescape
     * @return the unescaped string
     */
    // START_CHANGE: ISS-2025-0188 - Single-pass unescape to handle all edge cases correctly
    public static java.lang.String unescapeString(java.lang.String str) {
        StringBuilder sb = new StringBuilder(str.length());
        for (int i = 0; i < str.length(); i++) {
            char c = str.charAt(i);
            if (c == '\\' && i + 1 < str.length()) {
                char next = str.charAt(i + 1);
                switch (next) {
                    case '\\': sb.append('\\'); i++; break;
                    case '"':  sb.append('"');  i++; break;
                    case 'n':  sb.append('\n'); i++; break;
                    case 't':  sb.append('\t'); i++; break;
                    case 'r':  sb.append('\r'); i++; break;
                    // START_CHANGE: ISS-2025-0189 - Symmetric escape/unescape
                    case 'a':  sb.append('\u0007'); i++; break; // bell
                    case 'b':  sb.append('\b'); i++; break;     // backspace
                    case 'f':  sb.append('\f'); i++; break;     // form feed
                    case 'v':  sb.append('\u000B'); i++; break; // vertical tab
                    case '\'': sb.append('\''); i++; break;     // single quote
                    // END_CHANGE: ISS-2025-0189
                    default:   sb.append(c); break;
                }
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }
    // END_CHANGE: ISS-2025-0188
}