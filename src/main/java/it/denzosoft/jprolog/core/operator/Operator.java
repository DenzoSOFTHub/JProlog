package it.denzosoft.jprolog.core.operator;

import java.util.Objects;

/**
 * Represents a Prolog operator with precedence and associativity.
 * Implements ISO Prolog operator system.
 */
public class Operator {
    
    /**
     * Operator types according to ISO Prolog.
     */
    public enum Type {
        FX,   // Prefix, non-associative
        FY,   // Prefix, right-associative
        XF,   // Postfix, non-associative
        YF,   // Postfix, left-associative
        XFX,  // Infix, non-associative
        XFY,  // Infix, right-associative
        YFX   // Infix, left-associative
    }
    
    private final int precedence;
    private final Type type;
    private final String name;
    
    /**
     * Create an operator.
     * 
     * @param precedence The operator precedence (1-1200)
     * @param type The operator type
     * @param name The operator name
     */
    public Operator(int precedence, Type type, String name) {
        if (precedence < 1 || precedence > 1200) {
            throw new IllegalArgumentException("Precedence must be between 1 and 1200");
        }
        
        this.precedence = precedence;
        this.type = Objects.requireNonNull(type, "Type cannot be null");
        this.name = Objects.requireNonNull(name, "Name cannot be null");
    }
    
    /**
     * Get the operator precedence.
     * 
     * @return The precedence
     */
    public int getPrecedence() {
        return precedence;
    }
    
    /**
     * Get the operator type.
     * 
     * @return The type
     */
    public Type getType() {
        return type;
    }
    
    /**
     * Get the operator name.
     * 
     * @return The name
     */
    public String getName() {
        return name;
    }
    
    /**
     * Check if this is a prefix operator.
     * 
     * @return true if prefix
     */
    public boolean isPrefix() {
        return type == Type.FX || type == Type.FY;
    }
    
    /**
     * Check if this is a postfix operator.
     * 
     * @return true if postfix
     */
    public boolean isPostfix() {
        return type == Type.XF || type == Type.YF;
    }
    
    /**
     * Check if this is an infix operator.
     * 
     * @return true if infix
     */
    public boolean isInfix() {
        return type == Type.XFX || type == Type.XFY || type == Type.YFX;
    }
    
    /**
     * Check if this operator is left-associative.
     * 
     * @return true if left-associative
     */
    public boolean isLeftAssociative() {
        return type == Type.YFX || type == Type.YF;
    }
    
    /**
     * Check if this operator is right-associative.
     * 
     * @return true if right-associative
     */
    public boolean isRightAssociative() {
        return type == Type.XFY || type == Type.FY;
    }
    
    /**
     * Check if this operator is non-associative.
     * 
     * @return true if non-associative
     */
    public boolean isNonAssociative() {
        return type == Type.XFX || type == Type.FX || type == Type.XF;
    }
    
    /**
     * Get the minimum precedence for left argument.
     * 
     * @return The minimum precedence
     */
    public int getLeftPrecedence() {
        switch (type) {
            case YFX:
            case YF:
                return precedence;
            case XFX:
            case XFY:
                return precedence - 1;
            default:
                return -1; // No left argument
        }
    }
    
    /**
     * Get the minimum precedence for right argument.
     * 
     * @return The minimum precedence
     */
    public int getRightPrecedence() {
        switch (type) {
            case XFY:
            case FY:
                return precedence;
            case XFX:
            case YFX:
                return precedence - 1;
            case FX:
                return precedence - 1;
            default:
                return -1; // No right argument
        }
    }
    
    /**
     * Parse operator type from string.
     * 
     * @param typeStr The type string
     * @return The operator type
     */
    public static Type parseType(String typeStr) {
        if (typeStr == null) {
            throw new IllegalArgumentException("Type string cannot be null");
        }
        
        try {
            return Type.valueOf(typeStr.toUpperCase());
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException("Invalid operator type: " + typeStr);
        }
    }
    
    @Override
    public String toString() {
        return "op(" + precedence + ", " + type.name().toLowerCase() + ", " + name + ")";
    }
    
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Operator)) return false;
        Operator operator = (Operator) o;
        return precedence == operator.precedence &&
               type == operator.type &&
               Objects.equals(name, operator.name);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(precedence, type, name);
    }
}