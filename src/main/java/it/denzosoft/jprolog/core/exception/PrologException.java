package it.denzosoft.jprolog.core.exception;

import it.denzosoft.jprolog.core.terms.Term;

/**
 * Base class for all ISO Prolog exceptions.
 * Implements the ISO Prolog error term structure.
 */
public class PrologException extends RuntimeException {
    
    private final Term errorTerm;
    private final ErrorType errorType;
    
    /**
     * ISO Prolog error types.
     */
    public enum ErrorType {
        INSTANTIATION_ERROR,
        UNINSTANTIATION_ERROR,
        TYPE_ERROR,
        DOMAIN_ERROR,
        EXISTENCE_ERROR,
        PERMISSION_ERROR,
        REPRESENTATION_ERROR,
        EVALUATION_ERROR,
        RESOURCE_ERROR,
        SYNTAX_ERROR,
        SYSTEM_ERROR
    }
    
    /**
     * Create a Prolog exception.
     * 
     * @param errorType The error type
     * @param errorTerm The ISO error term
     * @param message The error message
     */
    public PrologException(ErrorType errorType, Term errorTerm, String message) {
        super(message);
        this.errorType = errorType;
        this.errorTerm = errorTerm;
    }
    
    /**
     * Create a Prolog exception.
     * 
     * @param errorType The error type
     * @param errorTerm The ISO error term
     * @param message The error message
     * @param cause The underlying cause
     */
    public PrologException(ErrorType errorType, Term errorTerm, String message, Throwable cause) {
        super(message, cause);
        this.errorType = errorType;
        this.errorTerm = errorTerm;
    }
    
    /**
     * Get the error type.
     * 
     * @return The error type
     */
    public ErrorType getErrorType() {
        return errorType;
    }
    
    /**
     * Get the ISO error term.
     * 
     * @return The error term
     */
    public Term getErrorTerm() {
        return errorTerm;
    }
    
    /**
     * Check if this is a specific error type.
     * 
     * @param type The error type to check
     * @return true if matches
     */
    public boolean isErrorType(ErrorType type) {
        return errorType == type;
    }
    
    @Override
    public String toString() {
        return "PrologException{" +
               "type=" + errorType +
               ", term=" + errorTerm +
               ", message='" + getMessage() + '\'' +
               '}';
    }
}