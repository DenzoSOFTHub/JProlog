package it.denzosoft.jprolog.core.exceptions;

import it.denzosoft.jprolog.core.terms.Term;

/**
 * Represents a Prolog exception that can be thrown and caught within Prolog code.
 * This implements the ISO Prolog exception handling mechanism.
 */
public class PrologException extends RuntimeException {
    
    private final Term errorTerm;
    private final boolean isHalt;
    private final int exitCode;
    
    /**
     * Create a Prolog exception with an error term.
     * 
     * @param errorTerm The Prolog term representing the error
     */
    public PrologException(Term errorTerm) {
        super(errorTerm.toString());
        this.errorTerm = errorTerm;
        this.isHalt = false;
        this.exitCode = 0;
    }
    
    /**
     * Create a Prolog exception for halt/0 or halt/1.
     * 
     * @param exitCode The exit code for halt
     */
    public PrologException(int exitCode) {
        super("halt(" + exitCode + ")");
        this.errorTerm = null;
        this.isHalt = true;
        this.exitCode = exitCode;
    }
    
    /**
     * Create a Prolog exception for halt/0.
     */
    public PrologException() {
        this(0);
    }
    
    /**
     * Create a Prolog exception with a string message (for compatibility).
     * 
     * @param message The error message
     */
    public PrologException(String message) {
        super(message);
        this.errorTerm = new it.denzosoft.jprolog.core.terms.Atom(message);
        this.isHalt = false;
        this.exitCode = 0;
    }
    
    /**
     * Create a Prolog exception with a string message and cause (for compatibility).
     * 
     * @param message The error message
     * @param cause The underlying cause
     */
    public PrologException(String message, Throwable cause) {
        super(message, cause);
        this.errorTerm = new it.denzosoft.jprolog.core.terms.Atom(message);
        this.isHalt = false;
        this.exitCode = 0;
    }
    
    /**
     * Get the error term associated with this exception.
     * 
     * @return The error term, or null for halt exceptions
     */
    public Term getErrorTerm() {
        return errorTerm;
    }
    
    /**
     * Check if this is a halt exception.
     * 
     * @return true if this is a halt exception
     */
    public boolean isHalt() {
        return isHalt;
    }
    
    /**
     * Get the exit code for halt exceptions.
     * 
     * @return The exit code (0 if not a halt exception)
     */
    public int getExitCode() {
        return exitCode;
    }
    
    @Override
    public String toString() {
        if (isHalt) {
            return "halt(" + exitCode + ")";
        } else {
            return "throw(" + errorTerm + ")";
        }
    }
}