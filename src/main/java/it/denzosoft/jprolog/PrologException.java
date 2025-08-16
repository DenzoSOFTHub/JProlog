package it.denzosoft.jprolog;

public class PrologException extends RuntimeException {
    public PrologException(String message) {
        super(message);
    }

    public PrologException(String message, Throwable cause) {
        super(message, cause);
    }
}
