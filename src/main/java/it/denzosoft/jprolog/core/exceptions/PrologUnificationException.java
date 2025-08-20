package it.denzosoft.jprolog.core.exceptions;

public class PrologUnificationException extends PrologException {
    public PrologUnificationException(String message) {
        super(message);
    }

    public PrologUnificationException(String message, Throwable cause) {
        super(message, cause);
    }
}
