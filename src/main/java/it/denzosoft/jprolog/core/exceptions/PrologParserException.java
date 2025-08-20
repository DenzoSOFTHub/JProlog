package it.denzosoft.jprolog.core.exceptions;

public class PrologParserException extends PrologException {
    public PrologParserException(String message) {
        super(message);
    }

    public PrologParserException(String message, Throwable cause) {
        super(message, cause);
    }
}
