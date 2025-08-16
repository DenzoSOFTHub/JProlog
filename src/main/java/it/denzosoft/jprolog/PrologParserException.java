package it.denzosoft.jprolog;

public class PrologParserException extends PrologException {
    public PrologParserException(String message) {
        super(message);
    }

    public PrologParserException(String message, Throwable cause) {
        super(message, cause);
    }
}
