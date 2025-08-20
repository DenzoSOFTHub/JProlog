package it.denzosoft.jprolog.core.exceptions;

public class PrologEvaluationException extends PrologException {
    public PrologEvaluationException(String message) {
        super(message);
    }

    public PrologEvaluationException(String message, Throwable cause) {
        super(message, cause);
    }
}
