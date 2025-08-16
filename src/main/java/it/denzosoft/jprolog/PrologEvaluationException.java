package it.denzosoft.jprolog;

public class PrologEvaluationException extends PrologException {
    public PrologEvaluationException(String message) {
        super(message);
    }

    public PrologEvaluationException(String message, Throwable cause) {
        super(message, cause);
    }
}
