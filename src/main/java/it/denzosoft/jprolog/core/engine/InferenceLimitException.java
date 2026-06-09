package it.denzosoft.jprolog.core.engine;

/**
 * Thrown when a query exceeds its inference (step) budget (see {@code Prolog.setInferenceBudget}).
 * It is deliberately NOT a {@code PrologException}, so an untrusted program cannot trap it with
 * {@code catch/3} and keep running — it propagates straight out of the resolution loop to the
 * embedder, who bounds CPU on runaway/malicious queries. (ISS-2025-0339)
 */
public class InferenceLimitException extends RuntimeException {
    public InferenceLimitException(long budget) {
        super("Inference budget exceeded (" + budget + " steps)");
    }
}
