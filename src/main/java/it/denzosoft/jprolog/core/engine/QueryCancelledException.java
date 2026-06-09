package it.denzosoft.jprolog.core.engine;

/**
 * Thrown when a running query is cancelled (the solver thread was interrupted, e.g. by the IDE's
 * Stop button). It is NOT a {@code PrologException}, so it is not catchable by user {@code catch/3}
 * and propagates straight out of the resolution loop to abort the query. (IDE-P0: working Stop.)
 */
public class QueryCancelledException extends RuntimeException {
    public QueryCancelledException() { super("Query cancelled"); }
}
