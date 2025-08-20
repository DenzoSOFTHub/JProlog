package it.denzosoft.jprolog.core.engine;

public interface PrologEngine {

    /**
     * Consults a Prolog program from a string.
     *
     * @param program The Prolog program as a string.
     */
    void consult(String program);

    /**
     * Executes a Prolog query.
     *
     * @param query The Prolog query as a string.
     * @return The result of the query.  This might be a boolean (success/failure),
     *         or a list of variable bindings, depending on the implementation.
     */
    Object query(String query);
}
