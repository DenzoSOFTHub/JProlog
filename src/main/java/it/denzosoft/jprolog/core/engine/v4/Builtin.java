package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.terms.Term;

// START_CHANGE: ISS-2025-0443 - engine v4, design B.5 (the v4 built-in SPI).
/**
 * A v4 built-in predicate.
 *
 * <p>The old {@code BuiltIn.execute(goal, Map, List<Map>)} contract is eager by construction: it
 * materialises every solution as a full binding map before the first one is used (design limit
 * L-08). A v4 built-in instead receives the goal's <b>dereferenced argument cells</b> and acts on
 * the machine directly: it binds through {@link Machine#unify}, pushes goals, or installs a
 * {@link Generator} for the nondeterministic case, and it can raise ISO errors.
 *
 * <p>Determinism is expressed by what the built-in does, not declared: bind and return
 * {@link Outcome#SUCCESS} (no choice point is created at all), or call
 * {@link Machine#pushGenerator} and return {@link Outcome#SUSPENDED}.
 */
public interface Builtin {

    /** What a built-in call did. */
    enum Outcome {
        /** Succeeded deterministically; execution continues with the next goal. */
        SUCCESS,
        /** Failed; the machine backtracks. */
        FAILURE,
        /** The built-in installed its own continuation (goals and/or a choice point). */
        SUSPENDED
    }

    /**
     * Run the built-in.
     *
     * @param m    the machine (bindings, trail, choice points, engine context, ISO error factory)
     * @param args the goal's arguments, NOT dereferenced (use {@link Machine#deref})
     */
    Outcome call(Machine m, Term[] args);
}
// END_CHANGE: ISS-2025-0443
