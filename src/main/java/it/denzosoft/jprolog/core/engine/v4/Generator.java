package it.denzosoft.jprolog.core.engine.v4;

// START_CHANGE: ISS-2025-0443 - engine v4, design B.5 (lazy nondeterministic built-ins).
/**
 * The nondeterministic half of the v4 built-in SPI: a lazy, possibly infinite supply of solutions.
 *
 * <p>{@link #next} is called once per solution — first when the built-in installs the generator and
 * again on every redo, after the machine has undone the trail to the choice point's mark. It binds
 * through the machine and returns {@code true}, or returns {@code false} when it is exhausted. That
 * is what makes {@code repeat/0}, {@code between/3} and {@code length/2} enumeration cost O(1)
 * memory per redo instead of a pre-built (and therefore bounded) list of solutions.
 */
public interface Generator {

    /** Produce the next solution by binding through {@code m}; false when exhausted. */
    boolean next(Machine m);

    /** Release resources when the choice point is cut away. */
    default void cut() {}
}
// END_CHANGE: ISS-2025-0443
