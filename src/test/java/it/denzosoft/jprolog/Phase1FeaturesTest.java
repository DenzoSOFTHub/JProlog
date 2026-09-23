package it.denzosoft.jprolog;

import it.denzosoft.jprolog.core.engine.Prolog;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Integration tests for the Phase 1 ISO features: the arithmetic functions and the operator
 * system.
 *
 * <p>START_CHANGE: ISS-2025-0665 - retargeted at the engine. The class evaluated terms with the
 * legacy {@code core.engine.ArithmeticEvaluator} (not the engine's arithmetic since 3.x; the
 * engine runs {@code core.arith.v2.ArithEvaluator}) and defined operators through the legacy
 * {@code OperatorDefinition} and its JVM-wide static table (shared with every other test class).
 * Everything now goes through {@code is/2}, {@code op/3} and {@code current_op/3} on a private
 * {@link Prolog}, with exact results (integer vs float included). END_CHANGE: ISS-2025-0665
 */
public class Phase1FeaturesTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    private void holds(String query) {
        assertEquals(query, 1, prolog.solve(query).size());
    }

    @Test
    public void testMathematicalFunctions() {
        holds("X is sqrt(16.0), X == 4.0.");
        holds("X is sin(pi / 2), abs(X - 1.0) < 1.0e-12.");
        holds("X is abs(-5.5), X == 5.5.");
        holds("X is abs(-5), X == 5.");
        holds("X is log(e), abs(X - 1.0) < 1.0e-12.");
    }

    @Test
    public void testOperatorDefinitionSystem() {
        holds("op(500, xfx, myop).");
        holds("current_op(P, T, myop), P == 500, T == xfx.");
        holds("X = (1 myop 2), X == myop(1, 2).");
    }

    @Test
    public void testStandardOperatorsPreloaded() {
        holds("current_op(500, yfx, +).");
        holds("current_op(200, xfy, ^).");
        holds("current_op(700, xfx, =..).");
    }

    @Test
    public void testCurrentOpQuery() {
        holds("current_op(700, xfx, =).");
        assertTrue("many operators", prolog.solve("current_op(_, _, _).").size() > 10);
    }

    @Test
    public void testComplexArithmeticExpressions() {
        holds("X is sqrt(abs(-16)), X == 4.0.");
        holds("A is pi / 4, X is sin(A) ** 2 + cos(A) ** 2, abs(X - 1.0) < 1.0e-12.");
    }

    @Test
    public void testISOArithmeticFunctions() {
        holds("X is sign(5.5), X == 1.0.");
        holds("X is sign(-3.2), X == -1.0.");
        holds("X is sign(-3), X == -1.");
        holds("X is truncate(3.8), X == 3.");
        holds("X is floor(3.7), X == 3.");
        holds("X is ceiling(3.2), X == 4.");
        holds("X is round(2.5), X == 3.");
        holds("catch(X is foo(1), error(E, _), true), E == type_error(evaluable, foo/1).");
    }
}
