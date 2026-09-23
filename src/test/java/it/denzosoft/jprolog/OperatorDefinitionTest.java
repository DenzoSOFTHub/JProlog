package it.denzosoft.jprolog;

import it.denzosoft.jprolog.core.engine.Prolog;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * op/3 and current_op/3.
 *
 * <p>START_CHANGE: ISS-2025-0665 - this class drove the legacy
 * {@code builtin.system.OperatorDefinition} directly (execute() with a bindings map and its
 * static {@code getOperator}), which the engine has not dispatched to since op/3 and current_op/3
 * went native (4.3.0) — and the static table it mutated was shared by every test in the JVM.
 * Retargeted at the engine: each test has its own {@link Prolog}, so its operator table is
 * per-engine and nothing leaks into other classes. Every assertion checks the exact answer or
 * the exact ISO error term. END_CHANGE: ISS-2025-0665
 */
public class OperatorDefinitionTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    private int count(String query) {
        return prolog.solve(query).size();
    }

    @Test
    public void testDefineNewOperator() {
        assertEquals(1, count("op(500, xfx, myop)."));
        assertEquals(1, count("current_op(P, T, myop), P == 500, T == xfx."));
        assertEquals("the operator is usable by the reader", 1, count("X = (a myop b), X == myop(a, b)."));
    }

    @Test
    public void testRedefineExistingOperator() {
        assertEquals(1, count("op(500, xfx, testop)."));
        assertEquals(1, count("op(600, yfx, testop)."));
        // the infix definition is replaced, not duplicated
        assertEquals(1, count("findall(P-T, current_op(P, T, testop), L), L == [600-yfx]."));
    }

    @Test
    public void testPrecedenceZeroRemovesAndOutOfRangeRaises() {
        assertEquals(1, count("op(700, xfx, badop1), op(0, xfx, badop1)."));
        assertEquals(0, count("current_op(_, _, badop1)."));
        assertEquals(1, count("catch(op(1201, xfx, badop2), error(E, _), true), "
            + "E == domain_error(operator_priority, 1201)."));
        assertEquals(0, count("current_op(_, _, badop2)."));
    }

    @Test
    public void testInvalidOperatorType() {
        assertEquals(1, count("catch(op(500, invalid, badop), error(E, _), true), "
            + "E == domain_error(operator_specifier, invalid)."));
        assertEquals(1, count("catch(op(500, _, badop), error(E, _), true), E == instantiation_error."));
    }

    @Test
    public void testCurrentOpWithGroundArguments() {
        assertEquals(1, count("op(750, xfy, testcurrent)."));
        assertEquals(1, count("current_op(750, xfy, testcurrent)."));
        assertEquals(0, count("current_op(751, xfy, testcurrent)."));
    }

    @Test
    public void testCurrentOpWithVariable() {
        assertEquals(1, count("op(800, fx, testvarop)."));
        assertEquals(1, count("current_op(P, T, testvarop), P == 800, T == fx."));
    }

    @Test
    public void testCurrentOpFindAllOperators() {
        assertEquals(1, count("current_op(500, yfx, +)."));
        assertEquals(1, count("current_op(700, xfx, =)."));
        assertEquals(1, count("current_op(700, xfx, is)."));
        assertTrue("the default table has dozens of entries",
            prolog.solve("current_op(_, _, _).").size() > 40);
    }

    @Test
    public void testStandardISOOperatorsPreloaded() {
        assertEquals(1, count("findall(P-T, current_op(P, T, +), L), msort(L, S), S == [200-fy, 500-yfx]."));
        assertEquals(1, count("findall(P-T, current_op(P, T, -), L), msort(L, S), S == [200-fy, 500-yfx]."));
        assertEquals(1, count("current_op(700, xfx, is)."));
        assertEquals(1, count("current_op(1000, xfy, ',')."));
        assertEquals(1, count("findall(P-T, current_op(P, T, :-), L), msort(L, S), S == [1200-fx, 1200-xfx]."));
    }

    @Test
    public void testExistentialQuantificationOperator() {
        assertEquals(1, count("current_op(200, xfy, ^)."));
    }

    @Test
    public void testUnivOperator() {
        assertEquals(1, count("current_op(700, xfx, =..)."));
    }

    @Test
    public void testOperatorsArePerEngine() {
        assertEquals(1, count("op(700, xfx, only_here_p7)."));
        Prolog other = new Prolog();
        assertEquals("another engine does not see this engine's operator",
            0, other.solve("current_op(_, _, only_here_p7).").size());
    }
}
