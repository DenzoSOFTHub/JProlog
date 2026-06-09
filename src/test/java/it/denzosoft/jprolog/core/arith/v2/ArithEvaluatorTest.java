package it.denzosoft.jprolog.core.arith.v2;

import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.operator.OperatorTable;
import it.denzosoft.jprolog.core.parser.v2.TermReader;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import org.junit.Test;

import java.util.Arrays;
import java.util.HashMap;

import static org.junit.Assert.*;

/** Validates the clean-room v2 {@link ArithEvaluator}: type rules, big integers, and ISO errors. */
public class ArithEvaluatorTest {

    private final OperatorTable ops = OperatorTable.getDefault();
    private Number e(String expr) {
        return ArithEvaluator.eval(TermReader.parseTerm(expr, ops), new HashMap<>());
    }
    private static void assertInt(Number n, long v) { assertTrue("expected integer: " + n, n.isInteger()); assertEquals(v, n.longValue()); }
    private static void assertFloat(Number n, double v) { assertTrue("expected float: " + n, n.isFloat()); assertEquals(v, n.doubleValue(), 1e-9); }

    @Test public void basicAndPrecedence() {
        assertInt(e("1 + 2 * 3"), 7);
        assertInt(e("(1 + 2) * 3"), 9);
        assertInt(e("10 - 3 - 2"), 5);
        assertInt(e("2 * 3 + 4 * 5"), 26);
    }

    @Test public void integerVsFloatDivision() {
        assertInt(e("10 / 2"), 5);            // exact -> integer
        assertFloat(e("7 / 2"), 3.5);         // inexact -> float
        assertInt(e("7 // 2"), 3);
        assertInt(e("7 mod 3"), 1);
        assertInt(e("-7 mod 3"), 2);          // floor mod
        assertInt(e("7 rem 3"), 1);
        assertInt(e("-7 div 2"), -4);         // floor div ('div' operator now registered, ISS-0300)
    }

    @Test public void bigIntegerPrecision() {
        Number n = e("2 ^ 100");
        assertTrue(n.isInteger());
        assertEquals(new java.math.BigInteger("1267650600228229401496703205376"), n.bigIntegerValue());
    }

    @Test public void powerTypes() {
        assertFloat(e("2 ** 3"), 8.0);        // ** is float (ISO)
        assertInt(e("2 ^ 3"), 8);             // ^ is integer
    }

    @Test public void minMaxPreserveType() {
        assertInt(e("min(2, 3.0)"), 2);       // selected operand keeps its type
        assertFloat(e("max(2, 3.0)"), 3.0);
        assertInt(e("max(5, 2)"), 5);
    }

    @Test public void absSignGcd() {
        assertInt(e("abs(-7)"), 7);
        assertInt(e("sign(-3)"), -1);
        assertInt(e("gcd(12, 8)"), 4);
        assertFloat(e("abs(-2.5)"), 2.5);
    }

    @Test public void rounding() {
        assertInt(e("truncate(3.7)"), 3);
        assertInt(e("round(2.5)"), 3);
        assertInt(e("floor(-1.5)"), -2);
        assertInt(e("ceiling(1.2)"), 2);
        assertFloat(e("float(3)"), 3.0);
    }

    @Test public void bitwise() {
        assertInt(e("5 /\\ 3"), 1);
        assertInt(e("5 \\/ 2"), 7);
        assertInt(e("5 xor 1"), 4);
        assertInt(e("1 << 4"), 16);
        assertInt(e("16 >> 2"), 4);
    }

    @Test public void floatFunctions() {
        assertFloat(e("sqrt(16)"), 4.0);
        assertFloat(e("exp(0)"), 1.0);
        assertFloat(e("log(1)"), 0.0);
    }

    // ---- ISO error terms ----
    @Test public void instantiationError() {
        Term expr = new CompoundTerm(new Atom("+"), Arrays.asList(new Variable("X"), new Number(1L)));
        assertErr(() -> ArithEvaluator.eval(expr, new HashMap<>()), "instantiation_error");
    }
    @Test public void typeErrorIntegerForFloatMod() {
        assertErr(() -> e("7.5 mod 2"), "type_error");
    }
    @Test public void typeErrorEvaluableUnknown() {
        assertErr(() -> e("foo(1, 2)"), "evaluable");
    }
    @Test public void zeroDivisor() {
        assertErr(() -> e("3 / 0"), "zero_divisor");
        assertErr(() -> e("3 // 0"), "zero_divisor");
    }

    private void assertErr(Runnable r, String expectInTerm) {
        try {
            r.run();
            fail("expected a PrologException containing '" + expectInTerm + "'");
        } catch (PrologException ex) {
            String s = String.valueOf(ex.getErrorTerm());
            assertTrue("error term '" + s + "' should mention '" + expectInTerm + "'", s.contains(expectInTerm));
        }
    }
}
