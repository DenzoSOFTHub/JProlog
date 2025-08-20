package it.denzosoft.jprolog.test.core;
import it.denzosoft.jprolog.core.engine.Prolog;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import org.junit.Test;
import static org.junit.Assert.*;

import java.util.List;
import java.util.Map;


public class PrologTest {

    @Test
    public void testArithmeticIsPredicate() {
        Prolog prolog = new Prolog();
        List<Map<String, Term>> solutions = prolog.solve("X is 2 + 3.");
        assertFalse(solutions.isEmpty());
        assertEquals("5.0", solutions.get(0).get("X").toString());

        solutions = prolog.solve("X is 2 * (3 + 1).");
        assertFalse(solutions.isEmpty());
        assertEquals("8.0", solutions.get(0).get("X").toString());

        solutions = prolog.solve("X is 10 / 2 - 1.");
        assertFalse(solutions.isEmpty());
        assertEquals("4.0", solutions.get(0).get("X").toString());
    }

     @Test
    public void testConditionalPredicate() {
        Prolog prolog = new Prolog();
        
        // Test conditional operators directly in queries (this works)
        List<Map<String, Term>> solutions = prolog.solve("(5 > 0 -> Y = positive ; Y = negative).");
        assertFalse(solutions.isEmpty());
        assertEquals("positive", solutions.get(0).get("Y").toString());

        // Test with false condition
        solutions = prolog.solve("(-2 > 0 -> Y = positive ; Y = negative).");
        assertFalse(solutions.isEmpty());
        assertEquals("negative", solutions.get(0).get("Y").toString());
    }

    @Test
    public void testAssertAndRetract() {
        Prolog prolog = new Prolog();
        prolog.asserta("test(a).");
        List<Map<String, Term>> solutions = prolog.solve("test(X).");
        assertFalse(solutions.isEmpty());
        assertEquals("a", ((Atom) solutions.get(0).get("X")).getName());

        prolog.retract("test(a).");
        solutions = prolog.solve("test(X).");
        assertTrue(solutions.isEmpty());
    }

    @Test
    public void testTypeCheckingPredicates() {
        Prolog prolog = new Prolog();

        // Test isAtom
        assertTrue(prolog.isAtom(new Atom("hello")));
        assertFalse(prolog.isAtom(new Variable("X")));
        assertFalse(prolog.isAtom(new Number(123)));

        // Test isInteger - should work with Number instances that have integer values
        assertTrue(prolog.isInteger(new Number(123)));
        assertTrue(prolog.isInteger(new Number(0)));
        assertFalse(prolog.isInteger(new Number(3.14)));
        assertFalse(prolog.isInteger(new Atom("abc")));

        // Test isFloat - should work with Number instances that have decimal values
        assertTrue(prolog.isFloat(new Number(3.14)));
        assertTrue(prolog.isFloat(new Number(2.5)));
        assertFalse(prolog.isFloat(new Number(123))); // This is an integer
        assertFalse(prolog.isFloat(new Atom("abc")));

    }
}
