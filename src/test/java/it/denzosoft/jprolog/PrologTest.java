package it.denzosoft.jprolog;

import it.denzosoft.jprolog.terms.Atom;
import it.denzosoft.jprolog.terms.Term;
import it.denzosoft.jprolog.terms.Variable;
import org.junit.Test;
import static org.junit.Assert.*;

import java.util.List;
import java.util.Map;


public class PrologTest {

    @Test
    public void testArithmeticIsPredicate() {
        Prolog prolog = new Prolog();
        List<Map<Variable, Term>> solutions = prolog.solve("X is 2 + 3.");
        assertFalse(solutions.isEmpty());
        assertEquals("5.0", ((Atom) solutions.get(0).get(new Variable("X"))).getName());

        solutions = prolog.solve("X is 2 * (3 + 1).");
        assertFalse(solutions.isEmpty());
        assertEquals("8.0", ((Atom) solutions.get(0).get(new Variable("X"))).getName());

        solutions = prolog.solve("X is 10 / 2 - 1.");
        assertFalse(solutions.isEmpty());
        assertEquals("4.0", ((Atom) solutions.get(0).get(new Variable("X"))).getName());
    }

     @Test
    public void testConditionalPredicate() {
        Prolog prolog = new Prolog();
        prolog.consult("test(X) :- (X > 0 -> Y = positive ; Y = negative).");

        List<Map<Variable, Term>> solutions = prolog.solve("test(5).");
        assertFalse(solutions.isEmpty());
        assertEquals("positive", ((Atom) solutions.get(0).get(new Variable("Y"))).getName());

        solutions = prolog.solve("test(-2).");
        assertFalse(solutions.isEmpty());
        assertEquals("negative", ((Atom) solutions.get(0).get(new Variable("Y"))).getName());
    }

    @Test
    public void testAssertAndRetract() {
        Prolog prolog = new Prolog();
        prolog.asserta("test(a).");
        List<Map<Variable, Term>> solutions = prolog.solve("test(X).");
        assertFalse(solutions.isEmpty());
        assertEquals("a", ((Atom) solutions.get(0).get(new Variable("X"))).getName());

        prolog.retract("test(a).");
        solutions = prolog.solve("test(X).");
        assertTrue(solutions.isEmpty());
    }

    @Test
    public void testTypeCheckingPredicates() {
        Prolog prolog = new Prolog();

        assertTrue(prolog.isAtom(new Atom("hello")));
        assertFalse(prolog.isAtom(new Variable("X")));

        assertTrue(prolog.isInteger(new Atom("123")));
        assertFalse(prolog.isInteger(new Atom("abc")));

        assertTrue(prolog.isFloat(new Atom("3.14")));
        assertFalse(prolog.isFloat(new Atom("abc")));

    }
}
