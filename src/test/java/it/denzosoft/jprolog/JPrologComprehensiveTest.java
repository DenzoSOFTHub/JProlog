package it.denzosoft.jprolog;

import it.denzosoft.jprolog.terms.Atom;
import it.denzosoft.jprolog.terms.Term;
import it.denzosoft.jprolog.terms.Variable;
import org.junit.Test;
import static org.junit.Assert.*;

import java.util.List;
import java.util.Map;


public class JPrologComprehensiveTest {

    @Test
    public void testBasicFactsAndRules() {
        Prolog prolog = new Prolog();
        
        // Load facts
        prolog.consult("parent(tom, bob).");
        prolog.consult("parent(bob, liz).");
        
        // Load rules
        prolog.consult("grandparent(X, Z) :- parent(X, Y), parent(Y, Z).");
        
        // Test fact query
        List<Map<String, Term>> solutions = prolog.solve("parent(tom, bob)");
        assertEquals(1, solutions.size());
        
        // Test variable query
        solutions = prolog.solve("parent(tom, X)");
        assertEquals(1, solutions.size());
        Term xValue = solutions.get(0).get(new Variable("X"));
        assertEquals("bob", ((Atom) xValue).getName());
        
        // Test rule query
        solutions = prolog.solve("grandparent(tom, liz)");
        assertEquals(1, solutions.size());
    }

    @Test
    public void testArithmeticEvaluation() {
        Prolog prolog = new Prolog();
        
        List<Map<String, Term>> solutions = prolog.solve("X is 2 + 3");
        assertEquals(1, solutions.size());
        Term xValue = solutions.get(0).get(new Variable("X"));
        assertEquals("5.0", ((Atom) xValue).getName());
        
        solutions = prolog.solve("X is 10 / 2 - 1");
        assertEquals(1, solutions.size());
        xValue = solutions.get(0).get(new Variable("X"));
        assertEquals("4.0", ((Atom) xValue).getName());
    }

    @Test
    public void testFindall() {
        Prolog prolog = new Prolog();
        
        // Load facts
        prolog.consult("fruit(apple).");
        prolog.consult("fruit(orange).");
        prolog.consult("fruit(banana).");
        
        // Test findall
        List<Map<String, Term>> solutions = prolog.solve("findall(X, fruit(X), Fruits)");
        assertEquals(1, solutions.size());
        
        // Check that we got all fruits
        assertTrue(solutions.get(0).containsKey(new Variable("Fruits")));
    }

    @Test
    public void testAssertRetract() {
        Prolog prolog = new Prolog();
        
        // Assert a fact
        prolog.asserta("temporary(test).");
        
        // Verify it exists
        List<Map<String, Term>> solutions = prolog.solve("temporary(test)");
        assertEquals(1, solutions.size());
        
        // Retract it
        prolog.retract("temporary(test).");
        
        // Verify it's gone
        solutions = prolog.solve("temporary(test)");
        assertEquals(0, solutions.size());
    }
}
