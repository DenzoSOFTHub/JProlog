package it.denzosoft.jprolog.test.core;
import it.denzosoft.jprolog.core.engine.Prolog;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
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
        Term xValue = solutions.get(0).get("X");
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
        Term xValue = solutions.get(0).get("X");
        assertEquals("5.0", xValue.toString());
        
        solutions = prolog.solve("X is 10 / 2 - 1");
        assertEquals(1, solutions.size());
        xValue = solutions.get(0).get("X");
        assertEquals("4.0", xValue.toString());
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
        assertTrue(solutions.get(0).containsKey("Fruits"));
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
    
    @Test
    public void testTypeConversions() {
        Prolog prolog = new Prolog();
        
        // Test atom_number conversion
        List<Map<String, Term>> solutions = prolog.solve("atom_number('123', N).");
        assertEquals(1, solutions.size());
        Term nValue = solutions.get(0).get("N");
        assertNotNull(nValue);
        assertEquals("123.0", nValue.toString());
        
        // Test atom_chars conversion
        solutions = prolog.solve("atom_chars('hello', L).");
        assertEquals(1, solutions.size());
        Term lValue = solutions.get(0).get("L");
        assertNotNull(lValue);
        
        // Test number_chars conversion
        solutions = prolog.solve("number_chars(456, L).");
        assertEquals(1, solutions.size());
        lValue = solutions.get(0).get("L");
        assertNotNull(lValue);
    }
}
