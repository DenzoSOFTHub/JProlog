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
        assertEquals("5", xValue.toString());
        
        solutions = prolog.solve("X is 10 / 2 - 1");
        assertEquals(1, solutions.size());
        xValue = solutions.get(0).get("X");
        assertEquals("4", xValue.toString());
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
        
        // Check that we got all fruits (ISS-2025-0663: the value, not just the key)
        assertEquals(1, prolog.solve("findall(X, fruit(X), Fruits), Fruits == [apple, orange, banana]").size());
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
        assertEquals("123", nValue.toString());
        
        // Test atom_chars conversion
        // ISS-2025-0663: the values, not just "bound"
        solutions = prolog.solve("atom_chars('hello', L), L == [h, e, l, l, o].");
        assertEquals(1, solutions.size());
        
        // Test number_chars conversion
        solutions = prolog.solve("number_chars(456, L), L == ['4', '5', '6'].");
        assertEquals(1, solutions.size());
        assertEquals(1, prolog.solve("atom_number('123', N), N == 123.").size());
    }
}
