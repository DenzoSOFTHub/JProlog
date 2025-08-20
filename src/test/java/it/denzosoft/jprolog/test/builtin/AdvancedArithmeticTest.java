package it.denzosoft.jprolog.test.builtin;
import it.denzosoft.jprolog.core.engine.Prolog;

import org.junit.Before;
import org.junit.Test;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * Test suite for advanced ISO Prolog arithmetic predicates.
 */
public class AdvancedArithmeticTest {
    
    private Prolog prolog;
    
    @Before
    public void setUp() {
        prolog = new Prolog();
    }
    
    @Test
    public void testBetweenGeneration() {
        // Test generating values in a range
        List<Map<String, Term>> solutions = prolog.solve("between(1, 3, X)");
        assertEquals("Should generate 3 solutions", 3, solutions.size());
        
        assertEquals("1.0", solutions.get(0).get("X").toString());
        assertEquals("2.0", solutions.get(1).get("X").toString());
        assertEquals("3.0", solutions.get(2).get("X").toString());
        
        // Test generating with larger range
        solutions = prolog.solve("between(5, 8, X)");
        assertEquals("Should generate 4 solutions", 4, solutions.size());
    }
    
    @Test
    public void testBetweenChecking() {
        // Test checking if value is in range
        List<Map<String, Term>> solutions = prolog.solve("between(1, 5, 3)");
        assertFalse("between(1, 5, 3) should succeed", solutions.isEmpty());
        
        solutions = prolog.solve("between(1, 5, 7)");
        assertTrue("between(1, 5, 7) should fail", solutions.isEmpty());
        
        solutions = prolog.solve("between(1, 5, 0)");
        assertTrue("between(1, 5, 0) should fail", solutions.isEmpty());
        
        // Test boundary values
        solutions = prolog.solve("between(1, 5, 1)");
        assertFalse("between(1, 5, 1) should succeed", solutions.isEmpty());
        
        solutions = prolog.solve("between(1, 5, 5)");
        assertFalse("between(1, 5, 5) should succeed", solutions.isEmpty());
    }
    
    @Test
    public void testBetweenEdgeCases() {
        // Test empty range
        List<Map<String, Term>> solutions = prolog.solve("between(5, 3, X)");
        assertTrue("Empty range should fail", solutions.isEmpty());
        
        // Test single value range
        solutions = prolog.solve("between(5, 5, X)");
        assertEquals("Single value range should generate one solution", 1, solutions.size());
        assertEquals("5.0", solutions.get(0).get("X").toString());
        
        // Test negative range
        solutions = prolog.solve("between(-2, 1, X)");
        assertEquals("Should generate 4 solutions", 4, solutions.size());
    }
    
    @Test
    public void testSucc() {
        // Test getting successor
        List<Map<String, Term>> solutions = prolog.solve("succ(1, X)");
        assertFalse("succ(1, X) should succeed", solutions.isEmpty());
        assertEquals("2.0", solutions.get(0).get("X").toString());
        
        // Test getting predecessor
        solutions = prolog.solve("succ(X, 5)");
        assertFalse("succ(X, 5) should succeed", solutions.isEmpty());
        assertEquals("4.0", solutions.get(0).get("X").toString());
        
        // Test checking relationship
        solutions = prolog.solve("succ(3, 4)");
        assertFalse("succ(3, 4) should succeed", solutions.isEmpty());
        
        solutions = prolog.solve("succ(3, 5)");
        assertTrue("succ(3, 5) should fail", solutions.isEmpty());
    }
    
    @Test
    public void testSuccEdgeCases() {
        // Test with zero
        List<Map<String, Term>> solutions = prolog.solve("succ(0, 1)");
        assertFalse("succ(0, 1) should succeed", solutions.isEmpty());
        
        // Test negative numbers (should fail)
        solutions = prolog.solve("succ(-1, 0)");
        assertTrue("succ(-1, 0) should fail", solutions.isEmpty());
        
        solutions = prolog.solve("succ(1, -1)");
        assertTrue("succ(1, -1) should fail", solutions.isEmpty());
        
        // Test getting predecessor of 0 (should fail)
        solutions = prolog.solve("succ(X, 0)");
        assertTrue("succ(X, 0) should fail", solutions.isEmpty());
    }
    
    @Test
    public void testPlus() {
        // Test addition
        List<Map<String, Term>> solutions = prolog.solve("plus(2, 3, X)");
        assertFalse("plus(2, 3, X) should succeed", solutions.isEmpty());
        assertEquals("5.0", solutions.get(0).get("X").toString());
        
        // Test first subtraction
        solutions = prolog.solve("plus(X, 3, 8)");
        assertFalse("plus(X, 3, 8) should succeed", solutions.isEmpty());
        assertEquals("5.0", solutions.get(0).get("X").toString());
        
        // Test second subtraction
        solutions = prolog.solve("plus(7, X, 10)");
        assertFalse("plus(7, X, 10) should succeed", solutions.isEmpty());
        assertEquals("3.0", solutions.get(0).get("X").toString());
        
        // Test checking relationship
        solutions = prolog.solve("plus(4, 5, 9)");
        assertFalse("plus(4, 5, 9) should succeed", solutions.isEmpty());
        
        solutions = prolog.solve("plus(4, 5, 10)");
        assertTrue("plus(4, 5, 10) should fail", solutions.isEmpty());
    }
    
    @Test
    public void testPlusWithNegatives() {
        // Test with negative numbers
        List<Map<String, Term>> solutions = prolog.solve("plus(-2, 5, X)");
        assertFalse("plus(-2, 5, X) should succeed", solutions.isEmpty());
        assertEquals("3.0", solutions.get(0).get("X").toString());
        
        solutions = prolog.solve("plus(X, -3, 2)");
        assertFalse("plus(X, -3, 2) should succeed", solutions.isEmpty());
        assertEquals("5.0", solutions.get(0).get("X").toString());
        
        solutions = prolog.solve("plus(2, X, -1)");
        assertFalse("plus(2, X, -1) should succeed", solutions.isEmpty());
        assertEquals("-3.0", solutions.get(0).get("X").toString());
    }
    
    @Test
    public void testPlusWithFloats() {
        // Test with floating point numbers
        List<Map<String, Term>> solutions = prolog.solve("plus(1.5, 2.5, X)");
        assertFalse("plus(1.5, 2.5, X) should succeed", solutions.isEmpty());
        assertEquals("4.0", solutions.get(0).get("X").toString());
        
        solutions = prolog.solve("plus(X, 1.5, 3.7)");
        assertFalse("plus(X, 1.5, 3.7) should succeed", solutions.isEmpty());
        assertEquals("2.2", solutions.get(0).get("X").toString());
    }
    
    @Test
    public void testSimpleIntegration() {
        // Test simple combination of predicates
        List<Map<String, Term>> solutions = prolog.solve("between(1, 3, X), succ(X, Y)");
        assertEquals("Should generate 3 solutions", 3, solutions.size());
        
        // Test plus with simple values
        solutions = prolog.solve("plus(2, 3, Y), Y = 5");
        assertFalse("Simple integration should succeed", solutions.isEmpty());
    }
    
    @Test
    public void testArithmeticSequences() {
        // Test basic arithmetic sequences
        List<Map<String, Term>> solutions = prolog.solve("plus(3, 3, B), succ(B, C)");
        assertFalse("Arithmetic sequence should succeed", solutions.isEmpty());
        assertEquals("6.0", solutions.get(0).get("B").toString());
        assertEquals("7.0", solutions.get(0).get("C").toString());
    }
    
    @Test
    public void testArithmeticWithFindall() {
        // Test collecting arithmetic results
        List<Map<String, Term>> solutions = prolog.solve("findall(X, between(1, 5, X), L)");
        assertFalse("findall with between should succeed", solutions.isEmpty());
        assertTrue("Should contain list of numbers", solutions.get(0).get("L").toString().contains("1.0"));
        assertTrue("Should contain list of numbers", solutions.get(0).get("L").toString().contains("5.0"));
    }
}