package it.denzosoft.jprolog.test.builtin;
import it.denzosoft.jprolog.core.engine.Prolog;

import org.junit.Before;
import org.junit.Test;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * Simplified test suite for ISO Prolog dynamic database operations.
 */
public class DynamicDatabaseSimpleTest {
    
    private Prolog prolog;
    
    @Before
    public void setUp() {
        prolog = new Prolog();
    }
    
    @Test
    public void testAssertzBasic() {
        // Test basic assertz functionality
        prolog.solve("assertz(fact(test))");
        
        List<Map<String, Term>> solutions = prolog.solve("fact(test)");
        assertFalse("Should find asserted fact", solutions.isEmpty());
    }
    
    @Test
    public void testAssertaBasic() {
        // Test basic asserta functionality
        prolog.solve("asserta(fact(first))");
        prolog.solve("asserta(fact(second))");
        
        List<Map<String, Term>> solutions = prolog.solve("fact(X)");
        assertEquals("Should have 2 solutions", 2, solutions.size());
        assertEquals("second", solutions.get(0).get("X").toString());
        assertEquals("first", solutions.get(1).get("X").toString());
    }
    
    @Test
    public void testRetractBasic() {
        // Test basic retract functionality
        String program = "initial(a). initial(b). initial(c).";
        prolog.consult(program);
        
        prolog.solve("retract(initial(b))");
        
        // Check that b is gone
        List<Map<String, Term>> solutions = prolog.solve("initial(b)");
        assertTrue("Should not find retracted fact", solutions.isEmpty());
        
        // Check that a and c still exist
        solutions = prolog.solve("initial(a)");
        assertFalse("Should still find a", solutions.isEmpty());
        solutions = prolog.solve("initial(c)");
        assertFalse("Should still find c", solutions.isEmpty());
    }
    
    @Test
    public void testRetractallBasic() {
        // Test basic retractall functionality
        String program = "multi(1, a). multi(2, b). multi(1, c). multi(3, d).";
        prolog.consult(program);
        
        prolog.solve("retractall(multi(1, Y))");
        
        // Check that all multi(1, _) are gone
        List<Map<String, Term>> solutions = prolog.solve("multi(1, X)");
        assertTrue("Should remove all matching facts", solutions.isEmpty());
        
        // Check that others still exist
        solutions = prolog.solve("multi(2, b)");
        assertFalse("Should keep non-matching facts", solutions.isEmpty());
    }
    
    // Note: abolish and current_predicate tests require predicate indicator syntax
    // which the current parser doesn't support well
    
    @Test
    public void testAssertOrder() {
        // Test that assertz adds at end, asserta at beginning
        prolog.solve("assertz(order(1))");
        prolog.solve("assertz(order(2))");
        prolog.solve("asserta(order(0))");
        
        List<Map<String, Term>> solutions = prolog.solve("order(X)");
        assertEquals("Should have 3 solutions", 3, solutions.size());
        assertEquals("0.0", solutions.get(0).get("X").toString()); // asserta puts at beginning
        assertEquals("1.0", solutions.get(1).get("X").toString());
        assertEquals("2.0", solutions.get(2).get("X").toString());
    }
    
    @Test
    public void testRetractSuccess() {
        // Test that retract succeeds when it finds something
        prolog.solve("assertz(temp_fact(value))");
        
        List<Map<String, Term>> solutions = prolog.solve("retract(temp_fact(value))");
        assertFalse("retract should succeed when finding fact", solutions.isEmpty());
    }
    
    @Test
    public void testRetractallAlwaysSucceeds() {
        // Test that retractall always succeeds, even with no matches
        List<Map<String, Term>> solutions = prolog.solve("retractall(nonexistent(X))");
        assertFalse("retractall should always succeed", solutions.isEmpty());
    }
    
    @Test
    public void testCycleAssertRetract() {
        // Test assert-retract cycle
        prolog.solve("assertz(cyclic(test))");
        
        List<Map<String, Term>> solutions = prolog.solve("cyclic(test)");
        assertFalse("Should find after assert", solutions.isEmpty());
        
        prolog.solve("retract(cyclic(test))");
        solutions = prolog.solve("cyclic(test)");
        assertTrue("Should not find after retract", solutions.isEmpty());
        
        prolog.solve("assertz(cyclic(test))");
        solutions = prolog.solve("cyclic(test)");
        assertFalse("Should find after re-assert", solutions.isEmpty());
    }
}