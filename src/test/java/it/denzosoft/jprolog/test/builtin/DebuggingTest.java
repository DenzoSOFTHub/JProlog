package it.denzosoft.jprolog.test.builtin;
import it.denzosoft.jprolog.core.engine.Prolog;

import org.junit.Before;
import org.junit.Test;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.builtin.debug.Trace;
import it.denzosoft.jprolog.builtin.debug.Spy;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * Test suite for ISO Prolog debugging predicates.
 */
public class DebuggingTest {
    
    private Prolog prolog;
    private ByteArrayOutputStream outputStream;
    private PrintStream originalOut;
    
    @Before
    public void setUp() {
        prolog = new Prolog();
        // Capture output for testing
        outputStream = new ByteArrayOutputStream();
        originalOut = System.out;
        System.setOut(new PrintStream(outputStream));
        
        // Reset debugging state
        Trace.setTracingEnabled(false);
        Spy.clearAllSpyPoints();
    }
    
    @Test
    public void testTrace() {
        // Test enabling trace
        assertFalse("Tracing should be disabled initially", Trace.isTracingEnabled());
        
        List<Map<String, Term>> solutions = prolog.solve("trace");
        assertFalse("trace should succeed", solutions.isEmpty());
        assertTrue("Tracing should be enabled after trace", Trace.isTracingEnabled());
        assertTrue("Should output trace message", outputStream.toString().contains("Tracing enabled"));
    }
    
    @Test
    public void testNoTrace() {
        // Enable tracing first
        Trace.setTracingEnabled(true);
        assertTrue("Tracing should be enabled", Trace.isTracingEnabled());
        
        outputStream.reset();
        
        List<Map<String, Term>> solutions = prolog.solve("notrace");
        assertFalse("notrace should succeed", solutions.isEmpty());
        assertFalse("Tracing should be disabled after notrace", Trace.isTracingEnabled());
        assertTrue("Should output notrace message", outputStream.toString().contains("Tracing disabled"));
    }
    
    @Test
    public void testTraceNoTraceSequence() {
        // Test sequence of trace/notrace
        prolog.solve("trace");
        assertTrue("Tracing should be enabled", Trace.isTracingEnabled());
        
        prolog.solve("notrace");
        assertFalse("Tracing should be disabled", Trace.isTracingEnabled());
        
        prolog.solve("trace");
        assertTrue("Tracing should be enabled again", Trace.isTracingEnabled());
    }
    
    @Test
    public void testSpy() {
        // Test setting spy points
        assertTrue("No spy points should be set initially", Spy.getSpyPoints().isEmpty());
        
        List<Map<String, Term>> solutions = prolog.solve("spy(/(member, 2))");
        assertFalse("spy(/(member, 2)) should succeed", solutions.isEmpty());
        
        assertTrue("Spy point should be set", Spy.hasSpyPoint("member", 2));
        assertTrue("Should output spy message", outputStream.toString().contains("Spy point set on member/2"));
        
        outputStream.reset();
        
        // Test setting another spy point
        solutions = prolog.solve("spy(/(append, 3))");
        assertFalse("spy(/(append, 3)) should succeed", solutions.isEmpty());
        
        assertTrue("Spy point should be set", Spy.hasSpyPoint("append", 3));
        assertEquals("Should have 2 spy points", 2, Spy.getSpyPoints().size());
    }
    
    @Test
    public void testNoSpy() {
        // Set up spy points
        prolog.solve("spy(/(member, 2))");
        prolog.solve("spy(/(append, 3))");
        assertEquals("Should have 2 spy points", 2, Spy.getSpyPoints().size());
        
        outputStream.reset();
        
        // Remove one spy point
        List<Map<String, Term>> solutions = prolog.solve("nospy(/(member, 2))");
        assertFalse("nospy(/(member, 2)) should succeed", solutions.isEmpty());
        
        assertFalse("member/2 spy point should be removed", Spy.hasSpyPoint("member", 2));
        assertTrue("append/3 spy point should remain", Spy.hasSpyPoint("append", 3));
        assertEquals("Should have 1 spy point", 1, Spy.getSpyPoints().size());
        assertTrue("Should output nospy message", outputStream.toString().contains("Spy point removed from member/2"));
    }
    
    @Test
    public void testNoSpyAll() {
        // Set up spy points
        prolog.solve("spy(/(member, 2))");
        prolog.solve("spy(/(append, 3))");
        prolog.solve("spy(/(length, 2))");
        assertEquals("Should have 3 spy points", 3, Spy.getSpyPoints().size());
        
        outputStream.reset();
        
        // Remove all spy points with variable argument
        List<Map<String, Term>> solutions = prolog.solve("nospy(X)");
        assertFalse("nospy(X) should succeed", solutions.isEmpty());
        
        assertTrue("All spy points should be removed", Spy.getSpyPoints().isEmpty());
        assertTrue("Should output all spy points removed message", 
                  outputStream.toString().contains("All spy points removed"));
    }
    
    @Test
    public void testSpyWithDifferentArities() {
        // Test spy points with different arities of the same predicate
        List<Map<String, Term>> solutions = prolog.solve("spy(/(test, 1))");
        assertFalse("spy(/(test, 1)) should succeed", solutions.isEmpty());
        
        solutions = prolog.solve("spy(/(test, 2))");
        assertFalse("spy(/(test, 2)) should succeed", solutions.isEmpty());
        
        assertTrue("test/1 spy point should be set", Spy.hasSpyPoint("test", 1));
        assertTrue("test/2 spy point should be set", Spy.hasSpyPoint("test", 2));
        assertFalse("test/3 spy point should not be set", Spy.hasSpyPoint("test", 3));
        
        assertEquals("Should have 2 spy points", 2, Spy.getSpyPoints().size());
    }
    
    @Test
    public void testSpyErrorHandling() {
        // Test with invalid predicate indicator
        List<Map<String, Term>> solutions = prolog.solve("spy(invalidformat)");
        assertTrue("spy with invalid format should fail", solutions.isEmpty());
        
        // Test with unbound variable
        solutions = prolog.solve("spy(X)");
        assertTrue("spy with unbound variable should fail", solutions.isEmpty());
    }
    
    @Test
    public void testDebuggingCombination() {
        // Test combining trace and spy
        List<Map<String, Term>> solutions = prolog.solve("trace, spy(/(member, 2))");
        assertFalse("Combined debugging should succeed", solutions.isEmpty());
        
        assertTrue("Tracing should be enabled", Trace.isTracingEnabled());
        assertTrue("Spy point should be set", Spy.hasSpyPoint("member", 2));
        
        // Turn off debugging
        solutions = prolog.solve("notrace, nospy(X)");
        assertFalse("Combined cleanup should succeed", solutions.isEmpty());
        
        assertFalse("Tracing should be disabled", Trace.isTracingEnabled());
        assertTrue("All spy points should be removed", Spy.getSpyPoints().isEmpty());
    }
    
    @org.junit.After
    public void tearDown() {
        if (originalOut != null) {
            System.setOut(originalOut);
        }
        // Reset debugging state
        Trace.setTracingEnabled(false);
        Spy.clearAllSpyPoints();
    }
}