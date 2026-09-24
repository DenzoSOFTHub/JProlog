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
    private it.denzosoft.jprolog.core.engine.v4.EngineState.Spies spies;
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
        prolog.setTracing(false);   // ISS-2025-0437: tracing is per ENGINE now
        // ISS-2025-0477: spy points are per ENGINE now (wave W7, the tail of LIM-034), so the
        // assertions below read THIS engine's set rather than a process-global one.
        spies = prolog.getEngineState().spies();
        spies.clear();
    }
    
    @Test
    public void testTrace() {
        // Test enabling trace
        assertFalse("Tracing should be disabled initially", prolog.isTracing());
        
        List<Map<String, Term>> solutions = prolog.solve("trace");
        assertFalse("trace should succeed", solutions.isEmpty());
        assertTrue("Tracing should be enabled after trace", prolog.isTracing());
        assertTrue("Should output trace message", outputStream.toString().contains("Tracing enabled"));
    }
    
    @Test
    public void testNoTrace() {
        // Enable tracing first
        prolog.setTracing(true);    // ISS-2025-0437: tracing is per ENGINE now
        assertTrue("Tracing should be enabled", prolog.isTracing());
        
        outputStream.reset();
        
        List<Map<String, Term>> solutions = prolog.solve("notrace");
        assertFalse("notrace should succeed", solutions.isEmpty());
        assertFalse("Tracing should be disabled after notrace", prolog.isTracing());
        assertTrue("Should output notrace message", outputStream.toString().contains("Tracing disabled"));
    }
    
    @Test
    public void testTraceNoTraceSequence() {
        // Test sequence of trace/notrace
        prolog.solve("trace");
        assertTrue("Tracing should be enabled", prolog.isTracing());
        
        prolog.solve("notrace");
        assertFalse("Tracing should be disabled", prolog.isTracing());
        
        prolog.solve("trace");
        assertTrue("Tracing should be enabled again", prolog.isTracing());
    }
    
    @Test
    public void testSpy() {
        // Test setting spy points
        assertTrue("No spy points should be set initially", spies.snapshot().isEmpty());
        
        List<Map<String, Term>> solutions = prolog.solve("spy(/(member, 2))");
        assertFalse("spy(/(member, 2)) should succeed", solutions.isEmpty());
        
        assertTrue("Spy point should be set", spies.has("member", 2));
        assertTrue("Should output spy message", outputStream.toString().contains("Spy point set on member/2"));
        
        outputStream.reset();
        
        // Test setting another spy point
        solutions = prolog.solve("spy(/(append, 3))");
        assertFalse("spy(/(append, 3)) should succeed", solutions.isEmpty());
        
        assertTrue("Spy point should be set", spies.has("append", 3));
        assertEquals("Should have 2 spy points", 2, spies.snapshot().size());
    }
    
    @Test
    public void testNoSpy() {
        // Set up spy points
        prolog.solve("spy(/(member, 2))");
        prolog.solve("spy(/(append, 3))");
        assertEquals("Should have 2 spy points", 2, spies.snapshot().size());
        
        outputStream.reset();
        
        // Remove one spy point
        List<Map<String, Term>> solutions = prolog.solve("nospy(/(member, 2))");
        assertFalse("nospy(/(member, 2)) should succeed", solutions.isEmpty());
        
        assertFalse("member/2 spy point should be removed", spies.has("member", 2));
        assertTrue("append/3 spy point should remain", spies.has("append", 3));
        assertEquals("Should have 1 spy point", 1, spies.snapshot().size());
        assertTrue("Should output nospy message", outputStream.toString().contains("Spy point removed from member/2"));
    }
    
    @Test
    public void testNoSpyAll() {
        // Set up spy points
        prolog.solve("spy(/(member, 2))");
        prolog.solve("spy(/(append, 3))");
        prolog.solve("spy(/(length, 2))");
        assertEquals("Should have 3 spy points", 3, spies.snapshot().size());
        
        outputStream.reset();
        
        // Remove all spy points with variable argument
        List<Map<String, Term>> solutions = prolog.solve("nospy(X)");
        assertFalse("nospy(X) should succeed", solutions.isEmpty());
        
        assertTrue("All spy points should be removed", spies.snapshot().isEmpty());
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
        
        assertTrue("test/1 spy point should be set", spies.has("test", 1));
        assertTrue("test/2 spy point should be set", spies.has("test", 2));
        assertFalse("test/3 spy point should not be set", spies.has("test", 3));
        
        assertEquals("Should have 2 spy points", 2, spies.snapshot().size());
    }
    
    @Test
    public void testSpyErrorHandling() {
        // Test with invalid predicate indicator
        List<Map<String, Term>> solutions = prolog.solve("spy(invalidformat)");
        assertTrue("spy with invalid format should fail", solutions.isEmpty());
        
        // Test with unbound variable
        // ISS-2025-0797 (4.6 Q7): SWI raises instantiation_error (invariant 65); it failed
        solutions = prolog.solve("catch(spy(X), error(instantiation_error, _), true)");
        assertFalse("spy with unbound variable raises instantiation_error", solutions.isEmpty());
    }
    
    @Test
    public void testDebuggingCombination() {
        // Test combining trace and spy
        List<Map<String, Term>> solutions = prolog.solve("trace, spy(/(member, 2))");
        assertFalse("Combined debugging should succeed", solutions.isEmpty());
        
        assertTrue("Tracing should be enabled", prolog.isTracing());
        assertTrue("Spy point should be set", spies.has("member", 2));
        
        // Turn off debugging
        solutions = prolog.solve("notrace, nospy(X)");
        assertFalse("Combined cleanup should succeed", solutions.isEmpty());
        
        assertFalse("Tracing should be disabled", prolog.isTracing());
        assertTrue("All spy points should be removed", spies.snapshot().isEmpty());
    }
    
    @org.junit.After
    public void tearDown() {
        if (originalOut != null) {
            System.setOut(originalOut);
        }
        // Reset debugging state
        prolog.setTracing(false);   // ISS-2025-0437: tracing is per ENGINE now
        // ISS-2025-0477: spy points are per ENGINE now (wave W7, the tail of LIM-034), so the
        // assertions below read THIS engine's set rather than a process-global one.
        spies = prolog.getEngineState().spies();
        spies.clear();
    }
}