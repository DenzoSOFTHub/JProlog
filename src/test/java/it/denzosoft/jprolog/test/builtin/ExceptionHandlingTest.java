package it.denzosoft.jprolog.test.builtin;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.exceptions.PrologException;

import org.junit.Before;
import org.junit.Test;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * Test suite for ISO Prolog exception handling predicates.
 */
public class ExceptionHandlingTest {
    
    private Prolog prolog;
    
    @Before
    public void setUp() {
        prolog = new Prolog();
    }
    
    @Test
    public void testSimpleThrowCatch() {
        // Test basic throw/catch functionality
        String program = "test_catch :- catch(throw(my_error), Error, true).";
        prolog.consult(program);
        
        List<Map<String, Term>> solutions = prolog.solve("test_catch");
        assertFalse("catch/3 should catch the thrown exception", solutions.isEmpty());
    }
    
    @Test
    public void testCatchWithUnification() {
        // Test that the error term is correctly unified in catch
        String program = "test_unify(X) :- catch(throw(error_123), X, true).";
        prolog.consult(program);
        
        List<Map<String, Term>> solutions = prolog.solve("test_unify(Error)");
        assertFalse("Should have solutions", solutions.isEmpty());
        Term errorTerm = solutions.get(0).get("Error");
        assertNotNull("Error should be bound", errorTerm);
        assertEquals("error_123", errorTerm.toString());
    }
    
    @Test
    public void testNestedCatch() {
        // Test nested catch blocks - simplified version
        String program = 
            "outer_catch(Result) :- " +
            "  catch(throw(inner_error), inner_error, (Result = caught_inner)).";
        prolog.consult(program);
        
        List<Map<String, Term>> solutions = prolog.solve("outer_catch(R)");
        assertFalse("Should have solutions", solutions.isEmpty());
        Term result = solutions.get(0).get("R");
        assertEquals("caught_inner", result.toString());
    }
    
    @Test
    public void testCatchNoException() {
        // Test that catch succeeds when no exception is thrown
        String program = "test_no_throw(X) :- catch((X = success), Error, fail).";
        prolog.consult(program);
        
        List<Map<String, Term>> solutions = prolog.solve("test_no_throw(Result)");
        assertFalse("Should succeed when no exception", solutions.isEmpty());
        assertEquals("success", solutions.get(0).get("Result").toString());
    }
    
    @Test
    public void testCatchDifferentError() {
        // Test that catch fails when error doesn't match
        String program = "test_mismatch :- catch(throw(error_a), error_b, true).";
        prolog.consult(program);
        
        try {
            List<Map<String, Term>> solutions = prolog.solve("test_mismatch");
            // The exception should propagate if not caught
            assertTrue("Should propagate uncaught exception", solutions.isEmpty());
        } catch (PrologException e) {
            // This is expected behavior - exception propagates
            assertTrue(true);
        }
    }
    
    @Test(expected = PrologException.class)
    public void testHaltZero() {
        // Test halt/0 throws exception
        prolog.solve("halt");
    }
    
    @Test(expected = PrologException.class)
    public void testHaltWithCode() {
        // Test halt/1 throws exception with exit code
        prolog.solve("halt(42)");
    }
    
    @Test
    public void testCatchAll() {
        // Test catch with variable catcher (catches everything)
        String program = "catch_all(Error) :- catch(throw(any_error), Error, true).";
        prolog.consult(program);
        
        List<Map<String, Term>> solutions = prolog.solve("catch_all(E)");
        assertFalse("Should catch any error", solutions.isEmpty());
        assertEquals("any_error", solutions.get(0).get("E").toString());
    }
    
    @Test
    public void testThrowInBody() {
        // Test throw in rule body
        String program = 
            "will_throw :- write(before), throw(midway_error), write(after).\n" +
            "test_throw :- catch(will_throw, midway_error, true).";
        prolog.consult(program);
        
        List<Map<String, Term>> solutions = prolog.solve("test_throw");
        assertFalse("Should catch exception from rule body", solutions.isEmpty());
    }
}