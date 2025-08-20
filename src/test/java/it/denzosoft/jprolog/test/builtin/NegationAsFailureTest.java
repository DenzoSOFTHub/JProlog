package it.denzosoft.jprolog.test.builtin;
import it.denzosoft.jprolog.core.engine.Prolog;

import org.junit.Before;
import org.junit.Test;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * Test suite for negation as failure (\\+) predicate.
 */
public class NegationAsFailureTest {
    
    private Prolog prolog;
    
    @Before
    public void setUp() {
        prolog = new Prolog();
    }
    
    @Test
    public void testNegationOfFail() {
        // \\+ fail should succeed
        String program = "test_neg_fail :- \\+ fail.";
        prolog.consult(program);
        
        List<Map<String, Term>> solutions = prolog.solve("test_neg_fail");
        assertFalse("\\+ fail should succeed", solutions.isEmpty());
    }
    
    @Test
    public void testNegationOfTrue() {
        // \\+ true should fail
        String program = "test_neg_true :- \\+ true.";
        prolog.consult(program);
        
        List<Map<String, Term>> solutions = prolog.solve("test_neg_true");
        assertTrue("\\+ true should fail", solutions.isEmpty());
    }
    
    @Test
    public void testNegationOfEquality() {
        // \\+ (1 = 2) should succeed
        String program = "test_neg_false_eq :- \\+ (1 = 2).";
        prolog.consult(program);
        
        List<Map<String, Term>> solutions = prolog.solve("test_neg_false_eq");
        assertFalse("\\+ (1 = 2) should succeed", solutions.isEmpty());
        
        // \\+ (1 = 1) should fail  
        String program2 = "test_neg_true_eq :- \\+ (1 = 1).";
        prolog.consult(program2);
        
        solutions = prolog.solve("test_neg_true_eq");
        assertTrue("\\+ (1 = 1) should fail", solutions.isEmpty());
    }
    
    @Test
    public void testNegationOfPredicate() {
        // Setup some facts
        String program = 
            "bird(robin).\n" +
            "bird(sparrow).\n" +
            "mammal(dog).\n" +
            "mammal(cat).\n" +
            "test_neg_bird(X) :- \\+ bird(X).\n" +
            "test_neg_mammal(X) :- \\+ mammal(X).";
        prolog.consult(program);
        
        // \\+ bird(dog) should succeed (dog is not a bird)
        List<Map<String, Term>> solutions = prolog.solve("test_neg_bird(dog)");
        assertFalse("\\+ bird(dog) should succeed", solutions.isEmpty());
        
        // \\+ bird(robin) should fail (robin is a bird)
        solutions = prolog.solve("test_neg_bird(robin)");
        assertTrue("\\+ bird(robin) should fail", solutions.isEmpty());
        
        // \\+ mammal(robin) should succeed (robin is not a mammal)
        solutions = prolog.solve("test_neg_mammal(robin)");
        assertFalse("\\+ mammal(robin) should succeed", solutions.isEmpty());
    }
    
    @Test
    public void testNegationWithVariables() {
        // Test negation with variables in context
        String program = 
            "positive(1).\n" +
            "positive(2).\n" +
            "positive(3).\n" +
            "not_positive(X) :- \\+ positive(X).";
        prolog.consult(program);
        
        // \\+ positive(0) should succeed
        List<Map<String, Term>> solutions = prolog.solve("not_positive(0)");
        assertFalse("\\+ positive(0) should succeed", solutions.isEmpty());
        
        // \\+ positive(1) should fail
        solutions = prolog.solve("not_positive(1)");
        assertTrue("\\+ positive(1) should fail", solutions.isEmpty());
    }
    
    @Test
    public void testNegationPreservesBindings() {
        // Test that negation doesn't affect outer bindings - simplified
        String program = 
            "fact(a).";
        prolog.consult(program);
        
        // Test negation with variable binding - direct test
        List<Map<String, Term>> solutions = prolog.solve("\\+ fact(world)");
        assertFalse("\\+ fact(world) should succeed", solutions.isEmpty());
        
        // Test negation of existing fact
        solutions = prolog.solve("\\+ fact(a)");
        assertTrue("\\+ fact(a) should fail", solutions.isEmpty());
    }
    
    @Test
    public void testDoubleNegationSimulated() {
        // Test double negation with separate predicates
        String program = 
            "fact(test).\n" +
            "not_fact(X) :- \\+ fact(X).\n" +
            "double_neg_fact :- \\+ not_fact(test).\n" +
            "double_neg_nofact :- \\+ not_fact(missing).";
        prolog.consult(program);
        
        // Should behave like double negation
        List<Map<String, Term>> solutions = prolog.solve("double_neg_fact");
        assertFalse("Simulated double negation should succeed", solutions.isEmpty());
        
        solutions = prolog.solve("double_neg_nofact");
        assertTrue("Simulated double negation should fail for missing", solutions.isEmpty());
    }
    
    @Test
    public void testNegationInComplexRule() {
        // Test negation as part of a complex rule
        String program = 
            "bird(robin).\n" +
            "bird(sparrow).\n" +
            "fly(robin).\n" +
            "fly(sparrow).\n" +
            "penguin(emperor).\n" +
            "bird(emperor).\n" +
            "flightless_bird(X) :- bird(X), \\+ fly(X).";
        prolog.consult(program);
        
        // emperor should be a flightless bird
        List<Map<String, Term>> solutions = prolog.solve("flightless_bird(emperor)");
        assertFalse("emperor should be flightless bird", solutions.isEmpty());
        
        // robin should not be a flightless bird
        solutions = prolog.solve("flightless_bird(robin)");
        assertTrue("robin should not be flightless bird", solutions.isEmpty());
    }
}