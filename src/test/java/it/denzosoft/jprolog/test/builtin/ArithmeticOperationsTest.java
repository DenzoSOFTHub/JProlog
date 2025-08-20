package it.denzosoft.jprolog.test.builtin;
import it.denzosoft.jprolog.core.engine.Prolog;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Test;
import static org.junit.Assert.*;

import java.util.List;
import java.util.Map;

public class ArithmeticOperationsTest {

    @Test
    public void testBasicArithmetic() {
        Prolog prolog = new Prolog();
        
        // Test addition
        try {
            List<Map<String, Term>> solutions = prolog.solve("X is 5 + 3.");
            assertEquals(1, solutions.size());
            Term result = solutions.get(0).get("X");
            assertEquals("8.0", result.toString());
        } catch (Exception e) {
            fail("Basic addition should work: " + e.getMessage());
        }
    }

    @Test
    public void testArithmeticOperations() {
        Prolog prolog = new Prolog();
        
        try {
            // Subtraction
            List<Map<String, Term>> solutions = prolog.solve("X is 10 - 3.");
            assertEquals(1, solutions.size());
            assertEquals("7.0", solutions.get(0).get("X").toString());
            
            // Multiplication
            solutions = prolog.solve("X is 4 * 5.");
            assertEquals(1, solutions.size());
            assertEquals("20.0", solutions.get(0).get("X").toString());
            
            // Division
            solutions = prolog.solve("X is 15 / 3.");
            assertEquals(1, solutions.size());
            assertEquals("5.0", solutions.get(0).get("X").toString());
        } catch (Exception e) {
            fail("Basic arithmetic operations should work: " + e.getMessage());
        }
    }

    @Test
    public void testArithmeticPrecedence() {
        Prolog prolog = new Prolog();
        
        try {
            // Test precedence: 2 + 3 * 4 should be 2 + 12 = 14
            List<Map<String, Term>> solutions = prolog.solve("X is 2 + 3 * 4.");
            assertEquals(1, solutions.size());
            assertEquals("14.0", solutions.get(0).get("X").toString());
            
            // Test with parentheses: (2 + 3) * 4 should be 5 * 4 = 20
            solutions = prolog.solve("X is (2 + 3) * 4.");
            assertEquals(1, solutions.size());
            assertEquals("20.0", solutions.get(0).get("X").toString());
        } catch (Exception e) {
            fail("Arithmetic precedence should work: " + e.getMessage());
        }
    }

    @Test
    public void testArithmeticComparisons() {
        Prolog prolog = new Prolog();
        
        try {
            // Test =:=
            List<Map<String, Term>> solutions = prolog.solve("3 + 2 =:= 5.");
            assertEquals(1, solutions.size());
            
            solutions = prolog.solve("3 + 2 =:= 6.");
            assertEquals(0, solutions.size());
            
            // Test =\\=
            solutions = prolog.solve("3 + 2 =\\= 6.");
            assertEquals(1, solutions.size());
            
            solutions = prolog.solve("3 + 2 =\\= 5.");
            assertEquals(0, solutions.size());
        } catch (Exception e) {
            fail("Arithmetic comparisons should work: " + e.getMessage());
        }
    }

    @Test
    public void testRelationalOperators() {
        Prolog prolog = new Prolog();
        
        try {
            // Test >
            List<Map<String, Term>> solutions = prolog.solve("5 > 3.");
            assertEquals(1, solutions.size());
            
            solutions = prolog.solve("3 > 5.");
            assertEquals(0, solutions.size());
            
            // Test <
            solutions = prolog.solve("3 < 5.");
            assertEquals(1, solutions.size());
            
            solutions = prolog.solve("5 < 3.");
            assertEquals(0, solutions.size());
            
            // Test >=
            solutions = prolog.solve("5 >= 5.");
            assertEquals(1, solutions.size());
            
            solutions = prolog.solve("4 >= 5.");
            assertEquals(0, solutions.size());
            
            // Test =<
            solutions = prolog.solve("5 =< 5.");
            assertEquals(1, solutions.size());
            
            solutions = prolog.solve("6 =< 5.");
            assertEquals(0, solutions.size());
        } catch (Exception e) {
            fail("Relational operators should work: " + e.getMessage());
        }
    }

    @Test
    public void testNegativeNumbers() {
        Prolog prolog = new Prolog();
        
        try {
            List<Map<String, Term>> solutions = prolog.solve("X is -5 + 3.");
            assertEquals(1, solutions.size());
            assertEquals("-2.0", solutions.get(0).get("X").toString());
            
            solutions = prolog.solve("X is 5 + (-3).");
            assertEquals(1, solutions.size());
            assertEquals("2.0", solutions.get(0).get("X").toString());
        } catch (Exception e) {
            fail("Negative numbers should work: " + e.getMessage());
        }
    }

    @Test
    public void testComplexExpressions() {
        Prolog prolog = new Prolog();
        
        try {
            // Test nested expressions
            List<Map<String, Term>> solutions = prolog.solve("X is (10 + 5) / (2 + 1).");
            assertEquals(1, solutions.size());
            assertEquals("5.0", solutions.get(0).get("X").toString());
            
            // Test multiple operations
            solutions = prolog.solve("X is 2 * 3 + 4 * 5.");
            assertEquals(1, solutions.size());
            assertEquals("26.0", solutions.get(0).get("X").toString());
        } catch (Exception e) {
            fail("Complex expressions should work: " + e.getMessage());
        }
    }
}