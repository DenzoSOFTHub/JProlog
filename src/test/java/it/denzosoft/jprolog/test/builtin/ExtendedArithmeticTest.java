package it.denzosoft.jprolog.test.builtin;
import it.denzosoft.jprolog.core.engine.Prolog;

import org.junit.Before;
import org.junit.Test;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * Test suite for extended arithmetic operators (mod, **).
 */
public class ExtendedArithmeticTest {
    
    private Prolog prolog;
    
    @Before
    public void setUp() {
        prolog = new Prolog();
    }
    
    @Test
    public void testModuloFunctionCall() {
        // Test mod as function call
        List<Map<String, Term>> solutions = prolog.solve("X is mod(7, 3)");
        assertFalse("mod(7, 3) should succeed", solutions.isEmpty());
        assertEquals("1.0", solutions.get(0).get("X").toString());
        
        solutions = prolog.solve("X is mod(10, 4)");
        assertFalse("mod(10, 4) should succeed", solutions.isEmpty());
        assertEquals("2.0", solutions.get(0).get("X").toString());
        
        solutions = prolog.solve("X is mod(15, 6)");
        assertFalse("mod(15, 6) should succeed", solutions.isEmpty());
        assertEquals("3.0", solutions.get(0).get("X").toString());
    }
    
    @Test
    public void testModuloInfixOperator() {
        // Test mod as infix operator
        List<Map<String, Term>> solutions = prolog.solve("X is 7 mod 3");
        assertFalse("7 mod 3 should succeed", solutions.isEmpty());
        assertEquals("1.0", solutions.get(0).get("X").toString());
        
        solutions = prolog.solve("X is 10 mod 4");
        assertFalse("10 mod 4 should succeed", solutions.isEmpty());
        assertEquals("2.0", solutions.get(0).get("X").toString());
        
        solutions = prolog.solve("X is 15 mod 6");
        assertFalse("15 mod 6 should succeed", solutions.isEmpty());
        assertEquals("3.0", solutions.get(0).get("X").toString());
    }
    
    @Test
    public void testPowerFunctionCall() {
        // Test ** as function call
        List<Map<String, Term>> solutions = prolog.solve("X is **(2, 3)");
        assertFalse("**(2, 3) should succeed", solutions.isEmpty());
        assertEquals("8.0", solutions.get(0).get("X").toString());
        
        solutions = prolog.solve("X is **(3, 2)");
        assertFalse("**(3, 2) should succeed", solutions.isEmpty());
        assertEquals("9.0", solutions.get(0).get("X").toString());
        
        solutions = prolog.solve("X is **(2, 4)");
        assertFalse("**(2, 4) should succeed", solutions.isEmpty());
        assertEquals("16.0", solutions.get(0).get("X").toString());
    }
    
    @Test
    public void testPowerInfixOperator() {
        // Test ** as infix operator
        List<Map<String, Term>> solutions = prolog.solve("X is 2 ** 3");
        assertFalse("2 ** 3 should succeed", solutions.isEmpty());
        assertEquals("8.0", solutions.get(0).get("X").toString());
        
        solutions = prolog.solve("X is 3 ** 2");
        assertFalse("3 ** 2 should succeed", solutions.isEmpty());
        assertEquals("9.0", solutions.get(0).get("X").toString());
        
        solutions = prolog.solve("X is 2 ** 4");
        assertFalse("2 ** 4 should succeed", solutions.isEmpty());
        assertEquals("16.0", solutions.get(0).get("X").toString());
    }
    
    @Test
    public void testArithmeticPrecedence() {
        // Test that ** has higher precedence than *
        List<Map<String, Term>> solutions = prolog.solve("X is 2 * 3 ** 2");
        assertFalse("2 * 3 ** 2 should succeed", solutions.isEmpty());
        assertEquals("18.0", solutions.get(0).get("X").toString()); // Should be 2 * (3 ** 2) = 2 * 9 = 18
        
        // Test that mod has same precedence as *
        solutions = prolog.solve("X is 10 * 3 mod 7");
        assertFalse("10 * 3 mod 7 should succeed", solutions.isEmpty());
        assertEquals("2.0", solutions.get(0).get("X").toString()); // Should be (10 * 3) mod 7 = 30 mod 7 = 2
    }
    
    @Test
    public void testComplexArithmeticExpressions() {
        // Test complex expressions with new operators
        List<Map<String, Term>> solutions = prolog.solve("X is 2 ** 3 + 3 * 4 mod 5");
        assertFalse("Complex expression should succeed", solutions.isEmpty());
        assertEquals("10.0", solutions.get(0).get("X").toString()); // 2**3 + 3*4 mod 5 = 8 + 12 mod 5 = 8 + 2 = 10
        
        solutions = prolog.solve("X is (7 + 3) mod 4 * 2");
        assertFalse("Complex expression with parentheses should succeed", solutions.isEmpty());
        assertEquals("4.0", solutions.get(0).get("X").toString()); // (7 + 3) mod 4 * 2 = 10 mod 4 * 2 = 2 * 2 = 4
    }
    
    @Test
    public void testNegativeNumbers() {
        // Test modulo with negative numbers
        List<Map<String, Term>> solutions = prolog.solve("X is -7 mod 3");
        assertFalse("-7 mod 3 should succeed", solutions.isEmpty());
        assertEquals("-1.0", solutions.get(0).get("X").toString());
        
        // Test power with negative base
        solutions = prolog.solve("X is -2 ** 2");
        assertFalse("-2 ** 2 should succeed", solutions.isEmpty());
        assertEquals("4.0", solutions.get(0).get("X").toString());
    }
    
    @Test
    public void testFractionalNumbers() {
        // Test modulo with fractional numbers
        List<Map<String, Term>> solutions = prolog.solve("X is 7.5 mod 2.5");
        assertFalse("7.5 mod 2.5 should succeed", solutions.isEmpty());
        assertEquals("0.0", solutions.get(0).get("X").toString());
        
        // Test power with fractional exponent
        solutions = prolog.solve("X is 4 ** 0.5");
        assertFalse("4 ** 0.5 should succeed", solutions.isEmpty());
        assertEquals("2.0", solutions.get(0).get("X").toString());
    }
}