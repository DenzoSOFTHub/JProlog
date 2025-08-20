package it.denzosoft.jprolog;
import it.denzosoft.jprolog.core.engine.Prolog;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import org.junit.Test;
import static org.junit.Assert.*;

import java.util.List;
import java.util.Map;


public class BuiltInTests {

    @Test
    public void testUnificationBuiltIn() {
        Prolog prolog = new Prolog();
        
        // Test basic unification
        List<Map<String, Term>> solutions = prolog.solve("=(a, a).");
        assertEquals(1, solutions.size());
        
        solutions = prolog.solve("=(a, b).");
        assertEquals(0, solutions.size());
        
        // Test variable unification
        solutions = prolog.solve("=(X, a).");
        assertEquals(1, solutions.size());
        assertTrue(solutions.get(0).containsKey("X"));
        assertEquals("a", solutions.get(0).get("X").toString());
    }

    @Test
    public void testUnifyWithOccursCheckBuiltIn() {
        Prolog prolog = new Prolog();
        
        // Test basic unification
        List<Map<String, Term>> solutions = prolog.solve("unify_with_occurs_check(a, a).");
        assertEquals(1, solutions.size());
        
        solutions = prolog.solve("unify_with_occurs_check(a, b).");
        assertEquals(0, solutions.size());
        
        // Test variable unification
        solutions = prolog.solve("unify_with_occurs_check(X, a).");
        assertEquals(1, solutions.size());
        assertTrue(solutions.get(0).containsKey("X"));
        assertEquals("a", solutions.get(0).get("X").toString());
    }

    @Test
    public void testVarBuiltIn() {
        Prolog prolog = new Prolog();
        
        // Test with variable
        List<Map<String, Term>> solutions = prolog.solve("var(X).");
        assertEquals(1, solutions.size());
        
        // Test with atom (should fail)
        solutions = prolog.solve("var(a).");
        assertEquals(0, solutions.size());
    }

    @Test
    public void testNonVarBuiltIn() {
        Prolog prolog = new Prolog();
        
        // Test with atom
        List<Map<String, Term>> solutions = prolog.solve("nonvar(a).");
        assertEquals(1, solutions.size());
        
        // Test with variable (should fail)
        solutions = prolog.solve("nonvar(X).");
        assertEquals(0, solutions.size());
    }

    @Test
    public void testAtomBuiltIn() {
        Prolog prolog = new Prolog();
        
        // Test with atom
        List<Map<String, Term>> solutions = prolog.solve("atom(hello).");
        assertEquals(1, solutions.size());
        
        // Test with number (should fail)
        solutions = prolog.solve("atom(42).");
        assertEquals(0, solutions.size());
    }

    @Test
    public void testIntegerBuiltIn() {
        Prolog prolog = new Prolog();
        
        // Test with integer
        List<Map<String, Term>> solutions = prolog.solve("integer(42).");
        assertEquals(1, solutions.size());
        
        // Test with float (should fail)
        solutions = prolog.solve("integer(3.14).");
        assertEquals(0, solutions.size());
        
        // Test with atom (should fail)
        solutions = prolog.solve("integer(hello).");
        assertEquals(0, solutions.size());
    }

    @Test
    public void testFloatBuiltIn() {
        Prolog prolog = new Prolog();
        
        // Test with float
        List<Map<String, Term>> solutions = prolog.solve("float(3.14).");
        assertEquals(1, solutions.size());
        
        // Test with integer (should fail)
        solutions = prolog.solve("float(42).");
        assertEquals(0, solutions.size());
        
        // Test with atom (should fail)
        solutions = prolog.solve("float(hello).");
        assertEquals(0, solutions.size());
    }

    @Test
    public void testAtomicBuiltIn() {
        Prolog prolog = new Prolog();
        
        // Test with atom
        List<Map<String, Term>> solutions = prolog.solve("atomic(hello).");
        assertEquals(1, solutions.size());
        
        // Test with number
        solutions = prolog.solve("atomic(42).");
        assertEquals(1, solutions.size());
        
        // Test with compound term (should fail)
        solutions = prolog.solve("atomic(f(a)).");
        assertEquals(0, solutions.size());
    }

    @Test
    public void testCompoundBuiltIn() {
        Prolog prolog = new Prolog();
        
        // Test with compound term
        List<Map<String, Term>> solutions = prolog.solve("compound(f(a)).");
        assertEquals(1, solutions.size());
        
        // Test with atom (should fail)
        solutions = prolog.solve("compound(hello).");
        assertEquals(0, solutions.size());
    }

    @Test
    public void testNumberBuiltIn() {
        Prolog prolog = new Prolog();
        
        // Test with integer
        List<Map<String, Term>> solutions = prolog.solve("number(42).");
        assertEquals(1, solutions.size());
        
        // Test with float
        solutions = prolog.solve("number(3.14).");
        assertEquals(1, solutions.size());
        
        // Test with atom (should fail)
        solutions = prolog.solve("number(hello).");
        assertEquals(0, solutions.size());
    }
}
