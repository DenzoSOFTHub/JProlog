package it.denzosoft.jprolog.test.builtin;
import it.denzosoft.jprolog.core.engine.Prolog;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Test;
import static org.junit.Assert.*;

import java.util.List;
import java.util.Map;

public class BuiltInPredicatesTest {

    @Test
    public void testUnification() {
        Prolog prolog = new Prolog();
        
        try {
            // Test basic unification
            List<Map<String, Term>> solutions = prolog.solve("X = 5.");
            assertEquals(1, solutions.size());
            assertEquals("5.0", solutions.get(0).get("X").toString());
            
            // Test atom unification
            solutions = prolog.solve("X = hello.");
            assertEquals(1, solutions.size());
            assertEquals("hello", solutions.get(0).get("X").toString());
            
            // Test failed unification
            solutions = prolog.solve("5 = 6.");
            assertEquals(0, solutions.size());
        } catch (Exception e) {
            fail("Unification should work: " + e.getMessage());
        }
    }

    @Test
    public void testTypeChecking() {
        Prolog prolog = new Prolog();
        
        try {
            // Test var/1
            List<Map<String, Term>> solutions = prolog.solve("var(X).");
            assertEquals(1, solutions.size());
            
            solutions = prolog.solve("var(hello).");
            assertEquals(0, solutions.size());
            
            // Test nonvar/1
            solutions = prolog.solve("nonvar(hello).");
            assertEquals(1, solutions.size());
            
            solutions = prolog.solve("nonvar(X).");
            assertEquals(0, solutions.size());
            
            // Test atom/1
            solutions = prolog.solve("atom(hello).");
            assertEquals(1, solutions.size());
            
            solutions = prolog.solve("atom(42).");
            assertEquals(0, solutions.size());
            
            // Test number/1
            solutions = prolog.solve("number(42).");
            assertEquals(1, solutions.size());
            
            solutions = prolog.solve("number(hello).");
            assertEquals(0, solutions.size());
        } catch (Exception e) {
            fail("Type checking predicates should work: " + e.getMessage());
        }
    }

    @Test
    public void testIntegerAndFloat() {
        Prolog prolog = new Prolog();
        
        try {
            // Test integer/1
            List<Map<String, Term>> solutions = prolog.solve("integer(42).");
            assertEquals(1, solutions.size());
            
            solutions = prolog.solve("integer(3.14).");
            assertEquals(0, solutions.size());
            
            // Test float/1
            solutions = prolog.solve("float(3.14).");
            assertEquals(1, solutions.size());
            
            solutions = prolog.solve("float(42).");
            assertEquals(0, solutions.size());
        } catch (Exception e) {
            fail("Integer and float checking should work: " + e.getMessage());
        }
    }

    @Test
    public void testCompoundAndAtomic() {
        Prolog prolog = new Prolog();
        
        try {
            // Test compound/1
            List<Map<String, Term>> solutions = prolog.solve("compound(f(a)).");
            assertEquals(1, solutions.size());
            
            solutions = prolog.solve("compound(hello).");
            assertEquals(0, solutions.size());
            
            // Test atomic/1
            solutions = prolog.solve("atomic(hello).");
            assertEquals(1, solutions.size());
            
            solutions = prolog.solve("atomic(42).");
            assertEquals(1, solutions.size());
            
            solutions = prolog.solve("atomic(f(a)).");
            assertEquals(0, solutions.size());
        } catch (Exception e) {
            fail("Compound and atomic checking should work: " + e.getMessage());
        }
    }

    @Test
    public void testTermConstruction() {
        Prolog prolog = new Prolog();
        
        try {
            // Test functor/3
            List<Map<String, Term>> solutions = prolog.solve("functor(f(a,b), F, A).");
            assertEquals(1, solutions.size());
            assertEquals("f", solutions.get(0).get("F").toString());
            assertEquals("2.0", solutions.get(0).get("A").toString());
            
            // Test arg/3
            solutions = prolog.solve("arg(1, f(a,b,c), X).");
            assertEquals(1, solutions.size());
            assertEquals("a", solutions.get(0).get("X").toString());
            
            solutions = prolog.solve("arg(2, f(a,b,c), X).");
            assertEquals(1, solutions.size());
            assertEquals("b", solutions.get(0).get("X").toString());
        } catch (Exception e) {
            fail("Term construction predicates should work: " + e.getMessage());
        }
    }

    @Test
    public void testTermComparison() {
        Prolog prolog = new Prolog();
        
        try {
            // Test @<
            List<Map<String, Term>> solutions = prolog.solve("a @< b.");
            assertEquals(1, solutions.size());
            
            solutions = prolog.solve("b @< a.");
            assertEquals(0, solutions.size());
            
            // Test ==
            solutions = prolog.solve("f(a,b) == f(a,b).");
            assertEquals(1, solutions.size());
            
            solutions = prolog.solve("f(a,b) == f(b,a).");
            assertEquals(0, solutions.size());
            
            // Test \\==
            solutions = prolog.solve("f(a,b) \\== f(b,a).");
            assertEquals(1, solutions.size());
            
            solutions = prolog.solve("f(a,b) \\== f(a,b).");
            assertEquals(0, solutions.size());
        } catch (Exception e) {
            fail("Term comparison should work: " + e.getMessage());
        }
    }

    @Test
    public void testListOperations() {
        Prolog prolog = new Prolog();
        
        try {
            // Test append/3
            List<Map<String, Term>> solutions = prolog.solve("append([a,b], [c,d], X).");
            assertEquals(1, solutions.size());
            // Result should be [a,b,c,d] in dot notation
            String result = solutions.get(0).get("X").toString();
            assertTrue(result.contains("a") && result.contains("b") && result.contains("c") && result.contains("d"));
            
            // Test member/2
            solutions = prolog.solve("member(b, [a,b,c]).");
            assertEquals(1, solutions.size());
            
            solutions = prolog.solve("member(d, [a,b,c]).");
            assertEquals(0, solutions.size());
            
            // Test length/2
            solutions = prolog.solve("length([a,b,c], N).");
            assertEquals(1, solutions.size());
            assertEquals("3.0", solutions.get(0).get("N").toString());
        } catch (Exception e) {
            fail("List operations should work: " + e.getMessage());
        }
    }

    @Test
    public void testConversionPredicates() {
        Prolog prolog = new Prolog();
        
        try {
            // Test atom_number/2
            List<Map<String, Term>> solutions = prolog.solve("atom_number('123', N).");
            assertEquals(1, solutions.size());
            assertEquals("123.0", solutions.get(0).get("N").toString());
            
            // Test atom_chars/2
            solutions = prolog.solve("atom_chars(hello, L).");
            assertEquals(1, solutions.size());
            String result = solutions.get(0).get("L").toString();
            assertTrue(result.contains("h") && result.contains("e") && result.contains("l") && result.contains("o"));
            
            // Test number_chars/2
            solutions = prolog.solve("number_chars(123, L).");
            assertEquals(1, solutions.size());
            result = solutions.get(0).get("L").toString();
            assertTrue(result.contains("1") && result.contains("2") && result.contains("3"));
        } catch (Exception e) {
            fail("Conversion predicates should work: " + e.getMessage());
        }
    }

    @Test
    public void testFindall() {
        Prolog prolog = new Prolog();
        
        try {
            // Setup facts
            prolog.consult("likes(mary, food).");
            prolog.consult("likes(mary, wine).");
            prolog.consult("likes(john, wine).");
            prolog.consult("likes(john, mary).");
            
            // Test findall/3
            List<Map<String, Term>> solutions = prolog.solve("findall(X, likes(mary, X), L).");
            assertEquals(1, solutions.size());
            String result = solutions.get(0).get("L").toString();
            assertTrue(result.contains("food") && result.contains("wine"));
            
            // Test findall with different template
            solutions = prolog.solve("findall(Person, likes(Person, wine), L).");
            assertEquals(1, solutions.size());
            result = solutions.get(0).get("L").toString();
            assertTrue(result.contains("mary") && result.contains("john"));
        } catch (Exception e) {
            fail("Findall should work: " + e.getMessage());
        }
    }

    @Test
    public void testDynamicPredicates() {
        Prolog prolog = new Prolog();
        
        try {
            // Test assert and retract
            prolog.asserta("dynamic_fact(test).");
            
            List<Map<String, Term>> solutions = prolog.solve("dynamic_fact(test).");
            assertEquals(1, solutions.size());
            
            prolog.retract("dynamic_fact(test).");
            
            solutions = prolog.solve("dynamic_fact(test).");
            assertEquals(0, solutions.size());
        } catch (Exception e) {
            fail("Dynamic predicates should work: " + e.getMessage());
        }
    }
}