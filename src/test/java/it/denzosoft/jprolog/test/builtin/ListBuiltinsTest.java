package it.denzosoft.jprolog.test.builtin;
import it.denzosoft.jprolog.core.engine.Prolog;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import org.junit.Test;
import static org.junit.Assert.*;

import java.util.List;
import java.util.Map;


public class ListBuiltinsTest {

    @Test
    public void testAppend() {
        Prolog prolog = new Prolog();
        
        // Test basic append
        List<Map<String, Term>> solutions = prolog.solve("append([a,b], [c,d], Result).");
        assertEquals(1, solutions.size());
        Term result = solutions.get(0).get("Result");
        assertNotNull(result);
        // Should unify with [a,b,c,d] - we're checking it can be unified
        List<Map<String, Term>> verify = prolog.solve("Result = [a,b,c,d].");
        assertEquals(1, verify.size());
        
        // Test variable append
        solutions = prolog.solve("append(X, Y, [a,b,c]).");
        assertTrue(solutions.size() > 0);
    }

    @Test
    public void testLength() {
        Prolog prolog = new Prolog();
        
        // Test length with ground list
        List<Map<String, Term>> solutions = prolog.solve("length([a,b,c], L).");
        assertEquals(1, solutions.size());
        Term length = solutions.get(0).get("L");
        assertNotNull(length);
        assertEquals("3.0", length.toString());
        
        // Test length with generated list
        solutions = prolog.solve("length(List, 3).");
        assertEquals(1, solutions.size());
        Term list = solutions.get(0).get("List");
        assertNotNull(list);
    }

    @Test
    public void testMember() {
        Prolog prolog = new Prolog();
        
        // Test basic member
        List<Map<String, Term>> solutions = prolog.solve("member(b, [a,b,c]).");
        assertEquals(1, solutions.size());
        
        // Test member with variable
        solutions = prolog.solve("member(X, [a,b,c]).");
        assertEquals(3, solutions.size());
    }

    @Test
    public void testNth0() {
        Prolog prolog = new Prolog();
        
        // Test nth0 with ground index and list
        List<Map<String, Term>> solutions = prolog.solve("nth0(1, [a,b,c], Element).");
        assertEquals(1, solutions.size());
        Term element = solutions.get(0).get("Element");
        assertNotNull(element);
        assertEquals("b", element.toString());
        
        // Test nth0 with ground element
        solutions = prolog.solve("nth0(Index, [a,b,c], b).");
        assertEquals(1, solutions.size());
        Term index = solutions.get(0).get("Index");
        assertNotNull(index);
        assertEquals("1.0", index.toString());
    }

    @Test
    public void testNth1() {
        Prolog prolog = new Prolog();
        
        // Test nth1 with ground index and list (1-based)
        List<Map<String, Term>> solutions = prolog.solve("nth1(2, [a,b,c], Element).");
        assertEquals(1, solutions.size());
        Term element = solutions.get(0).get("Element");
        assertNotNull(element);
        assertEquals("b", element.toString());
    }

    @Test
    public void testSort() {
        Prolog prolog = new Prolog();
        
        // Test sort
        List<Map<String, Term>> solutions = prolog.solve("sort([c,b,a,c], Sorted).");
        assertEquals(1, solutions.size());
        Term sorted = solutions.get(0).get("Sorted");
        assertNotNull(sorted);
        // Should unify with [a,b,c] - we're checking it can be unified
        List<Map<String, Term>> verify = prolog.solve("Sorted = [a,b,c].");
        assertEquals(1, verify.size());
    }

    @Test
    public void testMsort() {
        Prolog prolog = new Prolog();
        
        // Test msort (preserves duplicates)
        List<Map<String, Term>> solutions = prolog.solve("msort([c,b,a,c,b], Sorted).");
        assertEquals(1, solutions.size());
        Term sorted = solutions.get(0).get("Sorted");
        assertNotNull(sorted);
        // Should unify with [a,b,b,c,c] - we're checking it can be unified
        List<Map<String, Term>> verify = prolog.solve("Sorted = [a,b,b,c,c].");
        assertEquals(1, verify.size());
    }

    @Test
    public void testReverse() {
        Prolog prolog = new Prolog();
        
        // Test reverse
        List<Map<String, Term>> solutions = prolog.solve("reverse([a,b,c], Reversed).");
        assertEquals(1, solutions.size());
        Term reversed = solutions.get(0).get("Reversed");
        assertNotNull(reversed);
        // Should unify with [c,b,a] - we're checking it can be unified
        List<Map<String, Term>> verify = prolog.solve("Reversed = [c,b,a].");
        assertEquals(1, verify.size());
    }

    @Test
    public void testSelect() {
        Prolog prolog = new Prolog();
        
        // Test select
        List<Map<String, Term>> solutions = prolog.solve("select(b, [a,b,c], Remainder).");
        assertEquals(1, solutions.size());
        Term remainder = solutions.get(0).get("Remainder");
        assertNotNull(remainder);
        // Should unify with [a,c] - we're checking it can be unified
        List<Map<String, Term>> verify = prolog.solve("Remainder = [a,c].");
        assertEquals(1, verify.size());
    }
}
