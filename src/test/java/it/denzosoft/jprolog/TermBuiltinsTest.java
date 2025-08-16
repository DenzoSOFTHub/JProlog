package it.denzosoft.jprolog;

import it.denzosoft.jprolog.terms.Atom;
import it.denzosoft.jprolog.terms.Term;
import it.denzosoft.jprolog.terms.Variable;
import org.junit.Test;
import static org.junit.Assert.*;

import java.util.List;
import java.util.Map;



public class TermBuiltinsTest {

    @Test
    public void testTermComparison() {
        Prolog prolog = new Prolog();
        
        // Test @<
        List<Map<String, Term>> solutions = prolog.solve("a @< b.");
        assertEquals(1, solutions.size());
        
        solutions = prolog.solve("b @< a.");
        assertEquals(0, solutions.size());
        
        // Test ==
        solutions = prolog.solve("a == a.");
        assertEquals(1, solutions.size());
        
        solutions = prolog.solve("a == b.");
        assertEquals(0, solutions.size());
        
        // Test \==
        solutions = prolog.solve("a \\== b.");
        assertEquals(1, solutions.size());
        
        solutions = prolog.solve("a \\== a.");
        assertEquals(0, solutions.size());
    }

    @Test
    public void testFunctor() {
        Prolog prolog = new Prolog();
        
        // Test functor extraction
        List<Map<String, Term>> solutions = prolog.solve("functor(f(a,b), F, A).");
        assertEquals(1, solutions.size());
        assertEquals("f", solutions.get(0).get("F").toString());
        assertEquals("2.0", solutions.get(0).get("A").toString());
        
        // Test functor construction
        solutions = prolog.solve("functor(T, hello, 2).");
        assertEquals(1, solutions.size());
        assertNotNull(solutions.get(0).get("T"));
    }

    @Test
    public void testArg() {
        Prolog prolog = new Prolog();
        
        // Test arg extraction
        List<Map<String, Term>> solutions = prolog.solve("arg(2, f(a,b,c), X).");
        assertEquals(1, solutions.size());
        assertEquals("b", solutions.get(0).get("X").toString());
    }

    @Test
    public void testUniv() {
        Prolog prolog = new Prolog();
        
        // Test =.. with compound term
        List<Map<String, Term>> solutions = prolog.solve("f(a,b) =.. L.");
        assertEquals(1, solutions.size());
        // Verify that L can be unified with [f,a,b]
        List<Map<String, Term>> verify = prolog.solve("L = [f,a,b].");
        assertEquals(1, verify.size());
        
        // Test =.. constructing term
        solutions = prolog.solve("T =.. [f,a,b].");
        assertEquals(1, solutions.size());
        // Verify that T can be unified with f(a,b)
        verify = prolog.solve("T = f(a,b).");
        assertEquals(1, verify.size());
    }

    @Test
    public void testCopyTerm() {
        Prolog prolog = new Prolog();
        
        // Test copy_term
        List<Map<String, Term>> solutions = prolog.solve("copy_term(f(A,B), Copy), A = 1, B = 2.");
        assertEquals(1, solutions.size());
        assertNotNull(solutions.get(0).get("Copy"));
    }

    @Test
    public void testArithmeticComparison() {
        Prolog prolog = new Prolog();
        
        // Test =:=
        List<Map<String, Term>> solutions = prolog.solve("1 + 2 =:= 3.");
        assertEquals(1, solutions.size());
        
        solutions = prolog.solve("1 + 2 =:= 4.");
        assertEquals(0, solutions.size());
        
        // Test =\=
        solutions = prolog.solve("1 + 2 =\\= 4.");
        assertEquals(1, solutions.size());
        
        solutions = prolog.solve("1 + 2 =\\= 3.");
        assertEquals(0, solutions.size());
        
        // Test <, =<, >, >=
        solutions = prolog.solve("1 < 2.");
        assertEquals(1, solutions.size());
        
        solutions = prolog.solve("2 =< 2.");
        assertEquals(1, solutions.size());
        
        solutions = prolog.solve("3 > 2.");
        assertEquals(1, solutions.size());
        
        solutions = prolog.solve("2 >= 2.");
        assertEquals(1, solutions.size());
    }
}
